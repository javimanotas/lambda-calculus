module Parsing.Parser ( parse ) where

import LambdaExpr
import qualified Enviroment as Env
import Repl
import Parsing.Tokenizer
import Parsing.ParsingTree
import Eval

import Control.Monad
import Data.List
import Data.Char
import Data.Function
import qualified Data.Set as Set
import Control.Monad.State

parse :: String -> State Env.Env (Either String Result)
parse input = do
    env <- get
    let parsed = do
            tokens <- tokenize input
            if Equal `notElem` tokens
                then Expr <$> getTree tokens env
            else
                let grouped = groupBy ((==) `on` (== Equal)) tokens in
                case grouped of
                    [[Name str@(x:xs)], [Equal], tokens] -> do
                        when (isAsciiLower x) $
                            Left "Definitions must start with uppercase"
                        tree <- getTree tokens env
                        return $ Definition str tree
                    _ -> Left "Syntax error on assignment"
    case parsed of
        (Right (Definition name var)) -> modify (Env.add name var)
        _ -> return ()
    return parsed

getTree :: [Token] -> Env.Env -> Either String LambdaExpr
getTree tokens env = makeTree tokens >>= parseTree env


makeTree :: [Token] -> Either String ParsingTree
makeTree (LeftBracket:xs) = do
    (consumed, unconsumed) <- consumeBracket xs
    do
        l <- makeTree consumed
        r <- makeTree unconsumed
        return $ Nested [l] <> r -- nested gives priority
makeTree (Lambda:xs) =
    let (consumed, unconsumed) = break (==Dot) xs in
    case unconsumed of
         [] -> Left "Unmatched \"λ\""
         [Dot] -> Left "Missing body of lambda"
         (Dot:xs) -> do
            r <- makeTree xs
            -- reverse because foldM acts like foldMl but a foldMr was required
            foldM folding r $ reverse consumed
                where
                    folding b (Name n) = return $ Node n b
                    folding _ token    = Left $ "Unexpected \"" ++ show token ++ "\""
makeTree (Name n : xs) = (Leaf n <>) <$> makeTree xs
makeTree (other:_) = Left $ "Unexpected \"" ++ show other ++ "\""
makeTree [] = return $ Nested []


parseTree :: Env.Env -> ParsingTree -> Either String LambdaExpr
parseTree env sTree = do
    tree <- parseTree' sTree
    foldM replaceVar tree $ definedVars sTree
    where
        parseTree' :: ParsingTree -> Either String LambdaExpr
        parseTree' (Leaf x) = return $ Var $ read x
        parseTree' (Nested l) = do
            subTrees <- mapM parseTree' l
            return $ foldl1 Appl subTrees
        parseTree' (Node str@(x:xs) y)
            | isAsciiUpper x = Left "Lambda parameter can't start with capital letter"
            | otherwise      = Abstr (read str) <$> parseTree' y
        
        definedVars :: ParsingTree -> Set.Set String
        definedVars (Leaf str@(x:xs))
            | isAsciiUpper x = Set.singleton str
            | otherwise      = Set.empty
        definedVars (Nested l) = foldMap definedVars l
        definedVars (Node _ x) = definedVars x
        
        replaceVar :: LambdaExpr -> String -> Either String LambdaExpr
        replaceVar lambda var = case Env.get var env of
            Nothing -> Left $ "Undefined variable " ++ var
            Just expr -> Right $ replace Set.empty (read var) expr lambda