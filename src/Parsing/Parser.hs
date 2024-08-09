module Parsing.Parser ( parse ) where

import LambdaExpr
import State
import Parsing.Tokenizer
import Parsing.Ast
import Eval

import Control.Monad
import Data.List
import Data.Char
import Data.Function
import qualified Data.Map as Map
import qualified Data.Set as Set


parse :: String -> Env -> (Either String ReplResult, Env)
parse input env =
    let parsed = do
            tokens <- tokenize input
            if Equal `notElem` tokens
                then Expr <$> (makeTree tokens >>= parseTree env)
            else 
                let grouped = groupBy ((==) `on` (== Equal)) tokens in
                case grouped of
                    [[Name str@(x:xs)], [Equal], tokens] -> do
                        when (isAsciiLower x) $
                            Left "Definitions must start with uppercase"
                        sTree <- makeTree tokens
                        tree <- parseTree env sTree
                        return $ Definition str tree
                    _ -> Left "Syntax error on assignement" in
    (parsed, insertMaybe parsed env)
    where
        insertMaybe (Right (Definition name var)) map = Map.insert name var map
        insertMaybe _                             map = map


consumeBracket :: [Token] -> Either String ([Token], [Token])
consumeBracket = go 0 []
    where
          go _ _   []                = Left "Unmatched \")\""
          go 0 acc (RightBracket:xs) = return (reverse acc, xs)
          go n acc (x:xs)            = go (n + m) (x:acc) xs
                where
                    m = case x of
                        LeftBracket -> 1
                        RightBracket -> -1
                        _ -> 0


makeTree :: [Token] -> Either String SyntaxTree
makeTree (LeftBracket:xs) = do
    (consumed, unconsumed) <- consumeBracket xs
    do
        l <- makeTree consumed
        r <- makeTree unconsumed
        return $ Nested [l] <> r
makeTree (Lambda:xs) =
    let (consumed, unconsumed) = break (==Dot) xs in
    case unconsumed of
         [] -> Left "Unmatched \"λ\""
         [Dot] -> Left "Missing body of lambda"
         (_:xs) -> do
            r <- makeTree xs
            let vars = reverse consumed
            let folding b (Name n) = return $ Node n b
                folding b _ = Left "asd"
            foldM folding r vars
makeTree [Name n] = return $ Leaf n
makeTree ((Name n) : xs) = (Leaf n <>) <$> makeTree xs
makeTree (other:_) = Left $ "Unexpected \"" ++ show other ++ "\""
makeTree [] = return $ Nested []


parseTree :: Env -> SyntaxTree -> Either String LambdaExpr
parseTree env sTree = do
    tree <- parseTree' sTree
    foldM replaceVar tree $ definedVars sTree
    where
        definedVars :: SyntaxTree -> Set.Set String
        definedVars (Leaf str@(x:xs))
            | isAsciiUpper x = Set.singleton str
            | otherwise      = Set.empty
        definedVars (Nested l) = foldMap definedVars l
        definedVars (Node _ x) = definedVars x
        
        parseTree' :: SyntaxTree -> Either String LambdaExpr
        parseTree' (Leaf x) = return $ Var $ read x
        parseTree' (Nested l) = do
            subTrees <- mapM parseTree' l
            return $ foldl1 Appl subTrees
        parseTree' (Node str@(x:xs) y)
            | isAsciiUpper x = Left "Lambda parameter can't start with capital letter"
            | otherwise      = Abstr (read str) <$> parseTree' y

        replaceVar :: LambdaExpr -> String -> Either String LambdaExpr
        replaceVar lambda var = case env Map.!? var of
            Nothing -> Left $ "Undefined variable " ++ var
            Just expr -> Right $ replace Set.empty (read var) expr lambda