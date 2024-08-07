module Parsing.Parser ( parse ) where

import LambdaExpr
import Parsing.Tokenizer
import Parsing.Ast

import Control.Monad


parse :: String -> Either String LambdaExpr
parse = (fmap (parseTree . flatten) <$> makeTree) <=< tokenize


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


parseTree :: SyntaxTree -> LambdaExpr
parseTree (Leaf x) = Var $ read x
parseTree (Nested l) = foldl1 Appl (map parseTree l)
parseTree (Node x y) = Abstr (read x) (parseTree y)