module Parsing.Tokenizer ( Token(..), tokenize, consumeBracket ) where

import Data.Char

data Token = LeftBracket
           | RightBracket
           | Lambda
           | Dot
           | Equal
           | Name String
           deriving Eq

instance Show Token where
    show LeftBracket  = "("
    show RightBracket = ")"
    show Lambda       = "λ"
    show Dot          = "."
    show Equal        = "="
    show (Name str)   = str

symbols = [('(', LeftBracket)
          ,(')', RightBracket)
          ,('\\', Lambda)
          ,('λ', Lambda)
          ,('.', Dot)
          ,('=', Equal)]

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

tokenize :: String -> Either String [Token]
tokenize ""       = Right []
tokenize (' ':xs) = tokenize xs
tokenize l@(x:xs) = case x `lookup` symbols of
    Just s -> (s:) <$> tokenize xs
    Nothing -> if not $ isAlpha x
        then Left ("Invalid character \"" ++ x : "\"" )
        else
            let (consumed, unconsumed) = span isAlphaNum l in
            (Name consumed:) <$> tokenize unconsumed