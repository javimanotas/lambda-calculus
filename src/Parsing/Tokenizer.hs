module Parsing.Tokenizer ( Token(..), isBracket, tokenize ) where

import Data.Char

data Token = LeftBracket
           | RightBracket
           | Lambda
           | Dot
           | Equal
           | Name String
           deriving Eq

isBracket :: Token -> Bool
isBracket LeftBracket  = True
isBracket RightBracket = True
isBracket _            = False

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