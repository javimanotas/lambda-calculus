module Main where

import Parsing.Parser
import Eval

import Control.Monad

main :: IO ()
main = do
    putStr "λ> "
    line <- getLine
    when (line == "")
        main
    case parse line of
        Left err -> print err
        Right val -> print $ eval val
    main