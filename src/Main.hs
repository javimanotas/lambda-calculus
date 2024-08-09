module Main ( main ) where

import Parsing.Parser
import Eval
import State

import Control.Monad
import qualified Data.Map as Map


runRepl :: Env -> IO ()
runRepl env = do
    putStr "λ> "
    line <- getLine
    unless (line == "") (do
        let (result, env') = parse line env
        case result of
            Left e -> print $ "Error: " ++ e
            Right (Expr e) -> print $ eval e
            _ -> return ()
        runRepl env')


main :: IO ()
main = runRepl Map.empty