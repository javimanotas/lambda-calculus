module Main ( main ) where

import Parsing.Parser
import Eval
import State

import Control.Monad
import Control.Monad.State
import qualified Data.Map as Map


runRepl :: Env -> IO ()
runRepl env = do
    putStr "λ> "
    line <- getLine
    unless (line == "") (do
        let (result, env') = runState (parse line) env
        case result of
            Left e -> putStrLn $ "Error: " ++ e
            Right (Expr e) -> print $ eval e
            _ -> return ()
        runRepl env')


main :: IO ()
main = runRepl Map.empty