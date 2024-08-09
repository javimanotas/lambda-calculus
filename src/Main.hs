module Main ( main ) where

import Parsing.Parser
import Eval
import qualified Enviroment as Env
import Repl

import Control.Monad
import Control.Monad.State


runRepl :: Env.Env -> IO ()
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
main = runRepl Env.empty