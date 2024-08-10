module Main ( main ) where

import Parsing.Parser
import Eval
import qualified Enviroment as Env
import Repl

import Control.Monad
import Control.Monad.State
import Control.Exception

main :: IO ()
main = loadFile Env.empty "env.lambda" >>= runRepl


runRepl :: Env.Env -> IO ()
runRepl env = do
    putStr "λ> "
    line <- getLine
    if isCommand line
        then runCommand line env
    else 
        evalLine env line >>= runRepl


evalLine :: Env.Env -> String -> IO Env.Env
evalLine env line =
    if null line
        then return env
        else do
            let (result, env') = runState (parse line) env
            case result of
                Left e -> putStrLn $ "Error: " ++ e
                Right (Expr e) -> print $ eval e
                _ -> return ()
            return env'


runCommand :: String -> Env.Env -> IO ()
runCommand line env = do
    let command = parseCommand line
    case command of
        Just (Quit, _) -> return ()
        _ -> do
            runRepl =<< case command of
                
                Nothing -> do
                    putStrLn $ "Command " ++ takeWhile (/= ' ') line ++ " not found"
                    putStrLn "Enter :? for help"
                    return env

                Just (Help, _) -> do
                    putStrLn "Commands:"
                    mapM_ print descriptions
                    return env
                
                Just (Run, l) -> foldM loadFile env l
                
                Just (Print, [arg]) -> do
                        case Env.get arg env of
                            Nothing -> putStrLn ("Error: Undefined var " ++ arg)
                            Just x -> print x
                        return env
                Just (Print, _) -> do
                            putStrLn "Invalid number of arguments for :p"
                            putStrLn "Enter :? for help"
                            return env


loadFile :: Env.Env -> FilePath -> IO Env.Env
loadFile env file = catch action handler
    where
        action = do
            content <- lines <$> readFile file
            putStrLn $ "Running " ++ file
            foldM evalLine env content
        
        handler :: SomeException -> IO Env.Env
        handler e = do
            print e
            return env