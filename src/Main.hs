module Main ( main ) where

import Parsing.Parser
import Eval
import qualified Enviroment as Env
import Repl

import Control.Monad
import Control.Monad.State
import Control.Exception

main :: IO ()
main = do
    env <- execStateT (loadFile "env.lambda") Env.empty
    evalStateT runRepl env


runRepl :: StateT Env.Env IO ()
runRepl = do
    liftIO $ putStr "λ> "
    line <- liftIO getLine
    if isCommand line
        then runCommand line
    else do
        evalLine line
        runRepl


evalLine :: String -> StateT Env.Env IO ()
evalLine line =
    unless (null line) $ do
        env <- get
        let (result, env') = runState (parse line) env
        put env'
        case result of
            Left e -> liftIO $ putStrLn $ "Error: " ++ e
            Right (Expr e) -> liftIO $ print $ eval e
            _ -> return ()


runCommand :: String -> StateT Env.Env IO ()
runCommand line = do
    let (command, args) = parseCommand line
    if command == Just Quit
        then return ()
    else do
        env <- get
        case command of
            
            Nothing -> liftIO $ do
                putStrLn $ "Command " ++ takeWhile (/= ' ') line ++ " not found"
                putStrLn "Enter :? for help"
            
            Just Quit -> return ()
            
            Just Help -> liftIO $ do
                putStrLn "Commands:"
                mapM_ print descriptions
            
            Just Load -> mapM_ loadFile args
            
            Just Save -> liftIO $ case args of
                [file] -> tryWriteFile env file
                _ -> do
                    putStrLn "Invalid number of arguments for :s"
                    putStrLn "Enter :? for help"
            
            Just Print -> case args of
                [arg] -> liftIO $ case Env.get arg env of
                            Nothing -> putStrLn $ "Error: Undefined var " ++ arg
                            Just x -> print x
                _ -> liftIO $ do
                    putStrLn "Invalid number of arguments for :p"
                    putStrLn "Enter :? for help"
            
            Just Remove -> modify (`removeVars` args)
                where removeVars = foldl (flip Env.remove)

        runRepl


loadFile :: FilePath -> StateT Env.Env IO ()
loadFile file = do
    content <- liftIO $ lines <$> (readFile file `catch` handler)
    mapM_ evalLine content
  where
    handler :: SomeException -> IO String
    handler = const $ return ""


tryWriteFile :: Show p => p -> String -> IO ()
tryWriteFile env file = action `catch` (print :: SomeException -> IO ())
    where
        action = writeFile file (show env)