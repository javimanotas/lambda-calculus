module Main ( main ) where

import Parser
import Eval
import LambdaExpr
import qualified Enviroment as Env
import Repl

import Control.Monad
import Control.Monad.State
import Control.Exception

main :: IO ()
main = do
    putStrLn "Lambda calculus interactive REPL"
    putStrLn "Enter :? for help"
    env <- execStateT (loadFile "lambdaenv") Env.empty
    evalStateT runRepl env


runRepl :: StateT Env.Env IO ()
runRepl = do
    liftIO $ putStr "λ> "
    line <- liftIO getLine
    if isCommand line
    then do
        runCommand line
        runRepl
    else do
        evalLine True line
        runRepl


evalLine :: Bool -> String -> StateT Env.Env IO ()
evalLine show line =
    unless (null line) $ do
        env <- get
        case parseLine line env of
            Left e -> liftIO $ putStrLn $ "Error: " ++ e
            Right (Evaluate e) ->
                let result = eval e in
                liftIO $ do
                    print result
                    when show $ showMatches result env
            Right (Definition s e) -> do
                modify (Env.add s e)
                liftIO $ when show $ showMatches e env
    where
        showMatches :: LambdaExpr -> Env.Env -> IO ()
        showMatches lambda env =
            let definitions = Env.content env
                matches = [name | (name, exp) <- definitions, exp == lambda]
            in case matches of
                [] -> return ()
                l -> putStrLn $ "Matches: " ++ foldl1 (\a b -> a ++ ", " ++ b) l


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


loadFile :: FilePath -> StateT Env.Env IO ()
loadFile file = do
    content <- liftIO $ lines <$> (readFile file `catch` handler)
    mapM_ (evalLine False) content
  where
    handler :: SomeException -> IO String
    handler = const $ return ""


tryWriteFile :: Show p => p -> String -> IO ()
tryWriteFile env file = action `catch` (print :: SomeException -> IO ())
    where
        action = writeFile file (show env)