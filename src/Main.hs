module Main ( main ) where

import Parser
import Eval
import LambdaExpr
import qualified Enviroment as Env
import Repl

import Control.Monad
import Control.Monad.State
import Control.Exception
import System.IO

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    putStrLn "Lambda calculus interactive REPL"
    putStrLn "Enter :? for help"
    env <- execStateT (loadFile "src/lambdaenv") Env.empty
    evalStateT runRepl env


runRepl :: StateT Env.Env IO ()
runRepl = do
    lift $ putStr "λ> "
    line <- lift getLine
    if isCommand line
    then
        runCommand line
    else do
        evalLine True line
        runRepl


evalLine :: Bool -> String -> StateT Env.Env IO ()
evalLine showMatches line = do
    env <- get
    case parseLine line env of
        Left e -> lift $ putStrLn $ "Error: " ++ e
        Right None -> return ()
        Right (Evaluate e) ->
            let result = eval e in
            lift $ do
                print result
                when showMatches $ matches result env
        Right (Definition s e) -> do
            modify (Env.add s e)
            lift $ when showMatches $ matches e env
    where
        matches :: LambdaExpr -> Env.Env -> IO ()
        matches lambda env =
            let definitions = Env.content env
                matching = [alias | (alias, expr) <- definitions, expr == lambda] ++ case matchedChurch lambda of
                    Nothing -> []
                    Just x -> [show x]
            in case matching of
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
            Just Help -> lift $ do
                putStrLn "Commands:"
                mapM_ print descriptions
            
            Just Load -> mapM_ loadFile args
            
            Just Save -> lift $ case args of
                [file] -> tryWriteFile env file
                _ -> do
                    putStrLn "Invalid number of arguments for :s"
                    putStrLn "Enter :? for help"
            
            Just Print -> lift $ case args of
                [arg] -> case Env.get arg env of
                            Nothing -> putStrLn $ "Error: Undefined var " ++ arg
                            Just x -> print x
            
            Just Remove -> modify (`removeVars` args)
                where removeVars = foldl (flip Env.remove)

            Just Env -> case map fst $ Env.content env of
                            [] -> return ()
                            l -> lift $ putStrLn $ foldl1 (\a b -> a ++ ", " ++ b) l

            _ -> lift $ do
                putStrLn $ "Command " ++ takeWhile (/= ' ') line ++ " not found"
                putStrLn "Enter :? for help"
        runRepl


loadFile :: FilePath -> StateT Env.Env IO ()
loadFile file = do
    content <- lift $ lines <$> (readFile file `catch` handler)
    mapM_ (evalLine False) content
  where
    handler :: SomeException -> IO String
    handler = const $ return ""


tryWriteFile :: Show p => p -> String -> IO ()
tryWriteFile env file = action `catch` (print :: SomeException -> IO ())
    where
        action = writeFile file (show env)