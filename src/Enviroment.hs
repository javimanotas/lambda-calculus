module Enviroment ( Env, empty, add, get, remove ) where

import LambdaExpr

import qualified Data.Map as Map


newtype Env = Env { getEnv :: Map.Map String LambdaExpr }

instance Show Env where
    show env =
        let (keys, values) = unzip $ Map.toList $ getEnv env
            exprs = map show values in
        unlines $ zipWith (\a b -> a ++ " = " ++ b) keys exprs

empty :: Env
empty = Env Map.empty

add :: String -> LambdaExpr -> Env -> Env
add str lambda = Env . Map.insert str lambda . getEnv

get :: String -> Env -> Maybe LambdaExpr
get str = Map.lookup str . getEnv

remove :: String -> Env -> Env
remove str = Env . Map.delete str . getEnv