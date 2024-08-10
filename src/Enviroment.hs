module Enviroment ( Env, empty, add, get ) where

import LambdaExpr

import qualified Data.Map as Map


type Env = Map.Map String LambdaExpr

empty :: Env
empty = Map.empty

add :: String -> LambdaExpr -> Env -> Env
add = Map.insert

get :: String -> Env -> Maybe LambdaExpr
get = Map.lookup