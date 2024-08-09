module Enviroment ( Env, empty, add, get ) where

import LambdaExpr

import qualified Data.Map as Map


type Env = Map.Map String LambdaExpr

empty :: Env
empty = Map.empty

add :: Ord k => k -> a -> Map.Map k a -> Map.Map k a
add = Map.insert

get :: Ord k => k -> Map.Map k a -> Maybe a
get = Map.lookup