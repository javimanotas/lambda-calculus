module State where

import LambdaExpr

import qualified Data.Map as Map

type Env = Map.Map String LambdaExpr

data ReplResult = Definition String LambdaExpr
                | Expr LambdaExpr