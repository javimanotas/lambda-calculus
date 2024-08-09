module Repl ( Result(..) ) where

import LambdaExpr

data Result = Definition String LambdaExpr
            | Expr LambdaExpr