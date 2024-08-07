module LambdaExpr ( LambdaExpr(..), Identifier(..), nextId ) where


newtype Identifier = Id (String, Int) deriving (Eq, Ord)

instance Show Identifier where
    show (Id (str, 0)) = str
    show (Id (str, n)) = str ++ show n

instance Read Identifier where
    readsPrec _ str = [(Id (str, 0), "")]

nextId :: Identifier -> Identifier
nextId (Id (str, n)) = Id (str, n + 1)


data LambdaExpr = Var Identifier
            | Abstr Identifier LambdaExpr
            | Appl LambdaExpr LambdaExpr

instance Show LambdaExpr where
    show (Var x)     = show x
    show (Abstr x y) = "λ" ++ show x ++ showAbstr y
        -- support reduced form
        where showAbstr (Abstr x y) = ' ' : show x ++ showAbstr y
              showAbstr other       = '.' : show other
    show (Appl x y)  = x' ++ ' ' : y'
        where x' = case x of
                    a@(Abstr _ _) -> '(' : show a ++ ")"
                    other         -> show other
              y' = case y of
                    v@(Var _) -> show v
                    other -> '(' : show other ++ ")"

instance Eq LambdaExpr where
    (==) _ _ = False -- TODO 