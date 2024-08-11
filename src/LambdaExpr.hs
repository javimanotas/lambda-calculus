module LambdaExpr ( LambdaExpr(..), Identifier(..), nextId ) where

import Data.Char
import Data.Function
import qualified Data.Map as Map

data Identifier = Id { name :: String
                     , index :: Int }
                     deriving (Eq, Ord)

instance Show Identifier where
    show (Id { name = str, index = 0 }) = str
    show (Id { name = str, index = n }) = str ++ show n

instance Read Identifier where
    readsPrec _ str =
        let (revNums, revStr) = span isDigit $ reverse str
            name = reverse revStr
            num = case revNums of
                [] -> 0
                xs -> read $ reverse xs in
        [(Id { name = name, index = num }, "")]

nextId :: Identifier -> Identifier
nextId id = id { index = index id + 1}


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

data BrujinIdx = BVar (Maybe Int)
               | BAbstr BrujinIdx
               | BAppl BrujinIdx BrujinIdx
               deriving Eq

instance Eq LambdaExpr where
    (==) = (==) `on` toBrujin
        where
            toBrujin :: LambdaExpr -> BrujinIdx
            toBrujin = toBrujin' Map.empty
                where
                    toBrujin' m (Var x) = BVar $ Map.lookup x m
                    toBrujin' m (Appl x y) = BAppl (toBrujin' m x) (toBrujin' m y)
                    toBrujin' m (Abstr x y) =
                        let inserted = Map.insert x 0 m
                            m' = fmap (+1) inserted in
                        BAbstr $ toBrujin' m' y