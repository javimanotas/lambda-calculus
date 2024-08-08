module Eval ( eval ) where

import LambdaExpr

import Data.List
import Data.Function
import qualified Data.Set as Set
import qualified Data.Map as Map


eval :: LambdaExpr -> LambdaExpr
eval = simplifyRename . eval' Set.empty
    where
        eval' _   (Var x)     = Var x
        eval' set (Abstr x y) = Abstr x $ eval' (Set.insert x set) y
        eval' set (Appl x y)  =
            let x' = eval' set x in
            let y' = eval' set y in
            case x' of
                Abstr z w -> eval' set $ replace set z y' w
                _         -> Appl x' y'


-- replaces var with exp in lambda
replace :: Set.Set Identifier -> Identifier -> LambdaExpr -> LambdaExpr -> LambdaExpr
replace bounded var exp (Var x)
    | var == x  = alphaRename bounded exp
    | otherwise = Var x
replace bounded var exp (Appl x y) = Appl x' y'
    where
        x' = replace bounded var exp x
        y' = replace bounded var exp y
replace bounded var exp a@(Abstr x y)
    | var == x  = a
    | otherwise = Abstr x $ replace (Set.insert x bounded) var exp y


alphaRename :: Set.Set Identifier -> LambdaExpr -> LambdaExpr
alphaRename set exp =
    let bounded = boundedVars exp in
    let collisions = Set.intersection set bounded in
    case Set.lookupMin collisions of
        Nothing -> exp
        Just x ->
            let newX = until ((&&) <$> (not . (`Set.member` bounded)) <*> (not . (`Set.member` set))) nextId x in
            change x newX exp

boundedVars :: LambdaExpr -> Set.Set Identifier
boundedVars (Var x) = Set.singleton x
boundedVars (Appl x y) = boundedVars x `Set.union` boundedVars y
boundedVars (Abstr x y) = Set.insert x $ boundedVars y

change :: Identifier -> Identifier -> LambdaExpr -> LambdaExpr
change old new (Var x) = Var $ substitute old new x
change old new (Appl x y) = Appl (change old new x) (change old new y)
change old new (Abstr x y) = Abstr (substitute old new x) $ change old new y


substitute :: Eq p => p -> p -> p -> p
substitute old new var = if var == old then new else var


simplifyRename :: LambdaExpr -> LambdaExpr
simplifyRename expr =
    let bounded = Set.toAscList $ boundedVars expr in -- its sorted
    let grouped = groupBy ((==) `on` name) bounded in
    let renames = concatMap (zipWith (\i x -> x {index = i} ) [0..]) grouped in
    foldl (flip $ uncurry change) expr $ zip bounded renames