module Eval ( eval, replace ) where

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
            let x' = eval' set x
                y' = eval' set y in
            case x' of
                Abstr z w -> eval' set $ replace set z y' w
                _         -> Appl x' y'


-- replaces var with exp in lambda
replace :: Set.Set Identifier -> Identifier -> LambdaExpr -> LambdaExpr -> LambdaExpr
replace = replace' Set.empty
    where
        replace' inBounded outBounded var exp (Var x)
            | var == x  = alphaRename inBounded outBounded exp
            | otherwise = Var x
        replace' inBounded outBounded var exp (Appl x y) = Appl x' y'
            where
                x' = replace' inBounded outBounded var exp x
                y' = replace' inBounded outBounded var exp y
        replace' inBounded outBounded var exp a@(Abstr x y)
            | var == x  = a
            | otherwise = Abstr x $ replace' (Set.insert x inBounded) outBounded var exp y


alphaRename :: Set.Set Identifier -> Set.Set Identifier -> LambdaExpr -> LambdaExpr
alphaRename inBounded outBounded exp =
    let bounded = boundedVars exp
        collisions = Set.intersection inBounded bounded
        set = Set.union inBounded outBounded in
    case Set.lookupMin collisions of
        Nothing -> exp
        Just x ->
            let newX = until ((&&) <$> (`Set.notMember` bounded) <*> (`Set.notMember` set)) nextId x in
            alphaRename inBounded outBounded $ change x newX exp


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
    let bounded = Set.toAscList $ boundedVars expr -- its sorted
        grouped = groupBy ((==) `on` name) bounded
        renames = concatMap (zipWith (\i x -> x {index = i} ) [0..]) grouped in
    foldl (flip $ uncurry change) expr $ zip bounded renames