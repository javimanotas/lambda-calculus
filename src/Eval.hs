module Eval ( eval, betaReduction ) where

import LambdaExpr
import Data.List
import qualified Data.Set as Set
import Data.Function


eval :: LambdaExpr -> LambdaExpr
eval = simplifyRename . eval'
    where
        eval' expr = case tryReduce expr of
            Var v   -> Var v
            Abstr v m -> Abstr v (eval' m)
            Appl m n -> Appl (eval' m) (eval' n)
            where
                tryReduce appl@(Appl m a) = case tryReduce m of
                    Abstr v f -> tryReduce $ betaReduction v a f
                    _ -> appl
                tryReduce term = term


-- replaces var with exp in lambda
betaReduction :: Identifier -> LambdaExpr -> LambdaExpr -> LambdaExpr
betaReduction = replace' Set.empty
    where
        replace' inBounded var exp (Var x)
            | var == x  = alphaRename inBounded exp
            | otherwise = Var x
        replace' inBounded var exp (Appl x y) = Appl x' y'
            where
                x' = replace' inBounded var exp x
                y' = replace' inBounded var exp y
        replace' inBounded var exp a@(Abstr x y)
            | var == x  = a
            | otherwise = Abstr x $ replace' (Set.insert x inBounded) var exp y


alphaRename :: Set.Set Identifier -> LambdaExpr -> LambdaExpr
alphaRename set exp =
    let collisions = Set.intersection set $ identifiers exp
        bounded = Set.union set collisions
    in case Set.lookupMin collisions of
        Nothing -> exp
        Just x ->
            let newX = until (`Set.notMember` bounded) nextId x in
            alphaRename set $ change x newX exp


identifiers :: LambdaExpr -> Set.Set Identifier
identifiers (Var x) = Set.singleton x
identifiers (Appl x y) = identifiers x `Set.union` identifiers y
identifiers (Abstr x y) = Set.insert x $ identifiers y


change :: Identifier -> Identifier -> LambdaExpr -> LambdaExpr
change old new (Var x) = Var $ substitute old new x
change old new (Appl x y) = Appl (change old new x) (change old new y)
change old new (Abstr x y) = Abstr (substitute old new x) $ change old new y


substitute :: Eq p => p -> p -> p -> p
substitute old new var
    | var == old = new
    | otherwise  = var


simplifyRename :: LambdaExpr -> LambdaExpr
simplifyRename expr =
    let bounded = Set.toAscList $ identifiers expr -- its sorted
        grouped = groupBy ((==) `on` name) bounded
        renames = concatMap (zipWith (\i x -> x {index = i} ) [0..]) grouped
    in foldl (flip $ uncurry change) expr $ zip bounded renames