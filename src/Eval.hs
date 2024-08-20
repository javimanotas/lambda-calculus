module Eval ( normalForm, betaReduction ) where

import LambdaExpr
import Data.List
import qualified Data.Set as Set
import Data.Function


-- Tries calculating the normal form of a lambda expression
-- Warning: if the expression doesn't have normal form this function won't halt
normalForm :: LambdaExpr -> LambdaExpr
normalForm = simplifyRename . eval'
    where
        eval' expr = case tryReduce expr of
            Var v   -> Var v
            Abstr v m -> Abstr v (eval' m)
            Appl m n -> Appl (eval' m) (eval' n)
            where
                tryReduce appl@(Appl m a) = case tryReduce m of
                    Abstr v f -> tryReduce $ betaReduction v a f
                    _ -> appl
                tryReduce other = other

        simplifyRename expr = -- Tries undoing the names ocurred on beta reduction
            let bounded = Set.toAscList $ identifiers expr
                grouped = groupBy ((==) `on` name) bounded
                renames = concatMap (zipWith (\i x -> x {index = i} ) [0..]) grouped
            in foldl (flip $ uncurry change) expr $ zip bounded renames


-- replaces var with exp in lambda
betaReduction :: Identifier -> LambdaExpr -> LambdaExpr -> LambdaExpr
betaReduction = replace' Set.empty
    where
        replace' inBounded var expr (Var x)
            | var == x  = alphaRename inBounded expr
            | otherwise = Var x
        replace' inBounded var expr (Appl x y) = Appl x' y'
            where
                x' = replace' inBounded var expr x
                y' = replace' inBounded var expr y
        replace' inBounded var expr a@(Abstr x y)
            | var == x  = a
            | otherwise = Abstr x $ replace' (Set.insert x inBounded) var expr y


alphaRename :: Set.Set Identifier -> LambdaExpr -> LambdaExpr
alphaRename set expr =
    let collisions = Set.intersection set $ identifiers expr
        bounded = Set.union set collisions
    in case Set.lookupMin collisions of
        Nothing -> expr
        Just x ->
            let newX = until (`Set.notMember` bounded) nextId x in
            alphaRename set $ change x newX expr


identifiers :: LambdaExpr -> Set.Set Identifier
identifiers (Var x)     = Set.singleton x
identifiers (Appl x y)  = identifiers x `Set.union` identifiers y
identifiers (Abstr x y) = Set.insert x $ identifiers y


change :: Identifier -> Identifier -> LambdaExpr -> LambdaExpr
change old new (Var x)     = Var $ substitute old new x
change old new (Appl x y)  = Appl (change old new x) (change old new y)
change old new (Abstr x y) = Abstr (substitute old new x) $ change old new y


substitute :: Eq p => p -> p -> p -> p
substitute old new var
    | var == old = new
    | otherwise  = var