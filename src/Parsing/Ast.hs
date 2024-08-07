module Parsing.Ast ( SyntaxTree(..), flatten) where

{- Used as a middle point to convert from all the tokens
to the tree used for evaluating expressions -}

data SyntaxTree = Leaf String
                | Nested [SyntaxTree]
                | Node String SyntaxTree
                deriving Show

instance Semigroup SyntaxTree where
    Nested l <> Nested r = Nested (l <> r)
    Nested l <> other    = Nested (l <> [other])
    other    <> Nested r = Nested (other : r)
    otherl   <> otherr   = Nested (otherl : [otherr])

flatten :: SyntaxTree -> SyntaxTree
flatten l@(Leaf _) = l
flatten (Nested [n]) = flatten n
flatten (Nested l) = Nested $ map flatten l
flatten (Node t s) = Node t $ flatten s