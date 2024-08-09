module Parsing.ParsingTree ( ParsingTree(..) ) where

{- Used as a middle point to convert from all the tokens,
to the tree used for evaluating expressions -}

data ParsingTree = Leaf String
                | Nested [ParsingTree]
                | Node String ParsingTree
                deriving Show

instance Semigroup ParsingTree where
    Nested l <> Nested r = Nested (l <> r)
    Nested l <> other    = Nested (l <> [other])
    other    <> Nested r = Nested (other : r)
    otherl   <> otherr   = Nested (otherl : [otherr])