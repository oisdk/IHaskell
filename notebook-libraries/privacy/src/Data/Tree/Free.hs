module Data.Tree.Free where

newtype Tree v a b = Tree
    { runTree :: (a, Either b (v (Tree v a b)))
    }

newtype Annotated v a b = Annotated
    { runAnnotated :: Tree v b a
    }
