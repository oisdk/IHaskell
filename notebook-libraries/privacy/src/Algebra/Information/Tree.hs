{-# LANGUAGE AllowAmbiguousTypes #-}

module Algebra.Information.Tree where

import           Algebra.Information

import           Data.Semigroup            (Semigroup((<>)))
import           Data.Semigroup.Foldable   (Foldable1 (foldMap1))

import qualified Data.Tree                 as Rose

import           Diagrams.Backend.Cairo    (Cairo)
import           Diagrams.Prelude          (Diagram, bg, centerXY, circle,
                                            dashingG, fc, fontSizeL, lc,
                                            opacity, pad, text, white, ( # ),
                                            (~~))
import           Diagrams.TwoD.Layout.Tree (renderTree', symmLayout)

import           IHaskell.Display          (IHaskellDisplay (display))
import           IHaskell.Display.Diagrams (diagram)

data Tree b a
    = Leaf { measure :: b
           , val     :: a }
    | Node { measure :: b
           , lchild  :: Tree b a
           , rchild  :: Tree b a }

instance Foldable (Tree a) where
    foldMap f (Leaf _ x) = f x
    foldMap f (Node _ l r) = mappend (foldMap f l) (foldMap f r)

instance Foldable1 (Tree a) where
    foldMap1 f (Leaf _ x)   = f x
    foldMap1 f (Node _ l r) = foldMap1 f l <> foldMap1 f r

instance (Show a, Show b) => IHaskellDisplay (Tree a b) where
    display = display . diagram . drawTree . toTree
      where
        toTree (Leaf i x)
            = Rose.Node (True,show i) [Rose.Node (False,show x) []]
        toTree (Node i l r)
            = Rose.Node (True,show i) [toTree l, toTree r]
        renderNode n = text n # fontSizeL 0.2
                    <> circle 0.2 # fc white # lc white
        renderBranch (_,l) (vr,r)
          | fst vr = l ~~ r
          | otherwise = l ~~ r
                      # dashingG [0.05, 0.05] 0
        drawTree
            = pad 1.1
            . centerXY
            . renderTree' (renderNode . snd) renderBranch
            . symmLayout

privateTree
    :: (Semigroup c, Show c, Show a, Show b) => (b -> Bool) -> (a -> c) -> Tree b a -> Diagram Cairo
privateTree reveal generalize tree = diagram $ bg white $ drawTree $ toTree tree
  where
    toTree (Leaf i x) =
        Rose.Node (reveal i, show i) [Rose.Node (reveal i, show x) []]
    toTree nd@(Node i l r)
      | reveal (measure l) && reveal (measure r) =
          Rose.Node (reveal i, show i) [toTree l, toTree r]
      | otherwise =
          Rose.Node
              (reveal i, show i)
              [ Rose.Node
                    (reveal i, show (foldMap1 generalize nd))
                    [toHiddenTree l, toHiddenTree r]]
    toHiddenTree (Node i l r) =
        Rose.Node (False, show i) [toHiddenTree l, toHiddenTree r]
    toHiddenTree (Leaf i x) =
        Rose.Node (False, show i) [Rose.Node (False, show x) []]

    drawTree =
        pad 1.1 .
        centerXY .
        renderTree'
            (\(b,n) ->
                  text n # fontSizeL 0.2 # maybeOp b <> circle 0.2 # fc white #
                  lc white)
            (\((lb,_),ll) ((rb,_),rr) ->
                  maybeOp (lb && rb) (ll ~~ rr)) .
        symmLayout
    maybeOp True = id
    maybeOp False = opacity 0.5
