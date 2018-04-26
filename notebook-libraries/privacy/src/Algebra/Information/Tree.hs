{-# LANGUAGE AllowAmbiguousTypes #-}

module Algebra.Information.Tree where

import           Algebra.Information

import           Data.Semigroup            ((<>))
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

data Tree f a
    = Leaf { measure :: f a
           , val     :: a }
    | Node { measure :: f a
           , lchild  :: Tree f a
           , rchild  :: Tree f a }

instance Foldable (Tree f) where
    foldMap f (Leaf _ x) = f x
    foldMap f (Node _ l r) = mappend (foldMap f l) (foldMap f r)

instance Foldable1 (Tree a) where
    foldMap1 f (Leaf _ x)   = f x
    foldMap1 f (Node _ l r) = foldMap1 f l <> foldMap1 f r

instance (Show a, Show (f a)) => IHaskellDisplay (Tree f a) where
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
    :: forall g a f.
       (Information g a, Show (g a), Show a, Show (f a))
    => (f a -> Bool) -> Tree f a -> Diagram Cairo
privateTree reveal tree = diagram $ bg white $ drawTree $ toTree tree
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
                    (reveal i, show (generalize nd :: g a))
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
