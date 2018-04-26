{-# LANGUAGE DeriveFoldable      #-}
{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE FlexibleContexts    #-}

module Algebra.Information.Tree where

import           Algebra.Information

import           Data.Bifunctor
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

data Tree a b
    = Leaf { measure :: a
           , val     :: b }
    | Node { measure :: a
           , lchild  :: Tree a b
           , rchild  :: Tree a b }
    deriving (Functor, Foldable)

instance Foldable1 (Tree a) where
    foldMap1 f (Leaf _ x)   = f x
    foldMap1 f (Node _ l r) = foldMap1 f l <> foldMap1 f r

instance Bifunctor Tree where
    second = fmap
    first f = go where
      go (Leaf x y)   = Leaf (f x) y
      go (Node x l r) = Node (f x) (go l) (go r)
    bimap f g = go where
      go (Leaf x y)   = Leaf (f x) (g y)
      go (Node x l r) = Node (f x) (go l) (go r)

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

privateTree :: (Show b, Information b, Show a, Show (Domain b))
            => proxy b
            -> (a -> Bool)
            -> Tree a (Domain b)
            -> Diagram Cairo
privateTree p reveal tree = diagram $ bg white $ drawTree $ toTree tree
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
                    (reveal i, show (generalize nd `asTypeOf` getProxy p))
                    [toHiddenTree l, toHiddenTree r]]
    toHiddenTree (Node i l r) =
        Rose.Node (False, show i) [toHiddenTree l, toHiddenTree r]
    toHiddenTree (Leaf i x) =
        Rose.Node (False, show i) [Rose.Node (False, show x) []]
    getProxy :: proxy a -> a
    getProxy = undefined
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
