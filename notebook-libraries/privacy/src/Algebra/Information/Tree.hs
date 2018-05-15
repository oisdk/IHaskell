module Algebra.Information.Tree
  (Tree(..)
  ,privateTree
  ,Path(..)
  ,foldMapWithPath
  ,codeBook
  ,followPath
  ,foldMapWithMeasure
  ,truncateTree
  ,histogramDistortion
  ,mse)
  where

import           Data.Bifunctor
import           Data.Bool                     (bool)
import           Data.Coerce.Utilities
import           Data.Semigroup                (Semigroup ((<>)), Sum (..))
import           Data.Semigroup.Foldable       (Foldable1 (foldMap1))
import           Control.Lens hiding ((#))

import           Data.Map.Strict               (Map)
import qualified Data.Map.Strict               as Map

import qualified Data.Tree                     as Rose

import           Diagrams.Backend.Cairo        (Cairo)
import           Diagrams.Prelude              (Diagram, bg, centerXY, circle,
                                                dashingG, fc, fontSizeL, lc,
                                                opacity, pad, text, white,
                                                ( # ), (~~))
import           Diagrams.TwoD.Layout.Tree     (renderTree', symmLayout)

import qualified Text.Blaze.Html5              as Blaze

import           IHaskell.Display              (IHaskellDisplay (display))
import           IHaskell.Display.Blaze        ()
import           IHaskell.Display.Diagrams     (diagram)

import           Algebra.Information.Histogram

data Tree b a
    = Leaf { measure :: b
           , val     :: a }
    | Node { measure :: b
           , lchild  :: Tree b a
           , rchild  :: Tree b a}
    deriving (Functor, Foldable, Traversable, Eq, Ord)

instance Bifunctor Tree where
    first f (Leaf x y)   = Leaf (f x) y
    first f (Node x l r) = Node (f x) (first f l) (first f r)
    second = fmap
    bimap f g (Leaf x y)   = Leaf (f x) (g y)
    bimap f g (Node x l r) = Node (f x) (bimap f g l) (bimap f g r)

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

data ShowNode a
    = ShowNode { revealed :: Bool
               , _count   :: a}
    | Terminal { revealed :: Bool
               , _val     :: String}

showVal :: Show a => ShowNode a -> String
showVal (ShowNode _ c) = show c
showVal (Terminal _ s) = s

privateTree
    :: (Semigroup c, Show c, Show a, Show b)
    => (b -> Bool) -> (a -> c) -> Tree b a -> Diagram Cairo
privateTree reveal generalize tree =
    diagram $ bg white $ drawTree $ toTree tree
  where
    toTree (Leaf i x) =
        Rose.Node
            (ShowNode (reveal i) i)
            [Rose.Node (Terminal (reveal i) (show x)) []]
    toTree nd@(Node i l r)
      | reveal (measure l) && reveal (measure r) =
          Rose.Node (ShowNode (reveal i) i) [toTree l, toTree r]
      | otherwise =
          Rose.Node
              (ShowNode (reveal i) i)
              [ Rose.Node
                    (Terminal (reveal i) (show (foldMap1 generalize nd)))
                    [toHiddenTree l, toHiddenTree r]]
    toHiddenTree (Node i l r) =
        Rose.Node (ShowNode False i) [toHiddenTree l, toHiddenTree r]
    toHiddenTree (Leaf i x) =
        Rose.Node (ShowNode False i) [Rose.Node (Terminal False (show x)) []]
    drawTree =
        pad 1.1 . centerXY . renderTree' renderNode renderBranch . symmLayout
    maybeOp True  = id
    maybeOp False = opacity 0.5
    renderNode nd =
        text (showVal nd) # fontSizeL 0.2 # maybeOp (revealed nd) <> circle 0.2 #
        fc white #
        lc white
    renderBranch (lb,ll) (rb,rr) =
        maybeOp (revealed lb && revealed rb) (ll ~~ rr) #
        case rb of
            ShowNode{} -> id
            Terminal{} -> dashingG [0.05, 0.05] 0

newtype Path = Path { getPath :: [Bool] }

instance Blaze.ToMarkup Path where
    toMarkup = Blaze.pre . Blaze.toMarkup . map (bool '0' '1') .# getPath

instance Show Path where
    showsPrec _ = flip (foldr ((:) . bool '0' '1')) .# getPath

instance IHaskellDisplay Path where
    display = display . Blaze.toMarkup

foldMapWithPath :: Semigroup m => (Path -> b -> m) -> Tree a b -> m
foldMapWithPath = go .# (. Path)
  where
    go f (Leaf _ x) = f [] x
    go f (Node _ xs ys) =
        go (f . (:) False) xs <> go (f . (:) True) ys

foldMapWithMeasure :: Semigroup m => (a -> b -> m) -> Tree a b -> m
foldMapWithMeasure f = go where
  go (Leaf x y)   = f x y
  go (Node _ l r) = go l <> go r

codeBook :: Ord b => Tree a b -> Map b Path
codeBook = foldMapWithPath (flip Map.singleton)

followPath :: Path -> Tree a b -> Either (Tree a b) b
followPath = foldr f Left .# getPath where
  f _ _ (Leaf _ x)     = Right x
  f d k (Node _ ls rs) = k (bool ls rs d)

truncateTree
    :: Semigroup m
    => (a -> Bool) -> (a -> b -> m) -> Tree a b -> Tree a m
truncateTree reveal summarize = go
  where
    go nd@(Node n l r)
      | reveal (measure l) && reveal (measure r) = Node n (go l) (go r)
      | otherwise = Leaf n (foldMapWithMeasure summarize nd)
    go (Leaf n x) = Leaf n (summarize n x)

histogramDistortion :: (Integral a, Fractional b, Ord b) => Tree a (Histogram a b) -> b
histogramDistortion =
    average . auf (histIso . mapping (_Unwrapping Sum) . from histIso) foldMap dist
  where
    dist xs = mapHistNum (\x -> average (mapHistNum (mse x) xs)) xs

mse :: Num a => a -> a -> a
mse x y =
    let z = x Prelude.- y
    in z Prelude.* z

mapHistNum :: (Num n, Ord b) => (a -> b) -> Histogram n a -> Histogram n b
mapHistNum = over (histIso . mapping (_Unwrapping Sum) . from histIso) . mapHist
