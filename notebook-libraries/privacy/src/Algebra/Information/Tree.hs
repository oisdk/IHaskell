module Algebra.Information.Tree
  (Tree(..)
  ,privateTree
  ,Path(..)
  ,foldMapWithPath
  ,codeBook
  ,followPath
  ,rates,kdistortion)
  where

import           Data.Bool                 (bool)
import           Data.Coerce.Utilities
import           Data.Semigroup            (Semigroup ((<>)))
import           Data.Semigroup.Foldable   (Foldable1 (foldMap1))
import           Data.Bifunctor
import           Data.List.ZipLongest

import           Data.Map.Strict           (Map)
import qualified Data.Map.Strict           as Map

import qualified Data.Tree                 as Rose

import           Diagrams.Backend.Cairo    (Cairo)
import           Diagrams.Prelude          (Diagram, bg, centerXY, circle,
                                            dashingG, fc, fontSizeL, lc,
                                            opacity, pad, text, white, ( # ),
                                            (~~))
import           Diagrams.TwoD.Layout.Tree (renderTree', symmLayout)

import qualified Text.Blaze.Html5          as Blaze

import           IHaskell.Display          (IHaskellDisplay (display))
import           IHaskell.Display.Blaze    ()
import           IHaskell.Display.Diagrams (diagram)

data Tree b a
    = Leaf { measure :: b
           , val     :: a }
    | Node { measure :: b
           , lchild  :: Tree b a
           , rchild  :: Tree b a}
    deriving (Functor, Foldable, Traversable, Eq, Ord)

instance Bifunctor Tree where
    first f (Leaf x y) = Leaf (f x) y
    first f (Node x l r) = Node (f x) (first f l) (first f r)
    second = fmap
    bimap f g (Leaf x y) = Leaf (f x) (g y)
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
               , _count :: a}
    | Terminal { revealed :: Bool
               , _val :: String}

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
    maybeOp True = id
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

codeBook :: Ord b => Tree a b -> Map b Path
codeBook = foldMapWithPath (flip Map.singleton)

followPath :: Path -> Tree a b -> Either (Tree a b) b
followPath = foldr f Left .# getPath where
  f _ _ (Leaf _ x)     = Right x
  f d k (Node _ ls rs) = k (bool ls rs d)

rates :: Ord a => Tree a b -> [a]
rates (Leaf x _) = [x]
rates (Node x l r) = x : zipLongest min (rates l) (rates r)

mse :: Num a => a -> a -> a
mse x y = let z = x - y in z * z

weightedAvg :: Fractional a => [(a,a)] -> a
weightedAvg xs = n / d
  where
    d = sum (map snd xs)
    n = sum (map (uncurry (*)) xs)

khide :: Ord b => Int -> Tree Int b -> Tree Int (Map b Int)
khide k nd@(Node n l r)
    | measure l < k || measure r < k = Leaf n (hists nd)
    | otherwise = Node n (khide k l) (khide k r)
khide _ (Leaf n x) = Leaf n (Map.singleton x n)

hists (Leaf m x) = Map.singleton x m
hists (Node _ l r) = Map.union (hists l) (hists r)

distortion :: Fractional b => Tree Int (Map b Int) -> b
distortion ys = weightedAvg ((foldr (:) [] ys) >>= f)
  where
    f mp =
        [ ( weightedAvg
                [ ((mse x y), fromIntegral m)
                | (y,m) <- xs ]
          , fromIntegral n)
        | (x,n) <- xs ]
      where
        xs = Map.toList mp

kdistortion :: (Fractional b, Ord b) => Int -> Tree Int b -> b
kdistortion k = distortion . khide k
