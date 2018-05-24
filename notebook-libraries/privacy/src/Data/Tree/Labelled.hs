module Data.Tree.Labelled where

import           Data.Bifunctor
import           Data.Bool
import           Data.Coerce.Utilities
import           Data.Map.Strict                 (Map)
import qualified Data.Map.Strict                 as Map
import           Data.Semigroup
import           Data.Semigroup.Foldable
import qualified Data.Tree                       as Rose

import           IHaskell.Display
import           IHaskell.Display.Blaze          ()
import           IHaskell.Display.Diagrams       (diagram)

import           Diagrams.Backend.Cairo          (Cairo)
import           Diagrams.Prelude                (Diagram, bg, centerXY, circle,
                                                  dashingG, fc, fontSizeL, lc,
                                                  opacity, pad, text, white,
                                                  ( # ), (~~))
import           Diagrams.TwoD.Layout.Tree       (renderTree', symmLayout)

import           Text.LaTeX.Base.Texy
import qualified Text.LaTeX.Packages.Trees       as LatexTree
import qualified Text.LaTeX.Packages.Trees.Qtree as LatexTree

import qualified Text.Blaze.Html5                as Blaze

data Tree a b
    = Leaf { measure :: a
           , val     :: b }
    | Node { measure :: a
           , lchild  :: Tree a b
           , rchild  :: Tree a b}
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

drawTree :: (Show a, Show b) => Tree a b -> Diagram Cairo
drawTree = go . toTree
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
        go
            = pad 1.1
            . centerXY
            . renderTree' (renderNode . snd) renderBranch
            . symmLayout

instance (Show a, Show b) => IHaskellDisplay (Tree a b) where
    display = display . diagram . drawTree

drawPrivateTree
    :: (Show a, Show b)
    => (a -> Bool) -> (Tree a b -> String) -> Tree a b -> Diagram Cairo
drawPrivateTree reveal generalize = diagram . bg white . go . toTree
  where
    toTree (Leaf i x) =
        Rose.Node
            (reveal i, Left i)
            [Rose.Node (reveal i, Right (show x)) []]
    toTree nd@(Node i l r)
      | reveal (measure l) && reveal (measure r) =
          Rose.Node (reveal i, Left i) [toTree l, toTree r]
      | otherwise =
          Rose.Node
              (reveal i,Left i)
              [ Rose.Node
                    (reveal i, Right $ show (generalize nd))
                    [toHiddenTree l, toHiddenTree r]]
    toHiddenTree (Node i l r) =
        Rose.Node (False, Left i) [toHiddenTree l, toHiddenTree r]
    toHiddenTree (Leaf i x) =
        Rose.Node (False, Left i) [Rose.Node (False, Right (show x)) []]
    go = pad 1.1 . centerXY . renderTree' renderNode renderBranch . symmLayout
    maybeOp True  = id
    maybeOp False = opacity 0.5
    renderNode nd =
        text (showVal nd) # fontSizeL 0.2 # maybeOp (fst nd) <> circle 0.2 #
        fc white #
        lc white
    renderBranch (lb,ll) (rb,rr) =
        maybeOp (fst lb && fst rb) (ll ~~ rr) #
        case snd rb of
            Left _ -> id
            Right _ -> dashingG [0.05, 0.05] 0

    showVal = either show id . snd

instance (Texy a, Texy b) => Texy (Tree a b) where
    texy = LatexTree.tree (either texy texy) . go where
      go (Leaf x y) = LatexTree.Node (Just (Left x)) [LatexTree.Leaf (Right y)]
      go (Node x l r) = LatexTree.Node (Just (Left x)) [go l, go r]

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
    go f (Node _ xs ys) = go (f . (:) False) xs <> go (f . (:) True) ys

foldMapWithMeasure :: Semigroup m => (a -> b -> m) -> Tree a b -> m
foldMapWithMeasure f = go
  where
    go (Leaf x y) = f x y
    go (Node _ l r) = go l <> go r

codeBook :: Ord b => Tree a b -> Map b Path
codeBook = foldMapWithPath (flip Map.singleton)

followPath :: Path -> Tree a b -> Either (Tree a b) b
followPath = foldr f Left .# getPath
  where
    f _ _ (Leaf _ x) = Right x
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
