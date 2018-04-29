module Data.Tree.Perfect where

import           Data.Bits

import qualified Data.Tree                 as Rose
import           Diagrams.Prelude          (centerXY, circle, fc, fontSizeL, lc,
                                            pad, text, white, ( # ), (~~), (<>), dashingG)
import           Diagrams.TwoD.Layout.Tree (renderTree', symmLayout)
import           IHaskell.Display          (IHaskellDisplay (display))
import           IHaskell.Display.Diagrams (diagram)

data Perfect a
    = Perfect !a
    | Nest !(Perfect (Branch a))
    deriving (Functor,Foldable,Traversable)

foldPerfect :: (Word -> b -> Word -> b -> b) -> (a -> b) -> Perfect a -> b
foldPerfect (f :: Word -> b -> Word -> b -> b) = flip go
  where
    go :: Perfect a -> (a -> b) -> b
    go (Perfect x) k = k x
    go (Nest tr) k =
        go tr $
        \(Branch ls l rs r) ->
             f ls (k l) rs (k r)

data Branch a =
    Branch {-# UNPACK #-} !Word
           !a
           {-# UNPACK #-} !Word
           !a
    deriving (Functor,Foldable,Traversable)

data Span = Span {-# UNPACK #-} !Word {-# UNPACK #-} !Word

instance Show Span where
    showsPrec n (Span l 0) = showsPrec n l
    showsPrec _ (Span l o)
        = showChar '['
        . shows (shiftL l (fromEnum o))
        . showChar ','
        . shows (shiftL (l + 1) (fromEnum o))
        . showChar ')'

makePerfect :: Word -> a -> Perfect a
makePerfect 0 x = Perfect x
makePerfect m x = Nest (makePerfect (pred m) (Branch 0 x 0 x))

incr :: (a -> a) -> Word -> Perfect a -> Perfect a
incr f i' tr = go i' 0 tr f
  where
    go :: Word -> Word -> Perfect a -> (a -> a) -> Perfect a
    go _ _ (Perfect x) c = Perfect (c x)
    go i p (Nest t) c =
        Nest $
        go i (p + 1) t $
            \(Branch sl l sr r) ->
              if testBit i (fromEnum p)
                then Branch sl l (sr + 1) (c r)
                else Branch (sl + 1) (c l) sr r

instance Show a =>
         IHaskellDisplay (Perfect a) where
    display = display
            . diagram
            . drawTree
            . giveReq 0
            . foldPerfect toTree leaf
      where
        leaf x = Right $ \sz -> Rose.Node (True, show sz) [Rose.Node (False, show x) []]
        giveReq = either id . flip ($)
        toTree x l y r =
            Left (Rose.Node (True, show (x + y)) [giveReq x l, giveReq x r])
        drawTree =
            pad 1.1 .
            centerXY .
            renderTree'
                (\n ->
                      text (snd n) # fontSizeL 0.2 <> circle 0.2 # fc white #
                      lc white)
                renderBranch .
            symmLayout
        renderBranch (_,l) (b,r)
          | fst b = l ~~ r
          | otherwise = l ~~ r # dashingG [0.05, 0.05] 0

subTree :: Int -> Word -> Perfect a -> Span
subTree k' i tr = foldPerfect go (const Span) tr 0 k
  where
    k = toEnum k'
    go sl lt sr rt l p
      | sl < k || sr < k = Span l p
      | testBit i (pred (fromEnum p)) = rt (shiftL l 1 .|. 1) (pred p)
      | otherwise = lt (shiftL l 1) (pred p)
