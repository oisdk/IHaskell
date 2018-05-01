module Control.Monad.State.Bidirectional where

import Control.Applicative
import Data.Coerce.Utilities

newtype State fw bw a = State
    { runState :: fw -> bw -> (a, fw, bw)
    }

runStateBoth :: fw -> bw -> State fw bw a -> (a, fw, bw)
runStateBoth x y s = runState s x y

instance Functor (State fw bw) where
    fmap f xs =
        State
            (\ !fw bw ->
                  case runState xs fw bw of
                      (x,bw',fw') -> (f x, bw', fw'))

instance Applicative (State fw bw) where
    liftA2 f xs ys =
        State
            (\ !fw bw ->
                  let (x,!fw' ,bw'') = runState xs fw bw'
                      (y,!fw'',bw' ) = runState ys fw' bw
                  in (f x y, fw'', bw''))
    {-# INLINE liftA2 #-}
    fs <*> xs =
        State
            (\ !fw bw ->
                  let (f,!fw' ,bw'') = runState fs fw bw'
                      (x,!fw'',bw' ) = runState xs fw' bw
                  in (f x, fw'', bw''))
    pure x =
        State
            (\ !fw bw ->
                  (x, fw, bw))
    {-# INLINE pure #-}

instance Monad (State fw bw) where
    xs >>= f =
        State $
        \ !fw bw ->
             let (x,fw' ,bw'') = runState xs fw bw'
                 (y,fw'',bw' ) = runState (f x) fw' bw
             in (y, fw'', bw'')

mapAccumLR
    :: Traversable t
    => (a -> fw -> bw -> (b, fw, bw)) -> fw -> bw -> t a -> (t b, fw, bw)
mapAccumLR f fw bw = runStateBoth fw bw . traverse (State #. f)
{-# INLINE mapAccumLR #-}

rotations :: [a] -> [[a]]
rotations = g . mapAccumLR f id id
  where
    g (x,_,_) = x
    f x fw bw = (x : bw (fw []), fw . (:) x, (:) x . bw)
