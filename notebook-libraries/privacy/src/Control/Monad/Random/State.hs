{-# LANGUAGE PatternSynonyms      #-}
{-# LANGUAGE ViewPatterns         #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Monad.Random.State
  (RandT
  ,pattern RandT
  ,runRandT
  ,evalRandT
  ,execRandT
  ,Rand
  ,pattern Rand
  ,runRand
  ,evalRand
  ,execRand
  ,rand
  ,evalRandIO
  ,choose
  ,MonadRandom(..))
  where

import Control.Monad.State.Strict

import Data.Functor.Identity
import Data.Coerce.Utilities
import Data.Coerce
import Data.Bifunctor
import Numeric.Natural

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.Fail
import Control.Monad.Cont
import Control.Applicative

import Control.Monad.Random.Class
import System.Random

newtype RandT g m a =
    RandT_ (StateT g m a)
    deriving (Functor,Applicative,Monad,MonadTrans,MonadFix,MonadFail
             ,MonadIO,Alternative,MonadPlus,MonadCont)

deriving instance MonadError e m  => MonadError e (RandT g m)
deriving instance MonadReader r m => MonadReader r (RandT g m)
deriving instance MonadWriter w m => MonadWriter w (RandT g m)

instance MonadState s m =>
         MonadState s (RandT s m) where
    state = lift . state
    get = lift get
    put = lift . put

instance (Monad m, RandomGen g) =>
         MonadRandom (RandT g m) where
    getRandom = rand random
    getRandoms = rand (first randoms . split)
    getRandomR = rand . randomR
    getRandomRs xy = rand $ first (randomRs xy) . split

pattern RandT :: (g -> m (a, g)) -> RandT g m a
pattern RandT x = RandT_ (StateT x)
{-# COMPLETE RandT #-}

rand :: Applicative m => (g -> (a, g)) -> RandT g m a
rand = RandT_ #. StateT #. (pure .)
{-# INLINE rand #-}

runRandT :: RandT g m a -> g -> m (a, g)
runRandT =
    (coerce :: (StateT s m a -> s -> m (a, s)) -> RandT s m a -> s -> m (a, s))
        runStateT
{-# INLINE runRandT #-}

evalRandT :: Functor m => RandT g m a -> g -> m a
evalRandT = (fmap fst .) .# runRandT
{-# INLINE evalRandT #-}

execRandT :: Functor m => RandT g m a -> g -> m g
execRandT = (fmap snd .) .# runRandT
{-# INLINE execRandT #-}

type Rand g = RandT g Identity

runRand :: Rand g a -> g -> (a, g)
runRand = (runIdentity .) #. runRandT
{-# INLINE runRand #-}

evalRand :: Rand g a -> g -> a
evalRand = (runIdentity .) #. evalRandT
{-# INLINE evalRand #-}

execRand :: Rand g a -> g -> g
execRand = (runIdentity .) #. execRandT
{-# INLINE execRand #-}

pattern Rand :: (g -> (a, g)) -> Rand g a
pattern Rand x <- (runRand -> x) where
  Rand x = RandT_ (StateT (Identity #. x))
{-# COMPLETE Rand #-}

evalRandIO :: Rand StdGen a -> IO a
evalRandIO = getStdRandom .# runRand
{-# INLINE evalRandIO #-}

-- |
-- prop> \(Positive n) (Positive m) -> count' n m === (n, m)
choose :: (Applicative m, RandomGen g)
       => Natural -> Natural -> RandT g m Bool
choose f' t' =
    RandT
        (\g ->
              case randomR (1, f + t) g of
                  (n,g') -> pure (n > f, g'))
  where
    (f,t) = (toInteger f', toInteger t')
{-# INLINE choose #-}

-- $setup
-- >>> import Test.QuickCheck
-- >>> choose' f t = [ n > f | n <- [1..f+t] ]
-- >>> count' f t = let xs = choose' f t in (length (filter not xs), length (filter id xs))
