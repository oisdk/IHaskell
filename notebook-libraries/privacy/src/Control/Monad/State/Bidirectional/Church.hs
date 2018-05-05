module Control.Monad.State.Bidirectional.Church where

newtype State fw bw a = State
    { runState :: ∀ x. fw -> bw -> (a -> fw -> bw -> x) -> x
    } deriving Functor

runStateBoth :: fw -> bw -> State fw bw a -> (a, fw, bw)
runStateBoth x y s = runState s x y (,,)

instance Applicative (State fw bw) where
    pure x =
        State
            (\fw bw k ->
                  k x fw bw)
    {-# INLINE pure #-}
    fs <*> xs = State $ \fw bw k ->
      let (f,fw',bw'') = runState fs fw bw' (,,)
          (x,fw'',bw') = runState xs fw' bw (,,)
      in k (f x) fw'' bw''

instance Monad (State fw bw) where
    xs >>= f = State  $ \fw bw k ->
      let (bwrec,res) = runState xs fw bwrec
                      $ \x fw' bw'' -> runState (f x) fw' bw
                      $ \y fw'' bw' -> (bw', k y fw'' bw'')
      in res

mapAccumLR
    :: Traversable t
    => (a -> fw -> bw -> (b, fw, bw)) -> fw -> bw -> t a -> (t b, fw, bw)
mapAccumLR f fw bw =
    runStateBoth fw bw .
    traverse
        (\x ->
              State
                  (\fw' bw' k ->
                        case f x fw' bw' of
                            (y,fw'',bw'') -> k y fw'' bw''))
{-# INLINE mapAccumLR #-}

-- |
-- >>> rotations "abcd"
-- ["abcd","bcda","cdab","dabc"]
rotations :: [a] -> [[a]]
rotations xs = runState (traverse f xs) id id (\x _ _ -> x)
  where
    f x = State (\fw bw k -> k (x : bw (fw [])) (fw . (:) x) ((:) x . bw))
    {-# INLINE f #-}
{-# INLINE rotations #-}
