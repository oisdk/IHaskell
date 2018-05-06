module Data.Stream where

import Data.List.NonEmpty (NonEmpty(..))
import Data.Function
import Prelude hiding (head, tail)

infixr 5 :<
data Stream a = (:<)
    { head :: a
    , tail :: Stream a
    } deriving (Functor,Foldable,Traversable)

unfoldStream :: (b -> (a, b)) -> b -> Stream a
unfoldStream f = go
  where
    go b = case f b of
      (x, xs) -> x :< go xs

toStream :: NonEmpty a -> Stream a
toStream = fix . flip (foldr (:<))

instance Applicative Stream where
    pure x = x :< pure x
    (f :< fs) <*> (x :< xs) = f x :< (fs <*> xs)

instance Monad Stream where
    x :< xs >>= f = head (f x) :< (xs >>= tail . f)
