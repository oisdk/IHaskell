module Data.Foldable.Safe where

minimumOn :: (Foldable f, Ord b) => (a -> b) -> f a -> Maybe a
minimumOn kf xs = foldr f (fmap snd) xs Nothing
  where
    f y a Nothing = a (Just (kf y, y))
    f y a b@(Just (xk,_))
      | yk < xk = a (Just (yk, y))
      | otherwise = a b
      where
        yk = kf y
