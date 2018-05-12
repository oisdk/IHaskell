module Data.List.ZipLongest where

zipLongest :: (a -> a -> a) -> [a] -> [a] -> [a]
zipLongest _ [] ys = ys
zipLongest _ xs [] = xs
zipLongest f (x:xs) (y:ys) = f x y : zipLongest f xs ys
