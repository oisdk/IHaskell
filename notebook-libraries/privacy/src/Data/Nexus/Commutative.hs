{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Nexus.Commutative
  (enumerateTrees)
  where

import           Data.Bits (shiftL)
import qualified Data.Tree as Rose
import           GHC.Exts  (oneShot)

data Tree a
    = Leaf {-# UNPACK #-} !Int a
    | Node [Tree a]
    deriving Functor

-- | Given a nondeterministic, commutative binary operator, and a list
-- of inputs, enumerate all possible applications of the operator to
-- all inputs, without recalculating subtrees.
--
-- <http://www.cambridge.org/gb/academic/subjects/computer-science/programming-languages-and-applied-logic/pearls-functional-algorithm-design?format=HB&isbn=9780521513388 Bird, Richard. ‘Hylomorphisms and Nexuses’. In Pearls of Functional Algorithm Design, 1st ed., 168–79. New York, NY, USA: Cambridge University Press, 2010.>
--
-- This can be used, for instance, to solve the "countdown" problem.
--
-- >>> import Debug.SimpleReflect
-- >>> import Data.Function
-- >>> import qualified Data.IntSet as IntSet
-- >>> data Op = Add | Dif | Mul | Div
-- >>> data Memoed = Memoed { expr :: Expr, result :: Int }
-- >>> binOp f g x y = Memoed ((f `on` expr) x y) ((g `on` result) x y)
-- >>> :{
-- apply :: Op -> Memoed -> Memoed -> Memoed
-- apply Add x y = binOp (+) (+) x y
-- apply Dif x y
--   | result y < result x = binOp (-) (-) x y
--   | otherwise = binOp (-) (-) y x
-- apply Mul x y = binOp (*) (*) x y
-- apply Div x y = binOp div div x y
-- :}
--
-- >>> :{
-- enumerateExprs :: [Int] -> [Memoed]
-- enumerateExprs = enumerateTrees cmb . map (\x -> Memoed (fromIntegral x) x)
--   where
--     cmb x y =
--         nubs $
--         x :
--         y :
--         [ apply op x y
--         | op <- [Add, Dif, Mul, Div]
--         , legal op (result x) (result y) ]
--     legal Add _ _ = True
--     legal Dif x y = x /= y
--     legal Mul _ _ = True
--     legal Div x y = x `mod` y == 0
--     nubs xs = foldr f (const []) xs IntSet.empty
--       where
--         f e a s
--           | IntSet.member (result e) s = a s
--           | otherwise = e : a (IntSet.insert (result e) s)
-- :}
--
-- >>> :{
-- countdown :: Int -> [Int] -> [Expr]
-- countdown targ = map expr . filter ((==) targ . result) . enumerateExprs
-- :}
--
-- >>> (mapM_ print . reduction . head) (countdown 586 [100,25,1,5,3,10])
-- 25 * 3 + 1 + (100 * 5 + 10)
-- 75 + 1 + (100 * 5 + 10)
-- 76 + (100 * 5 + 10)
-- 76 + (500 + 10)
-- 76 + 510
-- 586
--
-- <https://doi.org/10.1017/S0956796805005642 Bird, Richard, and Shin-Cheng Mu. ‘Countdown: A Case Study in Origami Programming’. Journal of Functional Programming 15, no. 05 (18 August 2005): 679.>

enumerateTrees :: (a -> a -> [a]) -> [a] -> [a]
enumerateTrees _ [] = []
enumerateTrees cmb xxs = (extract . steps . initial) xxs
  where
    step = map nodes . group
    {-# INLINE step #-}

    steps [x] = x
    steps xs  = steps (step xs)

    initial = map (Leaf 1 . flip Rose.Node [] . pure)
    {-# INLINE initial #-}

    extract (Leaf _ x) = Rose.rootLabel x
    extract (Node [x]) = extract x
    extract _ =
        errorWithoutStackTrace
            "Data.Nexus.Commutative.enumerateTrees: bug! (extract)"

    group [_] = []
    group (Leaf _ x:vs) = Node [ Leaf 2 [x, y] | Leaf _ y <- vs ] : group vs
    group (Node u:vs) = Node (zipWith comb (group u) vs) : group vs
    group _ =
        errorWithoutStackTrace
            "Data.Nexus.Commutative.enumerateTrees: bug! (group)"

    comb (Leaf n xs) (Leaf _ x) = Leaf (n + 1) (xs ++ [x])
    comb (Node us) (Node vs) = Node (zipWith comb us vs)
    comb _ _ =
        errorWithoutStackTrace
            "Data.Nexus.Commutative.enumerateTrees: bug! (comb)"

    forest ts = foldr f (const b) ts 0 []
      where
        f (Rose.Node x []) fw = oneShot (\ !k bw -> x : fw (k + 1) bw)
        f (Rose.Node x us) fw = oneShot (\ !k bw -> x : fw (k + 1) ((drop k us, k) : bw))
        {-# INLINE f #-}
        b [] = []
        b qs = foldl (uncurry . foldr f . const) b qs []
    {-# INLINE forest #-}

    nodes (Leaf n x) = Leaf 1 (node n x)
    nodes (Node xs)  = Node (map nodes xs)

    node n ts =
        Rose.Node (walk (shiftL (1 :: Int) n - 2) (forest ts) (const [])) ts
      where
        walk 0 xss k = k xss
        walk m (xs:xss) k =
            walk (m - 2) xss
                (oneShot
                 (\case
                     (ys:yss) ->
                         [ z
                         | x <- xs
                         , y <- ys
                         , z <- cmb x y ] ++
                         k yss
                     [] ->
                         errorWithoutStackTrace
                             "Data.Nexus.Commutative.enumerateTrees: bug! (walk)"))
        walk _ _ _ =
            errorWithoutStackTrace
                "Data.Nexus.Commutative.enumerateTrees: bug! (walk)"
    {-# INLINE node #-}
{-# INLINE enumerateTrees #-}
