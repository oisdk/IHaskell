{-# LANGUAGE TypeInType             #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE AllowAmbiguousTypes    #-}

module Text.Html.Table
  (Table(..)
  ,table)
  where

import           Data.Kind

import qualified Text.Blaze as Blaze
import qualified Text.Blaze.Html5 as Blaze

import           IHaskell.Display (IHaskellDisplay (display))
import           IHaskell.Display.Blaze ()

import           Data.Foldable (fold)

infixr 5 :*
data Vect xs where
    Unit :: Vect '[]
    (:*) :: !x -> !(Vect xs) -> Vect (x ': xs)

data Nat = Z | S Nat

infixr 5 :-
data List n a where
    Nil :: List 'Z a
    (:-) :: !a -> !(List n a) -> List ('S n) a

deriving instance Foldable (List n)
deriving instance Functor (List n)

type family FromTuple tuple = list | list -> tuple where
    FromTuple (a,b) = [a,b]
    FromTuple (a,b,c) = [a,b,c]
    FromTuple (a,b,c,d) = [a,b,c,d]
    FromTuple (a,b,c,d,e) = [a,b,c,d,e]
    FromTuple (a,b,c,d,e,f) = [a,b,c,d,e,f]
    FromTuple (a,b,c,d,e,f,g) = [a,b,c,d,e,f,g]
    FromTuple (a,b,c,d,e,f,g,h) = [a,b,c,d,e,f,g,h]
    FromTuple (a,b,c,d,e,f,g,h,i) = [a,b,c,d,e,f,g,h,i]

class KnownTuple tup where
    fromTuple :: tup -> Vect (FromTuple tup)

instance KnownTuple (a,b) where
    fromTuple (x,y) = x :* y :* Unit

instance KnownTuple (a,b,c) where
    fromTuple (x,y,z) = x :* y :* z :* Unit

instance KnownTuple (a,b,c,d) where
    fromTuple (w,x,y,z) = w :* x :* y :* z :* Unit

instance KnownTuple (a,b,c,d,e) where
    fromTuple (v,w,x,y,z) = v :* w :* x :* y :* z :* Unit

instance KnownTuple (a,b,c,d,e,f) where
    fromTuple (u,v,w,x,y,z) = u :* v :* w :* x :* y :* z :* Unit

instance KnownTuple (a,b,c,d,e,f,g) where
    fromTuple (t,u,v,w,x,y,z) = t :* u :* v :* w :* x :* y :* z :* Unit

instance KnownTuple (a,b,c,d,e,f,g,h) where
    fromTuple (s,t,u,v,w,x,y,z) = s :* t :* u :* v :* w :* x :* y :* z :* Unit

instance KnownTuple (a,b,c,d,e,f,g,h,i) where
    fromTuple (r,s,t,u,v,w,x,y,z) = r :* s :* t :* u :* v :* w :* x :* y :* z :* Unit

type family All (c :: Type -> Constraint) xs :: Constraint where
    All c '[] = ()
    All c (x ': xs) = (c x, All c xs)

type family Length xs where
    Length '[] = 'Z
    Length (x ': xs) = 'S (Length xs)

data Table xs where
        Table ::
          List (Length xs) Blaze.Markup ->
            List (Length xs) Blaze.Attribute -> [Vect xs] -> Table xs

toList :: All ((~) a) xs => Vect xs -> List (Length xs) a
toList Unit = Nil
toList (x :* xs) = x :- toList xs

toListWith
    :: ∀ c a xs.
       All c xs
    => (∀ x. c x => x -> a)
    -> Vect xs
    -> List (Length xs) a
toListWith _ Unit = Nil
toListWith (f :: ∀ x. c x => x -> a) (x :* xs) = f x :- toListWith @c f xs

table
    :: (KnownTuple header
       ,All ((~) String) (FromTuple header)
       ,KnownTuple attributes
       ,KnownTuple rows
       ,All ((~) Blaze.Attribute) (FromTuple attributes)
       ,Length (FromTuple header) ~ Length (FromTuple rows)
       ,Length (FromTuple attributes) ~ Length (FromTuple rows))
    => header -> attributes -> [rows] -> Table (FromTuple rows)
table header attr rows
    = Table (toListWith @ ((~) String) Blaze.toMarkup (fromTuple header))
            (toList @ Blaze.Attribute (fromTuple attr)) (map fromTuple rows)

zipWithN :: (a -> b -> c) -> List n a -> List n b -> List n c
zipWithN _ Nil Nil = Nil
zipWithN f (x :- xs) (y :- ys) = f x y :- zipWithN f xs ys

instance All Blaze.ToMarkup xs =>
         IHaskellDisplay (Table xs) where
    display = display . go
      where
        go (Table header attributes xs) =
            Blaze.table $
            Blaze.tr (foldMap Blaze.th header) `mappend`
            foldMap
                (Blaze.tr .
                 fold .
                 zipWithN
                     (\attr cell ->
                           Blaze.td cell Blaze.! attr)
                     attributes .
                 toListWith @ Blaze.ToMarkup Blaze.toMarkup)
                xs
