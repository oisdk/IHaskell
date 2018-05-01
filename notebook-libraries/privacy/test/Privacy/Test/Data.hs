{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}

module Privacy.Test.Data where

import Data.List.Indexed
import Numeric.Peano

cyclicTupleExpect :: [List (N 3) Int]
cyclicTupleExpect =
        [ 11 :- 13 :- 9  :- Nil
        , 13 :- 4  :- 6  :- Nil
        , 0  :- 5  :- 1  :- Nil
        , 10 :- 9  :- 7  :- Nil
        , 3  :- 0  :- 13 :- Nil
        , 1  :- 6  :- 0  :- Nil
        , 9  :- 1  :- 12 :- Nil
        , 5  :- 7  :- 10 :- Nil
        , 4  :- 10 :- 4  :- Nil
        , 7  :- 12 :- 5  :- Nil
        , 14 :- 2  :- 14 :- Nil
        , 6  :- 11 :- 2  :- Nil
        , 12 :- 14 :- 11 :- Nil
        , 2  :- 8  :- 8  :- Nil
        , 8  :- 3  :- 3  :- Nil]

cyclicTupleInput :: [List (N 3) Int]
cyclicTupleInput =
    [ 2 :- 3 :- 3 :- Nil
    , 5 :- 4 :- 2 :- Nil
    , 9 :- 6 :- 7 :- Nil
    , 4 :- 7 :- 9 :- Nil
    , 8 :- 1 :- 5 :- Nil
    , 7 :- 2 :- 6 :- Nil
    , 9 :- 4 :- 1 :- Nil
    , 8 :- 4 :- 2 :- Nil
    , 9 :- 7 :- 8 :- Nil
    , 6 :- 3 :- 1 :- Nil
    , 3 :- 4 :- 5 :- Nil
    , 1 :- 6 :- 8 :- Nil
    , 9 :- 5 :- 3 :- Nil
    , 2 :- 1 :- 3 :- Nil
    , 8 :- 7 :- 6 :- Nil]
