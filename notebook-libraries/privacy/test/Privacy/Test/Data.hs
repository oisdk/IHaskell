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

cyclicTupleExpectInds :: [List (N 3) (Int,Int)]
cyclicTupleExpectInds =
    [ (11,2 ) :- (13,4 ) :- (9 ,5 ) :- Nil
    , (13,5 ) :- (4 ,6 ) :- (6 ,2 ) :- Nil
    , (0 ,13) :- (5 ,10) :- (1 ,11) :- Nil
    , (10,4 ) :- (9 ,14) :- (7 ,14) :- Nil
    , (3 ,8 ) :- (0 ,1 ) :- (13,8 ) :- Nil
    , (1 ,7 ) :- (6 ,2 ) :- (0 ,9 ) :- Nil
    , (9 ,11) :- (1 ,5 ) :- (12,1 ) :- Nil
    , (5 ,9 ) :- (7 ,7 ) :- (10,3 ) :- Nil
    , (4 ,14) :- (10,13) :- (4 ,13) :- Nil
    , (7 ,6 ) :- (12,3 ) :- (5 ,0 ) :- Nil
    , (14,3 ) :- (2 ,8 ) :- (14,7 ) :- Nil
    , (6 ,0 ) :- (11,11) :- (2 ,12) :- Nil
    , (12,12) :- (14,9 ) :- (11,6 ) :- Nil
    , (2 ,1 ) :- (8 ,0 ) :- (8 ,4 ) :- Nil
    , (8 ,10) :- (3 ,12) :- (3 ,10) :- Nil]

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
