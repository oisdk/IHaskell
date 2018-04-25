module Main (main) where

import Test.SmallCheck
import Test.Tasty
import Test.Tasty.SmallCheck

prop_noTests :: TestTree
prop_noTests = testProperty "Any Tests" (forAll True)

main :: IO ()
main = defaultMain (testGroup "Tests" [prop_noTests])
