{-# OPTIONS_GHC -w #-}

module Cis194.Spring13.HW03Spec (spec) where

import Cis194.Spring13.HW03.Golf

import Test.Hspec

empty :: [Int]
empty = []

spec :: Spec
spec =
    describe "HW03" $ do
        describe "skips" $ do
            it "skips even lists" $
                skips "ABCD" `shouldBe`
                    ["ABCD", "BD", "C", "D"]

            it "skips odd lists" $
                skips "hello!" `shouldBe`
                    ["hello!", "el!", "l!", "l", "o", "!"]

            it "handles single lists" $
                skips [1] `shouldBe` [[1]]

            it "skips lists of booleans" $
                skips [True,False] `shouldBe` [[True,False], [False]]

            it "should skip empty lists" $
                skips empty `shouldBe` []
