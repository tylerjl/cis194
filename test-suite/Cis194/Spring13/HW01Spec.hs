module Cis194.Spring13.HW01Spec (spec) where

import Cis194.Spring13.HW01

import Test.Hspec

spec :: Spec
spec =
    describe "HW01" $ do
        describe "toDigits" $ do
            it "converts integers to lists of digits" $
                toDigits 1234 `shouldBe` [1, 2, 3, 4]

            it "returns an empty list for zero values" $
                toDigits 0 `shouldBe` []

            it "returns empty lists for negative values" $
                toDigits (-17) `shouldBe` []

        describe "toDigitsRev" $ do
            it "converts integers to reversed lists of digits" $
                toDigitsRev 1234 `shouldBe` [4, 3, 2, 1]

        describe "doubleEveryOther" $ do
            it "should double every other number of an even list from the right" $
                doubleEveryOther [8,7,6,5] `shouldBe` [16,7,12,5]

            it "should double every other number of an odd list from the right" $
                doubleEveryOther [1,2,3] `shouldBe` [1,4,3]
