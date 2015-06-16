module Cis194Spec (spec) where

import Cis194

import Test.Hspec
import Test.Hspec.QuickCheck

spec :: Spec
spec =
    describe "cis194" $ do
        it "returns the unit" $
            cis194 `shouldBe` ()

        prop "equals the unit value" $
            \ x -> cis194 == x
