module Cis194Spec (spec) where

import Test.Hspec

spec :: Spec
spec =
    describe "Cis194" $ do
        it "encapsulates the course" $
            pendingWith "functions to test in the top level"
