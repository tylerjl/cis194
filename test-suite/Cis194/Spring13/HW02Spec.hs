module Cis194.Spring13.HW02Spec (spec) where

import Cis194.Spring13.HW02.Log
import Cis194.Spring13.HW02.LogAnalysis

import Test.Hspec

spec :: Spec
spec =
    describe "HW02" $ do
        describe "parseMessage" $ do
            it "parses errors" $
                parseMessage "E 2 562 help help" `shouldBe`
                    LogMessage (Error 2) 562 "help help"

            it "parses infos" $
                parseMessage "I 29 la la la" `shouldBe`
                    LogMessage Info 29 "la la la"

            it "parses unknowns" $
                parseMessage "This is not in the right format"
                    `shouldBe` Unknown "This is not in the right format"
