module Cis194.Spring13.HW04Spec (spec) where

import Cis194.Spring13.HW04

import Test.Hspec

spec :: Spec
spec =
    describe "HW04" $ do
        describe "fun1'" $ do
            it "treats empty lists like fun1" $
                fun1' [] `shouldBe` fun1 []

            it "treats single-value odd lists like fun1" $
                fun1' [1] `shouldBe` fun1 [1]

            it "treats single-value even lists like fun1" $
                fun1' [2] `shouldBe` fun1 [2]

            it "treats two-value odd lists like fun1" $
                fun1' [1,3] `shouldBe` fun1 [1,3]

            it "treats two-value even lists like fun1" $
                fun1' [0,2] `shouldBe` fun1 [0,2]

            it "treats large mixed lists like fun1" $
                fun1' [1,6,3,0,1,3] `shouldBe` fun1 [1,6,3,0,1,3]

        describe "fun2'" $ do
            it "treats one values like fun2" $
                fun2' 1 `shouldBe` fun2 1

            it "treats small even values like fun2" $
                fun2' 2 `shouldBe` fun2 2

            it "treats large even values like fun2" $
                fun2' 100 `shouldBe` fun2 100

            it "treats small odd values like fun2" $
                fun2' 3 `shouldBe` fun2 3

            it "treats large odd values like fun2" $
                fun2' 101 `shouldBe` fun2 101

        -- describe "foldTree" $ do
        --     it "should fold strings" $
        --         foldTree "ABCDEFGHIJ" `shouldBe`
        --             Node 3
        --                 (Node 2
        --                     (Node 0 Leaf 'F' Leaf)
        --                     'I'
        --                     (Node 1 (Node 0 Leaf 'B' Leaf) 'C' Leaf))
        --                 'J'
        --                 (Node 2
        --                     (Node 1 (Node 0 Leaf 'A' Leaf) 'G' Leaf)
        --                     'H'
        --                     (Node 1 (Node 0 Leaf 'D' Leaf) 'E' Leaf))
