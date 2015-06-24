module Cis194.Spring13.HW02Spec (spec) where

import Cis194.Spring13.HW02.Log
import Cis194.Spring13.HW02.LogAnalysis

import Test.Hspec

oneTree :: MessageTree
oneTree = (Node
    Leaf
    (LogMessage (Error 5) 10 "zero")
    Leaf)

twoTree :: MessageTree
twoTree = (Node
    Leaf
    (LogMessage Info 100 "zero")
    (Node
        Leaf
        (LogMessage Warning 150 "one")
        Leaf))

threeTree :: MessageTree
threeTree = (Node
    (Node
        Leaf
        (LogMessage (Error 1) 50 "zero")
        Leaf)
    (LogMessage Info 100 "one")
    (Node
        Leaf
        (LogMessage Warning 150 "two")
        Leaf))

trees :: [MessageTree]
trees = [oneTree, twoTree, threeTree]

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

        describe "insert" $ do
            it "should not insert Unknown messages" $
                map (\t -> insert (Unknown "test") t) trees `shouldBe` trees

            it "should insert earlier messages to the left" $
                insert (LogMessage Info 5 "info") oneTree `shouldBe`
                    (Node
                    (Node
                        Leaf
                        (LogMessage Info 5 "info")
                        Leaf)
                    (LogMessage (Error 5) 10 "zero")
                    Leaf)

            it "should insert later messages to the right" $
                insert (LogMessage (Error 10) 125 "error") threeTree `shouldBe`
                    (Node
                        (Node
                            Leaf
                            (LogMessage (Error 1) 50 "zero")
                            Leaf)
                        (LogMessage Info 100 "one")
                        (Node
                            (Node
                                Leaf
                                (LogMessage (Error 10) 125 "error")
                                Leaf)
                            (LogMessage Warning 150 "two")
                            Leaf))

            it "should insert into empty trees" $
                insert (LogMessage Warning 100 "warning") Leaf `shouldBe`
                    (Node Leaf (LogMessage Warning 100 "warning") Leaf)

        describe "build" $ do
            it "should create single-node trees" $
                build [(LogMessage (Error 5) 10 "zero")] `shouldBe`
                    oneTree

            it "should create two-node trees" $
                build [(LogMessage Info 100 "zero"),
                       (LogMessage Warning 150 "one")]
                      `shouldBe` twoTree

            it "should create three-node trees" $
                build [(LogMessage Info 100 "one"),
                       (LogMessage (Error 1) 50 "zero"),
                       (LogMessage Warning 150 "two")]
                      `shouldBe` threeTree

        describe "inOrder" $ do
            it "should print single-node trees in order" $
                inOrder oneTree `shouldBe`
                    [(LogMessage (Error 5) 10 "zero")]

            it "should print two-node trees in order" $
                inOrder twoTree `shouldBe`
                    [(LogMessage Info 100 "zero"),
                     (LogMessage Warning 150 "one")]

            it "should print three-node trees in order" $
                inOrder threeTree `shouldBe`
                    [(LogMessage (Error 1) 50 "zero"),
                     (LogMessage Info 100 "one"),
                     (LogMessage Warning 150 "two")]

        describe "whatWentWrong" $ do
            it "should highlight log file errors" $
                testWhatWentWrong parse whatWentWrong "resources/sample.log" `shouldReturn`
                    [ "Way too many pickles"
                    , "Bad pickle-flange interaction detected"
                    , "Flange failed!"
                    ]
