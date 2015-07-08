module Cis194.Spring13.HW10Spec (spec) where

import Cis194.Spring13.HW10.AParser

import Test.Hspec

spec :: Spec
spec =
    describe "HW10" $ do
        describe "first" $ do
            it "applies f to the first member of a pair" $
                first (\x -> [x, x]) ('a', True) `shouldBe` ("aa", True)

        describe "abParser" $ do
            it "parses the first 'ab' from a string" $
                runParser abParser "abcdef" `shouldBe` Just (('a','b'),"cdef")

            it "parses nothing on unmatched input" $
                runParser abParser "aebcdf" `shouldBe` Nothing

        describe "abParser_" $ do
            it "returns () with a successful parse" $
                runParser abParser_ "abcdef" `shouldBe` Just ((),"cdef")

            it "returns Nothing with an unsuccessful parse" $
                runParser abParser_ "aebcdf" `shouldBe` Nothing

        describe "intPair" $ do
            it "parses pairs of integers" $
                runParser intPair "12 34" `shouldBe` Just ([12,34],"")

        describe "intOrUppercase" $ do
            it "throws away leading ints" $
                runParser intOrUppercase "342abcd" `shouldBe` Just ((),"abcd")

            it "throws away leading uppercase characters" $
                runParser intOrUppercase "XYZ" `shouldBe` Just ((),"YZ")

            it "fails without leading ints or uppercase characters" $
                runParser intOrUppercase "foo" `shouldBe` Nothing
