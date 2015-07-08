module Cis194.Spring13.HW11Spec (spec) where

import Cis194.Spring13.HW11.AParser
import Cis194.Spring13.HW11.SExpr

import Data.Char

import Test.Hspec

spec :: Spec
spec =
    describe "HW11" $ do
        describe "zeroOrMore" $ do
            it "parses capitalized string prefixes" $
                runParser (zeroOrMore (satisfy isUpper)) "ABCdEfgH" `shouldBe`
                    Just ("ABC", "dEfgH")

            it "succeeds without capitalized string prefixes" $
                runParser (zeroOrMore (satisfy isUpper)) "abcdeFGh" `shouldBe`
                    Just ("", "abcdeFGh")

        describe "oneOrMore" $ do
            it "parses capitalized string prefixes" $
                runParser (oneOrMore (satisfy isUpper)) "ABCdEfgH" `shouldBe`
                    Just ("ABC", "dEfgH")

            it "fails without capitalized string prefixes" $
                runParser (oneOrMore (satisfy isUpper)) "abcdeFGh" `shouldBe`
                    Nothing

        describe "spaces" $ do
            it "parses whitespaces characters" $
                runParser spaces "   foo" `shouldBe`
                    Just ("   ", "foo")

        describe "ident" $ do
            it "parses identifiers followed by whitespace" $
                runParser ident "foobar baz" `shouldBe`
                    Just ("foobar", " baz")

            it "parses whole identifiers with numerics" $
                runParser ident "foo33fA" `shouldBe`
                    Just ("foo33fA", "")

            it "fails to parse tokens with leading numerics" $
                runParser ident "2bad" `shouldBe`
                    Nothing

            it "fails to parse empty tokens" $
                runParser ident "" `shouldBe`
                    Nothing
