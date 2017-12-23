module Tests where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

import Main hiding (main)

main = hspec $ do

    describe "Adequately calculates valid input" $ do

        describe "Simple expressions" $ do

            it "Addition" $ do
                process "2+3" `shouldBe` "5"

            it "Subtraction" $ do
                process "2-3" `shouldBe` "-1"

            it "Multiplication" $ do
                process "2*3" `shouldBe` "6"

            it "Division" $ do
                process "6/3" `shouldBe` "2"

            it "Exponentiation" $ do
                process "2^3" `shouldBe` "8"

        describe "Deals with precedence" $ do

            it "Parens" $ do
                process "2*(2-3)" `shouldBe` "-2" 

            it "Multiplication over Addition" $ do
                process "2+3*2" `shouldBe` "8"

            it "Multiplication and Division at same level" $ do
                process "4/2*2" `shouldBe` "4" --ffff

            it "Exponentiation over Multiplication" $ do
                process "2*3^2" `shouldBe` "18"

        describe "Outputs appropriate decimal" $ do

            it "Result needs decimal" $ do
                process "3/4" `shouldBe` "0.75"
              
            it "Decimal is redundant (all zeros)" $ do
                process "8/4" `shouldBe` "2"

        describe "Deals with spaces while parsing" $ do

            it "No spaces is equivalent to variyng number of spaces" $ do
                process "2*3+2" `shouldBe` process "2 *   3 +2"
              
            it "Spaces are redundant next to all operators" $ do
                process "2+3-2*3/(2^3)" `shouldBe` process " 2 +  3 -  2 * 3  / ( 2   ^ 3 ) "
              
        describe "Adequately proccesses complex valid inputs" $ do

            it "Expression 1" $ do
                process "(67 + 2 * 3 - 67 + 2/1 - 7)" `shouldBe` "1" --ffff

            it "Expression 2" $ do
                process "(2) + (17*2-30) * (5)+2 - (8/2)*4" `shouldBe` "8"

            it "Expression 3" $ do
                process "(( ((2)) + 4))*((5))" `shouldBe` "30"

            it "Expression 4" $ do
                process "32.4 * 3 + 2^2^(2^2)" `shouldBe` "353.2"
                
            it "Expression 5" $ do
                process "5.3 * 4.34 ^3.33 - (1.45^1.234)" `shouldBe` "701.6795040477153"



--shouldBe f x y = f x == y

--equivalentTo f1 x f2 y = f1 x == f2 y
