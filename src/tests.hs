module Tests where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

import Lexic
import Parser
import ErrorHandler
import Calculator
import Main hiding (main)

main = hspec $ do
  
    describe "Lexical Context" $ do

        describe "Custom Instances for data type Token" $ do

            describe "Compares Operator constructors according to precedence" $ do

                it "Addition and Subtraction are equal" $ do
                    Op '+' == Op '-' `shouldBe` True
                  
                it "Multiplication and Division are equal" $ do
                    Op '*' == Op '/' `shouldBe` True
                  
                it "Multiplication is greater than Addition" $ do
                    Op '*' > Op '+' `shouldBe` True
                  
                it "Subtraction is lower than Division" $ do
                    Op '-' < Op '/' `shouldBe` True
                  
                it "Exponentiation is greater than Multiplication" $ do
                    Op '^' > Op '-' `shouldBe` True
                  
                it "Division is lower than Exponentiation" $ do
                    Op '/' < Op '^' `shouldBe` True

            describe "Show instance for each Token construnctor" $ do

                it "Operator constructor" $ do
                    show (Op '+') `shouldBe` "'+'"
                  
                it "Numeric constructor" $ do
                    show (Num 5) `shouldBe` "5.0"
                  
                it "Left Bracket constructor" $ do
                    show LBr `shouldBe` "("
                  
                it "Right Bracket constructor" $ do
                    show RBr `shouldBe` ")"
                  
                it "Error constructor" $ do
                    show (Err "This is an error") `shouldBe` "This is an error"
                  

    describe "Parsing" $ do

        describe "Reads and converts to Token" $ do

            it "Reads Number" $ do
                parse "2" `shouldBe` [Num 2]

            it "Reads Operator" $ do
                parse "+" `shouldBe` [Op '+']

            it "Reads Left Bracket" $ do
                show (parse "(") `shouldBe` show [LBr]

            it "Reads Right Bracket" $ do
                show (parse ")") `shouldBe` show [RBr]

            it "Reads invalid token as error" $ do
                show (parse "k") `shouldBe` show [Err "Invalid token: 'k'"]

            it "Identifies negative numbers at the beginning" $ do
                show (parse "-5 + 3") `shouldBe` "[-5.0,'+',3.0]"

            it "Simplifies consecutive addition/subtraction operators" $ do
                show (parse "5 -+ 4 -- 3 ++ 2 -+- 1 --- 0") `shouldBe` "[5.0,'-',4.0,'+',3.0,'+',2.0,'+',1.0,'-',0.0]"
                     
            it "Reads 2.3456 - (((4)))" $ do
                show (parse "2.3456 - (((4)))") `shouldBe` "[2.3456,'-',(,(,(,4.0,),),)]"

            it "Reads 2 + 3 * 5 - (2^3^2)" $ do
                show (parse "2 + 3 * 5 - (2^3^2)") `shouldBe` "[2.0,'+',3.0,'*',5.0,'-',(,2.0,'^',3.0,'^',2.0,)]"

            it "Reads 2 / (5 - 1) * 7 k" $ do
                show (parse "2 / (5 - 1) * 7 k") `shouldBe` "[2.0,'/',(,5.0,'-',1.0,),'*',7.0,Invalid token: 'k']"

            it "Reads -5 -+--+ 1 + (-1 + 3) + (1 -2) + (-1)" $ do
                show (parse "-5 -+--+ 1 + (-1 + 3) + (1 -2) + (-1)") `shouldBe` "[-5.0,'-',1.0,'+',(,-1.0,'+',3.0,),'+',(,1.0,'-',2.0,),'+',(,-1.0,)]"


        describe "Converts to postfix notation - Oxford Math Center Website examples" $ do

            it "A * B + C becomes A B * C +" $ do
                show (shunt [Num 2.0,Op '*',Num 3.0,Op '+',Num 4.0] []) `shouldBe` "[2.0,3.0,'*',4.0,'+']"
  
            it "A + B * C becomes A B C * +" $ do
                show (shunt [Num 2.0,Op '+',Num 3.0,Op '*',Num 4.0] []) `shouldBe` "[2.0,3.0,4.0,'*','+']"
  
            it "A * (B + C) becomes A B C + *" $ do
                show (shunt [Num 2.0,Op '*',LBr,Num 3.0,Op '+',Num 4.0,RBr] []) `shouldBe` "[2.0,3.0,4.0,'+','*']"
  
            it "A - B + C becomes A B - C +" $ do
                show (shunt [Num 2.0,Op '-',Num 3.0,Op '+',Num 4.0] []) `shouldBe` "[2.0,3.0,'-',4.0,'+']"
  
            it "A * B ^ C + D becomes A B C ^ * D +" $ do
                show (shunt [Num 2.0,Op '*',Num 3.0,Op '^',Num 4.0,Op '+',Num 5.0] []) `shouldBe` "[2.0,3.0,4.0,'^','*',5.0,'+']"
  
            it "A * (B + C * D) + E becomes A B C D * + * E +" $ do
                show (shunt [Num 2.0,Op '*',LBr,Num 3.0,Op '+',Num 4.0,Op '*',Num 5.0,RBr,Op '+',Num 6.0] []) `shouldBe` "[2.0,3.0,4.0,5.0,'*','+','*',6.0,'+']"

  
    describe "Calculating" $ do

        describe "Simple expressions" $ do

            it "Addition - 2+3" $ do
                process "2+3" `shouldBe` "5"

            it "Subtraction - 2-3" $ do
                process "2-3" `shouldBe` "-1"

            it "Multiplication - 2*3" $ do
                process "2*3" `shouldBe` "6"

            it "Division - 6/3" $ do
                process "6/3" `shouldBe` "2"

            it "Exponentiation - 2^3" $ do
                process "2^3" `shouldBe` "8"

        describe "Precedence" $ do

            it "Parens - 2*(2-3)" $ do
                process "2*(2-3)" `shouldBe` "-2" 

            it "Multiplication over Addition - 2+3*2" $ do
                process "2+3*2" `shouldBe` "8"

            it "Multiplication and Division at same level - 4/2*2" $ do
                process "4/2*2" `shouldBe` "4"

            it "Exponentiation over Multiplication - 2*3^2" $ do
                process "2*3^2" `shouldBe` "18"

        describe "Decimal" $ do

            it "Result needs decimal - 3/4" $ do
                process "3/4" `shouldBe` "0.75"
              
            it "Decimal is redundant (all zeros) - 8/4" $ do
                process "8/4" `shouldBe` "2"

        describe "Whitespace" $ do

            it "No spaces is equivalent to variyng number of spaces" $ do
                process "2*3+2" `shouldBe` process "2 *   3 +2"
              
            it "Spaces are redundant next to all operators" $ do
                process "2+3-2*3/(2^3)" `shouldBe` process " 2 +  3 -  2 * 3  / ( 2   ^ 3 ) "
              
        describe "Complex inputs" $ do

            it "(67 + 2 * 3 - 67 + 2/1 - 7)" $ do
                process "(67 + 2 * 3 - 67 + 2/1 - 7)" `shouldBe` "1"

            it "(2) + (17*2-30) * (5)+2 - (8/2)*4" $ do
                process "(2) + (17*2-30) * (5)+2 - (8/2)*4" `shouldBe` "8"

            it "(( ((2)) + 4))*((5))" $ do
                process "(( ((2)) + 4))*((5))" `shouldBe` "30"

            it "32.4 * 3 + 2^2^(2^2)" $ do
                process "32.4 * 3 + 2^2^(2^2)" `shouldBe` "353.2"
                
            it "5.3 * 4.34 ^3.33 - (1.45^1.234)" $ do
                process "5.3 * 4.34 ^3.33 - (1.45^1.234)" `shouldBe` "701.6795040477153"

            it "-2.34 +--+----+ (3.23 -0.34^3) +- (-2) * 4" $ do
                process "2.34 +--+----+ (3.23 -0.34^3) +- (-2) * 4" `shouldBe` "13.530695999999999"
