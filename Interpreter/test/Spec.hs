module Test where

import Test.Hspec

import Lexic
import Parser
import Calculator
import Printable

main :: IO ()
main = hspec $ do
  
    describe "Lexical Context" $

        describe "Custom Instances for data type Token" $ do

            describe "Compares Operator constructors according to precedence" $ do

                it "Addition and Subtraction are equal" $
                    Op '+' == Op '-' `shouldBe` True
                  
                it "Multiplication and Division are equal" $
                    Op '*' == Op '/' `shouldBe` True
                  
                it "Multiplication is greater than Addition" $
                    Op '*' > Op '+' `shouldBe` True
                  
                it "Subtraction is lower than Division" $
                    Op '-' < Op '/' `shouldBe` True
                  
                it "Exponentiation is greater than Multiplication" $
                    Op '^' > Op '-' `shouldBe` True
                  
                it "Division is lower than Exponentiation" $
                    Op '/' < Op '^' `shouldBe` True

            describe "Show instance for each Token construnctor" $ do

                it "Operator constructor" $
                    show (Op '+') `shouldBe` "'+'"
                  
                it "Numeric constructor" $
                    show (Num 5) `shouldBe` "5.0"
                  
                it "Left Bracket constructor" $
                    show LBr `shouldBe` "("
                  
                it "Right Bracket constructor" $
                    show RBr `shouldBe` ")"
                  
                it "Error constructor" $
                    show (Err "This is an error") `shouldBe` "This is an error"
                  

    describe "Parsing" $ do

        describe "Reads and converts to Token" $ do

            it "Reads Number" $
                parse "2" `shouldBe` [Num 2]

            it "Reads Operator" $
                parse "+" `shouldBe` [Op '+']

            it "Reads Left Bracket" $
                show (parse "(") `shouldBe` show [LBr]

            it "Reads Right Bracket" $
                show (parse ")") `shouldBe` show [RBr]

            it "Reads invalid token as error" $
                show (parse "k") `shouldBe` show [Err "Invalid token: 'k'"]

            it "Identifies negative numbers at the beginning" $
                show (parse "-5 + 3") `shouldBe` "[-5.0,'+',3.0]"

            it "Simplifies consecutive addition/subtraction operators" $
                show (parse "5 -+ 4 -- 3 ++ 2 -+- 1 --- 0") `shouldBe` "[5.0,'-',4.0,'+',3.0,'+',2.0,'+',1.0,'-',0.0]"
                     
            it "Reads 2.3456 - (((4)))" $
                show (parse "2.3456 - (((4)))") `shouldBe` "[2.3456,'-',(,(,(,4.0,),),)]"

            it "Reads 2 + 3 * 5 - (2^3^2)" $
                show (parse "2 + 3 * 5 - (2^3^2)") `shouldBe` "[2.0,'+',3.0,'*',5.0,'-',(,2.0,'^',3.0,'^',2.0,)]"

            it "Reads 2 / (5 - 1) * 7 k" $
                show (parse "2 / (5 - 1) * 7 k") `shouldBe` "[2.0,'/',(,5.0,'-',1.0,),'*',7.0,Invalid token: 'k']"

            it "Reads -5 -+--+ 1 + (-1 + 3) + (1 -2) + (-1)" $
                show (parse "-5 -+--+ 1 + (-1 + 3) + (1 -2) + (-1)") `shouldBe` "[-5.0,'-',1.0,'+',(,-1.0,'+',3.0,),'+',(,1.0,'-',2.0,),'+',(,-1.0,)]"


        describe "Converts to postfix notation - Oxford Math Center Website examples" $ do

            it "A * B + C becomes A B * C +" $
                show (shunt [Num 2.0,Op '*',Num 3.0,Op '+',Num 4.0] []) `shouldBe` "[2.0,3.0,'*',4.0,'+']"
  
            it "A + B * C becomes A B C * +" $
                show (shunt [Num 2.0,Op '+',Num 3.0,Op '*',Num 4.0] []) `shouldBe` "[2.0,3.0,4.0,'*','+']"
  
            it "A * (B + C) becomes A B C + *" $
                show (shunt [Num 2.0,Op '*',LBr,Num 3.0,Op '+',Num 4.0,RBr] []) `shouldBe` "[2.0,3.0,4.0,'+','*']"
  
            it "A - B + C becomes A B - C +" $
                show (shunt [Num 2.0,Op '-',Num 3.0,Op '+',Num 4.0] []) `shouldBe` "[2.0,3.0,'-',4.0,'+']"
  
            it "A * B ^ C + D becomes A B C ^ * D +" $
                show (shunt [Num 2.0,Op '*',Num 3.0,Op '^',Num 4.0,Op '+',Num 5.0] []) `shouldBe` "[2.0,3.0,4.0,'^','*',5.0,'+']"
  
            it "A * (B + C * D) + E becomes A B C D * + * E +" $
                show (shunt [Num 2.0,Op '*',LBr,Num 3.0,Op '+',Num 4.0,Op '*',Num 5.0,RBr,Op '+',Num 6.0] []) `shouldBe` "[2.0,3.0,4.0,5.0,'*','+','*',6.0,'+']"

  
    describe "Calculating" $ do

        describe "Simple expressions" $ do

            it "Addition - 2+3" $
                process "2+3" `shouldBe` "5"

            it "Subtraction - 2-3" $
                process "2-3" `shouldBe` "-1"

            it "Multiplication - 2*3" $
                process "2*3" `shouldBe` "6"

            it "Division - 6/3" $
                process "6/3" `shouldBe` "2"

            it "Exponentiation - 2^3" $
                process "2^3" `shouldBe` "8"

        describe "Precedence" $ do

            it "Parens - 2*(2-3)" $
                process "2*(2-3)" `shouldBe` "-2" 

            it "Multiplication over Addition - 2+3*2" $
                process "2+3*2" `shouldBe` "8"

            it "Multiplication and Division at same level - 4/2*2" $
                process "4/2*2" `shouldBe` "4"

            it "Exponentiation over Multiplication - 2*3^2" $
                process "2*3^2" `shouldBe` "18"

        describe "Decimal" $ do

            it "Result needs decimal - 3/4" $
                process "3/4" `shouldBe` "0.75"
              
            it "Decimal is redundant (all zeros) - 8/4" $
                process "8/4" `shouldBe` "2"

        describe "Whitespace" $ do

            it "No spaces is equivalent to variyng number of spaces" $
                process "2*3+2" `shouldBe` process "2 *   3 +2"
              
            it "Spaces are redundant next to all operators" $
                process "2+3-2*3/(2^3)" `shouldBe` process " 2 +  3 -  2 * 3  / ( 2   ^ 3 ) "
              
        describe "Complex valid inputs" $ do

            it "(67 + 2 * 3 - 67 + 2/1 - 7)" $
                process "(67 + 2 * 3 - 67 + 2/1 - 7)" `shouldBe` "1"

            it "(2) + (17*2-30) * (5)+2 - (8/2)*4" $
                process "(2) + (17*2-30) * (5)+2 - (8/2)*4" `shouldBe` "8"

            it "(( ((2)) + 4))*((5))" $
                process "(( ((2)) + 4))*((5))" `shouldBe` "30"

            it "32.4 * 3 + 2^2^(2^2)" $
                process "32.4 * 3 + 2^2^(2^2)" `shouldBe` "353.2"
                
            it "5.3 * 4.34 ^3.33 - (1.45^1.234)" $
                process "5.3 * 4.34 ^3.33 - (1.45^1.234)" `shouldBe` "701.6795040477153"

            it "-2.34 +--+----+ (3.23 -0.34^3) +- (-2) * 4" $
                process "2.34 +--+----+ (3.23 -0.34^3) +- (-2) * 4" `shouldBe` "13.530695999999999"


    describe "Error Handling" $ do

        describe "Invalid Tokens" $ do

            it "A digit" $
                process "2+l" `shouldBe` "Invalid token: 'l'"

            it "Unrecognised operator" $
                process "2#3" `shouldBe` "Invalid token: '#'"

            it "A corrupted number" $
                process "2*2.-43" `shouldBe` "Error while parsing number: 2.-"

            it "A number with two consecutive dots" $
                process "2*2..43" `shouldBe` "Error while parsing number: 2.."

            it "A number with two non-consecutive dots" $
                process "2*2.4.3" `shouldBe` "Error while parsing number: 2.4.3"

        describe "Syntax Errors" $ do

            describe "Unmatched parens" $ do
          
                it "Single unmatched left parens" $
                    process "2+2 * ((4)" `shouldBe` "Syntax error: 1 unmatched '('"

                it "Multiple unmatched left parens" $
                    process "2+2 * ((4) - (((3 - 1)" `shouldBe` "Syntax error: 3 unmatched '('"

                it "Unmatched right parens in the middle of input" $
                    process "2+2 * (4)) + 3" `shouldBe` "Syntax error: attempt to close unopened parens at ) '+'"

                it "Unmatched left parens at the end of input" $
                    process "2+2 * (4))" `shouldBe` "Syntax error: attempt to close unopened parens at the end of input"


            describe "Invalid beginning token" $ do

                it "Starts with '*'" $
                    process "* 2+3" `shouldBe` "Syntax error: expression cannot start with '*'"

                it "Starts with '/'" $
                    process "/ 2+3" `shouldBe` "Syntax error: expression cannot start with '/'"

                it "Starts with '^'" $
                    process "^ 2+3" `shouldBe` "Syntax error: expression cannot start with '^'"


            describe "Invalid ending token" $ do

                it "Ends with '+'" $
                    process "2+3 +" `shouldBe` "Syntax error: expression cannot end with '+'"

                it "Ends with '-'" $
                    process "2+3 -" `shouldBe` "Syntax error: expression cannot end with '-'"

                it "Ends with '*'" $
                    process "2+3 *" `shouldBe` "Syntax error: expression cannot end with '*'"

                it "Ends with '/'" $
                    process "2+3 /" `shouldBe` "Syntax error: expression cannot end with '/'"

                it "Ends with '^'" $
                    process "2+3 ^" `shouldBe` "Syntax error: expression cannot end with '^'"

                it "Ends with '('" $
                    process "2+3 + (" `shouldBe` "Syntax error: 1 unmatched '('\nSyntax error: expression cannot end with ("


            describe "Invalid token sequences" $ do

                it "Two numbers in a row" $
                    process "2+3 5" `shouldBe` "Syntax error: Invalid token sequence: 3 5"

                it "Invalid operator sequence" $
                    process "2+*3" `shouldBe` "Syntax error: Invalid token sequence: '+' '*'"

                it "Operator followed by right parens" $
                    process "(2+3 *)" `shouldBe` "Syntax error: Invalid token sequence: '*' )"

                it "Left parens followed by operator" $
                    process "(*2+3)" `shouldBe` "Syntax error: Invalid token sequence: ( '*'"

                it "Right parens followed by left parens" $
                    process "(2+3)(2)" `shouldBe` "Syntax error: Invalid token sequence: ) ("

                it "Empty parens" $
                    process "2+3 - ()" `shouldBe` "Syntax error: Invalid token sequence: ( )"

          
            describe "Prioritises invalid inputs over syntax ones" $

                it "Numerous syntax errors and one invalid character - outputs 'invalid token' only" $
                    process "2+3 ) *-/ 5))) (() k" `shouldBe`"Invalid token: 'k'"


            describe "Complex invalid inputs" $ do

                it "2 + 4 # k - ;" $
                    process "2 + 4 # k - ;" `shouldBe` "Invalid token: '#'\nInvalid token: 'k'\nInvalid token: ';'"

                it "(2)) + (17*/2-30) (5-)+ -" $
                    process "(2)) + (17*/2-30) (5-)+ -" `shouldBe` "Syntax error: attempt to close unopened parens at ) '+'\nSyntax error: Invalid token sequence: '*' '/'\nSyntax error: Invalid token sequence: ) (\nSyntax error: Invalid token sequence: '-' )\nSyntax error: expression cannot end with '-'"

                it "(((()) +--+* 8^(5 + (" $
                    process "(((()) +--+* 8^(5 + (" `shouldBe` "Syntax error: 4 unmatched '('\nSyntax error: Invalid token sequence: ( )\nSyntax error: Invalid token sequence: '+' '*'\nSyntax error: expression cannot end with ("

                it "* 3 + 2^^(^2) ---++ 8.455" $
                    process "* 3 + 2^^(^2) ---++ 8.455" `shouldBe` "Syntax error: expression cannot start with '*'\nSyntax error: Invalid token sequence: '^' '^'\nSyntax error: Invalid token sequence: ( '^'"
                

