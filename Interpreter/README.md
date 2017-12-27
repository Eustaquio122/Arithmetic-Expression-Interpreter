Arithmetic Interpreter
========================

An interpreter for simple arithmetic expressions.

Processes addition, subtraction, multiplication, division, and exponentiation operations.

Handles parens and multiple/consecutive addition/subtraction operations.

Produces parsing errors for invalid character inputs and a number of different syntax characters: unmatched parens, invalid sequences of operators, invalid elements at the beggining/end of input, etc.



Using the Program
===================

Clone this repository.

Go to src and either run:

```
> ghc -o Intepreter --make main.hs
> ./Interpreter

```

or:

```
> runhaskell main.hs

```

Example Usage:
----------------

```
~/repos/haskell/arithmetic-interpreter/src > ghc -o Intepreter --make main.hs
~/repos/haskell/arithmetic-interpreter/src > ./Intepreter
Enter expression ('q' to quit):
7 * (2.3^(1.2*0.456)) /1.23
8.97698295239268
Enter expression ('q' to quit):
8 - (((((4))))) + ((((3)-1)))
6
Enter expression ('q' to quit):
-3^2     * (2/1) -----++++ 4
14
Enter expression ('q' to quit):
* 4 - (5 * 2) - / 3 )
Syntax error: attempt to close unopened parens at the end of input
Syntax error: expression cannot start with '*'
Syntax error: Invalid token sequence: '-' '/'
Enter expression ('q' to quit):
5 - 3 + k * (8 # 2)
Invalid token: 'k'
Invalid token: '#'
Enter expression ('q' to quit):
(((((((4) + 3
Syntax error: 6 unmatched '('
Enter expression ('q' to quit):
q
~/repos/haskell/arithmetic-interpreter/src >

```


Tests
=======

To run the tests, go to src and:

```
> runhaskell tests.hs

```

Tests Output:
---------------

```
~/repos/haskell/arithmetic-interpreter/src > runhaskell tests.hs

Lexical Context
  Custom Instances for data type Token
    Compares Operator constructors according to precedence
      Addition and Subtraction are equal
      Multiplication and Division are equal
      Multiplication is greater than Addition
      Subtraction is lower than Division
      Exponentiation is greater than Multiplication
      Division is lower than Exponentiation
    Show instance for each Token construnctor
      Operator constructor
      Numeric constructor
      Left Bracket constructor
      Right Bracket constructor
      Error constructor
Parsing
  Reads and converts to Token
    Reads Number
    Reads Operator
    Reads Left Bracket
    Reads Right Bracket
    Reads invalid token as error
    Identifies negative numbers at the beginning
    Simplifies consecutive addition/subtraction operators
    Reads 2.3456 - (((4)))
    Reads 2 + 3 * 5 - (2^3^2)
    Reads 2 / (5 - 1) * 7 k
    Reads -5 -+--+ 1 + (-1 + 3) + (1 -2) + (-1)
  Converts to postfix notation - Oxford Math Center Website examples
    A * B + C becomes A B * C +
    A + B * C becomes A B C * +
    A * (B + C) becomes A B C + *
    A - B + C becomes A B - C +
    A * B ^ C + D becomes A B C ^ * D +
    A * (B + C * D) + E becomes A B C D * + * E +
Calculating
  Simple expressions
    Addition - 2+3
    Subtraction - 2-3
    Multiplication - 2*3
    Division - 6/3
    Exponentiation - 2^3
  Precedence
    Parens - 2*(2-3)
    Multiplication over Addition - 2+3*2
    Multiplication and Division at same level - 4/2*2
    Exponentiation over Multiplication - 2*3^2
  Decimal
    Result needs decimal - 3/4
    Decimal is redundant (all zeros) - 8/4
  Whitespace
    No spaces is equivalent to variyng number of spaces
    Spaces are redundant next to all operators
  Complex valid inputs
    (67 + 2 * 3 - 67 + 2/1 - 7)
    (2) + (17*2-30) * (5)+2 - (8/2)*4
    (( ((2)) + 4))*((5))
    32.4 * 3 + 2^2^(2^2)
    5.3 * 4.34 ^3.33 - (1.45^1.234)
    -2.34 +--+----+ (3.23 -0.34^3) +- (-2) * 4
Error Handling
  Invalid Tokens
    A digit
    Unrecognised operator
    A corrupted number
    A number with two consecutive dots
    A number with two non-consecutive dots
  Syntax Errors
    Unmatched parens
      Single unmatched left parens
      Multiple unmatched left parens
      Unmatched right parens in the middle of input
      Unmatched left parens at the end of input
    Invalid beginning token
      Starts with '*'
      Starts with '/'
      Starts with '^'
    Invalid ending token
      Ends with '+'
      Ends with '-'
      Ends with '*'
      Ends with '/'
      Ends with '^'
      Ends with '('
    Invalid token sequences
      Two numbers in a row
      Invalid operator sequence
      Operator followed by right parens
      Left parens followed by operator
      Right parens followed by left parens
      Empty parens
    Prioritises invalid inputs over syntax ones
      Numerous syntax errors and one invalid character - outputs 'invalid token' only
    Complex invalid inputs
      2 + 4 # k - ;
      (2)) + (17*/2-30) (5-)+ -
      (((()) +--+* 8^(5 + (
      * 3 + 2^^(^2) ---++ 8.455

Finished in 0.0388 seconds
76 examples, 0 failures

```
