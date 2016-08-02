## The Parsing Problem

Parsing is the mechanism we use to make sense of structured information, e.g. written or spoken language. In the case of written language, it involves several steps:

- recognizing the characters of the writing system,
- identifying words,
- identifying sentences,
- identifying paragraphs etc.

To be able to do so we need to know the writing system, spelling and grammar of the language in which the document is written.

For parsing structured text such as program source code, HTML or JSON, the problem is similar.

## Some handy functional machinery

### Returning functions as values


- So far, we have seen functions that take functions as arguments
- Functions can also return functions as values
- For example, partial application of a function:

~~~haskell
sum = foldl (+) 0
~~~

  Here _sum_ is the result returned by partial application of _foldl_.

- More explicitly, we can write this as:

~~~haskell
sum = \xs -> foldl (+) 0 xs
~~~

  Here _sum_ is a function resulting from partial application of _foldl_.

- Both are of course the same thing, just different interpretations.

### Function generators

- We can use this concept to generate parameterised functions

For example, the following function generates functions that add a constant number to their argument:

~~~haskell
gen_add_n = \n ->
    \x -> x+n

add_3 = gen_add_n 3
add_7 = gen_add_n 7

add_3 5 --> 8
add_7 4 --> 11
~~~

- This is of course not limited to numeric constants

For example, the following function generates functions that perform a given arithmetic operation on a constant number and their argument:

~~~haskell
gen_op_n = \op n ->
    \x -> x `op` n

add_3 = gen_op_n (+) 3
mult_7 = gen_op_n (*) 7

add_3 5 --> 8
mult_7 4 --> 28
~~~

## Practical parsing

### Cooking _soba_ noodles

To make the parsing problem more concrete, suppose you have to parse the following recipe and identify the different steps required in the preparation.

<blockquote><em>
Bring a large pot of water up to a boil. Unlike Italian pasta, you do not need to salt the water. Once it’s boiling, hold the noodles over the water and sprinkle them in strand by strand. Once all the noodles are in, stir gently so that they are all immersed in the water. Bring the water back up to a gentle boil, then lower the heat so that the water is just simmering. (This differs from the ’rolling boil’ that’s recommended for pasta.) If the water threatens to boil over, add about 1/2 cup of cold water (but if you lower the heat to the gentle simmer, and have a big enough pot, this shouldn’t be necessary). Cook for about 7 to 8 minutes, or following the package directions (for thinner noodles 5 to 6 minutes may be enough. Test by eating a strand - it should be cooked through, not al dente, but not mushy either).
</em></blockquote>

### Parsing text

1. Typically, a functional program is organised around a tree-like data structure with an algebraic data type that represents the core data
2. A parser reads text input and generates the tree
3. Functions perform transformations or traversals on the tree
4. Pretty-printer functions output the tree (original or transformed)

### Alternative approaches to parsing

- Don’t bother with parsing, just make the user provide in put in an awkward form. _A common approach, but please don’t do this!_
- Write the parser by hand, with just ordinary list processing functions. Possible, but hard and not reusable. Don’t do it.
- Write the parser using _regular expressions_. Tempting but limiting and not reusable. Don’t do it, unless your input text format is very simple.
- Use _parser combinators_. For most purposes, this is the recommended approach, for everything from basic input formats to medium size programming languages (e.g. Pascal) or subsets of languages like C and Fortran.
- Use a parser generator, e.g. *bison, antlr, [happy](https://www.haskell.org/happy/)*. The best approach for heavy-weight parsers, for very large programming languages.

### Parser combinators

- Parser combinators are functions that allow you to combine smaller parsers into bigger ones.
- They are higher-order functions that take functions as arguments and return functions
- A parser combinator library provides both basic parsers (for words, numbers etc.) and combinators.


### Parsec: monadic parsing combinators

- There are many parsing libraries for Haskell.
- One of the most widely used is Parsec, which is robust, flexible, expressive, and efficient.
- Parsec operates in a monad.
- You can find the source and links to documentation and examples for Parsec [on the Parsec GitHub page](https://github.com/aslatter/parsec).


### A quick primer on _monads_

You may have heard the term _monad_ before, and we will discuss the concept in detail in [a later session](https://www.futurelearn.com/courses/functional-programming-haskell/1/steps/97094). Haskell uses monads to structure computations. You have already encountered the IO monad, which you need to use to perform IO in a Haskell program. A typical example is

~~~haskell

hello :: String -> IO String
hello x =
  do
     putStrLn "Hello, "++x
     putStrLn "What's your name?"
     name <- getLine
     return name
~~~

This illustrates the key syntactic features of a monad: the `do` keyword, the sequence of commands, the way to extract information from a monadic computation using the left arrow `<-` and the `return` keyword. In fact, using the do-notation is quite similar to imperative programming.

Also note the return value of our _hello_ function: not just _String_ but _IO String_. A computation done in a monad returns a "monadic" type, we say that the string is returned inside the monad.


### The form of a parser

- For example, suppose we want to parse a string of the form _<tag>_, where _tag_ must be a word, and return the tag as a type _Tag_.

~~~haskell
data Tag = MkTag String

parseTag :: Parser Tag
parseTag =
  do  char "<"
      x <- identifier
      char ">"
      return (MkTag x)
~~~

As you can see, the parser consists of a number of functions (e.g. _char_ and _identfifier_) that are called sequentially. Also, the return value is of type _Parser Tag_, not simply _Tag_.
This is because `parseTag` is not returning a _value_, instead it returns a _parser_. We can combine this parser with other parsers, and then we can execute the final parser on our data. We will cover this approach in more detail [in the tutorial](https://www.futurelearn.com/courses/functional-programming-haskell/1/steps/108393).

### Testing a parser

To test your parser, start `ghci`:

~~~haskell
[wim@fp4 ~]$ ghci
GHCi, version 7.4.1: http://www.haskell.org/ghc/  :? for help
Loading package ghc-prim ... linking ... done.
Loading package integer-gmp ... linking ... done.
Loading package base ... linking ... done.
~~~

Then, import Parsec:

~~~haskell
Prelude> import Text.ParserCombinators.Parsec
~~~

Parsec provides the handy `parseTest` function, which takes a parser and a string and runs it. Let's try and run the parser `char 'b'` on the string `"cons"`:

~~~haskell
Prelude Text.ParserCombinators.Parsec> parseTest (char 'b') "cons"
Loading package bytestring-0.9.2.1 ... linking ... done.
Loading package transformers-0.2.2.0 ... linking ... done.
Loading package mtl-2.0.1.0 ... linking ... done.
Loading package array-0.4.0.0 ... linking ... done.
Loading package deepseq-1.3.0.0 ... linking ... done.
Loading package text-0.11.2.0 ... linking ... done.
Loading package parsec-3.1.2 ... linking ... done.
~~~

Because the string "cons" does not contain the character 'b', we get a parse error:

~~~haskell
parse error at (line 1, column 1):
unexpected 'c'
expecting 'b'
~~~

Let's try with `char 'c'`:

~~~haskell
Prelude Text.ParserCombinators.Parsec> parseTest (char 'c') "cons"
'c'
Prelude Text.ParserCombinators.Parsec>
~~~

This time the parse succeeded.

### Running the parser

The actual code for creating a complete parser is covered [in the tutorial](https://www.futurelearn.com/courses/functional-programming-haskell/1/steps/108393).

### Anatomy of a basic parser

- All parser combinators are functions that return functions.
- It is the returned function that operates on the string, not the parser combinator function.
- The basic parsers (_identifier_,_natural_,_char_) take either no arguments (e.g. _identifier_) or one or more strings for parametrisation (e.g. _char_).

~~~haskell
char = \ch -> \str ->
      -- try to match the character ch
      -- return the result
~~~

If the match succeeds, the matching string is removed from the input string; otherwise, the original string is returned, e.g.

~~~haskell
char "c" "cons" -->
"c"
char "b" "cons" -->
parse error at (line 1, column 1):
unexpected "c"
expecting "b"
~~~

### Anatomy of a parser combinator

- Parser combinators such as `<|>` and `parens` take other parsers as arguments.

~~~haskell
parens = \p ->
    \str ->
        -- first match "("
        -- perform the parse of p if "(" was found
        -- then match ")"
        -- return the result
~~~

### Parsing alternatives

- Often we want to try one parser; if that fails, then try another one instead. The choice combinator `<|>` provides this functionality.
- Example: `letter_digit` will match either a letter or a digit.

~~~haskell
letter_digit :: Parser Char
letter_digit =
  do  x <- letter <|> digit
      return x
~~~

#### Running alternative parsers

~~~haskell
*Main> run letter_digit "b2"
"b"

*Main> run letter_digit "2b"
"2"

*Main> run letter_digit "*2"
parse error at (line 1, column 1):
unexpected "*"
expecting letter or digit
~~~

### Parsing alternative strings

Suppose we want to match either _bag_ or _bog_, but nothing else.

~~~haskell
bag_bog :: Parser String
bag_bog =
  do  xs <- string "bag" <|> string "bog"
      return xs
~~~

#### Failed alternative consumes input

So far so good:
~~~haskell
*Main> run bag_bog "bag"
"bag"
~~~

And a non-matching string fails, as expected.

~~~haskell
*Main> run bag_bog "bug"
parse error at (line 1, column 1):
unexpected "u"
expecting "bag"
~~~

But there’s a problem!

~~~haskell
*Main> run bag_bog "bog"
parse error at (line 1, column 1):
unexpected "o"
expecting "bag"
~~~

The first parser _string “bag”_ matched the 'b' but then failed on the 'a'. _It has now consumed the 'b'._ The second parser _string “bog”_ now tries to match 'b' against 'o', which of course fails.

###*try* — don’t consume input on failed parse

To allow you to parse tentatively without consuming any input, Parsec provide the `try` function:

~~~haskell
bag_bog_try :: Parser String
bag_bog_try =
  do  xs <- try (string "bag") <|> string "bog"
      return xs
~~~

#### Trying a parse without consuming input

~~~haskell
*Main> run bag_bog_try "bag"
"bag"
~~~

~~~haskell
*Main> run bag_bog_try "bug"
parse error at (line 1, column 1):
unexpected "u"
expecting "bog"
~~~

~~~haskell
*Main> run bag_bog_try "bog"
"bog"
~~~

### Some parsers from the _Combinator_ library

The Parsec _Combinator_ library provides some small parsers that are useful for defining bigger ones:

- $$char\; "?"$$ — $$char$$ is applied to a character, and it gives a parser that matches that character
- $$letter$$ — matches any letter
- $$digit$$ — matches any digit
- $$string$$ — matches a string of characters
- $$stringLiteral\; "xyz*"$$ — matches the string argument
- $$many\; p$$ — matches 0 or more occurrences of parser $$p$$
- $$many1\; p$$ — matches 1 or more occurrences of parser $$p$$

#### Example: parsing variable names

Here is an example of a parser for variable names, using the predefined parsers `<|>`, `letter`, `digit` and `many`
~~~haskell
varname :: Parser String
varname =
  do  x <- letter
      xs <- many (letter <|> digit)
      return (x:xs)
~~~

~~~haskell
*Main> run varname "a4cc7*5"
"a4cc7"
*Main> run varname "34a"
parse error at (line 1, column 1):
unexpected "3"
expecting letter
~~~

### Building expression parsers using the _Expr_ library

- Arithmetic expressions are complex to parse because of the rules of precedence and the arity of the operators.
- The Parsec  _Expr_ library provides support for expression parsing, so you don’t have to write your own expression parser.

~~~haskell
import Text.ParserCombinators.Parsec.Expr

expr_parser :: Parser Expr
expr_parser = buildExpressionParser optable term <?> "expression"

optable =
  let
    op name assoc   =
      Infix ( do {  reservedOp name;
          return (\x y ->(Op (MkOpExpr name x y))) } ) assoc
    prefix name =
      Prefix  (
        reservedOp name >>
            return (\x->(Pref (MkPrefixOpExpr name x))) )
  in
    [ [ op "*"  AssocLeft, op "/"  AssocLeft, op "%" AssocLeft ]
    , [ op "+"  AssocLeft, op "-"  AssocLeft ], [ prefix "-" ] ]
~~~

#### Some extra syntax

This example uses some additional monad syntax: you can use braces and semicolons instead of indentation; and the `>>` operator is also a shorter way of writing the do notation:

~~~haskell
do
  expr1
  expr2
~~~

can be written as

~~~haskell
expr1 >> expr2
~~~

Also note the use of the `<?>` operator, this is used to define a custom error message in case a parse fails without consuming any input. This is a very useful debugging feature.

### Easy parsing of programming languages using the _Token_ library

- The Parsec _Token_ library also has support for parsing of programming languages with a mechanism to define the syntax and keywords through `makeTokenParser` which creates a token parser (aka 'lexer') from a language definition.
- For simple cases, you can use the empty  language definition `emptyDef`.

~~~haskell
import Text.ParserCombinators.Parsec.Expr
import qualified Text.ParserCombinators.Parsec.Token as P
lexer       = P.makeTokenParser emptyDef

parens          = P.parens lexer
commaSep        = P.commaSep lexer
-- and many more
~~~

An example of this functionality is given [in the tutorial](https://www.futurelearn.com/courses/functional-programming-haskell/1/steps/108393).
