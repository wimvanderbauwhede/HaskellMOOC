<h2 id="the-parsing-problem">The Parsing Problem</h2>

<p>Parsing is the mechanism we use to make sense of structured information, e.g. written or spoken language. In the case of written language, it involves recognizing the characters of the writing system, then identifying words, then sentences, paragraphs etc.</p><p>To do so we need to know the writing system, spelling and gramar of the language in which the document is written.</p>
<p>For parsing structured text such as program source code, HTML or JSON, the problem is similar.</p>

<h2 id="machinery">Some handy functional machinery</h2>

<h3 id="returning-functions-as-values-1">Returning functions as values</h3>

- So far, we have seen functions that take functions as arguments
- Functions can also return functions as values
- For example, partial application of a function:

~~~haskell

sum = foldl (+) 0

~~~

  Here `sum` is the result returned by partial application of `foldl`.

- More explicitly, we can write this as:

~~~haskell

    sum = \xs -> foldl (+) 0 xs

~~~

Here `sum` is a function resulting from partial application of `foldl`.

- Both are of course the same thing, just different interpretations.


<h3 id="function-generators-1">Function generators</h3>


- We can use this concept to generate parameterised functions.

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
gen_op_n = \op n ->;
    \x -> x `op` n

add_3 = gen_op_n (+) 3
mult_7 = gen_op_n (*) 7

add_3 5 --> 8
mult_7 4 --> 28
~~~

<h2 id="cooking-soba-noodles">Practical parsing</h2>


<h3 id="cooking-soba-noodles">Cooking soba noodles</h3>

<p>To make the parsing problem more concrete, suppose you have to parse the following recipe and identify the different steps required in the preparation.</p>

<blockquote style="font-family: times">
Bring a large pot of water up to a boil. Unlike Italian pasta, you do not need to salt the water. Once it’s boiling, hold the noodles over the water and sprinkle them in strand by strand. Once all the noodles are in, stir gently so that they are all immersed in the water. Bring the water back up to a gentle boil, then lower the heat so that the water is just simmering. (This differs from the ’rolling boil’ that’s recommended for pasta.) If the water threatens to boil over, add about 1/2 cup of cold water (but if you lower the heat to the gentle simmer, and have a big enough pot, this shouldn’t be necessary). Cook for about 7 to 8 minutes, or following the package directions (for thinner noodles 5 to 6 minutes may be enough. Test by eating a strand - it should be cooked through, not al dente, but not mushy either).
</blockquote>



<h3 id="parsing-text">Parsing text</h3>



<ol>
<li><p>Typically, a functional program is organised around a tree-like data structure with an algebraic data type that represents the core data</p></li>
<li><p>A parser reads text input and generates the tree</p></li>
<li><p>Functions perform transformations or traversals on the tree</p></li>
<li><p>Pretty-printer functions output the tree (original or transformed)</p></li>
</ol>


<h3 id="alternative-approaches-to-parsing">Alternative approaches to parsing</h3>
<ul>
<li><p>Don’t bother with parsing, just make the user provide in put in an awkward form. <em>A common approach, but please don’t do this!</em></p></li>
<li><p>Write the parser by hand, with just ordinary list processing functions. Possible, but hard and not reusable. Don’t do it.</p></li>
<li><p>Write the parser using <em>regular expressions</em>. Tempting but limiting and not reusable. Don’t do it, unless your input text format is very simple.</p></li>
<li><p>Use <em>parser combinators</em>. For most purposes, this is the recommended approach, for everything from basic input formats to medium size programming languages (e.g. Pascal) or subsets of languages like C and Fortran.</p></li>
<li><p>Use a parser generator, e.g. <em>bison, antlr, happy</em>. The best approach for heavy-weight parsers, for very large programming languages.</p></li>
</ul>

<h3 id="parser-combinators">Parser combinators</h3>
<ul>
<li><p>Parser combinators are functions that allow you to combine smaller parsers into bigger ones.</p></li>
<li><p>They are higher-order functions that take functions as arguments and return functions</p></li>
<li><p>A parser combinator library provides both basic parsers (for words, numbers etc.) and combinators.</p></li>
</ul>

<h3 id="parsec-monadic-parsing-combinators">Parsec: monadic parsing combinators</h3>

- There are many parsing libraries for Haskell.
- One of the most widely used is Parsec, which is robust, flexible, expressive, and efficient.
- Parsec operates in a monad.

### A Quick Primer on Monads

You may have heard the term _monad_ before, and we will discuss the concept in detail in a later session. Haskell uses monads to structure computations. You have already encountered the IO monad, which you need to use to perform IO in a Haskell program. A typical example is

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

<h3 id="form-of-a-parser">Form of a parser</h3>

- For example, suppose we want to parse a string of the form \(&lt;tag&gt;\), where \(tag\) must be a word, and return the tag as a type \(Tag\).

~~~haskell
data Tag = MkTag String

parseTag :: Parser Tag
parseTag =
  do
      char '<'
      x <- identifier
      char '>'
      return (MkTag x)
~~~

As you can see, the parser consists of a number of functions (e.g. _char_ and _identfifier_) that are called sequentially. Also, the return value is of type _Parser Tag_, not simply +Tag_.
This is because `parseTag` is not returning a _value_, instead it returns a _parser_. We can combine this parser with other parsers, and then we can execute the final parser on our data. We will cover this approach in more detail in the tutorial.




<h3 id="testing-a-parser">Testing a parser</h3>
<pre><code>[wim@fp4 ~]$ ghci
GHCi, version 7.4.1: http://www.haskell.org/ghc/  :? for help
Loading package ghc-prim ... linking ... done.
Loading package integer-gmp ... linking ... done.
Loading package base ... linking ... done.
Prelude&gt; import Text.ParserCombinators.Parsec
Prelude Text.ParserCombinators.Parsec&gt; parseTest (char &#39;b&#39;) &quot;cons&quot;
Loading package bytestring-0.9.2.1 ... linking ... done.
Loading package transformers-0.2.2.0 ... linking ... done.
Loading package mtl-2.0.1.0 ... linking ... done.
Loading package array-0.4.0.0 ... linking ... done.
Loading package deepseq-1.3.0.0 ... linking ... done.
Loading package text-0.11.2.0 ... linking ... done.
Loading package parsec-3.1.2 ... linking ... done.
parse error at (line 1, column 1):
unexpected &quot;c&quot;
expecting &quot;b&quot;
Prelude Text.ParserCombinators.Parsec&gt; parseTest (char &#39;c&#39;) &quot;cons&quot;
&#39;c&#39;
Prelude Text.ParserCombinators.Parsec&gt; </code></pre>
<h3 id="running-the-parser">Running the parser</h3>
<pre><code>*Main&gt; run parseTag &quot;&lt;div&gt;&quot;
Loading package parsec-2.1.0.1 ... linking ... done.
MkTag &quot;div&quot;</code></pre>
<pre><code>*Main&gt; run parseTag &quot;div&quot;
parse error at (line 1, column 1):
unexpected &quot;d&quot;
expecting &quot;&lt;&quot;
*Main&gt; </code></pre>


<h3 id="anatomy-of-a-basic-parser">Anatomy of a basic parser</h3>

- All parser combinators are functions that return functions.
- It is the returned function that operates on the string, not the parser combinator function.
- The basic parsers (\(identifier\),\(natural\),\(char\)) take either no arguments (e.g. \(identifier\)) or one or more strings for parametrisation (e.g. \(char\)).

~~~haskell
char = \ch -> \str ->
      -- try to match the character ch
      -- return the result
~~~

If the match succeeds, the matching string is removed from the input string; otherwise, the original string is returned, e.g.

<pre><code>char &#39;c&#39; &quot;cons&quot; --&gt;
&#39;c&#39;
char &#39;b&#39; &quot;cons&quot; --&gt;
parse error at (line 1, column 1):
unexpected &quot;c&quot;
expecting &quot;b&quot;</code></pre>

<h3 id="anatomy-of-a-parser-combinator">Anatomy of a parser combinator</h3>

- Parser combinators such as `<|>` and \(parens\) take other parsers as arguments.

~~~haskell
parens = \p ->
    \str ->
        -- first match '('
        -- perform the parse of p if '(' was found
        -- then match ')'
        -- return the result
~~~

<h3 id="parsing-alternatives">Parsing alternatives</h3>

- Often we want to try one parser; if that fails, then try another one instead. The choice combinator <code>&lt;|&gt;</code> provides this functionality.
- Example: \(letter\_digit\) will match either a letter or a digit.

~~~haskell
letter_digit :: Parser Char
letter_digit =
  do  x <- letter <|> digit
      return x
~~~

<h4 id="running-alternative-parsers">Running alternative parsers</h4>

<pre><code>*Main&gt; run letter_digit &quot;b2&quot;
&#39;b&#39;</code></pre>
<pre><code>*Main&gt; run letter_digit &quot;2b&quot;
&#39;2&#39;</code></pre>
<pre><code>*Main&gt; run letter_digit &quot;*2&quot;
parse error at (line 1, column 1):
unexpected &quot;*&quot;
expecting letter or digit</code></pre>

<h3 id="parsing-alternative-strings">Parsing alternative strings</h3>

Suppose we want to match either <em>bag</em> or <em>bog</em>, but nothing else.

~~~haskell
bag_bog :: Parser String
bag_bog =
  do  xs <- string "bag" <|> string "bog"
      return xs
~~~

<h4 id="failed-alternative-consumes-input">Failed alternative consumes input</h4>

So far so good:

<pre><code>*Main&gt; run bag_bog &quot;bag&quot;
&quot;bag&quot;</code></pre>

And a non-matching string fails, as expected.

<pre><code>*Main&gt; run bag_bog &quot;bug&quot;
parse error at (line 1, column 1):
unexpected &quot;u&quot;
expecting &quot;bag&quot;</code></pre>

But there’s a problem!

<pre><code>*Main&gt; run bag_bog &quot;bog&quot;
parse error at (line 1, column 1):
unexpected &quot;o&quot;
expecting &quot;bag&quot;</code></pre>

The first parser <em>string “bag”</em> matched the <code>b</code> but then failed on the <code>a</code>. <em>It has now consumed the <code>b</code>.</em> The second parser <em>string “bog”</em> now tries to match <code>b</code>against<code>o</code>, which of course fails.

<h3 id="try-dont-consume-input-on-failed-parse">try — don’t consume input on failed parse</h3>

~~~haskell
bag_bog_try :: Parser String
bag_bog_try =
  do  xs <- try (string "bag") <|> string "bog"
      return xs
~~~

<h4 id="trying-a-parse-without-consuming-input">Trying a parse without consuming input</h4>

<pre><code>*Main&gt; run bag_bog_try &quot;bag&quot;
&quot;bag&quot;</code></pre>
<pre><code>*Main&gt; run bag_bog_try &quot;bug&quot;
parse error at (line 1, column 1):
unexpected &quot;u&quot;
expecting &quot;bog&quot;</code></pre>
<pre><code>*Main&gt; run bag_bog_try &quot;bog&quot;
&quot;bog&quot;</code></pre>

<h3 id="some-parsers-from-the-library">Some parsers from the library</h3>

The Parsec library provides some small parsers that are useful for defining bigger ones:

- _char '?'_ — _char_ is applied to a character, and it gives a parser that matches that character
- _letter_ — matches any letter
- _digit_ — matches any digit
- _string_ — matches a string of characters
- _stringLiteral "xyz*"_ — matches the string argument
- _many p_ — matches 0 or more occurrences of parser _p_
- _many1 p_ — matches 1 or more occurrences of parser _p_

<h3 id="variable-names">Variable names</h3>

~~~haskell
varname :: Parser String
varname =
  do  x <- letter
      xs<- many (letter <|> digit)
      return (x:xs)
~~~

<pre><code>*Main&gt; run varname &quot;a4cc7*5&quot;
&quot;a4cc7&quot;
*Main&gt; run varname &quot;34a&quot;
parse error at (line 1, column 1):
unexpected &quot;3&quot;
expecting letter</code></pre>

<h3 id="expression-parsers">Expression parsers</h3>

- Arithmetic expressions are complex to parse because of the rules of precedence and the arity of the operators.
- Parsec provides support for expression parsing, so you don’t have to write your own expression parser.

~~~haskell
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

<h3 id="expression-parsers-1">Expression parsers</h3>

- Parsec also has support for programming languages with a mechanism to define the syntax and keywords through <em>makeTokenParser</em>.
- For simple cases, you can use <em>emptyDef</em>.

~~~haskell
import Text.ParserCombinators.Parsec.Expr
import qualified Text.ParserCombinators.Parsec.Token as P
lexer       = P.makeTokenParser emptyDef

parens          = P.parens lexer
commaSep        = P.commaSep lexer
-- and many more
~~~
