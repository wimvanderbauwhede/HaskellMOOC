<!-- Q1 ------------------------------------------------------------------------------------------------------------------------ -->

What is the type of the function `json_parser`?

~~~haskell
json_parser :: Parser __
json_parser = do
        whiteSpace
        j_top <- ( json_array_parser <|> json_obj_parser)
        return j_top
~~~
<!-- Cloze -->
json_parser :: Parser __
<!-- Feedback -->

A Parsec parser is a monadic parser, this means the type consists of the name of the monad (_Parser_) followed by the type of the input to the parser.
<!-- Answer -->

JValue
<!-- Hint -->

This is the data type for the JSON syntax representation
<!-- Q2------------------------------------------------------------------------------------------------------------------------ -->
A JSON array consists of a comma-separated list of JSON values enclosed by braces, e.g.

     [ 1, 'two',  [ 3, true] ]

To parse this format we use the function `json_array_parser` below. What is the correct type constructor for the return value?

~~~haskell
json_array_parser :: Parser JValue
json_array_parser = do    
    j_vals <- brackets $ commaSep json_value_parser
    return $ __ j_vals
~~~
<!-- Cloze -->
    return $ __ j_vals
<!-- Feedback -->
The return value of a parser of type `Parser T` must be a valid type constructor for the type `T`.
<!-- Answer -->
JArray
<!-- Hint -->
The return type must be the variant of JValue that indicate that this is a parsed JSON array value.
<!-- Q3------------------------------------------------------------------------------------------------------------------------ -->
The JSON format supports boolean values, named `true` and `false`.

In the boolean JSON value parser below, what is the missing combinator?

~~~haskell
json_bool_parser = do
    bstr <- ( symbol "true" __ symbol "false" )
    let
        bval = if bstr == "true" then True else False
    return $ JBool bval
~~~
<!-- Cloze -->
( symbol "true" __ symbol "false" )
<!-- Feedback -->
The parser `symbol` parses the exact string that is its argument, in this case either "true" or "false". You have to provide a Parsec parser combinator so that the correct option will be selected when calling the parser.
<!-- Answer -->
<|>
<!-- Hint -->
You need a parser combinator that expresses _choice_
<!-- Q4------------------------------------------------------------------------------------------------------------------------ -->
A JSON object is a list of key-value pairs, where the key is a string and the value a JSON value, enclosed in braces, e.g.

    { "Street" : "Lilybank Gardens",
      "Nr" : 18,
      "Org" : [
            "University of Glasgow",
            { "School" : "Computing Science"}
      ]
    }

The most general `JValue` parser is `json_value_parser`, which is built of parsers for specific JSON values:

~~~haskell
json_value_parser =
    json_array_parser <|>
    json_obj_parser <|>
    json_string_parser <|>
    json_number_parser <|>
    json_bool_parser <|>
    json_null_parser
~~~

In the JSON pair parser below, provide the name of the parser for the 'value' part of the pair    

~~~haskell
json_pair_parser = do
    k <- stringLiteral
    colon
    v <- __
    return $ mkJPair k v    
~~~
<!-- Cloze -->

v &lt;- __

<!-- Feedback -->

The parser for the 'value' of a pair in a JSON object must be able to parse any JSON value.

<!-- Answer -->

json_value_parser

<!-- Hint -->
This is the most general parser for `JValue`.
<!-- Q5------------------------------------------------------------------------------------------------------------------------ -->

In the JSON object parser below, complete the return expression.

~~~haskell
json_obj_parser :: Parser JValue
json_obj_parser = do
    j_vals <- braces $ commaSep json_pair_parser -- a list of pairs
    return $ __ j_vals
~~~

<!-- Cloze -->
    return $ mkJObj j_vals

<!-- Feedback -->
The JSON object is a list of key-value pairs that can be accessed through the key. The Haskell equivalent is provided by `Data.Map` and implemented as a type alias `JMap`.

<!-- Answer -->
mkJObj

<!-- Hint -->
Look at the code at the start of the quiz, and note that `JMap` is not the type that is returned by `json_pair_parser`.

<!-- Q6 -->

Given the following parser:

~~~haskell
yin_yang :: Parser String
yin_yang =
  do  xs <- string "yin" <|> string "yang"
      return xs
~~~

With the definition as above this parser will fail when trying to parse "yang":

~~~haskell
*Main> run yin_yang "yang"
parse error at (line 1, column 1):
unexpected "a"
expecting "yin"
~~~

How should you modify the parser so that it will work correctly?



yin_yang :: Parser String
yin_yang =
  do  xs <- string "yin" <|> try (string "yang")
      return xs

yin_yang :: Parser String
yin_yang =
  do  xs <- try (string "yin" <|> string "yang")
      return xs

yin_yang :: Parser String
yin_yang =
  do  xs <- try (string "yin") <|> string "yang"
      return xs
