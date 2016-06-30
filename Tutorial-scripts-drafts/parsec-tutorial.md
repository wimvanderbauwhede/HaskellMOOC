## Parsing Tutorial

### Aim

Illustrate the practical use of the Parsec library

### Steps

- Datatypes
- Common parsing infrastructure
- The JSON parser
- File IO
- The Main program

### Installing Parsec

- If you've installed the Haskell Platform, Parsec will be available on your system.
- Otherwise, you can install it using `cabal install parsec`

### Using Parsec

We want to demonstrate the use of Parsec for parsing JSON files.

#### Datatypes

First, we need to declare the datatypes we require (see the file `JSONTypes.hs`).

A JSON Value can be one of the following:
~~~haskell
data JValue = JString String
            | JNumber Integer
            | JObject JMap
            | JArray [JValue]
            | JBool Bool
            | JNull
~~~

A JSON Object is a map or dictionary so we use `Data.Map`:
~~~haskell
import Data.Map hiding ( map )

type JMap = Data.Map.Map String JValue
~~~

The `hiding` clause use used because Data.Map provides its own definition of `map` and we don't want to use it.

We introduce a few convenient functions for creating JSON object types:
~~~haskell
mkJPair k v = JObject (Data.Map.singleton k v)
~~~
This function just creates a pair as a singleton map and returns it as a JObject.

~~~haskell
mkJObj :: [JValue] -> JValue
mkJObj j_vals =
    let
        list_of_maps = map (\(JObject pair) -> pair) j_vals
        combined_map = Data.Map.unions list_of_maps
    in
        JObject combined_map
~~~

This function combines a list of pairs in to a larger map and returns it as a JObject.

#### Common parsing infrastructure

Now we create a module with some common infrastructure required for convenient parsing with Parsec. The content of this file (`JSONParserCommon.hs`) is quite generic, most parsers will require something similar.

We start by importing a number of Parsec-related modules:

~~~haskell
module JSONParserCommon
  where
    import JSONTypes
    import Text.Parsec
    import Text.Parsec.String (Parser)
    import qualified Text.Parsec.Token as P
    import Text.Parsec.Language
~~~

The first, `Text.Parsec`, is the main Parsec module. The next two modules make it easy to define lexers and token parsers.

 `Text.Parsec.Token` provides a large library of token parsers and `Text.Parsec.Language` provides a handy way to define properties of the text to be parsed, with a focus on programming languages (hence the name).
