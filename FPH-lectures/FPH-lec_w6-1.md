
Type class and instance declarations
-------------------------------


### Defining type classes

-   A *type class* is a set of types for which some operations are
    defined.

-   Haskell has some standard type classes that are defined in the
    Standard Prelude.

-   You can also define your own.


### A type for bright colors

Suppose we’re computing with colors. Here’s a type, and a couple of
functions.

~~~haskell
    data Bright
      = Blue
      | Red
      deriving (Read, Show)

    darkBright :: Bright -> Bool
    darkBright Blue = True
    darkBright Red  = False

    lightenBright :: Bright -> Bright
    lightenBright Blue = Red
    lightenBright Red = Red
~~~

### A type for milder colors

Now, suppose we have a different type that needs similar functions.

~~~haskell

    data Pastel
      = Turquoise
      | Tan
      deriving (Read, Show)

    darkPastel :: Pastel -> Bool
    darkPastel Turquoise = True
    darkPastel Tan       = False

    lightenPastel :: Pastel -> Pastel
    lightenPastel Turquoise = Tan
    lightenPastel Tan       = Tan
~~~

### Defining a type class

-   Both of our color types have functions to decide whether it’s dark,
    or to lighten it.

-   We can define a class $$Color$$ and its corresponding functions.

<!-- -->

~~~haskell

    class Color a where
      dark :: a -> Bool
      lighten :: a -> a
~~~

This says

-   $$Color$$ is a type class

-   The type variable $$a$$ stands for a particular type that is in the
    class $$Color$$

-   For any type $$a$$ in $$Color$$, there are two functions you can use:
    $$dark$$ and $$lighten$$, with the specified types.


### Defining instances for the type class

-   An $$instance$$ declaration says that a type is a member of a type
    class.

-   When you declare an instance, you need to define the class
    functions.

-   The following says that the type $$Bright$$ is in the class $$Color$$,
    and for that instance, the $$dark$$ function is actually $$darkBright$$.

<!-- -->

~~~haskell

    instance Color Bright where
      dark = darkBright
      lighten = lightenBright
~~~
-   Similarly, we can declare that $$Pastel$$ is in $$Color$$, but it has
    different functions to implement the class operations.

<!-- -->

~~~haskell

    instance Color Pastel where
      dark = darkPastel
      lighten = lightenPastel
~~~

## Predefined type classes

Haskell provides several standard type classes. We have a look at two of them: $$Num$$ and $$Show$$.

### The Num class


-   $$Num$$ is the class of numeric types.
-   Here is (part of) its class declaration:

~~~haskell
    class Num a where
      (+), (-), (*) :: a -> a -> a
~~~

#### Num instances

-   There are many numeric types; two of them are $$Int$$ and $$Double$$.

-   There are primitive monomorphic functions that perform arithmetic on
    these types (these aren’t the real names):

~~~haskell
addInt, subInt, mulInt :: Int -> Int -> Int
addDbl, subDbl, mulDbl :: Double -> Double -> Double

    instance Num Int where
      (+) = addInt
      (-) = subInt
      (*) = mulInt

    instance Num Double where
      (+) = addDbl
      (-) = subDbl
      (*) = mulDbl
~~~

#### Hierarchy of numeric classes

-   There are some operations (addition) that are valid for all numeric
    types.

-   There are some others (e.g. trigonometric functions) that are valid
    only for *some* numeric types.

-   Therefore there is a rich hierarchy of subclasses, including

    -   $$Integral$$ — class of numeric types that represent integer
        values, including $$Int$$, $$Integer$$, and more.

    -   $$Fractional$$ — class of types that can represent fractions.

    -   $$Floating$$ — class containing $$Float$$, $$Double$$, etc.

    -   $$Bounded$$ — class of numeric types that have a minimal and
        maximal element.

    -   $$Bits$$ — class of types where you can access the representation
        as a sequence of bits, useful for systems programming and
        digital circuit design.

-   If you want to get deeply into numeric classes and types, refer to
    [the Haskell documentation](https://www.haskell.org/onlinereport/basic.html).

### The Show class

-   We have been using $$show$$ to convert a data value to a string, which
    can then be written to output.

-   Some values can be “shown”, but not all.

-   For example, it is impossible in general to show a function.

-   Therefore $$show$$ needs a type class!

-   $$show :: Show\, a \Rightarrow a \rightarrow \,String$$


#### Defining your own Show instance

~~~haskell

    data Foo = Bar | Baz
~~~

We might want our own peculiar string representation:

~~~haskell

    instance Show Foo where
      show Bar = "it is a bar"
      show Baz = "this is a baz"
~~~

Recall that when you enter an expression $$exp$$ into ghci, it prints
$$show exp$$. So we can try out our strange instance declaration:

~~~haskell

    *Main> Bar
    it is a bar
    *Main> Baz
    this is a baz
~~~

#### Deriving Show

This is a similar type, but it has a $$deriving$$ clause.

~~~haskell

    data Foo2 = Bar2 | Baz2
      deriving (Read, Show)
~~~

Haskell will automatically define an instance of $$show$$ for $$Foo2$$,
using the obvious definition:

~~~haskell

    *Main> Bar2
    Bar2
    *Main> Baz2
    Baz2
~~~

### More standard typeclasses

Here is a summary of some of the type classes defined in the standard
libraries.

-   $$Num$$ — numbers, with many subclasses for specific kinds of number.

-   $$Read$$ — types that can be “read in from” a string.

-   $$Show$$ — types that can be “shown to” a string.

-   $$Eq$$ — types for which the equality operator $$==$$ is defined.

-   $$Ord$$ — types for which you can do comparisons like $$<$$, $$>$$, etc.

-   $$Enum$$ — types where the values can be enumerated in sequence; this
    is used for example in the notation $$[1..n]$$ and $$'a'..'z'$$.

~~~haskell

    *Main> [1..10]
    [1,2,3,4,5,6,7,8,9,10]
    *Main> ['a'..'z']
    "abcdefghijklmnopqrstuvwxyz"
~~~

