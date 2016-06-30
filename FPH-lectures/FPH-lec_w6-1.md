
Defining type classes
=====================

Class and instance declarations
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

The Num class
-------------


### The Num class

-   Haskell provides several standard type classes.

-   $$Num$$ is the class of numeric types.

-   Here is (part of) its class declaration:

<!-- -->

~~~haskell

    class Num a where
      (+), (-), (*) :: a -> a -> a
~~~

### Num instances

-   There are many numeric types; two of them are $$Int$$ and $$Double$$.

-   There are primitive monomorphic functions that perform arithmetic on
    these types (these aren’t the real names):

    -   $$addInt, subInt, MulInt :: Int -> Int -> Int$$

    -   $$addDbl, subDbl, MulDbl :: Double -> Double -> Double$$

<!-- -->

~~~haskell

    instance Num Int where
      (+) = addInt
      (-) = subInt
      (*) = mulInt

    instance Num Double where
      (+) = addDbl
      (-) = subDbl
      (*) = mulDbl
~~~

### Hierarchy of numeric classes

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
    the documentation.

The $$Show$$ class
----------------


### The $$Show$$ class

-   We have been using $$show$$ to convert a data value to a string, which
    can then be written to output.

-   Some values can be “shown”, but not all.

-   For example, it is impossible in general to show a function.

-   Therefore $$show$$ needs a type class!

-   $$show :: Show\, a \Rightarrow a \rightarrow \,String$$


### Defining your own Show instance

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

### Deriving Show

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

More standard typeclasses
-------------------------


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

<!-- -->

~~~haskell

    *Main> [1..10]
    [1,2,3,4,5,6,7,8,9,10]
    *Main> ['a'..'z']
    "abcdefghijklmnopqrstuvwxyz"
~~~

Introduction to Lambda Calculus
===============================


### Introduction to Lambda Calculus

-   The lambda calculus was developed in the 1930s by Alonzo Church
    (1903–1995), one of the leading developers of mathematical logic.

-   The lambda calculus was an attempt to formalise functions as a means
    of computing.


### Significance to computability theory

-   A major (really *the* major) breakthrough in computability theory
    was the proof that the lambda calculus and the Turing machine have
    exactly the same computational power.

-   This led to *Church’s thesis* — that the set of functions that are
    effectively computable are exactly the set computable by the Turing
    machine or the lambda calculus.

-   The thesis was strengthened when several other mathematical
    computing systems (Post Correspondence Problem, and others) were
    also proved equivalent to lambda calculus.

-   The point is that the set of effectively computable functions seems
    to be a fundamental reality, not just a quirk of how the {Turing
    machine, lambda calculus} was defined.


### Significance to programming languages

-   The lambda calculus has turned out to capture two aspects of a
    function:

    -   A mathematical object (set or ordered pairs from domain and
        range), and

    -   An abstract black box machine that takes an input and produces
        an output.

-   The lambda calculus is fundamental to denotational semantics, the
    mathematical theory of what computer programs mean.

-   Functional programming languages were developed with the explicit
    goal of turning lambda calculus into a practical programming
    language.

-   The ghc Haskell compiler operates by (1) desugaring the source
    program, (2) transforming the program into a version of lambda
    calculus called *System F*, and (3) translating the System F to
    machine language using *graph reduction*.


### Abstract syntax of lambda calculus

-   We will work with the basic lambda calculus “enriched” with some
    constants and primitive functions (strictly speaking, that is not
    necessary).

-   The language has constants, variables, applications, and functions.

<!-- -->

~~~haskell

    exp
      = const
      | var
      | exp exp
      | \ var -> exp
~~~

### Variables

-   Each occurrence of a variable in an expression is either *bound* or
    *free*

    -   In $$\backslash x \rightarrow x+1$$, the occurrence of $$x$$ in
        $$x+1$$ is *bound* by the $$\backslash x$$.

    -   In $$y*3$$, the occurrence or $$y$$ is *free*. It must be defined
        somewhere else, perhaps as a global definition.

-   In general, an occurrence of a variable is bound if there is some
    enclosing lambda expression that binds it; if there is no lambda
    binding, then the occurrence if free.

We need to be careful: the first occurrence of $$a$$ is free but the
second occurrence is bound.

~~~haskell

       a + (\ a -> 2^a) 3  -- >   a + 2^3
~~~

Being free or bound is a property of an *occurrence* of a variable, not
of the variable itself!


### Conversion rules

-   Computing in the lambda calculus is performed using three
    *conversion rules*.

-   The conversion rules allow you to replace an expression by another
    (“equal”) one.

-   Some conversions simplify an expression; these are called
    *reductions*.


### Alpha conversion

-   Alpha conversion lets you change the name of a function parameter
    consistently.

-   But you can’t change free variables with alpha conversion!

-   The detailed definition of alpha conversion is a bit tricky, because
    you have to be careful to be consistent and avoid “name capture”. We
    won’t worry about the details right now.

$$(\backslash x \rightarrow x+1) \,3$$
$$(\backslash y \rightarrow y+1) \,3$$


### Beta conversion

-   Beta conversion is the “workhorse” of lambda calculus: it defines
    how functions work.

-   To apply a lambda expression an argument, you take the body of the
    function, and replace each bound occurrence of the variable with the
    argument.

$$(\backslash x \rightarrow exp1) \;exp2$$ $$exp1[exp2/x]$$

Example:

$$(\backslash x \rightarrow 2*x + g\; x) \;42$$ $$2*42 + g \;42$$


### Eta conversion

-   Eta conversion says that a function is equivalent to a lambda
    expression that takes an argument and applies the function to the
    argument.



$$
(\backslash x \rightarrow f\, x)\, f
$$

Example (recall that $$(*3)$$ is a function that multiplies its argument
by 3)

$$(\backslash x \rightarrow (*3) \;x)$$ $$(*3)$$

Try applying both of these to 50:

$$(\backslash x \rightarrow (*3) \;x) \;50\rightsquigarrow (*3) \; 50$$
which is the same as $$(*3) \;50$$


### Removing a common trailing argument

There is a common usage of Eta conversion. Suppose we have a definition
like this:

$$f \;x \;y = g\; y$$

This can be rewritten as follows:

$$f = \backslash x \rightarrow (\backslash y \rightarrow g \;y)$$
$$f = \backslash x \rightarrow g$$ $$\equiv$$ $$f\, x = g$$

Thus the following two definitions are equivalent:

~~~haskell

    f x y = g y
    f x = g
~~~

In effect, since the last argument on both sides of the equation is the
same ($$y$$), it can be “factored out”.
