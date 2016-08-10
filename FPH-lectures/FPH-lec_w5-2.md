## Function types

-   Ordinary data types are for primitive data (like $$Int$$ and $$Char$$)
    and basic data structures (like $$[Int]$$ and $$[Char]$$).
-   Algebraic data types are types that combine other types either as records ('products'), e.g.

    ~~~haskell
      data Pair = Pair Int Double
    ~~~

    or as _variants_ ('sums'), e.g.

    ~~~haskell
      data Bool = False | True
    ~~~    

-   Functions have types containing an arrow, e.g. $$Int \rightarrow String$$.
-   We now look at function types in more detail.

### Lambda expressions

* Lambda expressions (named after the greek letter $$\lambda$$) play a very important role in functional programming in general and Haskell in particular.

#### Named and anonymous expressions

- You can give a name $$sum$$ to an expression $$2+2$$:

~~~haskell
    sum = 2+2
~~~

- But you can also write *anonymous expressions* — expressions that just
appear, but are not given names.

~~~haskell
    (-b) + sqrt (b^2 - 4*a*c)
~~~

- Without anonymous expressions, writing this would almost be like
assembly language:

~~~haskell
    e1 = (-b)
    e2 = b^2
    e3 = 4*a
    e4 = e3*c
    e5 = e2-e4
    e6 = e1+e5
~~~

#### Some background

-   Sometimes in a mathematics or physics book, there are statements
    like “the function $$x^2$$ is continuous$$\ldots$$”

-   This is ok when the context makes it clear what $$x$$ is.

-   But it can lead to problems. What does $$x*y$$ mean?

    -   Is it a constant, because both $$x$$ and $$y$$ have fixed values?

    -   Is it a function of  $$x$$, with a fixed value of $$y$$?

    -   Is it a function of $$y$$, with a fixed value of $$x$$?

    -   Is it a function of both $$x$$ and $$y$$?

-   In mathematical logic (and computer programming) we need to be
    precise about this!

-   A lambda expression $$\backslash x \rightarrow e$$ contains

    -   An explicit statement that the formal parameter is $$x$$, and

    -   the expression $$e$$ that defines the value of the function.


#### Anonymous functions

* A function can be defined and given a name using an equation:

~~~haskell
    f :: Int -> Int
    f x = x+1
~~~

-   Since functions are “first class”, they are ubiquitous, and it’s
    often useful to denote a function anonymously.

-   This is done using *lambda expressions*.

~~~haskell
    \x -> x+1
~~~

Pronounced “lambda x arrow x+1”.

There may be any number of arguments:

~~~haskell
    \x y z -> 2*x + y*z
~~~

#### Using a lambda expression

Functions are first class: you can use a lambda expression wherever a
function is needed. Thus

~~~haskell
    f = \x -> x+1
~~~

is equivalent to

~~~haskell
    f x = x+1
~~~

But lambda expressions are most useful when they appear inside larger
expressions.

~~~haskell
    map (\x -> 2*x + 1) xs
~~~

### Monomorphic and polymorphic functions


#### Monomorphic functions

Monomorphic means “having one form”.

~~~haskell
    f :: Int -> Char
    f i = "abcdefghijklmnopqrstuvwxyz" !! i

    x :: Int
    x = 3

    f x :: Char
    f x -- > 'c'
~~~

#### Polymorphic functions

Polymorphic means “having many forms”.

~~~haskell
    fst :: (a,b) -> a
    fst (x,y) = x

    snd :: (a,b) -> b
    snd (x,y) = y

    fst :: (a,b) -> a
    fst (a,b) = a

    snd :: (a,b) -> b
    snd (a,b) = b

~~~

### Currying

-   Most programming languages allow functions to have any number of
    arguments.

-   But this turns out to be unnecessary: we can restrict all functions
    to have just one argument, *without losing any expressiveness*.

-   This process is called *Currying*, in honor of Haskell Curry.

    -   The technique makes essential use of higher order functions.

    -   It has many advantages, both practical and theoretical.


#### A function with two arguments

You can write a definition like this, which appears to have two
arguments:

~~~haskell
    f :: Int -> Int -> Int
    f x y = 2*x + y
~~~

But it actually means the following:

~~~haskell
    f :: Int -> (Int -> Int)
    f 5 :: Int -> Int
~~~

The function takes its arguments one at a time:

~~~haskell
    f 3 4 = (f 3) 4

    g :: Int -> Int
    g = f 3
    g 10 -- > (f 3) 10 -- > 2*3 + 10
~~~


#### Grouping: arrow to the right, application left

-   The arrow operator takes two types $$a \rightarrow b$$, and gives the
    type of a function with argument type $$a$$ and result type $$b$$

-   An application $$e_1\; e_2$$ applies a function $$e_1$$ to an argument
    $$e_2$$

-   Note that for both types and applications, *a function has only one
    argument*

-   To make the notation work smoothly, arrows group to the right, and
    application groups to the left.

~~~haskell
    f :: a -> b -> c -> d
    f :: a -> (b -> (c -> d))

    f x y z = ((f x) y) z
~~~

## Type classes and ad-hoc polymorphism

### The type of $$(+)$$

Note that $$fst$$ has the following type, and there is no restriction on
what types $$a$$ and $$b$$ could be.

~~~haskell
    fst :: (a,b) -> a
~~~

What is the type of $$(+)$$? Could it be$$\ldots$$

~~~haskell
    (+) :: Int -> Int -> Int
    (+) :: Integer -> Integer -> Integer
    (+) :: Ratio Integer -> Ratio Integer -> Ratio Integer
    (+) :: Double -> Double -> Double

    (+) :: a -> a -> a  -- Wrong! has to be a number
~~~

### Type classes

Answer: $$(+)$$ has type $$a \rightarrow a \rightarrow a$$ for any type $$a$$
that is a member of the type class $$Num$$.

~~~haskell
    (+) :: Num a => a -> a -> a
~~~

-   The class $$Num$$ is a set of types for which $$(+)$$ is defined

-   It includes $$Int$$, $$Integer$$, $$Double$$, and many more.

-   But $$Num$$ does *not* contain types like $$Bool$$, $$[Char]$$,
    $$Int\rightarrow Double$$, and many more.


### Two kinds of polymorphism

-   *Parametric polymorphism.*

    -   A polymorphic type that can be instantiated to *any* type.

    -   Represented by a type variable. It is conventional to use $$a$$,
        $$b$$, $$c$$, $$\ldots$$

    -   Example: $$length :: [a] \rightarrow Int$$ can take the length of
        a list whose elements could have any type.

-   *Ad hoc polymorphism.*

    -   A polymorphic type that can be instantiated to any type chosen
        from a set, called a “*type class*”

    -   Represented by a type variable that is constrained using the
        $$\Rightarrow$$ notation.

    -   Example:
        $$(+) :: Num\, a \Rightarrow a \rightarrow a \rightarrow a$$ says
        that $$(+)$$ can add values of any type $$a$$, provided that $$a$$ is
        an element of the type class $$Num$$.

## Type inference

-   *Type checking* takes a type declaration and some code, and
    determines whether the code actually has the type declared.

-   *Type inference* is the analysis of code in order to infer its type.

-   Type inference works by

    -   Using a set of *type inference rules* that generate typings
        based on the program text

    -   Combining all the information obtained from the rules to produce
        the types.


### Type inference rules

The type system contains a number of *type inference rules*, with the
form

$$
  \frac
  {\hbox{assumption --- what you're given}}
  {\hbox{consequence --- what you can infer}}
$$

### Context

-   Statements about types are written in the form similar to
    $$\Gamma \vdash e :: \alpha$$

-   This means \`\`if you are given a set $$\Gamma$$ of types, then it is
    proven that $$e$$ has type $$\alpha$$.


### Type of constant

$$
\frac
  {\hbox{$c$ is a constant with a fixed type $T$}}
  {\Gamma \vdash c :: T}
$$

If we know the type $$T$$ of a constant $$c$$ (for example, we know that
$$'a'
:: Char$$), then this is expressed by saying that there is a given
theorem that $$c :: T$$. Furthermore, this holds given *any* context
$$\Gamma$$.


### Type of application

$$
\frac
  {\Gamma \vdash e_1 :: (\alpha \rightarrow \beta)
   \qquad
   \Gamma \vdash e_2 :: \alpha
  }
  {\Gamma \vdash (e_1 \ e_2) :: \beta}
$$

If $$e_1$$ is a function with type $$\alpha \rightarrow \beta$$, then the
application of $$e_1$$ to an argument of type $$\alpha$$ gives a result of
type $$\beta$$.


### Type of lambda expression

$$
\frac  
{\Gamma, x :: \alpha \quad \vdash \quad e :: \beta}
{\Gamma \vdash (\lambda x \rightarrow e)
   :: (\alpha \rightarrow \beta)}
$$

We have a context $$\Gamma$$. Suppose that if we’re also given that
$$x :: \alpha$$, then it can be proven that an expression $$e ::
\beta$$. Then we can infer that the function $$\lambda x \rightarrow
e$$ has type $$\alpha \rightarrow \beta$$.


$$
{\large
  {\hbox{$c$ is a constant with a fixed type $T$}}
}
$$
