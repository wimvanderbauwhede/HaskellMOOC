## What is a Monad?

-   A monad is an algebraic structure in category theory.

-   Monads are abstract, and they have many useful concrete instances.

-   Monads provide a way to structure a program.

-   They can be used (along with abstract data types) to allow actions
    (e.g. mutable variables) to be implemented safely.


## Building blocks of a monad

A monad has three building blocks:

-   A type constructor that produces the type of a computation, tiven
    the type of that computation’s result.

-   A function that takes a value, and returns a computation that—when
    executed—will produce that value.

-   A function that takes two computations and performs them one after
    the other, making the result of the first computation available to
    the second.

Let's restate this more precisely:


### Definition

A monad consists of three objects (below), which must satisfy the monad
laws (later).

-   A type constructor $$M$$, such that for any type $$a$$, the type $$M a$$
    is the type of a computation in the monad $$M$$ that produces a result
    of type $$a$$.

-   A function $$return :: a \rightarrow M\, a$$. Thus if $$x::a$$, then
    $$return \, x$$ is a computation in $$M$$ that, when executed, will
    produce a value of type $$a$$.

-   A function
    $$(>>=) :: M\, a \rightarrow  (a \rightarrow  M\, b) \rightarrow  M\, b$$.

    -   The first argument is a computation that produces a value of
        type $$a$$.

    -   The second argument is a function that requires an argument of
        type $$a$$ and returns a computation that produces a value of type
        $$b$$.

    -   The result is a computation that will produce a value of type
        $$b$$. It works by running the first computation and passing its
        result to the function that returns the second computation,
        which is then executed.


### The definition says what a monad is!

-   There are many metaphors or intuitions about what a monad is.

-   Example: a “computation” or an “action”.

-   But these terms are vague—they may help you to understand, but they
    can also be misleading at times.

-   *A monad is exactly what the definition says, no more and no less.*

Monads and Type classes
----------


### Generality of monads

-   Monads are abstract and general and useful.

-   Consequently, there are many instances of them.

-   This is captured by defining a type class for monads, along with
    many standard instances. And you can define your own.


### Monad type class
~~~haskell
    class Monad m where
        return :: a -> m a
        (>>=) :: m a -> (a -> m b) -> m b
        (>>)   :: m a -> m b -> m b
        fail   :: String -> m a
~~~
-   The $$fail$$ function is used by the system to help produce error
    messages when there is a pattern that fails to match; normally you
    don’t use it directly.

-   We’ll just pretend $$fail$$ isn’t there.


### The "bind" and “then” operators

-   The “bind” operator $$>>=$$ is a crucial part of a monad. It binds a value returned from a computation to another computation.

-   Sometimes the value returned by a computation is unimportant.

-   The $$>>$$ operator is like $$>>=$$, but it just ignores the value
    returned by the computation.

~~~haskell
    m >> n = m >>= \_ -> n
~~~

## Monad laws

Every monad must satisfy the following three laws.

The *right unit law*:

~~~haskell
    m >>= return = m

The *left unit law*:

    return x >>= f  = f x

The *associativity law*:

    (m >>= f) >>= g = m >>= (\x -> f x >>= g)
~~~

The Maybe Monad
===============

- We've already seen the Maybe type. Let's look at the Maybe monad, which makes using the Maybe type a lot easier.

The $$Maybe$$ type constructor
----------------------------

~~~haskell
    import Control.Monad

    data MyMaybe a = MyNothing | MyJust a
~~~

### Safe head and tail
~~~haskell
    myHead :: [a] -> Maybe a
    myHead [] = Nothing
    myHead (x:xs) = Just x

    myTail :: [a] -> Maybe [a]
    myTail [] = Nothing
    myTail (x:xs) = Just xs
~~~

Monad instance of Maybe
-----------------------

~~~haskell
    instance Monad MyMaybe where
        return           =   MyJust
        MyNothing  >>= f = MyNothing
        (MyJust x) >>= f = f x
        fail _           =   MyNothing

    instance MonadPlus MyMaybe where
        mzero               = MyNothing
        MyNothing `mplus` x = x
        x `mplus` _         = x
~~~
A computation using explicit Maybe
----------------------------------


### A computation using explicit Maybe
~~~haskell
    foo :: [a] -> Maybe a
    foo xs =
      case myTail xs of
        Nothing -> Nothing
        Just a -> case myTail a of
                    Nothing -> Nothing
                    Just b -> myHead b
~~~
A computation using the Maybe monad
-----------------------------------


### A computation using the Maybe monad
~~~haskell
    bar :: [a] -> Maybe a
    bar xs =
      myTail xs >>=
        (\a -> myTail a >>=
          (\b -> myHead b))
~~~

### Syntax: Changing the line breaks and indentation
~~~haskell
    bar2 :: [a] -> Maybe a
    bar2 xs =
      myTail xs >>= (\a ->
      myTail a >>=  (\b ->
      myHead b))
~~~

### Syntax: Removing unnecessary parentheses
~~~haskell
    bar3 :: [a] -> Maybe a
    bar3 xs =
      myTail xs >>= \a ->
      myTail a >>=  \b ->
      myHead b
~~~


### Example: Reduction of bar [5,6]
~~~haskell
        bar [5,6]

    -- > substitute [5,6] for xs in definition of bar

        myTail [5,6] >>=
         (\a -> myTail a >>=
          (\b -> myHead b))

    -- > def. myTail

        myJust [6]  >>=
         (\a -> myTail a >>=
          (\b -> myHead b))

    -- >  def.2 of (>>=)

         (\a -> myTail a >>=
          (\b -> myHead b))
            [6]
    -- > beta reduction, substitute [6] for a

         myTail [6] >>= (\b -> myHead b)

    -- > reduce myTail

         Just [] >>=  (\b -> myHead b)

    -- >  def.2 of (>>=)   

        (\b -> myHead b) []

    -- > beta reduction, substitute [] for b

       myHead []

    -- > def.1 myHead

      Nothing
~~~

### Example: Reduction of bar [5,6,7]
~~~haskell
        bar [5,6,7]

    -- > substitute [5,6,7] for xs in definition of bar

        myTail [5,6,7] >>=
         (\a -> myTail a >>=
          (\b -> myHead b))

    -- > def. myTail

        myJust [6,7]  >>=
         (\a -> myTail a >>=
          (\b -> myHead b))

    -- >  def.2 of (>>=)

         (\a -> myTail a >>=
          (\b -> myHead b))
            [6,7]
    -- > beta reduction, substitute [6,7] for a

         myTail [6,7] >>= (\b -> myHead b)

    -- > reduce myTail

         Just [7] >>=  (\b -> myHead b)

    -- >  def.2 of (>>=)   

        (\b -> myHead b) [7]

    -- > beta reduction, substitute [7] for b

        myHead [7]

    -- > def myHead

        Just 7
~~~

*do* notation
------------------------

Writing monadic computations using the bind and then operators is still somewhat clunky. Haskell provides a very convenient syntactic sugar for monadic computations called the "do notation":

~~~haskell
    baz :: [a] -> Maybe a

    baz xs =
      do  a <- myTail xs
          b <- myTail a
          c <- myHead b
          return c
~~~

### Syntax rules for do
~~~haskell
    do { x }  -- >  x

    do {x ; <xs> }  -- >  x >> do { <xs> }

    do { a <- x ; <xs> }  -- >  x >>= \a -> do { <xs> }

    do { let <declarations> ; xs }
      -- >
    let <declarations> in do { xs }
~~~
