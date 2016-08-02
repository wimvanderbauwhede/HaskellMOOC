<!-- ## Example: The *Maybe* Monad -->

- We've already seen the Maybe type. Let's look at the Maybe monad, which makes using the Maybe type a lot easier.

### The *Maybe* type constructor

You already know the definition of the *Maybe* type:

~~~haskell
    data Maybe a = Just a | Nothing
~~~

### Example use of Maybe: Safe *head* and *tail*

The *head* and *tail* functions from the Prelude are not safe in the sense that they fail when called on an empty list. We can define safe versions using *Maybe*:

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

Now we can make *Maybe* an instance of the *Monad* type class, simply by providing the appropriate definitions for *return*, *bind*, *then* and *fail*:

~~~haskell
    import Control.Monad

    instance Monad Maybe where
        return           =   Just
        Nothing  >>= f = Nothing
        (Just x) >>= f = f x
        fail _           =   Nothing
~~~

There are a few additional functions defined in the *MonadPlus* type class:

~~~haskell
    instance MonadPlus Maybe where
        mzero               = Nothing
        Nothing `mplus` x = x
        x `mplus` _         = x
~~~

That's it, we now have a *Maybe* monad!

*Note*: for users of `ghc 7.10` and higher, we need to do [a little bit more work](https://ghc.haskell.org/trac/ghc/wiki/Migration/7.10).  

Explicit Maybe versus the Maybe Monad
----------------------------------

Let's see what this monad gives us:

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

To combine computations that use the *Maybe* type, we need explicit `case` expressions to pattern match against the type.

### A computation using the Maybe monad

Let's write this computation using the *Maybe* monad, first using the `>>=` operator:

~~~haskell
    bar :: [a] -> Maybe a
    bar xs =
      myTail xs >>=
        (\a -> myTail a >>=
          (\b -> myHead b))
~~~

Now let's change the line breaks and indentation a bit to make it look nicer:

~~~haskell
    bar2 :: [a] -> Maybe a
    bar2 xs =
      myTail xs >>= (\a ->
      myTail a >>=  (\b ->
      myHead b))
~~~

Thanks to the associativity law, we can also remove unnecessary parentheses:

~~~haskell
    bar3 :: [a] -> Maybe a
    bar3 xs =
      myTail xs >>= \a ->
      myTail a >>=  \b ->
      myHead b
~~~

This is already a lot cleaner, but finally we can use the `do`-notation:

~~~haskell
    bar3 :: [a] -> Maybe a
    bar3 xs = do
      a <- myTail xs
      b <- myTail a
      myHead b
~~~

Clearly, the final monadic code is a lot more readable than the original non-monadic code.

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
