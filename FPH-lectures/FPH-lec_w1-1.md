## Expressions

### Haskell has no statements, only expressions!

* In an imperative language like C or Java,
  - there are _expressions_ that denote small scale computations (<code>2*x</code>), and
  - _statements_ that handle sequencing, looping, conditionals, and all the large scale operation of the program.
* _Pure functional programming languages don’t have any statements_ — no assignments, no jumps.
* Instead, _all computation is performed by evaluating expressions
* So, let’s start with expressions!
  - (We’ll still be working on expressions at the end of the course, since that’s all there is.)

### Examples of integer expressions

  An expression evaluates to a result (usually written $$e \rightsquigarrow r$$ but we’ll use `e -> r`). Haskell uses a similar notation for numbers and operators as most languages:

~~~haskell
    2 -- > 2
    3+4 -- > 7
    3+4*5    {equivalent to 3+(4*5)} -- > 23
    (3+4)*5   {equivalent to 7*5} -- > 35
~~~

### Syntax of expressions

- Parentheses are used for grouping, just as in mathematics.
- If you don’t need parentheses for grouping, they are optional.
- Operators have precedence, e.g. $$ * $$ has “tighter” precedence than $$ + $$, so $$2 + 3 * 4$$ means $$2 + (3 * 4)$$.
- Use [the reference documentation](https://www.haskell.org/onlinereport/exps.html) for complete list of operators and their precedences, if you need them.

### Function applications

- Expressions can contain function calls.
- A function takes argument(s), performs some computation, and produces result(s).  
- The function _abs_ gives the absolute value of a number.  
- To use a function, you apply it to an argument. Write the function followed by the argument, separated by a space.  

~~~haskell
  abs 5 -- > 5d
  abs (-6) -- > 6
~~~

### Parentheses are for grouping

Good style

~~~haskell
  2+3*5
  2+(3*5) -- might be clearer to some readers
  abs 7
~~~

You don’t need parentheses. The following are legal, but they look silly:

~~~haskell
    (2) + ((3+(((((5)))))))
    abs (5)
    abs (((5)))
~~~

### Functions with several arguments

- _min_ and _max_ are functions that take two arguments.  
- The arguments are given after the function, separated by whitespace.  
- Write `min 3 8`, don’t write `min(3, 8);`

~~~haskell
    min 3 8 -- > 3

    max 3  8 -- > 8
~~~

### Precedence of function application

- Function application binds tighter than anything else.  
- So <code>f x + 3</code> means <code>(f x) + 3</code> and not <code>f (x+3)</code>  
- If an argument to a function is an expression, you’ll need to put it in parentheses.  

## Equations

### Equations give names to values

- Equations are used to give names to values.  

~~~haskell
answer = 42
~~~

- An equation in Haskell is a mathematical equation: it says that the left hand side and the right hand side denote the same value.  
- The left hand side should be a name that you’re giving a value to.
- Correct: `x = 5*y`
- Incorrect: `2 * x = (3*x)**2` – Reassignment is not allowed in a pure FPL  


### Equations are not assignments

- A name _can be given only one value_.  
- Names are often called “variables”, but they _do not vary_.  
- In Haskell _variables are constant_!  

~~~haskell
    n = 1    -- just fine!
    x = 3*n  -- fine
    n = x    -- Wrong: can have only one definition of n
~~~

- Once you give a value to a name, you can never change it!  
- This is part of the meaning of “pure” and “no side effects”  

### What about _n = n+1_?

- In imperative languages, we frequently say _n := n + 1_</span>.
- This is an assignment, not an equation!  
- It means (1) compute the right hand side, using the old value of _n_; then (2) discard the old value of _n_ and overwrite it with the new value.  
- _There are no equations in imperative languages like C and Java._  

- In Haskell, it is valid to write _n = n + 1_.
   - This is an equation, not an assignment!  
- It means: compute the value of _n_ that has the property that _n = n + 1_.  
- Haskell will try, and it will fail.  


### How can you compute without assignments?

- Think of an assignment statement as doing three things:
    1.   It evaluates the right hand side: computing a useful value.  
    2.   It discards the value of the variable on the left hand side: destroying a value that might or might not be useful.  
    3.   It saves the useful value of the RHS into the variable.  
- In a pure functional language
   - We never destroy old values.  
- We just compute new useful ones.  
- If the old value was truly useless, the garbage collector will reclaim its storage.  

## Try Haskell!

### Haskell in your browser

- You can try out Haskell in your browser! Go to <a href="https://haskell.dcs.gla.ac.uk">https://haskell.dcs.gla.ac.uk</a>, our interactive Haskell environment, ready to use!

### Installing Haskell

- You can install the Haskell compiler/interpreter on your own computer. Go to <a href="https://www.haskell.org/platform">https://www.haskell.org/platform</a> to get the Haskell Platform for your system, it is very easy to install. For more details see [Installing Haskell for Yourself](https://www.futurelearn.com/courses/functional-programming-haskell/1/steps/104770) in Week 2.  
- _All software used in this course is free software._  
- Try experimenting with the expressions shown in this lecture.  
- And try some experiments of your own.  

### The Haskell interpreter ghci
To launch the Haskell interpreter _ghci_, just type `ghci` in your terminal:

    [wim@fp4 ~]$ghci
    GHCi, version 7.8.3: http://www.haskell.org/ghc/  :? for help
    Loading package ghc-prim ... linking ... done.
    Loading package integer-gmp ... linking ... done.
    Loading package base ... linking ... done.
    Prelude>  
    Evaluate an expression
    Prelude> 3+4
    7
