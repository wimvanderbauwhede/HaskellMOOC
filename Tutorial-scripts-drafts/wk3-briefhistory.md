## A Brief History of Haskell

Once upon a time, there was a mathematician named Alonzo Church at Princeton
University. 
Church was Alan Turing's PhD supervisor. Church devised a 
mathematical model of functions called lambda calculus. 
Yes, this is where modern-day lambdas come from!

We will explore the details of lambda calculus later in the course. For now,
all we need to know is that lambda calculus captures the *essence* of
computation. It involves function abstraction (like defining functions 
in Haskell) and application (like calling functions in Haskell).

Fast forward from Church in the 1930s to the early development of
programming languages in the 1950s. 
One of the first high-level programming languages was LISP (which stands for
List Processing). 
LISP adopted a functional style. It allowed user functions to be defined, and
passed around as values. 
LISP lives on ... more recent incarnations include
Scheme and Clojure.

Fast forward again. During the 1980s, lots of researchers were 
inventing and extending various functional programming languages.
Example languages include ML, Hope and Miranda. However research was 
fragmented across the various languages, and many of them did not have
'open-source' frameworks.
So a group of academics formed a committee to design and implement a
new language, which would be used as a vehicle for research as well
as for teaching functional programming.

After several years of work and arguments, the committee published the
first [Haskell Language Report](https://wiki.haskell.org/Language_and_library_specification)
in 1990.
This was a major milestone: at last there was a common functional
language around which the research community could unite.

The language has grown in popularity ever since, despite an avowed aim
to *avoid success at all costs*.
There are several freely available implementations. The
most commonly used is the Glasgow Haskell Compiler, which has an
interpreter (ghci) and a compiler (ghc). 
These form an integral part of the [Haskell Platform](https://www.haskell.org/platform). 
Lots of people contributed to this software ecosystem. 
Many of them have worked at
the University of Glasgow like 
[Simon Marlow](https://en.wikipedia.org/wiki/Simon_Marlow),
[Simon Peyton Jones](https://en.wikipedia.org/wiki/Simon_Peyton_Jones), 
and [Phil Wadler](https://en.wikipedia.org/wiki/Philip_Wadler).

Haskell is now widely used in teaching, research and industry. 
For instance, it's taught at several Scottish universities including Glasgow,
Edinburgh and St Andrews.
It has its own annual research conference, the 
[ACM Haskell Symposium](https://www.haskell.org/haskell-symposium/).
And there are many industrial users, including at Facebook.
We will be interviewing a software developer from Facebook within 
the next few weeks.

Optionally, if you want to learn more 
about the history of Haskell, please check these links:

*  read the [History of Haskell paper](https://wiki.haskell.org/History_of_Haskell)
*  watch this [lecture by Simon Peyton Jones](https://www.youtube.com/watch?v=3bjXGrycMhQ)


