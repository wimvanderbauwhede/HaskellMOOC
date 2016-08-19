Article - a brief history of Haskell

Once upon a time ... there was a mathematician named Alonzo Church at Princeton
University. 
Church was Alan Turing's PhD supervisor. Church devised a 
mathematical model of functions called lambda calculus. Yes - this is where
modern-day lambdas come from!

We will explore the details of lambda calculus later in the course. For now,
all we need to know is that lambda calculus captures the essence of
computation - and it involves function abstraction (like defining functions 
in Haskell) and application (like calling functions in Haskell).

Fast forward from Church in the 1930s to the early development of high-level programming languages in the 1950s. 
One of the first high-level programming languages was LISP (List Processing). 
This adopted a functional style - allowing user functions to be defined, and
passed around as values. Lisp lives on - more recent incarnations include
Scheme and Clojure.

Fast forward again ... the 1980s was a time when lots of researchers were 
inventing and extending various functional programming languages - 
examples include ML, Hope and Miranda. However research was 
fragmented across the various languages - many of which did not have
 `open-source' frameworks.
So a group of academics formed a committee to design and implement a
new language, which would be used as a vehicle for research as well
as for teaching functional programming.

After several years of work (and arguments) the committee published the
first Haskell Language Report (link) in 1990.
This was a major milestone - at last there was common functional
language for the community to unite around.
Mantra - "avoid success at all costs" - (CHECK).

The language has grown in popularity ever since. 
There are several freely available implementations - the
most commonly used is the Glasgow Haskell Compiler - which has an
interpreter (ghci) and a compiler (ghc). 
These form an integral part of the Haskell Platform. 
Lots of people worked on this software ecosystem - 
many of them used to be at U. Glasgow like 
Simon Peyton Jones and Simon Marlow.

Haskell is now widely used in teaching, in research and in industry. 
For instance, it's taught at lots of Scottish universities including Glasgow,
Edinburgh and St Andrews. It has its own annual 
research conference- the 
ACM Haskell Symposium. And there are lots of industrial users, including at Facebook-  we will be interviewing a software developer from Facebook in
 a few weeks.

Optionally, if you want to find out more information 
on the history of Haskell - you could
read this (long) paper
or watch this extended video from Simon Peyton Jones.


