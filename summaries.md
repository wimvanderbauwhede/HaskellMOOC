
# Summary of the key concepts covered in this activity

# Week 1: Haskell First Steps

### Haskell Basics: Expressions and Equations


$$
\begin{eqnarray}
& & \LARGE{\color{navy}{\mathsf{\bullet \;\hbox{ Expressions in Haskell are similar to those in other languages.}}}} \\
& & \LARGE{\color{navy}{\mathsf{\bullet \;\hbox{ But there are only expressions, no statements in Haskell.}}}} \\
& & \LARGE{\color{navy}{\mathsf{\bullet \;\hbox{ Assignments are not updates of values but equations.}}}} \\


\end{eqnarray}
$$

### Haskell Basics: Reduction, Functions and Lists


$$
\begin{eqnarray}
& & \LARGE{\color{navy}{\mathsf{\bullet \;\hbox{ Haskell programs compute by } \mathit{reduction} \hbox{.}}}} \\
& & \Large{\color{navy}{\mathsf{\; \; \hbox{ i.e. gradually replacing expressions by their values.}}}} \\
& & \LARGE{\color{navy}{\mathsf{\bullet \;\hbox{ A function takes one or more } \mathit{arguments} \hbox{ }}}} \\
& & \LARGE{\color{navy}{\mathsf{\; \; \hbox{   and computes a } \mathit{result} \hbox{.}}}} \\
& & \Large{\color{navy}{\mathsf{\; \; \hbox{ Given the same arguments, the result will always be the same.}}}} \\
& & \Large{\color{navy}{\mathsf{\; \; \hbox{ In Haskell there are no side-effects.}}}} \\
& & \LARGE{\color{navy}{\mathsf{\bullet \;\hbox{ The list is the key datastructure.}}}} \\
& & \Large{\color{navy}{\mathsf{\; \; \hbox{ It is quite similar to lists in other languages but immutable.}}}} \\


\end{eqnarray}
$$

# Week 3: Data Structures and Types

### Functions on Lists


$$
\begin{eqnarray}
& & \LARGE{\color{navy}{\mathsf{\bullet \;\hbox{ The basic mechanism for computing on}}}} \\
& & \LARGE{\color{navy}{\mathsf{\; \; \hbox{   any datastructure in Haskell is recursion.}}}} \\
& & \Large{\color{navy}{\mathsf{\; \; \hbox{ Recursion always has a } \mathit{base} \hbox{ case and an } \mathit{induction} \hbox{ case.}}}} \\
& & \LARGE{\color{navy}{\mathsf{\bullet \;\hbox{ For lists, the base case is the empty list [],}}}} \\
& & \LARGE{\color{navy}{\mathsf{\; \; \hbox{   the induction case is adding an element to the list } \mathit{x:xs} \hbox{.}}}} \\
& & \LARGE{\color{navy}{\mathsf{\bullet \;\hbox{ For list operations, it is usually easier}}}} \\
& & \LARGE{\color{navy}{\mathsf{\; \; \hbox{   to use higher-order functions}}}} \\
& & \Large{\color{navy}{\mathsf{\; \; \hbox{ like map (performing an operation on every element of a list)}}}} \\
& & \Large{\color{navy}{\mathsf{\; \; \hbox{ and foldl/foldr (reducing a list to a single value).}}}} \\


\end{eqnarray}
$$

# Week 4: When programs get bigger

### Parsing Text


$$
\begin{eqnarray}
& & \LARGE{\color{navy}{\mathsf{\bullet \;\hbox{ Parsing text is a very common and important operation.}}}} \\
& & \LARGE{\color{navy}{\mathsf{\bullet \;\hbox{ Monadic parser combinators are a convenient mechanism}}}} \\
& & \LARGE{\color{navy}{\mathsf{\; \; \hbox{   to build parsers by combining building blocks,}}}} \\
& & \Large{\color{navy}{\mathsf{\; \; \hbox{ and a good illustration of the practical use of a monad.}}}} \\
& & \LARGE{\color{navy}{\mathsf{\bullet \;\hbox{ Each parser is a higher-order function}}}} \\
& & \LARGE{\color{navy}{\mathsf{\; \; \hbox{   that returns a function.}}}} \\
& & \Large{\color{navy}{\mathsf{\; \; \hbox{ The parser combinators combine these functions into the final parser.}}}} \\


\end{eqnarray}
$$

# Week 5: Hardcore Haskell

### More about Types


$$
\begin{eqnarray}
& & \LARGE{\color{navy}{\mathsf{\bullet \;\hbox{ Function types describe the types of}}}} \\
& & \LARGE{\color{navy}{\mathsf{\; \; \hbox{   arguments and return value of a function.}}}} \\
& & \Large{\color{navy}{\mathsf{\; \; \hbox{ The type of an argument can be a type variable,}}}} \\
& & \Large{\color{navy}{\mathsf{\; \; \hbox{ in which case we call the function polymorphic.}}}} \\
& & \LARGE{\color{navy}{\mathsf{\bullet \;\hbox{ Currying means rewriting a function}}}} \\
& & \LARGE{\color{navy}{\mathsf{\; \; \hbox{   of more than one argument to a}}}} \\
& & \LARGE{\color{navy}{\mathsf{\; \; \hbox{   sequence of functions of a single argument.}}}} \\
& & \LARGE{\color{navy}{\mathsf{\bullet \;\hbox{ Type classes allow to impose restrictions}}}} \\
& & \LARGE{\color{navy}{\mathsf{\; \; \hbox{   on polymorphic type variables.}}}} \\
& & \Large{\color{navy}{\mathsf{\; \; \hbox{ Type classes express that e.g. a type represents}}}} \\
& & \Large{\color{navy}{\mathsf{\; \; \hbox{ a number, or something that can be ordered.}}}} \\
& & \LARGE{\color{navy}{\mathsf{\bullet \;\hbox{ Type inference is the analysis of code}}}} \\
& & \LARGE{\color{navy}{\mathsf{\; \; \hbox{   in order to infer its type.}}}} \\
& & \Large{\color{navy}{\mathsf{\; \; \hbox{ It works using type inference rules that}}}} \\
& & \LARGE{\color{navy}{\mathsf{\; \; \hbox{   generate typings based on the program text.}}}} \\


\end{eqnarray}
$$

# Week 6: Think like a functional programmer

### Type classes


$$
\begin{eqnarray}
& & \LARGE{\color{navy}{\mathsf{\bullet \;\hbox{ Type classes provide a set of function type signatures}}}} \\
& & \LARGE{\color{navy}{\mathsf{\bullet \;\hbox{ Haskell provides special keywords to create type classes}}}} \\
& & \LARGE{\color{navy}{\mathsf{\bullet \;\hbox{ To create an instance, we create functions}}}} \\
& & \LARGE{\color{navy}{\mathsf{\; \; \hbox{   for a given type that match the signature of the type class}}}} \\


\end{eqnarray}
$$

### Geek Greek


$$
\begin{eqnarray}
& & \LARGE{\color{navy}{\mathsf{\bullet \;\hbox{ The lambda calculus is a formal model}}}} \\
& & \LARGE{\color{navy}{\mathsf{\; \; \hbox{   for computation using only lambda functions.}}}} \\
& & \LARGE{\color{navy}{\mathsf{\bullet \;\hbox{ Haskell, as other functional languages,}}}} \\
& & \LARGE{\color{navy}{\mathsf{\; \; \hbox{   is based on the lambda calculus}}}} \\
& & \LARGE{\color{navy}{\mathsf{\bullet \;\hbox{ The reduction rules from the lambda calculus}}}} \\
& & \LARGE{\color{navy}{\mathsf{\; \; \hbox{   are use to reduce expressions in Haskell.}}}} \\


\end{eqnarray}
$$

### The M-word


$$
\begin{eqnarray}
& & \LARGE{\color{navy}{\mathsf{\bullet \;\hbox{ A monad is a mechanism for combining computations}}}} \\
& & \LARGE{\color{navy}{\mathsf{\bullet \;\hbox{ It is a typeclass providing the } \mathit{bind} \hbox{ and } \mathit{return} \hbox{ operations}}}} \\
& & \LARGE{\color{navy}{\mathsf{\bullet \;\hbox{ To be an actual monadic type, the implementations of}}}} \\
& & \LARGE{\color{navy}{\mathsf{\; \; \hbox{   `bind` and `return` must conform to the three monad laws}}}} \\
& & \LARGE{\color{navy}{\mathsf{\bullet \;\hbox{ The `Maybe` monad illustrates how to create}}}} \\
& & \LARGE{\color{navy}{\mathsf{\; \; \hbox{   a simple monad and its benefits.}}}} \\

\end{eqnarray}
$$

#