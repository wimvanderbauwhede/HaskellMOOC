Quiz: We love lambda!

This quiz tests your understanding lambda expressions.

Q1 What is the result of evaluation of the following expression?

~~~haskell
(\x y -> x*x-y*y) 3 4
~~~

A1: 25

Q2 What is the result of evaluation of the following expression?

~~~haskell
map (\x -> length x) ["This","is", "a","test"] 
~~~

a) an error

b) 4

c) [4,2,1,4]

Q3 What is the result of evaluation of the following expression?

~~~haskell
(\x -> (\y -> y x)) "x" (\y -> y)
~~~

A3 : "x"

Q4 What is the result of evaluation of the following expression?

~~~haskell
(\x -> (\y -> x y)) "x" 
~~~

a) a partially applied function

b) a type error 

c) a string value

Q5 What is the result of the evaluation of the following expression?

~~~haskell
(\x f -> f x) 4 (\x -> x*x) 
~~~

a) 4
b) 16
c) a partially applied function
d) a type error

Q6 What is the result of the evaluation of the following expression?

~~~haskell
(\x -> 1) 2
~~~

a) 1
b) 2
c) 0
d) a type error



