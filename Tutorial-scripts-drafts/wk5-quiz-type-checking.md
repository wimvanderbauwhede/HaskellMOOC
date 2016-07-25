Type checking

What are the types of the following expressions:

head 

head :: __ [a] -> a

putStrLn :: __

putStrLn :: String -> IO ()


Given the following type declarations:

f :: T1 -> T2
g :: T2 -> T3

And given that the following expression typechecks: 

v :: T1
v = h f g

What is te type of h?

h :: __ -> __ -> __


What is the type of the following function: 

    \f -> f f

MCQ

- True
- Bottom
- It is not possible to type this expression correctly in Haskell

To define a binary tree with the values stored in the lead nodes, complete the following type definition:


data Tree a = Node __ __ | Leaf __ 


data Tree a = Node (Tree a) (Tree a) | Leaf a 

What is the type of the following function (use a,b,c etc as type variables in order of occurence):

    \x y -> y

    
