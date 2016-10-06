-- our list
lst = [ 1.. 10]

-- map
f x = x*(x+1)
lst_ = map f lst

-- foldl
g = (/)
accl = foldl g 1 lst

-- foldr
g' = (/)
accr = foldr g' 1 lst

-- main
main = do
    print lst_
    print accl
    print accr
