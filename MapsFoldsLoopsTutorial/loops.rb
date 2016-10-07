# our list
lst = []
for i in 1 .. 10
    lst.push(i)
end

# operations to emulate map 
def f(x)
    x*(x+1)
end

lst_ = []

for elt in lst
    lst_.push( f(elt) )
end

puts lst_.inspect

# operations to emulate foldl
def g(acc,elt)
    acc/elt
end

acc = 1.0
for elt in lst
    acc = g(acc,elt)
end

puts acc

# operations to emulate foldr
def g_(acc,elt)
    elt/acc
end

acc = 1.0
for elt in lst.reverse
    acc = g_(acc,elt)
end

puts acc
