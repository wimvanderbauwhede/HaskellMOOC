def f(x)
    x*(x+1)
end

lst = []
for i in 1 .. 10
    lst.push(i)
end

lst_ = []

for elt in lst
    lst_.push( f(elt) )
end

puts lst_.inspect

def g(acc,elt)
    acc/elt
end

acc = 1.0
for elt in lst
    acc = g(acc,elt)
end

puts acc

def g_(acc,elt)
    elt/acc
end

acc = 1.0
for elt in lst.reverse
    acc = g_(acc,elt)
end

puts acc


