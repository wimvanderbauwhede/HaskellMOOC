data Pet = Cat | Dog | Fish | Parrot String

hello :: Pet ->  String
hello x  =
  case x of
    Cat  -> "meeow"
    Dog  -> "woof"
    Fish -> "bubble"
    Parrot name -> "pretty " ++ name

