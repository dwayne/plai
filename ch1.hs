module Ch1 where


data MisspelledAnimal
  = Caml Int
  | Yacc Double
  deriving (Eq, Show)


ma1 = Caml 2
ma2 = Yacc 1.9


good :: MisspelledAnimal -> Bool
-- good (Caml humps) = humps >= 2
-- good (Yacc height) = height > 2.1
good (Caml h) = h >= 2
good (Yacc h) = h > 2.1

test1 = good ma1 == True
test2 = good ma2 == False


-- What if you give the Caml data constructor a String?
-- (Caml "toe")
-- Ans: That's a type error.

-- What if you send a number to each version of good above?
-- good 1
-- Ans: That's a type error.
