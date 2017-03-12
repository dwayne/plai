module Ch3 where


data ArithC
  = NumC Int
  | PlusC ArithC ArithC
  | MultC ArithC ArithC
  deriving (Eq, Show)


interp :: ArithC -> Int
interp (NumC n) = n
interp (PlusC l r) = interp l + interp r
interp (MultC l r) = interp l * interp r


-- The "meaning" of addition and multiplication in this language is the addition
-- and multiplication of Haskell Int.

-- To change it we'd have to do the following (as an e.g. we use Integer):
--
-- 1. Use "NumC Integer"
-- 2. Use "interp :: ArithC -> Integer"


test :: ArithC -> Int -> Bool
test expr val = interp expr == val


tests =
  [ test (NumC 5) 5
  , test (PlusC (NumC 1) (NumC 2)) 3
  , test (MultC (NumC 2) (NumC 3)) 6
  , test (PlusC (MultC (NumC 1) (NumC 2))
                (PlusC (NumC 2) (NumC 3)))
         7
  ]
