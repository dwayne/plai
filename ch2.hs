module Ch2 where

-- I'll just skip the parsing stuff since I'm not interested in that anyway.
-- Instead, I will keep my focus on the AST and everything that comes after
-- that knowing fully well that when I need to focus on the concrete syntax
-- that there are books like "Compilers: Principles, Techniques and Tools" to
-- show me the way.

data ArithC
  = NumC Int
  | PlusC ArithC ArithC
  | MultC ArithC ArithC
  deriving (Eq, Show)


-- (1 * 2) + (2 + 3)
example :: ArithC
example =
  PlusC (MultC (NumC 1) (NumC 2))
        (PlusC (NumC 2) (NumC 3))
