module Ch4 where

-- Core language

data ArithC
  = NumC Int
  | PlusC ArithC ArithC
  | MultC ArithC ArithC
  deriving (Eq, Show)


interp :: ArithC -> Int
interp (NumC n) = n
interp (PlusC l r) = interp l + interp r
interp (MultC l r) = interp l * interp r

-- Surface language

data ArithS
  = NumS Int
  | PlusS ArithS ArithS
  | MultS ArithS ArithS
  | BMinusS ArithS ArithS
  | UMinusS ArithS
  deriving (Eq, Show)

-- Desugar
-- A parser for the language should return ArithS expressions which we then
-- desugar into ArithC expressions.

desugar :: ArithS -> ArithC
desugar (NumS n) = (NumC n)
desugar (PlusS l r) = (PlusC (desugar l) (desugar r))
desugar (MultS l r) = (MultC (desugar l) (desugar r))
desugar (BMinusS l r) =
  -- l - r = l + -1 * r
  (PlusC (desugar l)
         (MultC (NumC (-1)) (desugar r)))
desugar (UMinusS e) =
  -- -e = 0 - e
  -- desugar (BMinusS (NumS 0) e)
  --
  -- N.B. The embedding of an input term in another one and recursively calling
  -- desugar is a common pattern in desugaring tools called a macro.
  --
  -- It works but there are two problems in this case:
  --
  -- 1. The recursion is generative.
  -- 2. We are implicitly depending on what BMinusS means and if its meaning
  --    changes, so will that of UMinusS, even if we don't want it to.
  --
  -- A better alternative:
  -- -e = -1 * e
  (MultC (NumC (-1)) (desugar e))

-- What is generative recursion?
-- Read: http://www.htdp.org/2003-09-26/Book/curriculum-Z-H-31.html#node_part_V

-- N.B. If we attempt to fix the generative recursive problem by doing
-- (desugar e) within the UMinusS definition then we'd get a type check error
-- since we'd be passing an ArithC to BMinusS.

-- -((5 - 3) + -6)
-- = -(2 + -6)
-- = -(-4)
-- = 4
test =
  (==) (interp (desugar (UMinusS (PlusS (BMinusS (NumS 5) (NumS 3))
                                        (UMinusS (NumS 6))))))
       4
