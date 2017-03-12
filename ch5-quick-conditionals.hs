module Ch5QuickConditionals where

-- Core language

data ExprC
  = NumC Int
  | PlusC ExprC ExprC
  | MultC ExprC ExprC
  -- a conditional that treats 0 as false and everything else as true
  | IfC ExprC ExprC ExprC
  deriving (Eq, Show)


interp :: ExprC -> Int
interp (NumC n) = n
interp (PlusC l r) = interp l + interp r
interp (MultC l r) = interp l * interp r
interp (IfC cond consequent alternative) =
  if (interp cond) == 0
  then (interp alternative)
  else (interp consequent)


tests =
  [ interp (IfC (NumC 0) (NumC 1) (NumC 2)) == 2
  , interp (IfC (NumC 1) (NumC 1) (NumC 2)) == 1
  ]
