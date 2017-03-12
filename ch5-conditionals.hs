module Ch5Conditionals where


data BoolC
  = BoolC Bool
  | AndC BoolC BoolC
  | OrC BoolC BoolC
  | GtC ExprC ExprC
  | LtC ExprC ExprC
  | EqC ExprC ExprC
  deriving (Eq, Show)


data ExprC
  = NumC Int
  | PlusC ExprC ExprC
  | MultC ExprC ExprC
  | IfC BoolC ExprC ExprC
  deriving (Eq, Show)


interp :: ExprC -> Int
interp (NumC n) = n
interp (PlusC l r) = interp l + interp r
interp (MultC l r) = interp l * interp r
interp (IfC cond consequent alternative) =
  if (interpB cond)
  then (interp consequent)
  else (interp alternative)


interpB :: BoolC -> Bool
interpB (BoolC b) = b
interpB (AndC l r) = (interpB l) && (interpB r)
interpB (OrC l r) = (interpB l) || (interpB r)
interpB (GtC l r) = (interp l) > (interp r)
interpB (LtC l r) = (interp l) < (interp r)
interpB (EqC l r) = (interp l) == (interp r)


tests =
  [ interp (IfC (BoolC True) (NumC 1) (NumC 2)) == 1
  , interp (IfC (BoolC False) (NumC 1) (NumC 2)) == 2
  , (==) (interp (IfC (AndC (EqC (PlusC (NumC 2) (NumC 4))
                                 (MultC (NumC 3) (NumC 2)))
                            (GtC (NumC 5) (NumC 4)))
                      (NumC 1)
                      (NumC 2)))
                 1
  ]
