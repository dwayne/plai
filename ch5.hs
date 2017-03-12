module Ch5 where

-- TODO:
-- Learn about the name capture problem and fix substitution.
-- See Lambda Calculus for further details.


type Symbol = String


data ExprC
  = NumC Int
  | IdC Symbol
  | AppC Symbol ExprC
  | PlusC ExprC ExprC
  | MultC ExprC ExprC
  deriving (Eq, Show)


data FunDefC
  = FunC Symbol Symbol ExprC
  deriving (Eq, Show)


-- double x = x + x
double :: FunDefC
double = FunC "double" "x" (PlusC (IdC "x") (IdC "x"))


-- quadruple x = double (double x)
quadruple :: FunDefC
quadruple = FunC "quadruple" "x" (AppC "double" (AppC "double" (IdC "x")))


-- const5 _ = 5
const5 :: FunDefC
const5 = FunC "const5" "_" (NumC 5)


-- lazy
-- subst :: ExprC -> Symbol -> ExprC -> ExprC
-- subst expr name pattern =
--   case pattern of
--     NumC n -> pattern
--     IdC s -> if s == name then expr else pattern
--     AppC f a -> AppC f (subst expr name a)
--     PlusC l r -> PlusC (subst expr name l) (subst expr name r)
--     MultC l r -> MultC (subst expr name l) (subst expr name r)


-- eager
subst :: Int -> Symbol -> ExprC -> ExprC
subst val name pattern =
  case pattern of
    NumC n -> pattern
    IdC s -> if s == name then (NumC val) else pattern
    AppC f a -> AppC f (subst val name a)
    PlusC l r -> PlusC (subst val name l) (subst val name r)
    MultC l r -> MultC (subst val name l) (subst val name r)


test1 =
  (==)
  (subst 1 "x" (PlusC (IdC "x") (IdC "x")))
  (PlusC (NumC 1) (NumC 1))


interp :: ExprC -> [FunDefC] -> Int
interp e fds =
  case e of
    NumC n ->
      n

    IdC s ->
      error $ "unbound: " ++ s

    AppC f a ->
      let
        FunC _ p b = find f fds
      in
        -- lazy
        -- interp (subst a p b) fds
        --
        -- eager
        interp (subst (interp a fds) p b) fds


    PlusC l r ->
      (interp l fds) + (interp r fds)

    MultC l r ->
      (interp l fds) * (interp r fds)


fds = [ double, quadruple, const5 ]


test2 =
  (==)
  (interp (MultC (NumC 3) (AppC "double" (NumC 2))) fds)
  12


test3 =
  (==)
  (interp (AppC "quadruple" (NumC 2)) fds)
  8


test4 =
  (==)
  (interp (AppC "const5" (IdC "x")) fds)
  5


-- error: unbound: x
test5 = interp (IdC "x") []


-- error: undefined function: double
test6 = interp (AppC "double" (NumC 1)) []


-- Helpers

find :: Symbol -> [FunDefC] -> FunDefC
find f [] = error $ "undefined function: " ++ f
find f (fd@(FunC name _ _):fds)
  | f == name = fd
  | otherwise = find f fds
