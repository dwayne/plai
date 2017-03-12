module Ch7Closures where


type Symbol = String


data ExprC
  = NumC Int
  | IdC Symbol
  | AppC ExprC ExprC
  | PlusC ExprC ExprC
  | MultC ExprC ExprC
  | LamC Symbol ExprC
  deriving (Eq, Show)


data Value
  = NumV Int
  | ClosV Symbol ExprC Env
  deriving (Eq, Show)

-- Environment

data Binding
  = Bind Symbol Value
  deriving (Eq, Show)


type Env = [Binding]


mtEnv :: Env
mtEnv = []


extendEnv :: Binding -> Env -> Env
extendEnv = (:)


lookupEnv :: Symbol -> Env -> Value
lookupEnv s [] = error $ "unbound: " ++ s
lookupEnv s ((Bind t v):env) = if s == t then v else lookupEnv s env

-- Interpreter

interp :: ExprC -> Env -> Value
interp expr env =
  case expr of
    NumC n ->
      NumV n

    IdC s ->
      lookupEnv s env

    AppC e a ->
      let
        c = interp e env
      in
        apply c (interp a env)

    PlusC l r ->
      add (interp l env) (interp r env)

    MultC l r ->
      mul (interp l env) (interp r env)

    LamC p b ->
      ClosV p b env


test1 =
  (==)
  (interp (PlusC (NumC 10) (AppC (LamC "_" (NumC 5)) (NumC 10)))
          mtEnv)
  (NumV 15)


test2 =
  (==)
  (interp (AppC (LamC "x" (AppC (LamC "y" (PlusC (IdC "x") (IdC "y")))
                                (NumC 4)))
                (NumC 3))
          mtEnv)
  (NumV 7)


nested1 =
  LamC "x"
       (LamC "x" (PlusC (IdC "x") (IdC "x")))


appNested1 =
  AppC nested1 (NumC 5)


appNested2 =
  AppC appNested1 (NumC 4)


test3 =
  (==)
  (interp appNested1 mtEnv)
  (ClosV "x" (PlusC (IdC "x") (IdC "x")) [Bind "x" (NumV 5)])


-- Since the new binding of x will come before the old binding of x
-- i.e. [Bind "x" (NumV 4), Bind "x" (NumV 5)]
-- the result will be 8 instead of 10
test4 =
  (==)
  (interp appNested2 mtEnv)
  (NumV 8)


nested2 =
  LamC "x"
       (LamC "y" (PlusC (IdC "x") (IdC "y")))


appNested3 =
  AppC nested2 (NumC 5)


appNested4 =
  AppC appNested3 (NumC 4)


test5 =
  (==)
  (interp appNested3 mtEnv)
  (ClosV "y" (PlusC (IdC "x") (IdC "y")) [Bind "x" (NumV 5)])


test6 =
  (==)
  (interp appNested4 mtEnv)
  (NumV 9)


-- error: unbound x
-- Why?
--
-- (\f (\x (f 10)))(\y (x + y))(5) ; env = []
-- (\x (f 10))(5) ; env = f: (\y (x + y)) []
-- (f 10) ; env = x: 5, f: (\y (x + y)) []
--
-- Since f is a closure it will be interpreted in its environment which is
-- empty
--
-- ((\y (x + y)) 10) ; env = []
-- x + y ; env = y: 10
--
-- But x is still free, hence the result
test7 = interp (AppC (AppC (LamC "f" (LamC "x" (AppC (IdC "f") (NumC 10))))
                           (LamC "y" (PlusC (IdC "x") (IdC "y"))))
                     (NumC 5))
               mtEnv
-- This test shows that environments automatically implement capture-free
-- substitution
--
-- Why? Because closures are interpreted in their saved environments


-- Helpers

apply :: Value -> Value -> Value
apply (ClosV p b env) a = interp b (extendEnv (Bind p a) env)
apply _ _ = error "not a closure"


add :: Value -> Value -> Value
add (NumV a) (NumV b) = NumV (a + b)
add _ _ = error "one argument was not a number"


mul :: Value -> Value -> Value
mul (NumV a) (NumV b) = NumV (a * b)
mul _ _ = error "one argument was not a number"
