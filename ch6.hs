module Ch6 where


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


-- Environment

data Binding
  = Bind Symbol Int
  deriving (Eq, Show)


type Env = [Binding]


mtEnv :: Env
mtEnv = []


extendEnv :: Binding -> Env -> Env
extendEnv = (:)


lookupEnv :: Symbol -> Env -> Int
lookupEnv s [] = error $ "unbound: " ++ s
lookupEnv s ((Bind t v):env) = if s == t then v else lookupEnv s env

-- Interpreting with an environment

-- N.B. We don't need to expose the environment in the public API
interp :: ExprC -> Env -> [FunDefC] -> Int
interp e env fds =
  case e of
    NumC n ->
      n

    IdC s ->
      lookupEnv s env

    AppC f a ->
      let
        FunC _ p b = find f fds
      in
        -- dynamic scope
        -- test4 returns 7
        -- interp b (extendEnv (Bind p (interp a env fds)) env) fds
        --
        -- lexical scope or static scope
        -- test4 fails as expected
        interp b (extendEnv (Bind p (interp a env fds)) mtEnv) fds
        -- This implementation is correct insofar that it gives us
        -- the same results that substitution would have given us

    PlusC l r ->
      (interp l env fds) + (interp r env fds)

    MultC l r ->
      (interp l env fds) * (interp r env fds)


test1 =
  (==)
  (interp (PlusC (NumC 10) (AppC "const5" (NumC 10)))
          mtEnv
          [const5])
  15


test2 =
  (==)
  (interp (PlusC (NumC 10) (AppC "double" (PlusC (NumC 1) (NumC 2))))
          mtEnv
          [double])
  16


test3 =
  (==)
  (interp (PlusC (NumC 10) (AppC "quadruple" (PlusC (NumC 1) (NumC 2))))
          mtEnv
          [double, quadruple])
  22


-- error: unbound x
test4 = (interp (AppC "f1" (NumC 3))
                mtEnv
                [ FunC "f1" "x" (AppC "f2" (NumC 4))
                , FunC "f2" "y" (PlusC (IdC "x") (IdC "y"))
                ])


-- Helpers

find :: Symbol -> [FunDefC] -> FunDefC
find f [] = error $ "undefined function: " ++ f
find f (fd@(FunC name _ _):fds)
  | f == name = fd
  | otherwise = find f fds
