module Ch7NoClosures where


type Symbol = String


data ExprC
  = NumC Int
  | IdC Symbol
  | AppC ExprC ExprC
  | PlusC ExprC ExprC
  | MultC ExprC ExprC
  | FunC Symbol Symbol ExprC
  deriving (Eq, Show)


data Value
  = NumV Int
  | FunV Symbol Symbol ExprC
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
        f = interp e env
      in
        apply f (interp a env)

    PlusC l r ->
      add (interp l env) (interp r env)

    MultC l r ->
      mul (interp l env) (interp r env)

    FunC f p b ->
      FunV f p b


test1 =
  (==)
  (interp (PlusC (NumC 10) (AppC (FunC "const5" "_" (NumC 5)) (NumC 10)))
          mtEnv)
  (NumV 15)


-- error: unbound: x
test2 = interp (AppC (FunC "f1" "x" (AppC (FunC "f2" "y" (PlusC (IdC "x") (IdC "y")))
                                          (NumC 4)))
                     (NumC 3))
               mtEnv


nested1 =
  FunC "f1" "x"
       (FunC "f2" "x" (PlusC (IdC "x") (IdC "x")))


appNested1 =
  AppC nested1 (NumC 4)


test3 =
  (==)
  (interp appNested1 mtEnv)
  (FunV "f2" "x" (PlusC (IdC "x") (IdC "x")))


nested2 =
  FunC "f1" "x"
       (FunC "f2" "y" (PlusC (IdC "x") (IdC "y")))


appNested2 =
  AppC nested2 (NumC 4)


-- I'd expect this to be true but it isn't
test4 =
  (==)
  (interp appNested2 mtEnv)
  (FunV "f2" "y" (PlusC (NumC 4) (IdC "y")))


-- Helpers

apply :: Value -> Value -> Value
apply (FunV _ p b) a = interp b (extendEnv (Bind p a) mtEnv)
apply _ _ = error "not a function"


add :: Value -> Value -> Value
add (NumV a) (NumV b) = NumV (a + b)
add _ _ = error "one argument was not a number"


mul :: Value -> Value -> Value
mul (NumV a) (NumV b) = NumV (a * b)
mul _ _ = error "one argument was not a number"
