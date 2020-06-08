{-
- Programacion Declarativa 2020-1
- Tarea 6: Release the Monad! | eval
- Profesor: Dr. Favio Ezequiel Miranda Perea
- Ayudante: Javier EnriquezMendoza
- Alumnos: Juan Alfonso Garduño Solís
-          Emiliano Galeana Araujo
-}

module Eval where
import Control.Monad
import Data.List

data Expr = Num Int | Var Int | Div Expr Expr deriving Show

type Value = Int
type Env = [(Int, Value)]

data Exception = DivisionPorCero | VariableNoDefinida | MalaSuerte deriving Show

data ExprErr a = Raise Exception | Return a deriving Show

-- Dar una instancia del tipo ExprErr de la clase Monad, recordando que se deben
-- cumplir las leyes monádicas.
returnErr :: a -> ExprErr a
returnErr = Return

bindErr :: ExprErr a -> (a -> ExprErr b) -> ExprErr b
bindErr (Return x) f = f x
bindErr (Raise e) f = Raise e

instance Functor ExprErr where
  fmap f (Return x) = Return (f x)

pureErr :: a -> ExprErr a
pureErr = Return

gappErr :: ExprErr (a -> b) -> ExprErr a -> ExprErr b
gappErr (Return f) (Return x) = Return (f x)

instance Applicative ExprErr where
  pure = pureErr
  (<*>) = gappErr

instance Monad ExprErr where
  return = returnErr
  (>>=) = bindErr

-- Definir una función monádica evarEx que efectúa la evaluación reportando los
-- errores. Definir el tipo de esta función es parte del ejercicio pero recuerda
-- que se necesita de un ambiente para la evaluación de variables
evalEx :: Expr -> Env -> ExprErr Int
evalEx (Var v) [] = Raise VariableNoDefinida
evalEx (Var v) ((i, iv):xs) = if v == i
                              then evalEx (Num iv) xs
                              else evalEx (Var v) xs
evalEx (Num n) l = if n == 13
                   then Raise MalaSuerte
                   else if n == 0
                        then Raise DivisionPorCero
                        else return n
-- si dejo esto aquí, regresa Return n, sino solo n
evalEx (Div (Num 13) _) l = Raise MalaSuerte
evalEx (Div _ (Num 13)) l = Raise MalaSuerte
evalEx (Div _ (Num 0)) l = Raise DivisionPorCero
evalEx (Div e1 e2) l =
  do
    v1 <- evalEx e1 l
    v2 <- evalEx e2 l
    let r = (div v1 v2)
    if r == 13
      then Raise MalaSuerte
      else return (div v1 v2)

---------------------------------------------------------------------------------
--------                              Pruebas                            --------
---------------------------------------------------------------------------------

porCeroS = evalEx (Div (Num 90) (Num 0)) [(9, 1), (8, 2)]
-- Regresa: Raise DivisionPorCero

porCeroE = evalEx (Div (Num 90) (Var 0)) [(0, 0), (1, 9)]
-- Regresa: Raise DivisionPorCero

porTreceSL = evalEx (Div (Num 90) (Num 13)) [(9, 1), (8, 2)]
-- Regresa: Raise MalaSuerte

porTreceSR = evalEx (Div (Num 13) (Num 1)) [(9, 1), (8, 2)]
-- Regresa: Raise MalaSuerte

treceSolo = evalEx (Num 13) []
-- Regresa: Raise MalaSuerte

varNotinScope = evalEx (Div (Num 4) (Var 9)) [(1, 2), (2, 3), (3, 4)]
-- Regresa: Raise VariableNoDefinida

succVar = evalEx (Div (Num 4) (Var 9)) [(1, 2), (9, 2), (3, 4)]
-- Regresa: Return 2

unaBuena = evalEx (Div (Var 1) (Div (Var 2) (Var 3))) [(1, 16), (2, 24), (3, 3)]
-- Regresa: Return 2
