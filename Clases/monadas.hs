data Expr = Num Int | Div Expr Expr deriving (Show,Eq)


e1 = Div (Div (Num 1972) (Num 2)) (Num 23)

e2 = Div (Num 18) (Div e1 (Num 7))

e0 = Div (Div (Num 34) (Num 2)) (Num 0)

-- Variación cero
-- Evaluador 0: simple y directo con desgracia al dividir entre cero, al estilo de estudiante de Discretas :( 

eval0 :: Expr -> Int

eval0 (Num n) = n

eval0 (Div e1 e2) = eval0 e1 `div` eval0 e2


-- Variación 1
-- Evaluador 1: con manejo de excepción mediante Maybe
-- con la desgracia de complicar el bonito código de eval0

-- Función de división segura.

safediv :: Int -> Int -> Maybe Int

safediv n m = if m==0 then Nothing else
                            Just (div n m)

eval1 :: Expr -> Maybe Int

eval1 (Num n) = Just n

eval1 (Div e1 e2) = case (eval1 e1) of
                     Nothing -> Nothing
                     Just v1 -> case (eval1 e2) of
                                 Nothing -> Nothing
                                 Just v2 -> safediv v1 v2
-- Código espantoso, el manejo de Nothing/Just es meramente burocrático.

-- Variacion 2: evaluador que cuenta el número de divisiones realizadas en una evaluación y chin nos vale la división entre cero.

-- Evaluador 2: contador explícito.

eval2 :: Expr -> (Int,Int)

eval2 (Num n) = (n,0)

eval2 (Div e1 e2) = let (v1,c1) = eval2 e1
                        (v2,c2) = eval2 e2
                    in
                        (div v1 v2, c1+c2+1)

-- Estamos iniciando el contador en cero, ¿qué hacer si
-- queremos iniciar con un valor arbitrario?

-- Variación 3: efectos de salida, mostrando la traza de evaluación
-- es decir, la evaluación de las subexpresiones en un orden dado.

-- Evaluador 3: se encapsula el resultado de la evaluación con una cadena
--              que devuelve la traza.

type Output = String

displayline :: Expr -> Int -> Output

displayline e n = "eval (" ++ show e ++ ") <-- " ++ show n ++ " ==> "

--displayline e n = "eval (" ++ show e ++ ") <-- " ++ show n ++ "\n"

eval3 :: Expr -> (Output,Int)

eval3 (Num n) = (displayline (Num n) n , n)

eval3 (Div e1 e2) = let (d1,v1) = eval3 e1 
                        (d2,v2) = eval3 e2 
                    in
                      (d1 ++ d2 ++
                        displayline (Div e1 e2) (div v1 v2), div v1 v2)


--- Evaluador 3b: cambio sencillo en el orden de salida de las subexpresiones. Ejemplo de elegancia y sencillez en PF. 

eval3b :: Expr -> (Output,Int)

eval3b (Num n) = (displayline (Num n) n , n)

eval3b (Div e1 e2) = let (d1,v1) = eval3b e1 
                         (d2,v2) = eval3b e2 
                    in
                      (displayline (Div e1 e2) (div v1 v2) ++ d1 ++ d2 , div v1 v2)

---- Vamos saltando hacia las mónadas
----------------------------------------------------------
-- UN FRAUDE: puro código barroco que hace lo mismo que eval0 de hecho con la misma idea operacional.

type Ide a = a

returnIde :: a -> Ide a

returnIde x = x

bindIde :: Ide a -> (a -> Ide b) -> Ide b      --   M a -> ( a -> M b) -> M b

bindIde x f = f x

eval0ide :: Expr -> Ide Int

eval0ide (Num n) = returnIde n

--Grandioso ofuscamiento obscurantista

eval0ide (Div e1 e2) = (eval0ide e1) `bindIde`
                       (\ v1 -> eval0ide e2
                       `bindIde` (\ v2 -> returnIde (div v1 v2)))

-- ¿ Qué es toda está parafernalia?

-- OBSCURANTISMO MONÁDICO:

newtype Id a = Wrap a

-- Esto parece el mismo fraude, estamos
-- adornando al mismo tipo de manera barroca y
-- churrigueresca, pero hay una "ventaja", pensemos en
-- que el constructor Wrap está encapsulando un valor
-- para distinguirlo de un valor ordinario del mismo tipo.
-- Digamos que es un valor con un disfraz, pero es esencialmente el mismo.
-- Aunque la mona se vista de seda ...

-- Función de retorno
returnId :: a -> Id a

returnId = \ x -> Wrap x   -- i.e returnId es la función
                           -- que envuelve o pone el disfraz
-- Función de ligado:  
bindId :: Id a -> (a -> Id b) -> Id b
-- Tenemos un valor x: Id a y una función f: a -> Id b
-- La manera más intuitiva de devolver un valor de Id b es
-- quitar el disfraz a x y aplicarle f a dicho valor
-- Se liga o pasa un valor mónadico x a una función f que espera
-- un valor ordinario, nótese que f x es incorrecta por tipos.

bindId (Wrap x) f = f x  

-- Mismo código de eval0ide, lo que cambió es la
-- definición de return y bind.

-- Variación cero en estilo monádico

eval0m (Num n) = returnId n

eval0m (Div e1 e2) = (eval0m e1) `bindId`
                       (\ v1 -> eval0m e2
                       `bindId` (\ v2 -> returnId (div v1 v2)))

-- Este es un bonito y malsano código obscurantista en estilo monádico.Un evaluador monádico.
-- Nótese que se está forzando y haciendo explícito el órden de evaluación.


-- Vamos a hacer lo mismo con las otras variaciones.
-- Recordemos los dos ingredientes básicos:
-- 1. return:: a -> M a,
--  función que pone el disfraz y nada más (transforma un valor ordinario
-- en un valor monádico)

-- 2. bind :: M a -> (a -> M b) -> M b
-- función que aplica una función que espera
--          un valor ordinario a un valor disfrazado (monádico)
--          devolviendo otro valor disfrazado (monádico)


--Evaluador 2: manejo de la división mediante Maybe con operadores monádicos

returnMaybe :: a -> Maybe a

returnMaybe  = Just 

bindMaybe :: Maybe a -> (a -> Maybe b) -> Maybe b

bindMaybe Nothing _ = Nothing

bindMaybe (Just x) f = f x

--- Evaluador 1 versión monádica

eval1m :: Expr -> Maybe Int

eval1m (Num n) = returnMaybe n

eval1m (Div e1 e2) = (eval1m e1) `bindMaybe`
                        (\ v1 -> (eval1m e2 `bindMaybe` (\ v2 -> safediv v1 v2)))

-- Maybe es una mónada predefinida, no es necesario definirla como arriba, eval1mb es operacionalmente igual a eval1m

eval1mb :: Expr -> Maybe Int

eval1mb (Num n) = return n

-- El nombre reservado para un operador bind es >>=, ofuscamiento obscurantista.

eval1mb (Div e1 e2) = (eval1mb e1) >>=
                         (\ v1 -> (eval1mb e2 >>=
                                     (\ v2 -> safediv v1 v2)))

-- Variacion 2
-- Implementación simple de Estado
-- Conteo de divisiones desde un número dado.
-- El evaluador toma una expresion, un contador y devuelve un par con el valor de la expresion y el contador actualizado. eval2c es un evaluador con contador.

type Counter = Int

eval2c :: Expr -> Counter -> (Int,Counter)

eval2c (Num n) c = (n,c) --se cuentan solo las divisiones

eval2c (Div e1 e2) c = let (v1,c1) = eval2c e1 c in
                       let (v2,c2) = eval2c e2 c1 in
                         (div v1 v2, c2+1)
                         
-- A diferencia que eval2, se hace explícito el cambio de estado (contador)
-- y se generaliza a empezar con cualquier valor del contador no sólo cero.

-- Generalización:
-- Vamos a abstraer el tipo de contadores, a un tipo de estados y a encapsular la transformación/actualizacion de estados.

type State = Int  --En nuestro ejemplo los estados son los valores del contador.

type ST a = State -> (a,State) 

-- ST a es el tipo de transformadores de estados que involucran el cómputo de valores de tipo a, dado un estado se devuelve un valor de tipo a y un estado actualizado: Si s::State, tr :: ST a  y tr s = (v,s') entonces v es el valor computado de tipo a y s' es una actualización del estado s

-- Implementación de las funciones monádicas

returnST :: a -> ST a

--returnST x s = (x,s)                                 

returnST x = \ s -> (x,s)

bindST :: ST a -> (a -> ST b) -> ST b                 
                                                     
bindST st f s = let (x,s') = st s in
                  f x s'

-- Evaluador monádico eval2m que cuenta el número de divisiones.

eval2m :: Expr -> ST Int -- i.e. Exp -> State -> (Int,State)
                         
eval2m (Num n)  = returnST n 

eval2m (Div e1 e2)  = eval2m e1 `bindST`
                       (\ z1-> eval2m e2 `bindST`
                          (\ z2 -> (\ s -> (div z1 z2,s+1))))
                       
-- Obsérvese que los estados inicial e intermedios
-- están ocultos.
-- Para interactuar con el mundo "real" es necesario pasar un estado inicial
-- particular:   eval2m e s. En particular eval2 e = eval2m e 0 



-- Variacion 3:
-- Evaluador monádico eval3m con efectos de salida

type OutM a = (Output,a)

returnOutM :: a -> OutM a

returnOutM x = ("",x)

bindOutM :: OutM a -> (a -> OutM b) -> OutM b

bindOutM (o,x) f = let (o',y) = f x in
                     (o ++ o', y)

out :: Output -> OutM ()

out s = (s,())

-- out encaja un valor de salida (una cadena en este caso)
-- en la mónada OutM con valores triviales. 
-- Truco para desplegar las trazas de evaluación
-- Nótese la interacción entre dos instancias de la misma mónada
-- M () y M Int

eval3m :: Expr -> OutM Int

eval3m (Num n) = (out (displayline (Num n) n)) `bindOutM` (\ _ -> returnOutM n)

eval3m (Div e1 e2) = eval3m e1 `bindOutM`
                      (\ v1 -> eval3m e2 `bindOutM`
                        (\ v2 -> (out (displayline (Div e1 e2) (div v1 v2)))
                           `bindOutM`
                              (\ _ -> returnOutM (div v1 v2))))


--Se observa que todas las variaciones siguen un mismo patrón que es el uso adecuado de las funciones return y bind. 

------------------------------------------------------------
-- La romántica notación do.

-- La notación do es una bonita forma de azucar sintáctica que nos permite escribir de manera más simple los programas monádicos y nos recuerda el estilo imperativo.

-- Variacion cero, mónada identidad
 -- El mismo evaluador pero con la IdM instanciada a la clase monad
-- para poder usar notación do

data IdM a = Id a deriving Show --cambiar el nombre al constructor

returnIdM :: a -> IdM a

returnIdM = Id

bindIdM :: IdM a -> (a -> IdM b) -> IdM b

bindIdM (Id x) f = f x 

-- La instancia a Monad requiere instancias previas a
-- Functor y Applicative

instance Functor IdM where
  fmap f (Id x) = Id (f x)

pureIdM :: a -> IdM a

pureIdM x = Id x

gappIdM :: IdM (a -> b) -> IdM a -> IdM b

gappIdM (Id f) (Id x) = Id (f x)


instance Applicative IdM where
  pure = pureIdM
  (<*>) = gappIdM

instance Monad IdM where
  return = returnIdM
  (>>=) = bindIdM 


eval0md :: Expr -> IdM Int

eval0md (Num n) = return n

eval0md (Div e1 e2) = do
                       v1 <- eval0md e1
                       v2 <- eval0md e2
                       return (div v1 v2)

-- Variación 1, Maybe es una mónada predefinida
-- Evaluador monádico con manejo de excepción mediante Maybe con notación do
-- esencialmente el mismo que eval1m pero con código "bonito" estilo imperativo

eval1md :: Expr -> Maybe Int

eval1md (Num n) = return n

eval1md (Div e1 e2) = do
                    v1 <- eval1md e1
                    v2 <- eval1md e2
                    safediv v1 v2 

--- Variacion 2:
--No es posible instanciar el constructor de tipos ST a la clase mónada pues está definido mediante type, por lo tanto no es posible transformar el programa eval2m mediante la notación do.
-- Hay que utilizar data lo cual requiere un constructor
-- y por lo tanto un encapsulamiento de la función transformadora de estados:

data StateTransf a = ST (State -> (a,State))

instance Functor StateTransf where
  fmap f (ST tr) = ST ( \s -> let (v,s') = tr s in
                                 (f v,s') )

returnSTr :: a -> StateTransf a

returnSTr x = ST (\ s -> (x,s))

pureSTr = returnSTr

gappSTr :: StateTransf ( a -> b) -> StateTransf a -> StateTransf b

gappSTr (ST trf) (ST tra) = ST (\ s -> let (f,s') = trf s in
                                        let (a,s'') = tra s' in
                                            (f a, s''))

instance Applicative StateTransf where
  pure = pureSTr
  (<*>) = gappSTr

bindSTr :: StateTransf a -> (a -> StateTransf b) -> StateTransf b

bindSTr (ST tr) f = ST (\ s -> let (x,s') = tr s in let  ST tr' = f x in tr' s')

instance Monad StateTransf where
  return = returnSTr
  (>>=) = bindSTr

-- -- Evaluador monádico usando StateTransf con notación do

eval2md :: Expr -> StateTransf Int

eval2md (Num n)  = return n 

eval2md (Div e1 e2)  = do
                      v1 <- eval2md e1
                      v2 <- eval2md e2
                      ST (\ s -> (div v1 v2, s+1))

-- Lo malo es que no se puede interactuar con
-- el mundo afuera, al estar lidiando con funciones no
-- hay show y tampoco se puede pasar directamente el estado dado que
-- el transformador está encapsulado.


--Función que desencapsula el estado en StateTransf para poder ver los resultados de la ejecución del transformador de estados.

runState :: StateTransf a -> State -> (a,State)

runState (ST tr) s = tr s  -- desencapsula el transformador y pásale el estado


-- Evaluador monádico usando StateTransf y el contador explícito tick

-- Contador del estilo n:=n+1 fuchi!

tick :: StateTransf ()

tick = ST (\ n -> ((),n+1))

eval2mdb :: Expr -> StateTransf Int

eval2mdb (Num n)  = return n 

eval2mdb (Div e1 e2)  = do
                       v1 <- eval2mdb e1
                       v2 <- eval2mdb e2
                       tick  -- c:=c+1, fuchi!
                       return (div v1 v2)
                       
-- La notación do esconde toda referencia explícita a
-- estados. Ofuscamiento obscurantista!!                     


---------------------- OTRAS MÓNADAS Y MONADAS ------------------------------                       
-- Nuestras grandes amigas y fieles compañeras las listas son una monada
-- digo una mónada

-- La mónada de listas

returnL :: a -> [a]

returnL x = [x]

--concat [xs1,..,xsn] = xs1 ++ ... ++ xsn
-- map :: (a -> [b]) -> ([a] -> [[b]])

bindL :: [a] -> (a -> [b]) -> [b]

bindL xs f = concat (map f xs)

-- El cómputo que representan las listas es una elección no determinista, el cómputo [x1,..xn] encapsula una elección de los
-- valores x1..xn, es decir, [x1..xn] visto como un cómputo es cualquiera de
-- x1,..,xn.

-- Ejemplo del producto cartesiano de dos listas: el producto cartesiano se obtien   e al combinar todas las posibilidades de dos cómputos no deterministas.

-- prod [x1..xn] [y1..ym] = [(x1,y1)...(xn,ym)]

prodmd :: [a] -> [b] -> [(a,b)]

prodmd xs ys = do
              x <- xs
              y <- ys
              return (x,y)

-- Ejercicio: escribir prodmd sin notación do, usando return y bind.
              
-- La definición con listar por comprensión no es otra cosa
-- que azucar sintáctica de la notación do.

prod2 xs ys = [(x,y)|x<-xs,y<-ys]



-- La mónada de entrada y salida 
-- IO a es el tipo de las acciones que computan un valor de tipo a y que pueden llevar a cabo algún efecto de entrada y salida.

-- IDEA:  type IO a = World -> (a,World)


hm = putStrLn "Hola mundo!"


main = do
        putStrLn " Hola, cómo se llama usté?  "
        nombre <- getLine
        putStrLn (" Buen dia su paternidad " ++ "\n " ++ nombre ++ "\n placer de conocerle ")


 
                     





--}











-- Ejercicio: convertir la mónada OutM en una mónada que se pueda instanciar a la clase Monad.

-- -- Suma de los elementos de un árbol mediante la mónada identidad

-- data Tree a = Void | Node a (Tree a) (Tree a) deriving Show


-- sumTree :: Tree Int -> IdM Int

-- sumTree Void = return 0

-- sumTree (Node x t1 t2) = do
--                           n1 <- sumTree t1
--                           n2 <- sumTree t2
--                           return (n1+n2+x)



