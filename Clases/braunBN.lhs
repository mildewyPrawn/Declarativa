Versiones con numeros por paridad para recuperar el uso de patrones.
Esto evita el razonamiento con div y mod.


> data BTree a = E | N a (BTree a) (BTree a) -- deriving Show

> btshow E = "< >"

> btshow (N x s t) = "< " ++ show x ++ " | " ++ btshow s ++ " | " ++ btshow t ++ " > "


> instance Show a => Show (BTree a) where
>    show = btshow


> l x = N x E E

> t1 = N 1 E E

> t2 = N 2 E E

> t3 = N 3 t1 t2

> t4 = N 4 t3 t2

> t5 = N 4 (N 3 t1 E) (N 2 t1 E)

> t6 = N 1 (N 2 (l 4) (l 5)) (N 3 (l 6) E)


0, 2k ,2k+1

0, 2k+2, 2k+1


size t = 2 k +1   entonces los subarboles son de tam. k
             +2    el izq es de tam. k+1 y el der. de tam. k.



Un natural es el cero 0
o bien el doble de un natural no cero: 2k+2 = 2(k+1)     2k
o bien el sucesor del doble de un natural 2k+1

Idea : U b = 2b +1  D b = 2b + 2
-- Si fuera D b = 2b el 0 tendria infinitas representaciones 

> data BN = Z | U BN | D BN  -- deriving Show   

Conversion de BN a Int

> conv :: BN -> Int

> conv Z = 0

> conv (U b) = 2*(conv b) + 1

> conv (D b) = 2*(conv b) + 2

Bonita instancia pa pintar BN

> instance Show BN where

>   show =  show . conv

> (!) ::  BTree b -> BN -> b

> E ! i = error "index out of range"

> (N x l r) ! Z = x 

> (N x l r) ! U b = l ! b

> (N x l r) ! D b = r ! b





> update :: BTree b -> BN -> b -> BTree b

> update E b _ = error "index out of range"

> update (N x l r) Z y = N y l r

> update (N x l r) (U b) y = N x (update l b y) r

> update (N x l r) (D b) y = N x l (update r b y) 

> lowExt :: b -> BTree b -> BTree b   -- \oplus en el articulo de Okasaki

> lowExt y E = N y E E

> lowExt y (N x l r) = N y (lowExt x r) l

broot E = error "empty array"

broot (N x s t) = x

 lowRem (N x E E) = E

 lowRem (N x l r) = N (l!0) r (lowRem l)


bdelr E = error "empty array"

> lowRem :: BTree b -> BTree b

> lowRem (N x E E) = E    -- lowRem (N x E t) = t

> lowRem (N x l r) = N (l ! Z) r (lowRem l)


> highExt :: b -> BTree b -> BTree b

 highExt x E = N x E E

 highExt x t@(N y l r) | mod (size t - 1) 2 == 0 = N y (highExt x l) r
                       | otherwise = N y l (highExt x r)

> suc Z = U Z     -- suc 0 = 2*0 + 1

> suc (U b) = D b -- suc (2b+1) = 2b+2

> suc (D b) = U (suc b)  suc(2b+2) = 2b+3 = 2(b+1)+1  

> plus Z y = y

> plus x Z = x

> plus (U x) (U y) = D (plus x y)      2x+1 + 2y+1 = 2(x+y) + 2

> plus (U x) (D y) = U (suc (plus x y))

> plus (D x) (U y) = U (suc (plus x y))

> plus (D x) (D y) = D (suc (plus x y))

> size E = Z

> size (N x l r) = suc (plus (size l) (size r))

Z, b => b
    | a, Z  => a
    | U x, U y => D(plusBN x y)
    | D x, U y => U(sucBN (plusBN x y))
    | U x, D y => U(sucBN (plusBN x y))
    | D x, D y => D(sucBN (plusBN x y)) 



> highExt x E = N x E E

> highExt x t@(N y l r) = case size r of
>                          U b -> N y (highExt x l) r
>                          D b -> N y l (highExt x r)

bhins y r@(N x s t) = case toBN (bsize r) of
                       U b -> N x (bhins y s) t
                       D b -> N x s (bhins y t)


> highRem :: BTree b -> BTree b

> highRem E = error "Arbol vacio"

> highRem (N _ E E) = E

 highRem t@(N x l r)  | mod (size t - 2) 2 == 0 = N x (highRem l) r
                      | otherwise = N x l (highRem r)

> highRem t@(N x l r) = case size t of 
>                        U b -> N x l (highRem r) 
>                        D b -> N x (highRem l) r 





eqbn Z Z = True

eqbn (U a) (U b) = eqbn a b

eqbn (D a) (D b) = eqbn a b

eqbn _ _ = False



ltbn :: BN -> BN -> Bool 

ltbn Z Z = False

ltbn Z _ = True

ltbn (U a) (U b) = ltbn a b

ltbn (U a) (D b) = ltbn a b || eqbn a b     -- 2a+1 < 2b + 2 <-> a <= b   

ltbn (D a) (D b) = ltbn a b

ltbn (D a) (U b) = ltbn a b   -- 2a+2 < 2b+1 <-> a < b


sucBN Z = U Z

sucBN (U b) = D b

sucBN (D b) = U (sucBN b)

predBN Z = Z

predBN (U b) = D (predBN b)

predBN (D b) = U b


toBN :: Int -> BN

toBN 0 = Z

toBN (n+1) = sucBN (toBN n)




bsize :: BTree a -> Int

bsize E  = 0

bsize (N x s t) = 1 + bsize s + bsize t


diff E Z = 0

diff (N x E E) Z = 1

diff (N x s t) (U b) = diff s b

diff (N x s t) (D b) = diff t b


esize E = 0

esize (N x s t) = let m = esize t in 1 + 2*m + diff s (toBN m)



ins y E = N y E E

ins y (N x s t) = N y (ins x t) s

makebt xs = foldr ins E xs




> copy :: b -> BN -> BTree b

> copy x Z = E

> copy x (U b) = let t = copy x b in N x t t

> copy x (D b) = let t = copy x b in N x (lowExt x t) t


> toBN :: Int -> BN

> toBN 0 = Z

> toBN k | mod k 2 == 1 = U (toBN (div k 2))
>        | otherwise = D (toBN (div k 2 - 1))

> copy2 :: b -> BN -> (BTree b,BTree b) 

> copy2 x Z = (N x E E,E)

> copy2 x (U b) = let (s,t) = copy2 x b in (N x s t,N x t t)

> copy2 x (D b) = let (s,t) = copy2 x b in (N x s s,N x s t)

> ecopy x b = snd (copy2 x b)
