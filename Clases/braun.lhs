
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


class BArray ar where

 emptyArray :: ar b 

 isEmpty :: ar b -> Bool 
  
> size :: BTree b -> Integer

> size E = 0
> size (N x l r) = 1 + size l + size r

> esize E = 0

> esize (N x l r) = let m = esize r in 1 + 2*m + diff l m

> diff E 0 = 0

> diff (N x E E) 0 = 1

> diff (N x l r) k | mod k 2 == 1 = diff l (div k 2)
>                  | otherwise = diff r (div k 2 - 1)

 (!) ::  ar b -> Int -> b

> (!) ::  BTree b -> Int -> b

> E ! _ = error "Arbol vacio"
> (N x l r) ! 0 = x
> (N x l r) ! k | mod k 2 == 1 = l ! (div k 2)
>               | otherwise = r ! (div k 2 - 1)

update :: ar b -> Int -> b -> ar b

> update :: BTree b -> Int -> b -> BTree b

> update E _ _ = error "Arbol vacio"

> update (N y l r) 0 x = N x l r

> update (N y l r) k x | mod k 2 == 1 = N y (update l (div k 2) x) r ---  k = 2m + 1
>                      | otherwise = N y l (update r (div k 2 - 1) x)  -- k = 2m + 2 = 2(m+1) 

(update (N y l r) k x) ! k = x



 lowExt :: b -> ar b -> ar b

> lowExt :: b -> BTree b -> BTree b   -- \oplus en el articulo de Okasaki

> lowExt x E = N x E E

> lowExt x (N y l r) = N x (lowExt y r) l

lowExt x t ! 0 = x


 lowRem :: ar b -> ar b

> lowRem :: BTree b -> BTree b

> lowRem E = error "Arbol vacio"

> lowRem (N x E E) = E

> lowRem (N x l r) = N (l!0) r (lowRem l)

 highExt :: b -> ar b -> ar b

> highExt :: b -> BTree b -> BTree b

> highExt x E = N x E E

> highExt x t@(N y l r) | mod (size t - 1) 2 == 0 = N y (highExt x l) r
>                       | otherwise = N y l (highExt x r)


 highRem :: ar b -> ar b

> highRem :: BTree b -> BTree b

> highRem E = error "Arbol vacio"

> highRem (N _ E E) = E

> highRem t@(N x l r)  | mod (size t - 2) 2 == 0 = N x (highRem l) r
>                      | otherwise = N x l (highRem r)


copy :: b -> Int -> ar b

> copy :: b -> Int -> BTree b

> copy x 0 = E

 copy x k = N x (copy x (ceiling (fromIntegral (div (k-1) 2))))
                (copy x (floor (fromIntegral (div (k-1) 2))))


copy x 0 = E
copy x (2m+1) = let t = copy x m in N x t t
copy x (2m+2) = N x (copy x (m+1)) (copy x m)





> ecopy x 0 = E

> ecopy x k | mod k 2 == 1 = let t = ecopy x (div k 2) in N x t t
>           | otherwise = let d = div k 2 - 1 in N x (ecopy x (d+1)) (ecopy x d)


> ecopyle x 0 = E

> ecopyle x k | mod k 2 == 1 = let t = ecopyle x (div k 2) in N x t t
>             | otherwise = let r = ecopyle x (div k 2 - 1)
>                                 in N x (lowExt x r) r


> ecopy2 x 0 = (N x E E,E)

> ecopy2 x k | mod k 2 == 1 = let (l,r) = ecopy2 x (div k 2) in (N x l r, N x r r)
>            | otherwise = let (l,r) = ecopy2 x (div k 2 - 1)
>                            in (N x l l, N x l r)


mapArr :: (b -> c) -> ar b -> ar c

fromList :: [b] -> ar b

toList :: ar b -> [b]








infixl 9 !

class FArray a where
 
 (!) :: a x -> Int -> x

 update :: a x -> Int -> x -> a x

 empty :: a x

 isempty :: a x -> Bool

 size :: a x -> Int

 copy :: Int -> x -> a x

 cons :: x -> a x -> a x

 head :: a x -> x

 tail :: a x -> a x

 snoc :: a x -> x -> a x

 last :: a x -> x

 rlast :: a x -> a x

 makeArray :: [x] -> a x

 toList :: a x -> [x]  

 

mka = foldr bins E

