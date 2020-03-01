foldr :: (a -> b -> b) -> b  -> [a] -> b
foldr _ v [] = v
foldr f v (x:xs) = f x (foldr f v xs)

foldr (+) 0 [1,2,3] =
1 + (foldr (+) 0 [2,3]) =
1 + (2 + (foldr (+) 0 [3])) =
1 + (2 + (3 + (foldr (+) 0 []))) =
1 + (2 + (3 + 0)) =
1 + (2 + 3) =
6


foldl :: (b -> a -> b) -> b -> [a] -> b
foldl _ v [] = v
foldl f v (x:xs) = foldl f (f v x) xs 

foldl (+) 0 [1,2,3] =
foldl (+) (0 + 1) [2,3] =
foldl (+) ((0 + 1) + 2) [3] =
foldl (+) (((0 + 1) + 2) + 3) [] =
(((0 + 1) + 2) + 3) =
((1 + 2) + 3) =
(3 + 3) =
6
