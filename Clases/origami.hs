myConcatMap :: (a -> [b]) -> [a] -> [b]
myConcatMap _ [] = []
myConcatMap f (x:xs) = (f x) ++ myConcatMap f xs

cmF :: (a -> [b]) -> [a] -> [b]
cmF f = foldr (\x xs -> (f x) ++ xs) []
-- Point free Hsskell <3

myMap :: (a -> b) -> [a] -> [b]
myMap _ [] = []
myMap f (x:xs) = (f x) : (myMap f xs)

mF :: (a -> b) -> [a] -> [b]
mF f = foldr (\x xs -> (f x) : xs) [] 

myLength :: [a] -> Int
myLength [] = 0
myLength (x:xs) = 1 + (myLength xs)

lF :: [a] -> Int
lF = foldr (\x xs -> 1 + xs) 0