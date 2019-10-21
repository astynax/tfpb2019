module Main where 

import Prelude hiding (map, filter, zip, taske, head, zipWith, zipWith3, takeWhile, dropWhile, break, foldr, foldl)

filter :: (a -> Bool) -> [a] -> [a]
filter _ [] = []
filter p (x:xs) = 
    if p x 
    then x : filter p xs 
    else filter p xs

zip :: [a] -> [b] -> [(a,b)]
zip _     [] = []
zip []    _  = []
zip (x:xs) (y:ys) = (x,y): zip xs ys


zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith _ _ [] = []
zipWith _ [] _ = []
zipWith f (x:xs) (y:ys) = f x y : zipWith f xs ys

zipWith3 :: (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]
zipWith3 _ _ _ [] = []
zipWith3 _ _ [] _ = []
zipWith3 _ [] _ _ = []
zipWith3 f (x:xs) (y:ys) (z:zs) = f x y z : zipWith3 f xs ys zs

takeWhile, dropWhile :: (a -> Bool) -> [a] -> [a]
takeWhile _ [] = []
takeWhile p (x:xs) 
    | p x       = x: takeWhile p xs
    | otherwise = []

dropWhile _ [] = []
dropWhile p (x:xs)
    | p x       = dropWhile p xs
    | otherwise = x : xs



foldl :: (acc -> b -> acc) -> acc -> [b] -> acc
foldr :: (b -> acc -> acc) -> acc -> [b] -> acc

foldl _ acc []     =  acc 
foldl f acc (x:xs) =  foldl f (f acc x) xs

foldr _ acc []     =  acc 
foldr f acc (x:xs) =  f x (foldr f acc xs)



r = [1, 5, 3, 8, 3, 2, 3, 7, 4]
main = do
   print r
   print x
   where 
       x = foldl (+) 0 ws
       ws = zipWith (-) bs r
       bs = zipWith min bl br
       bl = tail $ scanl max 0 r
       br = init $ scanr max 0 r


