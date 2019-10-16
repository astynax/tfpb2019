module Main where

import Prelude hiding (map, filter, zip, take, head, tail
                      , zipWith, zipWith3
                      , takeWhile, dropWhile
                      , foldl, foldr, (.), (++))
import qualified Prelude

--

head :: [a] -> a
head (x:_) = x
head _     = error "empty list"

tail :: [a] -> [a]
tail (_:xs) = xs
tail _      = error "empty list"

take :: Int -> [a] -> [a]
take 0 _      = []
take n (x:xs) = x : take (n - 1) xs
take _ []     = []

map :: (a -> b) -> [a] -> [b]
map _ []     = []
map f (x:xs) = f x : map f xs

filter :: (a -> Bool) -> [a] -> [a]
filter _ []     = []
filter p (x:xs)
  | p x       = x : rest
  | otherwise =     rest
  where
    rest = filter p xs

zip :: [a] -> [b] -> [(a, b)]
zip = zipWith (,)

zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith _ _ []          = []
zipWith _ [] _          = []
zipWith f (x:xs) (y:ys) = f x y : zipWith f xs ys

zipWith3 :: (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]
zipWith3 _ _ _ []               = []
zipWith3 _ _ [] _               = []
zipWith3 _ [] _ _               = []
zipWith3 f (x:xs) (y:ys) (z:zs) =
  f x y z : zipWith3 f xs ys zs

zipDef :: a -> b -> [a] -> [b] -> [(a, b)]
zipDef _ _ [] []         = []
zipDef a b [] (y:ys)     = (a, y) : zipDef a b [] ys
zipDef a b (x:xs) []     = (x, b) : zipDef a b xs []
zipDef a b (x:xs) (y:ys) = (x, y) : zipDef a b xs ys

takeWhile, dropWhile :: (a -> Bool) -> [a] -> [a]
takeWhile _ [] = []
takeWhile p (x:xs)
  | p x       = x : takeWhile p xs
  | otherwise = []

dropWhile _ [] = []
dropWhile p (x:xs)
  | p x       = dropWhile p xs
  | otherwise = x : xs

--

-- (((a + 1) + 2) + 3) + 4
foldl :: (a -> b -> a) -> a -> [b] -> a
foldl _ acc []     = acc
foldl f acc (x:xs) = foldl f (f acc x) xs

-- 1 + (2 + (3 + (4 + a)))
foldr :: (b -> acc -> acc) -> acc -> [b] -> acc
foldr _ acc []     = acc
foldr f acc (x:xs) = f x (foldr f acc xs)

-- sum     = foldl (+)             0
-- product = foldl (*)             1
-- length  = foldl (\a _ -> a + 1) 0

-- foldr (:) [] [1,2,3]
-- 1 : foldr (:) [] [2, 3]
-- 1 : 2 : foldr (:) [] [3]
-- 1 : 2 : 3 : []

fibs :: [Int]
fibs = 1 : 1 : zipWith (+) fibs (tail fibs)

(.) :: (b -> c) -> (a -> b) -> (a -> c)
f . g = \x -> f (g x)

pipe :: (b -> c) -> (a -> b) -> [a] -> [c]
pipe f g = map f . map g

{-
   #
   #...#
   #...#
 #.#...#
 #.#...##
 ####.###
 ########
#########
-}

r = [1, 5, 3, 8, 3, 2, 3, 7, 4]

collectWater :: [Int] -> Int
collectWater r = x
  where
    x = foldl (+) 0 ws
    ws = zipWith (-) bs r
    bs = zipWith min bl br
    bl = tail $ scanl max 0 r
    br = init $ scanr max 0 r

(++) :: [a] -> [a] -> [a]
(x:xs) ++ ys = x : (xs ++ ys)
[]     ++ ys =            ys

window :: Int -> (a -> b -> a) -> a -> [a] -> b -> [a]
window s f acc []   x = [acc]
window s f _ (y:ys) x = take s $ f y x : y : ys

main = do
  print $ scanl (window 3 (const id) 0) [] [1..10]
  where
    f xs = zipWith3 (,,) xs (tail xs) (tail (tail xs))
