module ADT where

-- data Bool = True | False -- 

data RGB = RGB Int Int Int -- это констуктор значений. Первое слово RGB -> это имя констуктора
red = RGB 255 0 0 

-- Optional!
-- data Maybee a = Just a | Nothing

data T -- Вполне себе валидный тип

-- Функция принимающая что угодно и возвращающая что угодно, но строго в рамках своей области определения

frst :: (a,b) -> a
frst (x, _) = x 


fromMaybe :: Maybe a -> a -> a
fromMaybe (Just x) _ = x 
fromMaybe Nothing y = y


f :: a -> Int
f a = 5
-- f :: a -> b -- Так нельзя, так как функции чистые. А b -> не определено

data List a = Nil | Cons a (List a)
data NonEmpty a = NonEmpty a (List a)
data Tree a = Leaf | Node a (Tree a) (Tree a)
data Tree' a = Leaf' a | Node' (Tree' a) (Tree' a)

-- len :: List a -> Int
-- len Nil = 0
-- elen (Cons _ tail) = 1 + len tail -- _ -> это мы отбрасываем голову списка 


-- Type классы. Хочется диспечиризацию по значению, но так же и по типу 

class Sizeable t where
    len :: t -> Int

instance Sizeable () where
    len () = 0
instance Sizeable (List a) where
    len Nil = 0
    len (Cons _ tail) = 1 + len tail
foo :: Sizeable t => t -> Int
foo x = 2 * len x 

-- Можно автоматически определять имплементации для некоторых тайп классов
data Fruit = Apple | Orange | Lemon
    deriving (Ord, Eq)

-- А вот так можно определить JSON
data Json 
    = Null
    | Bool Bool
    | Number Float
    | String String
    | List [Json]
    | Object [(String, Json)]

main = frst (5, 3) 
