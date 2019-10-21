module Main where 

f :: Int -> Int
f x = x + 1

g x y = x * y 

b = \x -> x + 1

(+++) :: Int -> Int -> Int
x +++ y = x + y + x + y

data Foo = A | B

fromFoo :: Foo -> String 
fromFoo A = "A"
fromFoo B = "B"

fromFoo' :: Foo -> String 
fromFoo' x = 
    case x of
      A -> "A"
      B -> "B"

funk :: Int -> Int
funk x = undefined

goodNumber 1 = True
goodNumber 7 = True
goodNumber x 
    | x < 100 = True
    | x >= 100 = False

leng [] = 0
leng (_:xs) = 1 + leng xs 

lastElem [] = error "Oops!"
lastElem (x:[]) = x
lastElem (_:xs) = lastElem xs

ones = 1 : ones

main = do 
    print (g 2 4)
    print (f 5)
    print (b 11)
    print (1 +++ 2)
    print $ f 10 
    print $ goodNumber 22
    print $ goodNumber 100
    print $ leng [1,2,3,4]
    print $ lastElem [3,4,5]
    print $ leng (take 100000 ones)

-- main = funk 5
