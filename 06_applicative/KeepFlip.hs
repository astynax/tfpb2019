module KeepFlip where

import Data.Bifunctor

data KeepFlip a b
  = Keep a (KeepFlip a b)
  | Flip b (KeepFlip b a)
  | End
  deriving (Show)

instance Semigroup (KeepFlip a b) where
  End       <> x  = x
  Keep x xs <> ys = Keep x (xs <>      ys)
  Flip z zs <> ys = Flip z (zs <> flop ys)

instance Monoid (KeepFlip a b) where
  mempty = End

flop :: KeepFlip b a -> KeepFlip a b
flop End         = End
flop (Keep x xs) = Flip x xs
flop (Flip x xs) = Keep x xs

instance Functor (KeepFlip a) where
  fmap _ End         = End
  fmap f (Keep x xs) = Keep x     $        fmap f        xs
  fmap f (Flip x xs) = Flip (f x) $ flop $ fmap f $ flop xs

instance Bifunctor KeepFlip where
  bimap _ _ End = End
  bimap f g (Keep x xs) = Keep (f x) $ bimap f g xs
  bimap f g (Flip x xs) = Flip (g x) $ bimap g f xs

fromList :: [Either a b] -> KeepFlip a b
fromList [] = End
fromList (Left  x : xs) = Keep x $        fromList xs
fromList (Right x : xs) = Flip x $ flop $ fromList xs

toList :: KeepFlip a b -> [Either a b]
toList End = []
toList (Keep x xs) = Left  x : toList       xs
toList (Flip x xs) = Right x : toList (flop xs)

main :: IO ()
main =
  print
  $ toList
  $ mappend (Keep 5 $ Flip "Moe" End)
  $ flop
  $ first (++ "!") $ second (* 10)
  $ fromList
    [ Left "Bob"
    , Left "Alice"
    , Right 1
    , Right 2
    , Right 3
    , Left "Sam"
    , Right (4 :: Int)
    ]
