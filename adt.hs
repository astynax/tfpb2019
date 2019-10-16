{-# LANGUAGE DeriveFoldable #-}

module ADT where

import Data.Foldable

data List a = Nil | Cons a (List a)
  deriving (Foldable)

data List' a = List' (List a) | Pair a a

class Sizeable t where
  len :: t -> Int

instance Sizeable () where
  len () = 0

instance Sizeable (List a) where
  len Nil = 0
  len (Cons _ l) = 1 + len l

foo :: Sizeable t => t -> Int
foo x = 2 * len x

instance Sizeable a => Sizeable (Maybe a) where
  len Nothing = 0
  len (Just x) = len x

data FriutOr a = Apple | Orange | Lemon | Or a
  deriving (Ord, Eq, Show, Read)

data JSON
  = Null
  | Bool Bool
  | Number Float
  | String String
  | List [JSON]
  | Object [(String, JSON)]
  deriving (Ord, Eq, Show, Read)
