module Rope where

import Data.Bifunctor

data Rope a b = End | Chunk a (Rope b a) deriving (Show)

instance Functor (Rope a) where
  fmap _ End = End
  fmap _ (Chunk x End) = Chunk x End
  fmap f (Chunk x (Chunk y r)) = Chunk x (Chunk (f y) (fmap f r))

fmapOther :: (a -> b) -> Rope a c -> Rope b c
fmapOther f r =
  let Chunk _ r' = fmap f (Chunk (error "foo") r)
  in r'

instance Bifunctor Rope where
  -- first  :: (a -> b) ->             Rope a c -> Rope b c
  -- second ::             (c -> d) -> Rope a c -> Rope a d
  -- bimap  :: (a -> b) -> (c -> d) -> Rope a c -> Rope b d

  bimap _ _ End = End
  bimap f g (Chunk x r) = Chunk (f x) (bimap g f r)
