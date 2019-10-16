module Main where

data N = N1 | N2 | N3 | N4 deriving (Show)

data Direction = U | D | L | R

main :: IO ()
main = print
  $ Just (N1, N1)
    >>= move R
    >>= move U
    >>= move U
    >>= move R
    >>= move D
    >>= move L
-- можно думать, что (>>=) здесь имеет тип
-- (>>=) :: Maybe a -> (a -> Maybe b) -> Maybe b

move :: Direction -> (N, N) -> Maybe (N, N)
move d (x, y) = case d of
  U -> Just x  <+> succN y
  D -> Just x  <+> predN y
  R -> succN x <+> Just y
  L -> predN x <+> Just y
  where
    Just a <+> Just b = Just (a, b)
    _      <+> _      = Nothing

predN, succN :: N -> Maybe N

predN N1 = Nothing
predN N2 = Just N1
predN N3 = Just N2
predN N4 = Just N3

succN N1 = Just N2
succN N2 = Just N3
succN N3 = Just N4
succN N4 = Nothing
