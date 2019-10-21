module Main where

main :: IO ()
main =
  putStrLn
    $ visualize
    $ addWater
      [ 7, 5, 11, 4, 15, 4, 8, 9, 5, 2
      , 1, 3, 19, 15, 18, 5, 5, 1, 9, 10
      , 5, 2, 1, 3, 14, 4, 6, 2, 19, 11
      , 1, 16, 17, 6, 20, 18, 19, 6, 8, 10]

addWater :: [Int] -> [(Int, Int)]
addWater xs = zip xs $ zipWith (-) ws xs
  where
    ws = zipWith min
      (tail $ scanl max 0 xs)
      (init $ scanr max 0 xs)

visualize :: [(Int, Int)] -> String
visualize xs = unlines $ transpose rows
  where
    height = maximum $ map fst xs
    rows = map toRow xs
    toRow (t, w) =
      reverse $ take height
        $ replicate t '#'
          ++ replicate w '.'
          ++ repeat ' '
    transpose ([]:_) = []
    transpose rs =
      map head rs : transpose (map tail rs)

