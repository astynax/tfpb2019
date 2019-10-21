module Main where

data N = N1 | N2 | N3 | N4

data Row a = Row a a a a

data Color = Red | Blue deriving (Eq, Show)

data Field = Field Color (Row (Row (Maybe Color)))

main :: IO ()
main = do
  putStrLn $ drawField field
  print $ getWinner field
  where
   field =
     put Red N1 N1
     $ put Red N2 N2
     $ put Red N3 N3
     $ put Red N4 N4
     empty
   put c = modifyField (const $ Just c)

empty :: Field
empty = Field Red (Row r r r r)
  where
    r = Row n n n n
    n = Nothing

modify :: (a -> a) -> N -> Row a -> Row a
modify f N1 (Row x a b c) = Row (f x) a b c
modify f N2 (Row a x b c) = Row a (f x) b c
modify f N3 (Row a b x c) = Row a b (f x) c
modify f N4 (Row a b c x) = Row a b c (f x)

modifyField
  :: (Maybe Color -> Maybe Color) -> N -> N
  -> Field -> Field
modifyField f x y (Field c bla) =
  Field c $ modify (modify f x) y bla

getWinner :: Field -> Maybe Color
getWinner (Field _ r) =
  case firstNonEmpty cellList of
    Just c | not (any (oppositeTo c) cellList) -> Just c
    _                                          -> Nothing
  where
    cellList = concatMap toList $ toList r

    firstNonEmpty []           = Nothing
    firstNonEmpty (Nothing:xs) = firstNonEmpty xs
    firstNonEmpty (Just x :_)  = Just x

    oppositeTo _ Nothing  = False
    oppositeTo c (Just x) = x /= c

drawField :: Field -> String
drawField (Field _ f) =
  unlines $ map fromRow $ toList f
  where
    fromRow :: Row (Maybe Color) -> String
    fromRow r = map fromCell $ toList r

    fromCell :: Maybe Color -> Char
    fromCell (Just Red)  = 'R'
    fromCell (Just Blue) = 'B'
    fromCell _           = '.'

toList :: Row a -> [a]
toList (Row a b c d) = [a, b, c, d]
