module Lib
    ( parseInt 
    , parsePoint
    ) where

import Text.Read (readMaybe)
import qualified Data.List.Split as S

{-
  Shape parser 

  Input: 
    "T:2,3:5,8:9,12" -> Triangle with 3 vertices
  Output:
    Maybe Shape Triangle Point Point Point

  Input:
    "S:1,1:5,5" -> Square with 2 diagonal vertices
  Output:
    Maybe Shape Square Point Point
-}


data Point = Point Int Int 
  deriving (Show)

parseInt :: String -> Maybe Int
parseInt = readMaybe 

{- applicative and containers fmap -} 

parsePoint :: String -> Maybe Point
parsePoint s = case S.splitOn "," s of
  [x, y] -> Point <$> 
            parseInt x <*> 
            parseInt y
  _ -> Nothing

data TriangleT = TriangleC Point Point Point
  deriving (Show)
data SquareT = SquareC Point Point
  deriving (Show)

data ShapeT
  = ShapeTC TriangleT
  | ShapeSC SquareT
  deriving (Show)

parseTriangle s = case tokens of
  [x, y, z] -> TriangleC <$> 
               parsePoint x <*> 
               parsePoint y <*> 
               parsePoint z 
  _ -> Nothing
  where 
    tokens = S.splitOn ":" s

parseSquare s = case tokens of
  [x, y] -> SquareC <$>
            parsePoint x <*>
            parsePoint y
  _ -> Nothing
  where 
    tokens = S.splitOn ":" s

parseShape s = case tokens of
  ["T": xs] -> parseTriangle xs
  ["S": xs] -> parseSquare xs
  _ -> Nothing
  where 
    tokens = S.splitOn ":" s

{-

  [x,y] -> case (parseInt x) of
    Just x' -> case (parseInt y) of
      Just y' -> Just (Point x' y')
      _ -> nothing
    _ -> Nothing
  _ -> Nothing

-} 

