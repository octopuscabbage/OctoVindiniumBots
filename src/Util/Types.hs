module Util.Types where
-- Represents a cartesian point
data Point = Point {x :: Int, y :: Int} deriving (Eq, Read, Show)

fromTuple::(Int,Int) -> Point
fromTuple (x,y) = Point x y

toTuple:: Point -> (Int,Int)
toTuple (Point x y) = (x,y)


-- Represents the heuristic values for determining how valuable a map element is
data MapValues = MapValues {
  tavernValue :: Double,
  neutralGoldMine :: Double,
  controlledGoldmine :: Double,
  heroAtMaxHealth :: Double
}


