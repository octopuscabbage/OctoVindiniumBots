module Util.DistanceCalcs where

--Distance Finding Functionns

--Given a position and a set of other positions, find the element
--in the other set which is the least amount of (non-diagnal) spaces away
--and return the distance and the point
findNearestNeighbor :: (Int, Int) -> [(Int, Int)] -> (Int, Int)
findNearestNeighbor pointA points = snd $ foldl compareToMin maxBound points
	where compareToMin min test = if (distanceBetweenPointAndTest) < fst min then (distanceBetweenPointAndTest, test) else min
		where distanceBetweenPointAndTest = walkableDistance pointA test
	

--Given two points, calculate their walkable distance between each other
walkableDistance ::  Num a => (a, a) -> (a, a) -> a
walkableDistance (aX, aY) (bX, bY) = xdist + ydist
	where 	xdist = abs $ aX - bX
		ydist = abs $ aY - bY

