module Util.Pathfinding where
import Vindinium
import Util.Utils
import Util.DistanceCalcs
import Data.List
import Data.List.Utils
import qualified Data.Heap as H
import Data.Maybe


type PathFinderF = (Int,Int) -> (Int,Int) -> State -> Dir

findNextMove:: PathFinderF
findNextMove = slightlyImprovedWikiPathfinder

getAllAdjacent (x,y) = [(x+1,y),(x-1,y),(x,y+1),(x,y-1)]

--Walking from point a to point b what direction do you have to go
pointToDir:: (Int,Int) -> (Int,Int) -> Dir
pointToDir (aX,aY) (bX,bY)
	| (aX < bX) = East
	| (aX > bX) = West
	| (aY > bY) = North
	| (aY < bY) = South
	| otherwise = Stay

walkableAdjacents state p = filter (isWalkable state) $ getAllAdjacent p

containsE xs e = xs `intersect` [e] /= []

slightlyImprovedWikiPathfinder:: PathFinderF
slightlyImprovedWikiPathfinder pointA pointB state 
	| (contains [pointB] $  getAllAdjacent pointA) = pointToDir pointA pointB
	| (length adjacencies == 1) =  pointToDir pointA $ head $ adjacencies
	| otherwise =  wikiPathFinder pointA pointB state
	
	where adjacencies = walkableAdjacents state pointA 

wikiPathFinder::PathFinderF
wikiPathFinder pointA pointB state = if (null points) then Stay else  pointToDir pointA $ getPoint $ minimum $ filter (\ (PointValue p _)-> getAllAdjacent pointA `containsE` p) $ points
	where points = wikiPathFinder' pointA [PointValue pointB 0] [] state 

wikiPathFinder':: (Int,Int) -> [PointValue] -> [PointValue] -> State -> [PointValue]
wikiPathFinder' target queue oldQueue state
 | (oldQueue == queue) = [] 
 | (any (\(PointValue p _) -> p == target) newQueue)=  newQueue 
 | otherwise = wikiPathFinder' target newQueue queue state
	where 	getAdjacensies:: PointValue -> [PointValue]	
		getAdjacensies (PointValue testpoint c) = map (\p -> PointValue p (c+1)) $ getAllAdjacent testpoint
		newQueue:: [PointValue]
		newQueue =  filter (\(PointValue p _) -> isWalkable state p) $ concatMap getAdjacensies queue ++ queue
			where	getMinimumEquivolent:: PointValue -> [PointValue] -> PointValue 
				getMinimumEquivolent p  xs= minimum $ filter (\ (PointValue t _) -> t == getPoint p) xs


