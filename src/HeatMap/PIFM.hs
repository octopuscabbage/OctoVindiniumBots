
module HeatMap.PIFM where 

import Util.Pathfinding
import Util.Utils
import Util.DistanceCalcs
import Vindinium
import Data.List.Utils
import Data.Maybe
import Data.List

import Debug.Trace


traceShowThis a = traceShow a a 

data DecisionTree = Node PointValue [DecisionTree] deriving (Eq,Show)

getChildren (Node _ xs) = xs
instance Ord DecisionTree where
	(Node (PointValue _ c1) xs) `compare` (Node (PointValue _ c2) xs2) = (c1+treeSum(xs)) `compare` (c2+treeSum(xs2))
		where 	treeSum [] = 0
			treeSum xs = foldl (\cur (Node (PointValue _ c2) xs2) -> c1 + (c2 + treeSum xs2)) 0 xs

calc5Moves p state = calcNMoves 5 p state $ generateHeatMap state

calc5MovesFromHeatmap p state heatmap = calcNMoves 5 p state heatmap

calcNMoves:: Int -> (Int,Int) -> State -> [PointValue]-> Dir
calcNMoves n p state heatmap= pointToDir p $ rootPoint $ maximum $ map (dropUnreachableNodes state) $ getChildren $generateDecisionTree heatmap n (Node (PointValue p (getCost $ pointAt p heatmap)) [])
	where rootPoint (Node (PointValue p _) _) = p	

dropUnreachableNodes state tree@(Node _ []) = tree
dropUnreachableNodes state (Node point@(PointValue p _) xs) = if( isWalkable state p) then (Node point (map (dropUnreachableNodes state) xs)) else (Node point (map (dropUnreachableNodes state) $ filter (\(Node (PointValue t _) _) -> t /= p) xs))

generateDecisionTree:: [PointValue] -> Int -> DecisionTree -> DecisionTree
generateDecisionTree _ 0 curTree = curTree
generateDecisionTree heatmap depth curTree = generateDecisionTree heatmap (depth-1) $ (addOneToEach heatmap curTree)


addOneToEach heatmap (Node p []) = Node p (map (\adjacentPoint -> Node adjacentPoint []) $ filterToAdjacentsOnly (getPoint p) heatmap)
addOneToEach heatmap (Node p xs) = Node p (map (addOneToEach heatmap) xs)

pointAt::(Int,Int) -> [PointValue] -> PointValue
pointAt p heatmap = head $ filter (\(PointValue t _) -> t==p ) heatmap

getNextMove p state = findNextMoveFromHeatMap p $ generateHeatMap state

findNextMoveFromHeatMap p heatMap = pointToDir p $ getPoint $ maximum $ filterToAdjacentsOnly p heatMap

mineHeuristic:: State -> Tile -> Int
mineHeuristic state (MineTile (Just (HeroId  id))) = if (id ==  myId state) then -3 else (mineHeuristicBasedOnHealth state + 3) 
mineHeuristic state _  = mineHeuristicBasedOnHealth state 

mineHeuristicBasedOnHealth state = if (health > 20) then 10 else 0
		where health = fromIntegral $ heroLife $ stateHero state

playerHeuristic:: State -> Int -> Int
playerHeuristic state id = if (id == myId state) then 0  else bloodThirst 
	where bloodThirst = if myLife > enemyHeroLife then (3 * enemyMineCount) + lifeDifference  else -3 * lifeDifference
		where 	enemyHero = getHeroFromId id state
			enemyHeroLife = heroLife enemyHero
			enemyMineCount =(fromIntegral $ heroMineCount enemyHero)::Int
			myLife = heroLife $ getHero state
			lifeDifference = (fromIntegral $ abs (myLife - enemyHeroLife) `div` 10)::Int


tavernHeuristic:: Int -> Int
--tavernHeuristic health = round ((2-(logBase 10 (fromIntegral health))::Double) * 10)
tavernHeuristic health = if health >40 then -3 else (100 - health) * 3


filterToAdjacentsOnly:: (Int,Int) -> [PointValue] -> [PointValue]
filterToAdjacentsOnly p heatmap = filter (\(PointValue d _) -> contains [d] $ getAllAdjacent p) heatmap

generateHeatMap:: State -> [PointValue]
generateHeatMap state = computeHeats state $ blankHeatMap state

blankHeatMap:: State -> [PointValue]
blankHeatMap state = map (\p -> PointValue p 0) $ filterTiles (\t -> t /= WoodTile) state

computeHeats:: State -> [PointValue] -> [PointValue]
computeHeats state currentHeat = computePlayerHeat state $  computeMineHeat state  $ computeTavernHeat state currentHeat

heatPoint:: State -> (Int,Int) -> Int -> [PointValue] -> [PointValue]
heatPoint state source value currentHeat = map heatValue currentHeat
	where 	distance = walkableDistance source	
		heatValue pc@(PointValue p c) 
			|(source == p) = PointValue p (c + value*10000)
			|(not $ isTileType FreeTile state p) = (PointValue p c)
			|(distance p) <= (abs value) && canReach state source p = (PointValue p (c + (value - distance p )))
			| otherwise = pc


computePlayerHeat:: State -> [PointValue] -> [PointValue]
computePlayerHeat state currentHeat = applyHeuristicToPoints state (\(HeroTile (HeroId id)) -> playerHeuristic state id) currentHeat $ getAllHeros state

computeTavernHeat:: State -> [PointValue] -> [PointValue]
computeTavernHeat state currentHeat = applyHeuristicToPoints state (\_-> tavernHeuristic health) currentHeat  $ getAllTaverns state
        where   health:: Int 
                health = fromIntegral $ heroLife $ stateHero state

computeMineHeat:: State -> [PointValue] -> [PointValue]
computeMineHeat state currentHeat = applyHeuristicToPoints state (mineHeuristic state) currentHeat $ getAllMines state


applyHeuristicToPoints:: State -> (Tile -> Int) -> [PointValue] -> [(Int,Int)] -> [PointValue]
applyHeuristicToPoints state f currentHeat pointsToHeat= foldl (\cur point -> heatPoint state point (f (tileAt point state)) cur) currentHeat pointsToHeat


calc2Moves:: (Int,Int) -> State -> Dir
calc2Moves p state = if getCost highestPoint == 0 then (\n ->  findNextMove p n state) $ findClosestFreeMine state   else  pointToDir p $ getPoint highestPoint 
	where 	highestPoint = 	fst $ foldl1 maxTwo $ getAllTwoSteps 
		heatmap = generateHeatMap state
		maxTwo a@((PointValue _ ac1),(PointValue _ ac2)) b@((PointValue _ bc1),(PointValue _ bc2)) =  if ac1 + ac2 > bc1 + bc2 then a else b
		getAllTwoSteps =  concatMap (\p -> map (\n -> if tileAt (getPoint p) state /= FreeTile then (p,PointValue (getPoint n) 0) else (p,n)) $ filterToAdjacentsOnly (getPoint p) heatmap) $ adjacents
			where adjacents = filterToAdjacentsOnly p heatmap

