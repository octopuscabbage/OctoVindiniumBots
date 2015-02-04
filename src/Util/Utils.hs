module Util.Utils where


import Vindinium
import Vindinium.Types
import Data.List
import Util.DistanceCalcs
import Data.Tuple

getBoard:: State -> Board
getBoard = gameBoard . stateGame

getBoardTiles:: State-> [Tile]
getBoardTiles = boardTiles . getBoard

getBoardSize:: State -> Int
getBoardSize = boardSize . getBoard

getHeroID:: State -> HeroId
getHeroID = heroId . stateHero

getCurrentPosition:: State -> (Int,Int)
getCurrentPosition =  posToPoint . heroPos . stateHero

getHero = stateHero

findClosestTavern:: State -> (Int,Int)
findClosestTavern state = findClosestType TavernTile state

findClosestFreeMine:: State -> (Int,Int)
findClosestFreeMine state = findClosestType (MineTile Nothing) state

findClosestType:: Tile -> State -> (Int,Int)
findClosestType tileType state = findNearestNeighbor heroPos  $  getAllTileType tileType state
	where heroPos = getCurrentPosition state

	

--TileTypes
getAllTaverns:: State -> [(Int,Int)]
getAllTaverns = getAllTileType TavernTile

getAllMines:: State -> [(Int,Int)]
getAllMines state = filterTiles isMine state
        where   isMine (MineTile _ ) = True
                isMine _ = False

getFreeMines:: State -> [(Int,Int)]
getFreeMines = getAllTileType (MineTile Nothing)

getAllHeros state = filterTiles isHero state
        where   isHero (HeroTile _) = True
                isHero _ = False

getAllTileType:: Tile -> State -> [(Int,Int)]
getAllTileType tileType state = map (convertMapIndexToPoint size) $ foldl (\ cur (i,curType)-> if curType == tileType then i:cur else cur) [] $ zip [0..] tiles
	where 	tiles = getBoardTiles state
		size = getBoardSize state	

filterTiles:: (Tile-> Bool) -> State -> [(Int,Int)]
filterTiles f state =  map (\(index,_) -> convertMapIndexToPoint (getBoardSize state) index) $ filter (\(_,tiletype) -> f tiletype)  (zip [0..] $ getBoardTiles state)

isWalkable:: State -> (Int, Int) -> Bool
isWalkable state point  = inBoard state point && (isTileType FreeTile state  point || (isHero $ mapAtPoint point state))

isHero:: Tile -> Bool
isHero (HeroTile (HeroId _ )) = True
isHero _ = False
	

isTileType:: Tile -> State -> (Int, Int) -> Bool
isTileType tiletype state point = (tiles !! convertPointToMapIndex size point) == tiletype
	where 	tiles = getBoardTiles state
		size = getBoardSize state



--Map Conversion
mapAtPoint:: (Int,Int) -> State -> Tile
mapAtPoint p state = getBoardTiles state !! (convertPointToMapIndex (getBoardSize state) p)
convertPointToMapIndex::Int -> (Int,Int) -> Int
convertPointToMapIndex size (x,y)  = y * (size) + x


convertMapIndexToPoint::Int -> Int -> (Int, Int)
convertMapIndexToPoint size i = swap $ i `divMod` (size)

inBoard :: State -> (Int, Int) -> Bool
inBoard state (x, y) = x >= 0 && x < s && y >= 0 && y < s
	where s = getBoardSize state


posToPoint::Pos -> (Int,Int)
posToPoint pos = (posY pos,posX pos)


--ASCII ART
prettyPrintBoard:: State -> String
prettyPrintBoard state =  concat $ concat $  intersperse ["\n"] $ chunk size $ map show board
	where 	chunk _ [] = []
		chunk n xs = (take n xs):(chunk n(drop n xs))
		board = getBoardTiles state
		size = getBoardSize state

allPoints:: Int -> [(Int,Int)]
allPoints size = concatMap (\n -> [(n,x) | x <- [1..size] ]) [1..size]

data PointValue = PointValue (Int,Int) Int deriving (Eq, Show)
getPoint (PointValue p _) = p
getCost (PointValue _ c ) = c

instance Ord PointValue where
        (PointValue a _) `compare` (PointValue b _ ) = a `compare` b

tileAt p state = getBoardTiles state !! convertPointToMapIndex size p
        where size = getBoardSize state

myId state = getHeroIDValue $ getHeroID state
	where getHeroIDValue (HeroId id) = id

canReach state a b = all (isTileType FreeTile state) $ pointsBetween a b 

pointsBetween:: (Int,Int) -> (Int,Int) -> [(Int,Int)]
pointsBetween pointA pointB = pointsBetween' pointA pointB []
	where pointsBetween' a@(aX,aY) b@(bX,bY) cur
		| (a == b) = if null cur then [] else init cur 
		| otherwise = pointsBetween' nextPoint b (cur++[nextPoint])
			where nextPoint = calcYDelta $ calcXDelta
				where	calcXDelta
						| (aX > bX) = (aX-1,aY) 
						| (aX < bX) = (aX+1,aY)
						| otherwise = a
					calcYDelta cur@(cX,_)
						| (aY > bY) = (cX,aY-1)
						| (aY < bY) = (cX,aY+1)
						| otherwise = cur
getHeroFromId:: Int -> State -> Hero
getHeroFromId id state = head $ filter (\hero -> id == (getIdValue $ heroId $ hero)) $ (gameHeroes $ stateGame state)
	where getIdValue (HeroId x) = x
