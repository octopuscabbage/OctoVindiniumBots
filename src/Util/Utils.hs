module Util.Utils where


import Vindinium
import Vindinium.Types
import Data.List
import Util.DistanceCalcs
import Util.Types
import Data.Tuple
import Control.Monad.Reader

getBoard:: Reader State Board
getBoard = gameBoard $ stateGame ask

getBoardTiles:: Reader State [Tile]
getBoardTiles = boardTiles $ getBoard $ ask

getBoardSize:: Reader State Int
getBoardSize = boardSize $ getBoard $ ask

getHeroID:: Reader State HeroId
getHeroID = heroId $ stateHero $ ask

getCurrentPosition:: Reader State Point
getCurrentPosition =  posToPoint $ heroPos $ stateHero ask

getHeroes' :: Reader State [Hero]
getHeroes' =  gameHeroes $ stateGame $ ask

getHero = stateHero

findClosestTavern:: Reader State Point
findClosestTavern = findClosestType TavernTile ask

findClosestFreeMine:: Reader State Point
findClosestFreeMine  = findClosestType (MineTile Nothing) ask

findClosestType:: Tile -> Reader State Point
findClosestType tileType state = findNearestNeighbor heroPos  $  getAllTileType tileType state
	where heroPos = getCurrentPosition state

	

--TileTypes
getAllTaverns:: Reader State [Point]
getAllTaverns = getAllTileType TavernTile

getAllMines:: Reader State [Point]
getAllMines  = filterTiles isMine 
        where   isMine (MineTile _ ) = True
                isMine _ = False

getFreeMines:: Reader State [Point]
getFreeMines = getAllTileType (MineTile Nothing)

getAllHeros :: Reader State [Point]
getAllHeros  = filterTiles isHero 
        where   isHero (HeroTile _) = True
                isHero _ = False

getAllTileType:: Tile -> Reader  State [Point]
getAllTileType tileType state = map (convertMapIndexToPoint size) $ foldl (\ cur (i,curType)-> if curType == tileType then i:cur else cur) [] $ zip [0..] tiles
	where 	tiles = getBoardTiles state
		size = getBoardSize state	

filterTiles:: (Tile-> Bool) -> Reader State [Point]
filterTiles f =do
  boardTiles <- getBoardTiles
  boardSize <- getBoardSize
  return $ map (\(index,_) -> convertMapIndexToPoint (boardSize) index) $ filter (\(_,tiletype) -> f tiletype)  (zip [0..] $ boardTiles)
                    

isWalkable:: Point -> Reader State Bool
isWalkable point  = ask >>= \state -> inBoard state point && (isTileType FreeTile state  point || (isHero $ mapAtPoint point state))

isHero:: Tile -> Bool
isHero (HeroTile (HeroId _ )) = True
isHero _ = False
	

isTileType:: Tile -> Point ->  Reader State  Bool
isTileType tiletype point = do
                            tiles <- getBoardTiles
                            size <- getBoardSize
                            return (tiles !! convertPointToMapIndex size point) == tiletype

--Map Conversion
mapAtPoint:: Point -> Reader State Tile
mapAtPoint p state =do
  tiles <- getBoardTiles
  size <- getBoardSize 
  return $ tiles !! (convertPointToMapIndex size p)



convertPointToMapIndex:: Point -> Reader State Int
convertPointToMapIndex (x,y)  = getBoardSize >>= \size -> return $ y * size + x


convertMapIndexToPoint::Int -> Reader State (Int, Int)
convertMapIndexToPoint i = getBoardSize >>= \size -> return $ fromTuple $ swap $ i `divMod` size

inBoard ::  Point -> Reader State Bool
inBoard (Point x y) = getBoardSize >>= \s -> return $ x >= 0 && x < s && y >= 0 && y < s


posToPoint::Pos -> (Int,Int)
posToPoint pos = (posY pos,posX pos)


--ASCII ART
prettyPrintBoard:: Reader State String
prettyPrintBoard state =  do
  board <- getBoardTiles
  size <- getBoardSize
  return $ concat $ concat $  intersperse ["\n"] $ chunk size $ map show board
	where 	chunk _ [] = []
		chunk n xs = (take n xs):(chunk n(drop n xs))

allPoints:: Reader State [Point]
allPoints  = getBoardSize >>= \size -> concatMap (\n -> [(Point n x) | x <- [1..size] ]) [1..size]

data PointValue = PointValue (Int,Int) Int deriving (Eq, Show)
getPoint (PointValue p _) = p
getCost (PointValue _ c ) = c

instance Ord PointValue where
        (PointValue a _) `compare` (PointValue b _ ) = a `compare` b

tileAt:: Point -> Reader State Tile                                                     
tileAt p =do tiles <- getBoardTiles
             size <- getBoardSize
             return $ tiles !! convertPointToMapIndex size p

canReach:: Point -> Point -> Reader State Bool
canReach a b = all (isTileType FreeTile) $ pointsBetween a b 

pointsBetween:: Point -> Point -> [(Int,Int)]
pointsBetween pointA pointB = pointsBetween' pointA pointB []
	where pointsBetween' a@(Point aX aY) b@(Point bX bY) cur
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
getHeroFromId:: Int -> Reader State Hero
getHeroFromId id  = do
                    heros <- gameHeroes'
                    return $ head $ filter (\hero -> id == (getIdValue $ heroId $ hero)) $ (heros)
	where getIdValue (HeroId x) = x
