module Bot
        ( bot
        )
    where
import Vindinium


import Data.Maybe (fromJust)
import Control.Monad (liftM,mapM_)
import Control.Monad.IO.Class (liftIO)

import Util.Utils
import Util.Pathfinding 
import qualified HeatMap.PIFM as H

import Graphics.Gloss
import Graphics.Gloss.Data.Picture

import System.Console.ANSI
import Control.Concurrent

bot :: Bot
bot = heatMapTestBot

thotBot :: (State-> Dir) -> State -> Vindinium Dir
thotBot f currentState = do
	liftIO $ print $ "Current Position: " ++ show currentPosition 
	printState currentState
	return (f currentState)
	where 	currentBoard = getBoard currentState
		currentPosition = getCurrentPosition currentState

printState state  = do
	liftIO $ clearScreen
	liftIO $ setCursorPosition 0 0
	liftIO $ print $ "Current ID: " ++ (show $ ['a'..] !! (heroid-1))
	liftIO $ print $ "Current Position: " ++ show currentPosition
	liftIO $ putStr $ prettyPrintBoard state
	liftIO $ putStr "\n"
	where 	currentPosition = getCurrentPosition state
		heroid = myId state

heatmapBot = thotBot $ (\s -> H.getNextMove (getCurrentPosition s) s)

twoStepBot = thotBot $ (\s -> H.calc2Moves (getCurrentPosition s) s)

fiveStepBot = thotBot $ (\s -> H.calc5Moves (getCurrentPosition s) s)

heatMapTestBot:: Bot 
heatMapTestBot state = do
	printState state
	showHeatMap pos heatmap size 2
	return (H.calc5MovesFromHeatmap pos state heatmap)
	where 	heatmap = H.generateHeatMap state
		pos = getCurrentPosition state
		size = getBoardSize state
{--
showHeatMap:: [PointValue] -> IO ()
showHeatMap heatmap = display (InWindow "" (500,500) (0,0)) (makeColor 0 0 0 0) $ Pictures (map (\(PointValue (x,y) c) -> Color (makeColor (fromIntegral ((256) * (c `div`  mapMax))::Float) 0 0  256) (Polygon[toFloat (x*10,y*10), toFloat ((x*10)+10,(y*10)),toFloat (x*10,y*10+10),toFloat (x*10+10,y*10+10)])) heatmap)
	where	toFloat (x,y)=(fromIntegral x,fromIntegral y)
		mapMax = if maxCost == 0 then 1 else maxCost 
			where maxCost = getCost $ maximum heatmap
--}
showHeatMap (px,py)  heatmap cursorXOffset cursorYOffset= liftIO $ do
	mapM_ (\(PointValue (x,y) c) -> 
		do	setCursorPosition (y+cursorYOffset) (x+cursorXOffset)
			(putChar $ head $ show $ scaleByMax c)) heatmap 
	setCursorPosition (py+cursorYOffset) (px+cursorXOffset)
	putStrLn "P"
	where scaleByMax c = if  c > 0 then head $ show $ (9 *(c `div` mapMax)) else '-'
		where mapMax = if maxCost == 0 then 1 else maxCost 
			where maxCost = getCost $ maximum heatmap

{--		
heatmapDiagnosticBot:: Bot
heatmapDiagnosticBot state = do
	printState state
	showHeatMap (getBoardSize state + 4) heatMap
	return (H.findNextMoveFromHeatMap position state heatMap)
	where 	heatMap = H.generateHeatMap state
		position = getCurrentPosition state
--}

