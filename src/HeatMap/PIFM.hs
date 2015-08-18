module HeatMap.PIFM where 
import Control.Monad.Reader
import Vindinium.Types
import Util.Utils

type HeatMap = [Int]

emptyHeatMap:: Reader State HeatMap
emptyHeatMap = getBoardSize >>= \size -> map (map (const 0) [0..size]) [0..size]

createHeatMap:: MapValues -> Reader State HeatMap
createHeatMap mapValues = do tiles <- getTiles
                          heatableTiles <- filterTiles (\t -> t /= FreeTile && t /= )
                          compute 

-- | Heats all the tiles in a circle around using falloff, so a tile 1 distance away will heat heatValue - 1
heatPoint:: Int -> Point -> HeatMap -> Reader State HeatMap
heatPoint heatValue (Point x y) heatMap = allPoints >>= \points -> foldl1' (cur) heatmap
                       
