module HeatMapTest where 
import Test.Hspec
import TestData
import HeatMap.PIFM
import Util.Utils

heatmap_tests = heat_point_tests


testHeatMap = blankHeatMap testState


heat_point_tests = describe "make sure point heating works" $ do
		it "test 1 point heats correctly" $ (getCost $ findTile (2,1) $ heatPoint testState (1,1) 3 testHeatMap ) `shouldBe` 2
		where findTile p heatmap = head $ filter (\(PointValue testpoint _) -> testpoint == p) heatmap


