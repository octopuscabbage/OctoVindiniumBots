module DistanceCalcs_test where

import Util.DistanceCalcs
import Util.Types

import Test.Hspec

distancecalcs_test = do
		nearestNeighbor
		walkable_distance

nearestNeighbor = describe "nearest neighbor finding" $ do
		it "works in this event" $ findNearestNeighbor (Point 0 0) [(Point 100 100),(Point 50 50),(Point 1 1)] `shouldBe` (Point 1 1)
		it "works in thie other event" $ findNearestNeighbor (Point 1 1) [(Point 3 2),(Point 1 3)] `shouldBe` (Point 1 3)

walkable_distance = describe "walkable distance" $ do
		it "should be 0" $ walkableDistance (Point 0 0) (Point 0 0) `shouldBe` 0
		it "should be 1" $ walkableDistance (Point 0 0) (Point 0 1) `shouldBe` 1
		it "should be 2" $ walkableDistance (Point 0 0) (Point 1 1) `shouldBe` 2
		it "should be 4" $ walkableDistance (Point 0 0) (Point 2 2) `shouldBe` 4
		it "should be 4" $ walkableDistance (Point 0 0) (Point 1 3) `shouldBe` 4
