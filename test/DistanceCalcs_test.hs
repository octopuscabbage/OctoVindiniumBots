module DistanceCalcs_test where

import Util.DistanceCalcs

import Test.Hspec

distancecalcs_test = do
		nearestNeighbor
		walkable_distance

nearestNeighbor = describe "nearest neighbor finding" $ do
		it "works in this event" $ findNearestNeighbor (0,0) [(100,100),(50,50),(1,1)] `shouldBe` (1,1)
		it "works in thie other event" $ findNearestNeighbor (1,1) [(3,2),(1,3)] `shouldBe` (1,3)

walkable_distance = describe "walkable distance" $ do
		it "should be 0" $ walkableDistance (0,0) (0,0) `shouldBe` 0
		it "should be 1" $ walkableDistance (0,0) (0,1) `shouldBe` 1
		it "should be 2" $ walkableDistance (0,0) (1,1) `shouldBe` 2
		it "should be 4" $ walkableDistance (0,0) (2,2) `shouldBe` 4
		it "should be 4" $ walkableDistance (0,0) (1,3) `shouldBe` 4
