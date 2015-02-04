module Pathfinding_test where

import Util.Pathfinding
import Vindinium
import Test.Hspec
import TestData


pathfinding_tests = do
	find_next_move_test
	allAdjacentTest
	point_to_dir_test	

find_next_move_test = describe "figuring out pathfinding" $ do
	it "test that we move east" $ findNextMoveInCurrentState (1,1) (1,2) `shouldBe` South
	it "test that we move south" $ findNextMoveInCurrentState (1,1) (1,3)  `shouldBe` South
	it "test that we move east" $ findNextMoveInCurrentState (1,1) (3,1)  `shouldBe` East
	it "longest test" $ findNextMoveInCurrentState (1,1) (3,2) `shouldBe` West
	it "longest test, step 2" $ findNextMoveInCurrentState (2,1) (3,2) `shouldBe` West
	where findNextMoveInCurrentState a b = findNextMove a b testState
	
point_to_dir_test = describe "point to dir testing" $ do
	it "east" $ pointToDir (0,0) (1,0) `shouldBe` East
	it "west" $ pointToDir (1,0) (0,0) `shouldBe` West
	it "north" $ pointToDir (0,0) (0,1) `shouldBe` South
	it "south" $ pointToDir (0,1) (0,0) `shouldBe` North


allAdjacentTest = describe "making sure that all adjacents works" $ do
	it "general test" $ getAllAdjacent (2,2) `shouldBe` [(3,2),(1,2),(2,3),(2,1)]
	it "test walkable adjacents" $ walkableAdjacents testState (1,1)  `shouldBe` [(2,1),(1,2)]
