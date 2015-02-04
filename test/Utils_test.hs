module Utils_test where

import Test.QuickCheck
import Test.Hspec
import Util.Utils
import TestData
import Vindinium

utilts_tests = do
	conversions
	findFunctions
	getFunctions
	smallFunctions
	getStateFunctions
	pointFindingWorks
	filterTilesWorks
	pointsBetweenWorks

conversions:: SpecWith ()
conversions = 
	describe "map conversions" $ do
      			it "starting with a point do we convert back to the same point" $ property $ mapConversionIPQC 5
			it "starting with an index do we get back the same index" $ property $ mapConversionPIQC 5



mapConversionIPQC s ((Positive x),(Positive y)) = (convertMapIndexToPoint s $ convertPointToMapIndex s (x,y)) == (x,y) || x >= s || y >= s

mapConversionPIQC s (Positive i)  = (convertPointToMapIndex s $ convertMapIndexToPoint s i) == i || i >= (s*s)

findFunctions = describe "find locations" $ do
			it "find closest tavern" $ findClosestTavern testState `shouldBe` (1,3)

getFunctions = describe "get all of something" $ do
			it "should find four taverns" $ (length $getAllTaverns testState) `shouldBe` 2
			it "should find 2 free mines " $ (length $ getFreeMines testState) `shouldBe` 2
			it "should find 3 free tiles" $ (length $ getAllTileType FreeTile testState) `shouldBe` 3
			it "should find free taverens in correct places" $ getAllTaverns testState `shouldBe` [(1,3),(3,2)]

smallFunctions = describe "minor functions should work" $ do
			it "should not be free tile" $ isTileType FreeTile testState (0,0) `shouldBe` False
			it "should be a free tile"  $ isTileType FreeTile testState (2,1) `shouldBe` True
			it "should not be walkable" $ isWalkable testState (0,0) `shouldBe` False
			it "should be walkable" $ isWalkable testState (2,1) `shouldBe` True

getStateFunctions = describe "getting state functions" $ do
	it "board size should be 5" $ getBoardSize testState `shouldBe` 5	
	it "current position should be 1, 1" $ getCurrentPosition testState `shouldBe` (1,1)


pointFindingWorks = describe "make sure point finding works" $ do
	it "should be a free tile" $ test (2,1) FreeTile
	it "should be an open mine tile" $ test (3,1) (MineTile Nothing)
	it "shold be the hero" $ test (1,1) (HeroTile (HeroId 1))
	it "should be a tavern" $ test (1,3) TavernTile
	where test point tiletype = (getBoardTiles testState  !! convertPointToMapIndex (getBoardSize testState) point) `shouldBe` tiletype

filterTilesWorks = describe "make sure filter tiles works" $ do
	it "should only be free tiles " $ (all (\p -> tileAt p testState ==FreeTile) $  filterTiles (\t -> t==FreeTile) testState) `shouldBe` True

pointsBetweenWorks = describe "make sure point to point generator works" $ do
	it "should be these tiles" $ pointsBetween (0,0) (0,2) `shouldBe` [(0,1)]
	it "should be these tiles" $ pointsBetween (0,0) (3,3) `shouldBe` [(1,1),(2,2)]
