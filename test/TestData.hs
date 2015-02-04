module TestData where

import Data.Text
import Vindinium.Types

testBoard::Board
testBoard = Board {boardSize = 5, boardTiles = [WoodTile,WoodTile,WoodTile,WoodTile,WoodTile,WoodTile,HeroTile (HeroId 1), FreeTile, MineTile Nothing, WoodTile,WoodTile, FreeTile, WoodTile, TavernTile, WoodTile,WoodTile,TavernTile,FreeTile,MineTile Nothing, WoodTile,WoodTile,WoodTile,WoodTile,WoodTile,WoodTile]}

testPlayer:: Hero
testPlayer = Hero { 
	  heroId = (HeroId 1)
	, heroName = pack "test1"
	, heroUserId = Just $ pack"lwkjel"
	, heroElo = Just 1200
	, heroPos = (Pos 1 1)
	, heroLife = 100
	, heroGold = 100
	, heroMineCount = 4
	, heroSpawnPos = (Pos 1 1 )
	, heroCrashed = False

}
testHero1::Hero
testHero1 = Hero { 
	  heroId = (HeroId 2)
	, heroName = pack "test2"
	, heroUserId = Just $ pack "lwkjel"
	, heroElo = Just 1200
	, heroPos = (Pos 1 2)
	, heroLife = 100
	, heroGold = 100
	, heroMineCount = 4
	, heroSpawnPos = (Pos 1 2 )
	, heroCrashed = False
}
testGame::Game
testGame = Game {
	 gameId = (GameId $ pack"hewjhka")
	, gameTurn = 15	
	, gameMaxTurns = 20
	, gameHeroes = [testPlayer, testHero1]
	, gameBoard = testBoard
	, gameFinished = False
}
testState::State
testState = State {
	stateGame = testGame
	, stateHero = testPlayer
	, stateToken = pack "123loj123l"
	, stateViewUrl = pack "190238120312"
	, statePlayUrl = pack "19203120312"
}


