-- | This module contains the data types
--   which represent the state of the game
module Model where

data InfoToShow = ShowNothing
                | ShowANumber Int
                | ShowAChar   Char
                | ShowObject Player

nO_SECS_BETWEEN_CYCLES :: Float
nO_SECS_BETWEEN_CYCLES = 5

data GameState = GameState {
                   infoToShow  :: InfoToShow
                 , elapsedTime :: Float
                 }

initialState :: GameState
initialState = GameState ShowNothing 0

playState :: GameState
playState = GameState (ShowObject (Player 1 1 1 (Location 0 0))) 0

data Location = Location { myX :: Float, myY :: Float}

data Player = Player {
                    health :: Int,
                    speed :: Float,
                    damage :: Int,
                    playerLocation :: Location
}

data Tile = Tile {
                tileObject :: InfoToShow,
                tileLocation :: Location
}

type Level = [[Tile]]