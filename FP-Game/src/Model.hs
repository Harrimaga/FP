-- | This module contains the data types
--   which represent the state of the game
module Model where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Type

data GameState = GameState {
                   player :: Player,
                   elapsedTime :: Float,
                   keyState :: [KeyState]
                 }

playState :: GameState
playState = GameState (Player 1 (Point 0 0) 1 (Point 0 0)) 0 [Up,Up,Up,Up]

data Pos = Point Float Float

data Player = Player {
                    playerHealth :: Int,
                    playerSpeed :: Pos,
                    playerDamage :: Int,
                    playerLocation :: Pos
}

instance Move Player where
  movement a = newLocation a
    where newLocation :: Player -> Player
          newLocation Player {playerLocation = (Point c d), playerSpeed = (Point x y)} = a {playerLocation = Point (c + x) (d + y)}

instance Draw Player where
  draw Player {playerLocation = (Point a b)} = translate (a + 20) (b - 40) (color red (rectangleSolid 20 20))


data Enemy = Enemy {
                  enemyHealth :: Int,
                  enemySpeed :: Float,
                  enemyDamage :: Int,
                  enemyLocation :: Pos
}
data Tile = Tile {
                tileObject :: Char,
                tileLocation :: Pos
}

type Leveldata = [[Tile]]