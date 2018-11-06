-- | This module contains the data types
--   which represent the state of the game
module Model where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Type

data GameState = GameState {
                   player :: Player,
                   elapsedTime :: Float,
                   keyState :: [KeyState],
                   pause :: Bool,
                   enemyList :: [Enemy],
                   tiles :: [Tile]
                 }

--The initial play state
playState :: GameState
playState = GameState getPlayer 0 [Up,Up,Up,Up] False [getEnemy] [getTile]

--Give the player
getPlayer :: Player
getPlayer = Player 1 (0,0) 1 (Point 0 0)

--The width of player object
playerWidth :: Float
playerWidth = 20
--The height of player object
playerHeight :: Float
playerHeight = 40

--Give a dummy enemy back
getEnemy :: Enemy
getEnemy = Enemy 1 (1,0) 2 (Point (-100) 100) 20 20

--Give a dummy tile to test collision
getTile :: Tile
getTile = Tile 'a' (Point 200 0) 20 400

--Keep track of position
data Pos = Point Float Float

--Is the player of the game
data Player = Player {
                    playerHealth :: Int,
                    playerSpeed :: (Float,Float),
                    playerDamage :: Int,
                    playerLocation :: Pos
}

instance Move Player where
  movement a = newLocation a
    where newLocation :: Player -> Player
          newLocation Player {playerLocation = (Point c d), playerSpeed = (x,y)} = a {playerLocation = Point (c + x) (d + y)}

instance Draw Player where
  draw Player {playerLocation = (Point x y)} = translate x y (color red (rectangleSolid playerWidth playerHeight))

{-instance Collision Player where
  collides Player {playerLocation = (Point xp yp)} Enemy {enemyLocation = (Point xe ye)}
   = undefined-}

--Enemy attributes in the game
data Enemy = Enemy {
                  enemyHealth :: Int,
                  enemySpeed :: (Float,Float),
                  enemyDamage :: Int,
                  enemyLocation :: Pos,
                  enemyWidth :: Float,
                  enemyHeight :: Float
}

instance Draw Enemy where
  draw Enemy {enemyLocation = (Point x y), enemyWidth = w, enemyHeight = h} = translate x y (color black (rectangleSolid w h))

instance Move Enemy where
  movement a = newLocation a
    where newLocation :: Enemy -> Enemy
          newLocation Enemy {enemyLocation = (Point x y), enemySpeed = (x1,y1)} = a {enemyLocation = Point (x + x1) (y + y1)}

--Tile attributes
data Tile = Tile {
                tileObject :: Char,
                tileLocation :: Pos,
                tileWidth :: Float,
                tileHeight :: Float
}

instance Draw Tile where
  draw Tile {tileLocation = (Point x y), tileWidth = w, tileHeight = h} = translate x y (color green (rectangleSolid w h))

type Leveldata = [[Tile]]