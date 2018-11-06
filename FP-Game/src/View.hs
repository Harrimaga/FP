-- | This module defines how to turn
--   the game state into a picture
module View where

import Graphics.Gloss
import Model
import Type

--Draws the gamestate to the screen
view :: GameState -> IO Picture
view = return . viewPure

--Takes care of returning all the elements required for the view
viewPure :: GameState -> Picture
viewPure GameState {player = p, pause = pState, enemyList = e, tiles = tileList} 
  | pState = pictures (xs ++ pauseDraw)
  | otherwise = pictures xs
  where xs = map draw tileList ++ map draw e ++ [draw p]
        pauseDraw = [translate (-200) 100 (color black (text "Paused"))]