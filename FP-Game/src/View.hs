-- | This module defines how to turn
--   the game state into a picture
module View where

import Graphics.Gloss
import Model

view :: GameState -> IO Picture
view = return . viewPure

viewPure :: GameState -> Picture
viewPure gstate = case infoToShow gstate of
  ShowNothing   -> blank
  ShowANumber n -> color green (text (show n))
  ShowAChar   c -> pictures [color blue (text "A"), translate 20 20 (color green (circle 10))]  --color green (text [c])
  ShowObject a -> color red (rectangleSolid 20 20)