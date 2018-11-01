-- | This module defines how to turn
--   the game state into a picture
module View where

import Graphics.Gloss
import Model
import Type


view :: GameState -> IO Picture
view = return . viewPure

viewPure :: GameState -> Picture
viewPure GameState {player = p} = pictures xs -- (map viewPicture listOfPictures ++ xs)
  where xs = [draw p]

{-viewPure gstate = case infoToShow gstate of
  [ShowNothing]   -> blank
  [ShowANumber n] -> color green (text (show n))
  [ShowAChar c] -> pictures [color blue (text "A"), translate 20 20 (color green (circle 10))]  --color green (text [c])
  [ShowObject a] -> color red (rectangleSolid 20 20)
  --[ShowObject a {b = (Location c d)] -> translate  (color red (rectangleSolid 20 20))


viewPicture :: InfoToShow -> Picture
viewPicture info = case info of
    ShowNothing -> blank
    ShowANumber n -> color green (text (show n))
    ShowAChar c -> pictures [color blue (text "A"), translate 20 20 (color green (circle 10))]
    ShowObject Player {playerLocation = (Point a b)} -> translate a b (color red (rectangleSolid 20 20)) -}