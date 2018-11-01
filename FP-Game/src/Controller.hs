-- | This module defines how the state changes
--   in response to time and user input
module Controller where

import Model

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random
import Type

-- | Handle one iteration of the game
step :: Float -> GameState -> IO GameState
step secs gstate
  | Down `elem` keyState gstate = return gstate {player = movement (player gstate)}
  -- = -- Just update the elapsed time
  | otherwise = return $ gstate { elapsedTime = elapsedTime gstate + secs }

-- | Handle user input
input :: Event -> GameState -> IO GameState
input e gstate = return (inputKey e gstate)

inputKey :: Event -> GameState -> GameState
inputKey (EventKey (Char c) state _ _) gstate
   = case state of
      Down -> case c of
                 'w' -> gstate {player = movement (xs {playerSpeed = changeSpeed 0 1 (playerSpeed xs)}), keyState = setKeyState getStates 0 Down}
                 'a' -> gstate {player = movement (xs {playerSpeed = changeSpeed (-1) 0 (playerSpeed xs)}), keyState = setKeyState getStates 1 Down}
                 's' -> gstate {player = movement (xs {playerSpeed = changeSpeed 0 (-1) (playerSpeed xs)}), keyState = setKeyState getStates 2 Down}
                 'd' -> gstate {player = movement (xs {playerSpeed = changeSpeed 1 0 (playerSpeed xs)}), keyState = setKeyState getStates 3 Down}
                 _   -> gstate
      _ -> case c of
                 'w' -> gstate {player = movement (xs {playerSpeed = set0 2 (playerSpeed xs)}), keyState = setKeyState getStates 0 Up}
                 'a' -> gstate {player = movement (xs {playerSpeed = set0 1 (playerSpeed xs)}), keyState = setKeyState getStates 1 Up}
                 's' -> gstate {player = movement (xs {playerSpeed = set0 2 (playerSpeed xs)}), keyState = setKeyState getStates 2 Up}
                 'd' -> gstate {player = movement (xs {playerSpeed = set0 1 (playerSpeed xs)}), keyState = setKeyState getStates 3 Up}
                 _   -> gstate
      where
        xs = player gstate
        getStates = keyState gstate
        changeSpeed :: Float -> Float -> Pos -> Pos
        changeSpeed 0 a (Point b _) = Point b a
        changeSpeed a 0 (Point _ b) = Point a b
        set0 :: Float -> Pos -> Pos
        set0 1 (Point _ b) = Point 0 b
        set0 2 (Point b _) = Point b 0
        setKeyState :: [KeyState] -> Int -> KeyState -> [KeyState]
        setKeyState [] _ _ = []
        setKeyState (x:xs) a newState | a == 0 = newState : xs
                                      | otherwise = x : setKeyState xs (a-1) newState
inputKey2 _ gstate = gstate