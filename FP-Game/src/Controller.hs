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
  | pause gstate = return gstate                  --if paused, return the state unchanged
  | Down `elem` keyState gstate = return $ gstate {player = movement (player gstate), enemyList = monsterMovement}
  | otherwise = return $ gstate { elapsedTime = elapsedTime gstate + secs, enemyList = monsterMovement}
  where
    monsterMovement = map movement (enemyList gstate)   --takes care of AI movement (if applicable)

-- | Handle user input
input :: Event -> GameState -> IO GameState
input e gstate = return (inputKey e gstate)

-- Takes action according to eventkey, char and state of key
inputKey :: Event -> GameState -> GameState
inputKey (EventKey (Char c) state _ _) gstate
   = case state of
      Down -> case c of         --If key down, something gets done
                 'w' -> gstate {player = xs {playerSpeed = changeSpeed 0 5 (playerSpeed xs)}, keyState = setKeyState getStates 0 Down}
                 'a' -> gstate {player = xs {playerSpeed = changeSpeed (-5) 0 (playerSpeed xs)}, keyState = setKeyState getStates 1 Down}
                 's' -> gstate {player = xs {playerSpeed = changeSpeed 0 (-5) (playerSpeed xs)}, keyState = setKeyState getStates 2 Down}
                 'd' -> gstate {player = xs {playerSpeed = changeSpeed 5 0 (playerSpeed xs)}, keyState = setKeyState getStates 3 Down}
                 'p' -> gstate {pause = not (pause gstate)}
                 _   -> gstate
      _ -> case c of            --if key up, change back to original state
                 'w' -> gstate {player = xs {playerSpeed = set0 2 (playerSpeed xs)}, keyState = setKeyState getStates 0 Up}
                 'a' -> gstate {player = xs {playerSpeed = set0 1 (playerSpeed xs)}, keyState = setKeyState getStates 1 Up}
                 's' -> gstate {player = xs {playerSpeed = set0 2 (playerSpeed xs)}, keyState = setKeyState getStates 2 Up}
                 'd' -> gstate {player = xs {playerSpeed = set0 1 (playerSpeed xs)}, keyState = setKeyState getStates 3 Up}
                 _   -> gstate
      where
        xs = player gstate
        getStates = keyState gstate
        changeSpeed :: Float -> Float -> (Float,Float) -> (Float,Float) --Used to changespeed on x or y axis
        changeSpeed 0 a (b,_) = (b,a)
        changeSpeed a 0 (_,b) = (a,b)
        set0 :: Float -> (Float,Float) -> (Float,Float) --reset x or y axis to 0
        set0 1 (_,b) = (0,b)
        set0 2 (b,_) = (b,0)
        setKeyState :: [KeyState] -> Int -> KeyState -> [KeyState] --Give the gamestate the keystate, which will be used to update the state
        setKeyState [] _ _ = []
        setKeyState (x:xs) a newState | a == 0 = newState : xs
                                      | otherwise = x : setKeyState xs (a-1) newState
inputKey _ gstate = gstate

--Pauses the gamestates
pauseState :: GameState -> GameState
pauseState gstate@GameState{pause = p} = gstate {pause = not p}
