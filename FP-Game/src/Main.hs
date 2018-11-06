module Main where

import Controller
import Model
import View

import Graphics.Gloss.Interface.IO.Game

main :: IO ()
main = playIO (InWindow "Counter" (600, 600) (460, 100)) -- Or FullScreen
              (makeColor 0.6 0.6 1 1)            -- Background color
              30                                 -- Frames per second
              playState                       -- Initial state
              view                               -- View function
              input                              -- Event function
              step                               -- Step function