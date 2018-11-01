module Main where

import Controller
import Model
import View

import Graphics.Gloss.Interface.IO.Game

main :: IO ()
main = playIO (InWindow "Counter" (400, 400) (560, 200)) -- Or FullScreen
              (makeColor 0.6 0.6 1 1)            -- Background color
              10                                 -- Frames per second
              playState                       -- Initial state
              view                               -- View function
              input                              -- Event function
              step                               -- Step function