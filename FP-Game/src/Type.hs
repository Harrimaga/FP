module Type where

import Graphics.Gloss

class Move a where
    movement :: a -> a

class Draw a where
    draw :: a -> Picture

class Collision a where
    collides :: a -> b -> Bool