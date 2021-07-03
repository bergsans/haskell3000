module GameLoop
  ( gameLoop
  ) where

import           Constants
import qualified Data.Map                           as Map
import           Data.Maybe
import           Graphics.Gloss
import           Graphics.Gloss.Interface.Pure.Game
import           Text

window ∷ Display
window = InWindow "Haskell 3000" (1024, 768) (0, 0)

fps ∷ Int
fps = 60

drawing ∷ [Picture] → Picture
drawing pic =
  pictures ([translate 0 0 (head pic)] ++ [translate 0 0 (pic !! 1)])

gameLoop ∷ IO ()
gameLoop = do
  _background ← loadBMP "assets/background-1.bmp"
  astronaut ← loadBMP "assets/astro_hero_spritesheet.bmp"
  play
    window
    background
    fps
    undefined
    (\_ → drawing [_background, astronaut])
    (\event _ → show event)
    (\_ world → world)
