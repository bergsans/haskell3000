module GameLoop
  ( gameLoop
  , formatLevel
  , createLevel
  ) where

import           Constants
import           Graphics.Gloss
import           InitGame
import           Render
import           Update
import           HandleKeys

initialState ∷ GameState
initialState =
  GameState
    { ls = []
    , assets =
        Graphics
          {tiles = [],  other = [], astronaut = AstronautGraphics {left = [], right = []}}
    , heading = Constants.Right
    , state = Constants.Idle
    , shots = []
    , isShooting = False
    , velX = 0
    , velY = 0
    , pos = (0, 100)
    , spriteCount = 0
    , keyEvents = keys
    , direction = East
    }

gameLoop ∷ IO ()
gameLoop = do
  rawLevel ← readFile $ head levelData
  let level = createLevel rawLevel
  tileImgs ← traverse loadBMP bmpsTiles
  astronautLeft ← traverse loadBMP bmpsAstronautLeft
  astronautRight ← traverse loadBMP bmpsAstronautRight
  otherImgs ← traverse loadBMP bmpsOther
  let astronautImgs =
        AstronautGraphics {left = astronautLeft, right = astronautRight}
  let graphics = Graphics {tiles = tileImgs, astronaut = astronautImgs, other = otherImgs}
  let state = initialState {ls = level, assets = graphics}
  play window background fps state render handleKeys update
