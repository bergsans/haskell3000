module GameLoop
  ( gameLoop
  , formatLevel
  , createLevel
  ) where

import           Constants
import           Graphics.Gloss
import           Graphics.Gloss.Interface.Pure.Game
import           InitGame
import           Text

data GameData =
  GameData
    { ls     ∷ [Tile]
    , assets ∷ [Picture]
    }
  deriving (Show)

drawTile ∷ Tile → Picture → Picture
drawTile ((x, y), t) = uncurry translate (fromIntegral x, fromIntegral y)

render ∷ GameData → Picture
render gs =
  pictures $
  [drawTile cell (assets gs !! read (snd cell)) | cell ← ls gs] ++
  printText (0, 0) "Hello"

handleKeys ∷ Event → GameData → GameData
handleKeys ev gs = gs

update ∷ Float → GameData → GameData
update _ gs = gs

gameLoop ∷ IO ()
gameLoop = do
  pics ← traverse loadBMP bmps
  rawLevel ← readFile $ head levelData
  let level = createLevel rawLevel
  let state = GameData {ls = level, assets = pics}
  play window background fps state render handleKeys update
