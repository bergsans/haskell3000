module GameLoop
  ( gameLoop
  ) where

import           Constants
import           Graphics.Gloss
import           InitGame
import           Text

data GameData =
  GameData
    { ls     ∷ [String]
    , assets ∷ [Picture]
    }
  deriving (Show)

drawing ∷ Picture -- [Picture] → Picture
drawing = pictures $ printText (10, 10) "Hello"
  --([translate 0 0 (head pic)] ++ [translate 0 0 (pic !! 1)])

gameLoop ∷ IO ()
gameLoop
  --pics ← traverse loadBMP bmps
  --let gameData = GameData {ls = levelData, assets = pics}
  --print $ show gameData
 = do
  play
    window
    background
    fps
    undefined
    (\_ → drawing) -- $ assets gameData)
    (\event _ → show event)
    (\_ world → world)
