module InitGame
  ( bmps
  , levelData
  , fps
  , window
  , formatLevel
  , createLevel
  ) where

import           Constants
import           Data.List.Split
import           Graphics.Gloss
import           System.IO

getNumber ∷ Int → String
getNumber x
  | x < 10 = "00" ++ show x
  | x > 9 && x < 100 = "0" ++ show x
  | otherwise = show x

bmps ∷ [String]
bmps = ["assets/tiles/tile" ++ getNumber x ++ ".bmp" | x ← [0 .. 127]]

levelData ∷ [String]
levelData = ["data/1.level"]

window ∷ Display
window = InWindow "Haskell 3000" (1024, 768) (0, 0)

fps ∷ Int
fps = 60

formatLevel ∷ String → [[[Char]]]
formatLevel rawLevel = map (splitOn ",") $ lines rawLevel

makeRow ∷ [a] → Int → [(Position, a)]
makeRow row y =
  [ (((x * tileSize) - (516 - 24), (y * tileSize) - (384 - 24)), row !! x)
  | x ← [0 .. length row - 1]
  ]

buildLevel ∷ [[String]] → [Tile]
buildLevel rawData =
  concat [makeRow (rawData !! y) y | y ← [0 .. length rawData - 1]]

createLevel ∷ String → [Tile]
createLevel rawData = buildLevel . reverse $ formatLevel rawData
