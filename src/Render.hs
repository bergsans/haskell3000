module Render (
 render
) where

import           Graphics.Gloss
import qualified Data.Map                           as Map
import           Constants
import           Text

--f k gs =
  --if Map.lookup k (keyEvents gs) == Just True
    --then "TRUE"
    --else "FALSE"
    -- show $ map toUpper k ++

getSpriteCount ∷ Int → Int
getSpriteCount c
  | c < 30 = 0
  | c >= 30 && c < 60 = 1
  | otherwise = 0

getAstronautImg ∷ GameState → Picture
getAstronautImg gs
  | state gs == Idle =
    if heading gs == Constants.Left
      then (left $ astronaut $ getAssets gs) !!
           getSpriteCount (spriteCount gs)
      else (right $ astronaut $ getAssets gs) !!
           getSpriteCount (spriteCount gs)
  | state gs == Walk =
    if heading gs == Constants.Left
      then (left $ astronaut $ getAssets gs) !!
           (4 + getSpriteCount (spriteCount gs))
      else (right $ astronaut $ getAssets gs) !!
           (4 + getSpriteCount (spriteCount gs))
  | otherwise =
    if heading gs == Constants.Left
      then (left $ astronaut $ getAssets gs) !! 1
      else (right $ astronaut $ getAssets gs) !! 1

drawAstronaut ∷ GameState → [Picture]
drawAstronaut gs =
  [uncurry translate (fromIntegral x, fromIntegral y) $ getAstronautImg gs]
  where
    (x, y) = pos gs

drawTile ∷ Tile → Picture → Picture
drawTile ((x, y), t) = uncurry translate (fromIntegral x, fromIntegral y)

getAssets ∷ GameState → Graphics
getAssets = assets

render ∷ GameState → Picture
render gs =
  pictures $
  [drawTile cell (tiles graphics !! read (snd cell)) | cell ← ls gs] ++
  drawAstronaut gs -- ++ printText (0, 0) keys
  where
    graphics = getAssets gs
    -- keys = concatMap (\x → f x gs) ["KeyLeft", "KeyRight"]
