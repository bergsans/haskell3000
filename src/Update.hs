module Update (update) where

import Constants
import qualified Data.Map                           as Map

isKeyLeft ∷ GameState → Bool
isKeyLeft gs = Map.lookup "KeyLeft" (keyEvents gs) == Just True

isKeyRight ∷ GameState → Bool
isKeyRight gs = Map.lookup "KeyRight"(keyEvents gs) == Just True

getVelX ∷ GameState → Int
getVelX gs
  | isKeyLeft gs = -2
  | isKeyRight gs = 2
  | otherwise = 0

updateX ∷ GameState → Position
updateX gs =
  if x /= 0
    then ((fst $ pos gs) + x , snd $ pos gs)
    else pos gs
  where x = getVelX gs

updateSpriteCount ∷ GameState → Int
updateSpriteCount gs
  | state gs == Idle =
    if (spriteCount gs) + 1 >= 60
      then 0
      else (spriteCount gs) + 1
  | state gs == Walk =
    if (spriteCount gs) + 4 >= 60
      then 0
      else (spriteCount gs) + 4
  | otherwise = 0

getHeading ∷ GameState → Heading
getHeading gs
  | getVelX gs /= 0 = if getVelX gs < 0 then Constants.Left else Constants.Right
  | otherwise = heading gs

getState ∷ GameState → PlayerState
getState gs
  | getVelX gs /= 0 = Walk
  | otherwise = Idle

update ∷ Float → GameState → GameState
update _ gs = gs {pos = updateX gs, spriteCount = updateSpriteCount gs, heading = getHeading gs, state = getState gs }
