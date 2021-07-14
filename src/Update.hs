module Update (update) where

import Constants
import qualified Data.Map                           as Map

isKeyLeft ∷ GameState → Bool
isKeyLeft gs = Map.lookup "MoveLeft" (keyEvents gs) == Just True

isKeyRight ∷ GameState → Bool
isKeyRight gs = Map.lookup "MoveRight" (keyEvents gs) == Just True

isKeyJump ∷ GameState → Bool
isKeyJump gs = Map.lookup "Jump" (keyEvents gs) == Just True


isHit ∷ Position → Position → Bool
isHit (b1x, b1y) (b2x, b2y) =
  (b1x - 10) < b2x + tileSize &&
  b1x + tileSize > b2x && b1y < b2y + tileSize && b1y + tileSize > b2y

isCollision ∷ [Tile] → Position → [TileType]→ Bool
isCollision level pnt checkTypes =
  any
    (\((x, y), tileType) → any (\checkType → tileType == checkType && isHit pnt (x, y)) checkTypes)
    level

updateX ∷ Position → Float → Position
updateX (x, y) velX = (x + floor velX , y)

updateY ∷ Float → Position → Position
updateY velY (x, y) = (x, y + floor velY)

nextPos ∷ GameState → Position
nextPos gs = updateY (velY gs - gravity) $ updateX (pos gs) (velX gs)

updateMovement ∷ GameState → Position
updateMovement gs =  updateY (velY gs) $ updateX (pos gs) (velX gs)

updateSpriteCount ∷ GameState → Int
updateSpriteCount gs
  | state gs == Idle =
    if spriteCount gs + 1 >= 60
      then 0
      else spriteCount gs + 1
  | state gs == Walk =
    if spriteCount gs + 4 >= 60
      then 0
      else spriteCount gs + 4
  | otherwise = 0

getHeading ∷ GameState → Heading
getHeading gs
  | velX gs /= 0 = if velX gs < 0 then Constants.Left else Constants.Right
  | otherwise = heading gs

getState ∷ GameState → PlayerState
getState gs
  | velY gs > 0 || velY gs < 0 = Jump
  | velX gs /= 0 = Walk
  | otherwise = Idle

updateVelX ∷ GameState → Float
updateVelX gs
  | isKeyRight gs = 2
  | isKeyLeft gs = -2
  | otherwise = 0

updateVelY ∷ GameState → Float
updateVelY gs
  | not $ isCollision (ls gs) (nextPos gs) obstacles = velY gs - gravity
  | isKeyJump gs = velY gs + 12
  | otherwise = 0

update ∷ Float → GameState → GameState
update _ gs = gs {pos = updateMovement gs, spriteCount = updateSpriteCount gs, velX = updateVelX gs, velY = updateVelY gs, state = getState gs }
