module InitGame
  ( bmps
  , levelData
  , fps
  , window
  ) where

import           Control.Monad
import           Control.Monad.IO.Class (liftIO)
import           Data.List.Split
import           Graphics.Gloss
import           System.IO

bmps ∷ [String]
bmps = ["assets/tiles/orginal/ap1b_tileset_compact.png"]

levelData ∷ [String]
levelData = ["1.level"]

window ∷ Display
window = InWindow "Haskell 3000" (1024, 768) (0, 0)

fps ∷ Int
fps = 60
