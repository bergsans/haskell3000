module HandleKeys (handleKeys) where

import qualified Data.Map                           as Map
import           Graphics.Gloss.Interface.Pure.Game
import Constants

handleKeys ∷ Event → GameState → GameState
handleKeys (EventKey (SpecialKey KeyLeft) Down _ _) gs =
  gs
    { keyEvents = Map.insert "KeyLeft" True (keyEvents gs)
    }
handleKeys (EventKey (SpecialKey KeyLeft) Up _ _) gs =
  gs
    { keyEvents = Map.insert "KeyLeft" False (keyEvents gs)
    }
handleKeys (EventKey (SpecialKey KeyRight) Down _ _) gs =
  gs
    { keyEvents = Map.insert "KeyRight" True (keyEvents gs)
    }
handleKeys (EventKey (SpecialKey KeyRight) Up _ _) gs =
  gs
    { keyEvents = Map.insert "KeyRight" False (keyEvents gs)
    }
handleKeys _ gs = gs
