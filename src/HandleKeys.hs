module HandleKeys (handleKeys) where

import qualified Data.Map                           as Map
import           Graphics.Gloss.Interface.Pure.Game
import           Constants

handleKeys ∷ Event → GameState → GameState
handleKeys (EventKey (SpecialKey KeyLeft) Down _ _) gs =
  gs
    { keyEvents = Map.insert "MoveLeft" True (keyEvents gs), heading = Constants.Left
    }
handleKeys (EventKey (SpecialKey KeyLeft) Up _ _) gs =
  gs
    { keyEvents = Map.insert "MoveLeft" False (keyEvents gs)
    }
handleKeys (EventKey (SpecialKey KeyRight) Down _ _) gs =
  gs
    { keyEvents = Map.insert "MoveRight" True (keyEvents gs), heading = Constants.Right
    }
handleKeys (EventKey (SpecialKey KeyRight) Up _ _) gs =
  gs
    { keyEvents = Map.insert "MoveRight" False (keyEvents gs)
    }
handleKeys (EventKey (SpecialKey KeySpace) Down _ _) gs =
  gs
    { keyEvents = Map.insert "Jump" True (keyEvents gs)
    }
handleKeys (EventKey (SpecialKey KeySpace) Up _ _) gs =
  gs
    { keyEvents = Map.insert "Jump" False (keyEvents gs)
    }
handleKeys _ gs = gs
