module Main where

import           Control.Monad
import           Data.List.Split
import           GameLoop
import           System.IO

main ∷ IO ()
main = do
  j ← openFile "your-level.json" ReadMode
  contents ← hGetContents j
  let t = splitOn "," contents
  print contents
  print t
  gameLoop
