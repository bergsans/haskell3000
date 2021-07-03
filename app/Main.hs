module Main where

import           Control.Monad
import           Data.List.Split
import           GameLoop
import           System.IO

main ∷ IO ()
main
  --j ← openFile "1.level" ReadMode
  --contents ← hGetContents j
  --let t = splitOn "," contents
  --print contents
  --print t
 = do
  gameLoop
