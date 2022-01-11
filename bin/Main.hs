module Main (main) where

import Calculator
import Control.Monad

main :: IO ()
main = forever $ do
  putStr "> "
  a <- getLine
  print $ eval $ run a
