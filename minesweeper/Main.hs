-- | Main entry point for the Minesweeper game logic tests.
--   Run this file to execute all test cases.

module Main where

import qualified GameLogicTests
import TestUtils (createRandomTestState)
import TestUtils (printTrueBoard)

-- | Main entry point to run the tests and demo board
main :: IO ()
main = do
  -- Create and show a 9x9 board with 25 mines and (4,4) as safe field
  putStrLn "=== Sample True Board ==="
  gs <- createRandomTestState 9 9 25 (4,4)
  printTrueBoard gs
  putStrLn "\n=== Running Tests ==="
  GameLogicTests.runTests