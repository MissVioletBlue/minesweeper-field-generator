-- | Main entry point for the Minesweeper game logic tests.
--   Run this file to execute all test cases.

module Main where

import qualified GameLogicTests

-- | Main entry point to run the tests
main :: IO ()
main = GameLogicTests.runTests