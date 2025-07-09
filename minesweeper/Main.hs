-- | Main entry point for the Minesweeper game logic tests.
--   Run this file to execute all test cases including both pure logic tests
--   and IO-based random state generation tests.

module Main where

import qualified GameLogicTests
import TestUtils (createRandomTestState, printTrueBoard)

-- | Main entry point to run tests and show demo boards
main :: IO ()
main = do
  putStrLn "=========================================="
  putStrLn "  MINESWEEPER TEST SUITE"
  putStrLn "=========================================="
  
  -- Show sample random board first
  putStrLn "\n=== Sample Random Board Generation ==="
  putStrLn "Creating a 9x9 board with 25 mines, safe zone at (4,4):"
  gs <- createRandomTestState 9 9 25 (4,4)
  printTrueBoard gs
  putStrLn ""
  putStrLn "Legend: * = Mine, Numbers = Hints, . = Empty"
  
  -- Run all pure tests
  putStrLn "\n=========================================="
  GameLogicTests.runTests
  
  -- Run all IO-based tests  
  putStrLn "\n=========================================="
  GameLogicTests.runIOTests
  
  -- Summary
  putStrLn "\n=========================================="
  putStrLn "Test suite completed!"
  putStrLn ""
  putStrLn "What was tested:"
  putStrLn "- Core game logic (reveal, flag, win/lose conditions)"
  putStrLn "- Flood fill algorithm for empty areas"
  putStrLn "- Random board generation with safe zones"
  putStrLn "- Edge cases and boundary conditions"
  putStrLn "- Mine placement validation"
  putStrLn ""
  putStrLn "If all tests passed, the minesweeper logic is working correctly!"