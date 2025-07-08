-- | Test suite for GameLogic module.
--   This module contains all test cases for the core game logic,
--   following Test-Driven Development principles.

module GameLogicTests
  ( runTests
  , tests
  ) where

import CoreTypes
import GameLogic (revealCell, toggleFlag) -- Import the game logic functions
import TestUtils
import qualified Data.Map.Strict as Map

-- | A simple test runner.
runTests :: IO ()
runTests = do
  putStrLn "Running GameLogic tests..."
  let results = all snd tests
  if results
    then putStrLn "All tests passed!"
    else putStrLn "SOME TESTS FAILED!"
  mapM_ (\(name, result) -> putStrLn $ name ++ ": " ++ if result then "PASSED" else "FAILED") tests

-- | List of all tests to run.
tests :: [(String, Bool)]
tests =
  -- Existing tests
  [ ("testRevealMine", testRevealMine)
  , ("testRevealHint", testRevealHint)
  , ("testRevealAlreadyRevealed", testRevealAlreadyRevealed)
  , ("testFloodFill", testFloodFill)
  , ("testPlaceFlagOnHidden", testPlaceFlagOnHidden)
  , ("testRemoveFlagFromFlagged", testRemoveFlagFromFlagged)
  , ("testFlagRevealedCell", testFlagRevealedCell)
  -- New tests for win condition
  , ("testGameWon", testGameWon)
  , ("testGameStillPlaying", testGameStillPlaying)
  ]

-- ... (previous tests remain unchanged) ...

testRevealMine :: Bool
testRevealMine = let s = createTestState 3 3 [(1,1)] in gameStatus (revealCell s (1,1)) == Lost
testRevealHint :: Bool
testRevealHint = let s = createTestState 3 3 [(0,0)] in let s' = revealCell s (1,1) in Map.lookup (1,1) (gameBoard s') == Just (Revealed (Hint 1)) && Map.lookup (2,2) (gameBoard s') == Just Hidden && gameStatus s' == Playing
testRevealAlreadyRevealed :: Bool
testRevealAlreadyRevealed = let s = createTestState 3 3 [(0,0)] in let s1 = revealCell s (1,1) in let s2 = revealCell s1 (1,1) in gameBoard s1 == gameBoard s2

-- Test 4: Revealing an empty cell should trigger a "flood fill".
testFloodFill :: Bool
testFloodFill =
  let
    -- Board: 5x5 with one mine at the corner (4,4)
    -- This creates a large empty area to test the flood fill.
    initialState = createTestState 5 5 [(4,4)]
    -- ACTION: Reveal the empty cell at (0,0)
    finalState = revealCell initialState (0,0)
    finalBoard = gameBoard finalState

    -- ASSERTIONS:
    -- 1. The revealed empty cell (0,0) should be revealed.
    isOriginRevealed = Map.lookup (0,0) finalBoard == Just (Revealed Empty)
    -- 2. A distant empty cell (2,2) should also be revealed by the flood fill.
    isDistantEmptyRevealed = Map.lookup (2,2) finalBoard == Just (Revealed Empty)
    -- 3. The hint cell (3,3), which borders the empty area, should be revealed.
    isBorderHintRevealed = Map.lookup (3,3) finalBoard == Just (Revealed (Hint 1))
    -- 4. [FIXED] Cell (4,3), also on the border, should also be revealed as a hint.
    isOtherBorderHintRevealed = Map.lookup (4,3) finalBoard == Just (Revealed (Hint 1))
  in
    isOriginRevealed && isDistantEmptyRevealed && isBorderHintRevealed && isOtherBorderHintRevealed

testPlaceFlagOnHidden :: Bool
testPlaceFlagOnHidden = let s = createTestState 3 3 [] in Map.lookup (1,1) (gameBoard (toggleFlag s (1,1))) == Just Flagged
testRemoveFlagFromFlagged :: Bool
testRemoveFlagFromFlagged = let s = createTestState 3 3 [] in let s' = toggleFlag s (1,1) in Map.lookup (1,1) (gameBoard (toggleFlag s' (1,1))) == Just Hidden
testFlagRevealedCell :: Bool
testFlagRevealedCell = let s = createTestState 3 3 [(0,0)] in let s' = revealCell s (1,1) in gameBoard s' == gameBoard (toggleFlag s' (1,1))

-- Test 8: Game status should change to Won when the last non-mine cell is revealed.
testGameWon :: Bool
testGameWon =
  let
    -- A 2x2 board with one mine. Three cells must be revealed to win.
    -- M .
    -- . .
    initialState = createTestState 2 2 [(0,0)]
    -- Reveal all non-mine cells
    state1 = revealCell initialState (0,1)
    state2 = revealCell state1 (1,0)
    finalState = revealCell state2 (1,1)
  in
    gameStatus finalState == Won

-- Test 9: Game status should remain Playing if non-mine cells are still hidden.
testGameStillPlaying :: Bool
testGameStillPlaying =
  let
    -- A 3x3 board with one mine.
    initialState = createTestState 3 3 [(0,0)]
    -- Reveal one cell, but others remain hidden.
    finalState = revealCell initialState (1,1)
  in
    gameStatus finalState == Playing