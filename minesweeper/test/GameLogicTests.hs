-- | Test suite for GameLogic module.
--   This module contains all test cases for the core game logic,
--   following Test-Driven Development principles.

module GameLogicTests
  ( runTests
  , tests
  ) where

import CoreTypes
import GameLogic (revealCell, toggleFlag) -- Import the new function
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
  -- Existing tests for revealCell
  [ ("testRevealMine", testRevealMine)
  , ("testRevealHint", testRevealHint)
  , ("testRevealAlreadyRevealed", testRevealAlreadyRevealed)
  , ("testFloodFill", testFloodFill)
  -- New tests for toggleFlag
  , ("testPlaceFlagOnHidden", testPlaceFlagOnHidden)
  , ("testRemoveFlagFromFlagged", testRemoveFlagFromFlagged)
  , ("testFlagRevealedCell", testFlagRevealedCell)
  ]

-- Test 1: Revealing a mine should result in a 'Lost' game state.
testRevealMine :: Bool
testRevealMine =
  let
    initialState = createTestState 3 3 [(1,1)] -- Mine at the center
    finalState = revealCell initialState (1,1)
  in
    gameStatus finalState == Lost

-- Test 2: Revealing a hint cell should reveal only that cell.
testRevealHint :: Bool
testRevealHint =
  let
    initialState = createTestState 3 3 [(0,0)] -- Mine at corner creates hints
    finalState = revealCell initialState (1,1)
    isRevealed = Map.lookup (1,1) (gameBoard finalState) == Just (Revealed (Hint 1))
    neighborHidden = Map.lookup (2,2) (gameBoard finalState) == Just Hidden
  in
    isRevealed && neighborHidden && gameStatus finalState == Playing

-- Test 3: Revealing an already revealed cell should not change the state.
testRevealAlreadyRevealed :: Bool
testRevealAlreadyRevealed =
  let
    initialState = createTestState 3 3 [(0,0)]
    stateAfterFirstReveal = revealCell initialState (1,1)
    stateAfterSecondReveal = revealCell stateAfterFirstReveal (1,1)
  in
    gameBoard stateAfterFirstReveal == gameBoard stateAfterSecondReveal

-- Test 4: Revealing an empty cell should trigger a "flood fill".
testFloodFill :: Bool
testFloodFill =
  let
    initialState = createTestState 5 5 [(4,4)]
    finalState = revealCell initialState (0,0)
    finalBoard = gameBoard finalState
    isOriginRevealed = Map.lookup (0,0) finalBoard == Just (Revealed Empty)
    isDistantEmptyRevealed = Map.lookup (2,2) finalBoard == Just (Revealed Empty)
    isBorderHintRevealed = Map.lookup (3,3) finalBoard == Just (Revealed (Hint 1))
    isHintRevealed = Map.lookup (4,3) finalBoard == Just (Revealed (Hint 1))
  in
    isOriginRevealed && isDistantEmptyRevealed && isBorderHintRevealed && isHintRevealed

-- Test 5: Placing a flag on a hidden cell should mark it as Flagged.
testPlaceFlagOnHidden :: Bool
testPlaceFlagOnHidden =
  let
    initialState = createTestState 3 3 []
    -- ACTION: Flag the cell at (1,1)
    finalState = toggleFlag initialState (1,1)
    cellView = Map.lookup (1,1) (gameBoard finalState)
  in
    cellView == Just Flagged

-- Test 6: Removing a flag from a flagged cell should mark it as Hidden.
testRemoveFlagFromFlagged :: Bool
testRemoveFlagFromFlagged =
  let
    initialState = createTestState 3 3 []
    -- ACTION: Flag the cell, then flag it again to remove the flag.
    stateAfterPlacing = toggleFlag initialState (1,1)
    stateAfterRemoving = toggleFlag stateAfterPlacing (1,1)
    cellView = Map.lookup (1,1) (gameBoard stateAfterRemoving)
  in
    cellView == Just Hidden

-- Test 7: Attempting to flag a revealed cell should have no effect.
testFlagRevealedCell :: Bool
testFlagRevealedCell =
  let
    initialState = createTestState 3 3 [(0,0)] -- Mine at (0,0)
    -- Reveal the cell at (1,1), which is a Hint.
    revealedState = revealCell initialState (1,1)
    -- ACTION: Attempt to flag the already revealed cell.
    finalState = toggleFlag revealedState (1,1)
  in
    -- The board state should not have changed.
    gameBoard revealedState == gameBoard finalState