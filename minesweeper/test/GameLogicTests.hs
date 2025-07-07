-- | Test suite for GameLogic module.
--   This module contains all test cases for the core game logic,
--   following Test-Driven Development principles.

module GameLogicTests 
  ( runTests
  , tests
  ) where

import CoreTypes
import GameLogic (revealCell)
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
  [ ("testRevealMine", testRevealMine)
  , ("testRevealHint", testRevealHint)
  , ("testRevealAlreadyRevealed", testRevealAlreadyRevealed)
  , ("testFloodFill", testFloodFill)
  ]

-- Test 1: Revealing a mine should result in a 'Lost' game state.
testRevealMine :: Bool
testRevealMine =
  let
    initialState = createTestState 3 3 [(1,1)] -- Mine at the center
    -- ACTION: Reveal the mine at (1,1)
    finalState = revealCell initialState (1,1)
  in
    gameStatus finalState == Lost

-- Test 2: Revealing a hint cell should reveal only that cell.
testRevealHint :: Bool
testRevealHint =
  let
    initialState = createTestState 3 3 [(0,0)] -- Mine at corner creates hints
    -- ACTION: Reveal the hint at (1,1)
    finalState = revealCell initialState (1,1)
    -- Check that (1,1) is revealed
    isRevealed = Map.lookup (1,1) (gameBoard finalState) == Just (Revealed (Hint 1))
    -- Check that a neighbor, (2,2), is still hidden
    neighborHidden = Map.lookup (2,2) (gameBoard finalState) == Just Hidden
  in
    isRevealed && neighborHidden && gameStatus finalState == Playing

-- Test 3: Revealing an already revealed cell should not change the state.
testRevealAlreadyRevealed :: Bool
testRevealAlreadyRevealed =
  let
    initialState = createTestState 3 3 [(0,0)]
    -- ACTION: Reveal a cell twice
    stateAfterFirstReveal = revealCell initialState (1,1)
    stateAfterSecondReveal = revealCell stateAfterFirstReveal (1,1)
  in
    -- The game board should be identical after the second reveal
    gameBoard stateAfterFirstReveal == gameBoard stateAfterSecondReveal

-- Test 4: Revealing an empty cell should trigger a "flood fill".
testFloodFill :: Bool
testFloodFill =
  let
    -- Board: 5x5 with one mine at the corner (4,4)
    -- This creates a large empty area to test the flood fill.
    -- . . . . .
    -- . . . . .
    -- . . . . .
    -- . . . 1 .
    -- . . . 1 M
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
    -- 4. Cell (4,3) should also be revealed as Hint 1 (it's adjacent to the mine)
    isHintRevealed = Map.lookup (4,3) finalBoard == Just (Revealed (Hint 1))
  in
    isOriginRevealed && isDistantEmptyRevealed && isBorderHintRevealed && isHintRevealed