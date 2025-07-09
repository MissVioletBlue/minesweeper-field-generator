-- | Test suite for GameLogic module and TestUtils.
--   This module contains all test cases for the core game logic and
--   random test state generation, following Test-Driven Development principles.

module GameLogicTests
  ( runTests
  , tests
  , runIOTests
  ) where

import CoreTypes
import GameLogic (revealCell, toggleFlag)
import TestUtils
import qualified Data.Map.Strict as Map
import Data.List (nub)

-- | A simple test runner for pure tests.
runTests :: IO ()
runTests = do
  putStrLn "Running GameLogic tests..."
  let results = all snd tests
  if results
    then putStrLn "All pure tests passed!"
    else putStrLn "SOME PURE TESTS FAILED!"
  mapM_ (\(name, result) -> putStrLn $ name ++ ": " ++ if result then "PASSED" else "FAILED") tests

-- | Test runner for IO-based tests (random state generation)
runIOTests :: IO ()
runIOTests = do
  putStrLn "\nRunning IO-based tests..."
  test1 <- testCreateRandomTestStateIO
  test2 <- testCreateRandomTestStateRandomness
  test3 <- testCreateRandomTestStateEdgePosition
  test4 <- testCreateRandomTestStateLargeBoard
  
  let ioTests = [ ("testCreateRandomTestStateIO", test1)
                , ("testCreateRandomTestStateRandomness", test2)
                , ("testCreateRandomTestStateEdgePosition", test3)
                , ("testCreateRandomTestStateLargeBoard", test4)
                ]
  
  let allPassed = all snd ioTests
  if allPassed
    then putStrLn "All IO tests passed!"
    else putStrLn "SOME IO TESTS FAILED!"
  
  mapM_ (\(name, result) -> putStrLn $ name ++ ": " ++ if result then "PASSED" else "FAILED") ioTests

-- | List of all pure tests to run.
tests :: [(String, Bool)]
tests =
  -- Original game logic tests
  [ ("testRevealMine", testRevealMine)
  , ("testRevealHint", testRevealHint)
  , ("testRevealAlreadyRevealed", testRevealAlreadyRevealed)
  , ("testFloodFill", testFloodFill)
  , ("testPlaceFlagOnHidden", testPlaceFlagOnHidden)
  , ("testRemoveFlagFromFlagged", testRemoveFlagFromFlagged)
  , ("testFlagRevealedCell", testFlagRevealedCell)
  , ("testGameWon", testGameWon)
  , ("testGameStillPlaying", testGameStillPlaying)
  
  -- New tests for createRandomTestState (deterministic versions)
  , ("testRandomStateCorrectMineCount", testRandomStateCorrectMineCount)
  , ("testRandomStateNoMinesInSafeZone", testRandomStateNoMinesInSafeZone)
  , ("testRandomStateCorrectDimensions", testRandomStateCorrectDimensions)
  , ("testRandomStateValidGameStatus", testRandomStateValidGameStatus)
  , ("testRandomStateSafeZoneEdgeCase", testRandomStateSafeZoneEdgeCase)
  , ("testRandomStateCornerSafeZone", testRandomStateCornerSafeZone)
  , ("testRandomStateDifferentResults", testRandomStateDifferentResults)
  , ("testRandomStateMaxMines", testRandomStateMaxMines)
  , ("testRandomStateMinimalBoard", testRandomStateMinimalBoard)
  , ("testRandomStateSafeZoneProperties", testRandomStateSafeZoneProperties)
  ]

-- ===============================
-- Original Game Logic Tests
-- ===============================

testRevealMine :: Bool
testRevealMine = let s = createTestState 3 3 [(1,1)] in gameStatus (revealCell s (1,1)) == Lost

testRevealHint :: Bool
testRevealHint = let s = createTestState 3 3 [(0,0)] in let s' = revealCell s (1,1) in Map.lookup (1,1) (gameBoard s') == Just (Revealed (Hint 1)) && Map.lookup (2,2) (gameBoard s') == Just Hidden && gameStatus s' == Playing

testRevealAlreadyRevealed :: Bool
testRevealAlreadyRevealed = let s = createTestState 3 3 [(0,0)] in let s1 = revealCell s (1,1) in let s2 = revealCell s1 (1,1) in gameBoard s1 == gameBoard s2

testFloodFill :: Bool
testFloodFill =
  let
    initialState = createTestState 5 5 [(4,4)]
    finalState = revealCell initialState (0,0)
    finalBoard = gameBoard finalState
    isOriginRevealed = Map.lookup (0,0) finalBoard == Just (Revealed Empty)
    isDistantEmptyRevealed = Map.lookup (2,2) finalBoard == Just (Revealed Empty)
    isBorderHintRevealed = Map.lookup (3,3) finalBoard == Just (Revealed (Hint 1))
    isOtherBorderHintRevealed = Map.lookup (4,3) finalBoard == Just (Revealed (Hint 1))
  in
    isOriginRevealed && isDistantEmptyRevealed && isBorderHintRevealed && isOtherBorderHintRevealed

testPlaceFlagOnHidden :: Bool
testPlaceFlagOnHidden = let s = createTestState 3 3 [] in Map.lookup (1,1) (gameBoard (toggleFlag s (1,1))) == Just Flagged

testRemoveFlagFromFlagged :: Bool
testRemoveFlagFromFlagged = let s = createTestState 3 3 [] in let s' = toggleFlag s (1,1) in Map.lookup (1,1) (gameBoard (toggleFlag s' (1,1))) == Just Hidden

testFlagRevealedCell :: Bool
testFlagRevealedCell = let s = createTestState 3 3 [(0,0)] in let s' = revealCell s (1,1) in gameBoard s' == gameBoard (toggleFlag s' (1,1))

testGameWon :: Bool
testGameWon =
  let
    initialState = createTestState 2 2 [(0,0)]
    state1 = revealCell initialState (0,1)
    state2 = revealCell state1 (1,0)
    finalState = revealCell state2 (1,1)
  in
    gameStatus finalState == Won

testGameStillPlaying :: Bool
testGameStillPlaying =
  let
    initialState = createTestState 3 3 [(0,0)]
    finalState = revealCell initialState (1,1)
  in
    gameStatus finalState == Playing

-- ===============================
-- Random Test State Tests (Pure)
-- ===============================

-- Test that the correct number of mines are placed
testRandomStateCorrectMineCount :: Bool
testRandomStateCorrectMineCount = 
  let
    w = 8
    h = 8
    numMines = 10
    safeCoord = (3, 3)
    gs = createTestStateFromParams w h numMines safeCoord
    mineCount = countMinesInBoard (trueBoard gs)
  in
    mineCount == numMines

-- Test that no mines are placed in the safe zone
testRandomStateNoMinesInSafeZone :: Bool
testRandomStateNoMinesInSafeZone =
  let
    w = 10
    h = 10
    numMines = 15
    safeCoord = (4, 4)
    gs = createTestStateFromParams w h numMines safeCoord
    safeZone = calculateSafeZone w h safeCoord
    minePositions = getMinePositions (trueBoard gs)
  in
    not (any (`elem` safeZone) minePositions)

-- Test that the board has correct dimensions
testRandomStateCorrectDimensions :: Bool
testRandomStateCorrectDimensions =
  let
    w = 7
    h = 9
    numMines = 5
    safeCoord = (2, 2)
    gs = createTestStateFromParams w h numMines safeCoord
    config = gameConfig gs
  in
    configWidth config == w && configHeight config == h && configMines config == numMines

-- Test that the game starts in Playing status
testRandomStateValidGameStatus :: Bool
testRandomStateValidGameStatus =
  let
    gs = createTestStateFromParams 6 6 8 (2, 2)
  in
    gameStatus gs == Playing

-- Test safe zone calculation at board edges
testRandomStateSafeZoneEdgeCase :: Bool
testRandomStateSafeZoneEdgeCase =
  let
    w = 5
    h = 5
    safeCoord = (0, 0)  -- Corner position
    safeZone = calculateSafeZone w h safeCoord
    expectedSafeZone = [(0,0), (0,1), (1,0), (1,1)]  -- Only valid neighbors
  in
    length safeZone == length expectedSafeZone && all (`elem` expectedSafeZone) safeZone

-- Test safe zone at opposite corner
testRandomStateCornerSafeZone :: Bool
testRandomStateCornerSafeZone =
  let
    w = 4
    h = 4
    safeCoord = (3, 3)  -- Bottom-right corner
    safeZone = calculateSafeZone w h safeCoord
    expectedSafeZone = [(2,2), (2,3), (3,2), (3,3)]  -- Only valid neighbors
  in
    length safeZone == length expectedSafeZone && all (`elem` expectedSafeZone) safeZone

-- Test that different deterministic generations produce different results
testRandomStateDifferentResults :: Bool
testRandomStateDifferentResults =
  let
    w = 8
    h = 8
    numMines = 10
    safeCoord = (3, 3)
    mines1 = generateTestMinePositions w h numMines safeCoord
    mines2 = generateAlternateMinePositions w h numMines safeCoord
  in
    mines1 /= mines2

-- Test maximum possible mines (board size - safe zone size)
testRandomStateMaxMines :: Bool
testRandomStateMaxMines =
  let
    w = 5
    h = 5
    safeCoord = (2, 2)
    safeZone = calculateSafeZone w h safeCoord
    maxPossibleMines = (w * h) - length safeZone
    gs = createTestStateFromParams w h maxPossibleMines safeCoord
    actualMines = countMinesInBoard (trueBoard gs)
  in
    actualMines == maxPossibleMines

-- Test minimal board size
testRandomStateMinimalBoard :: Bool
testRandomStateMinimalBoard =
  let
    w = 3
    h = 3
    safeCoord = (1, 1)  -- Center of 3x3 board
    safeZone = calculateSafeZone w h safeCoord
    maxMines = (w * h) - length safeZone
    gs = createTestStateFromParams w h maxMines safeCoord
  in
    countMinesInBoard (trueBoard gs) == maxMines

-- Test properties of the safe zone calculation
testRandomStateSafeZoneProperties :: Bool
testRandomStateSafeZoneProperties =
  let
    w = 10
    h = 10
    safeCoord = (5, 5)  -- Center position
    safeZone = calculateSafeZone w h safeCoord
    includesSafeCoord = safeCoord `elem` safeZone
    validSize = length safeZone <= 9
    allValidCoords = all (\(x,y) -> x >= 0 && x < w && y >= 0 && y < h) safeZone
    noDuplicates = length safeZone == length (nub safeZone)
  in
    includesSafeCoord && validSize && allValidCoords && noDuplicates

-- ===============================
-- IO-Based Random Tests
-- ===============================

-- Test that createRandomTestState produces valid game states
testCreateRandomTestStateIO :: IO Bool
testCreateRandomTestStateIO = do
  gs <- createRandomTestState 8 8 12 (3, 3)
  let config = gameConfig gs
      mineCount = countMinesInBoard (trueBoard gs)
      safeZone = calculateSafeZone 8 8 (3, 3)
      minePositions = getMinePositions (trueBoard gs)
      noMinesInSafeZone = not (any (`elem` safeZone) minePositions)
  return $ configWidth config == 8 &&
           configHeight config == 8 &&
           configMines config == 12 &&
           mineCount == 12 &&
           noMinesInSafeZone &&
           gameStatus gs == Playing

-- Test that multiple calls produce different results (probabilistic)
testCreateRandomTestStateRandomness :: IO Bool
testCreateRandomTestStateRandomness = do
  gs1 <- createRandomTestState 10 10 15 (4, 4)
  gs2 <- createRandomTestState 10 10 15 (4, 4)
  let mines1 = getMinePositions (trueBoard gs1)
      mines2 = getMinePositions (trueBoard gs2)
  return $ mines1 /= mines2

-- Test safe zone at edge position
testCreateRandomTestStateEdgePosition :: IO Bool
testCreateRandomTestStateEdgePosition = do
  gs <- createRandomTestState 6 6 8 (0, 0)  -- Corner position
  let safeZone = calculateSafeZone 6 6 (0, 0)
      minePositions = getMinePositions (trueBoard gs)
      noMinesInSafeZone = not (any (`elem` safeZone) minePositions)
      correctMineCount = length minePositions == 8
  return $ noMinesInSafeZone && correctMineCount

-- Test with large board
testCreateRandomTestStateLargeBoard :: IO Bool
testCreateRandomTestStateLargeBoard = do
  gs <- createRandomTestState 20 20 50 (10, 10)
  let config = gameConfig gs
      mineCount = countMinesInBoard (trueBoard gs)
      safeZone = calculateSafeZone 20 20 (10, 10)
      minePositions = getMinePositions (trueBoard gs)
      noMinesInSafeZone = not (any (`elem` safeZone) minePositions)
  return $ configWidth config == 20 &&
           configHeight config == 20 &&
           mineCount == 50 &&
           noMinesInSafeZone

-- ===============================
-- Helper Functions
-- ===============================

-- Calculate the 3x3 safe zone around a coordinate, bounded by board edges
calculateSafeZone :: Int -> Int -> Coords -> [Coords]
calculateSafeZone w h (sx, sy) =
  [(x, y) | dx <- [-1..1], dy <- [-1..1],
            let x = sx + dx,
            let y = sy + dy,
            x >= 0, x < w, y >= 0, y < h]

-- Count the number of mines in a true board
countMinesInBoard :: Map.Map Coords CellState -> Int
countMinesInBoard board = Map.size $ Map.filter (== Mine) board

-- Extract all mine positions from a true board
getMinePositions :: Map.Map Coords CellState -> [Coords]
getMinePositions board = Map.keys $ Map.filter (== Mine) board

-- Create a test state with specific parameters (deterministic for testing)
createTestStateFromParams :: Int -> Int -> Int -> Coords -> GameState
createTestStateFromParams w h numMines safeCoord =
  let minePositions = generateTestMinePositions w h numMines safeCoord
  in createTestState w h minePositions

-- Generate mine positions deterministically for testing
generateTestMinePositions :: Int -> Int -> Int -> Coords -> [Coords]
generateTestMinePositions w h numMines safeCoord =
  let allCoords = [(x, y) | x <- [0..w-1], y <- [0..h-1]]
      safeZone = calculateSafeZone w h safeCoord
      eligible = filter (`notElem` safeZone) allCoords
  in take numMines eligible

-- Generate alternate mine positions for difference testing
generateAlternateMinePositions :: Int -> Int -> Int -> Coords -> [Coords]
generateAlternateMinePositions w h numMines safeCoord =
  let allCoords = [(x, y) | x <- [0..w-1], y <- [0..h-1]]
      safeZone = calculateSafeZone w h safeCoord
      eligible = filter (`notElem` safeZone) allCoords
  in take numMines (reverse eligible)  -- Different order