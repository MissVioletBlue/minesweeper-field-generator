-- | Utility functions for creating test game states.
--   This module provides helper functions to set up specific board
--   configurations for testing purposes.

module TestUtils 
  ( createTestState,
    createRandomTestState,
    printTrueBoard
  ) where

import CoreTypes
import qualified Data.Map.Strict as Map
import System.Random.Shuffle (shuffleM)

-- | Creates a simple GameState for testing purposes.
--   It takes a list of mine coordinates to precisely control the board layout.
createTestState :: Int -> Int -> [Coords] -> GameState
createTestState w h mineCoords =
  let
    config = GameConfig w h (length mineCoords)
    allCoords = [(x, y) | x <- [0..w-1], y <- [0..h-1]]
    -- Create the true board
    trueBoard' = foldl (\b c -> Map.insert c Mine b) (createEmptyTrueBoard allCoords) mineCoords
    finalTrueBoard = calculateAllHints trueBoard'
    -- Create the player-visible board (all hidden)
    gameBoard' = Map.fromList $ zip allCoords (repeat Hidden)
  in
    GameState gameBoard' finalTrueBoard Playing config
  where
    createEmptyTrueBoard = Map.fromList . map (\c -> (c, Empty))
    
    -- Simplified hint calculation logic for tests
    calculateAllHints board = Map.mapWithKey (calcHint board) board
    
    calcHint board coord cell = case cell of
      Mine -> Mine
      Empty -> let
        neighbors = [(x,y) | dx<-[-1..1], dy<-[-1..1], 
                    let x=fst coord+dx, let y=snd coord+dy, 
                    (dx,dy)/=(0,0), x>=0, x<w, y>=0, y<h]
        mineCount = length $ filter (\c -> Map.lookup c board == Just Mine) neighbors
        in if mineCount > 0 then Hint mineCount else Empty

createRandomTestState :: Int -> Int -> Int -> Coords -> IO GameState
createRandomTestState w h numMines safeCoord = do
  let
    -- All board coordinates
    allCoords = [(x, y) | x <- [0..w-1], y <- [0..h-1]]

    -- Calculate 3x3 safe zone centered at safeCoord
    safeZone = [(x, y) | dx <- [-1..1], dy <- [-1..1],
                         let x = fst safeCoord + dx,
                         let y = snd safeCoord + dy,
                         x >= 0, x < w, y >= 0, y < h]

    -- All mine-eligible coordinates (excluding the safe zone)
    eligibleCoords = filter (`notElem` safeZone) allCoords

  -- Make sure we have enough space to place the requested mines
  if length eligibleCoords < numMines
    then error "Not enough space to place mines outside the 3x3 safe zone!"
    else do
      shuffledCoords <- shuffleM eligibleCoords
      let mineCoords = take numMines shuffledCoords
      return $ createTestState w h mineCoords

-- | Print the current visible board to the console.
printBoard :: GameState -> IO ()
printBoard gs = do
  let (w, h) = (configWidth (gameConfig gs), configHeight (gameConfig gs))
      board = gameBoard gs
  mapM_ (putStrLn . unwords . rowToStrView board w) [y | y <- [0..h-1]]

rowToStrView :: Map.Map Coords CellView -> Int -> Int -> [String]
rowToStrView board w y =
  [cellToCharView (Map.findWithDefault Hidden (x, y) board) | x <- [0..w-1]]

cellToCharView :: CellView -> String
cellToCharView Hidden        = "#"
cellToCharView Flagged       = "F"
cellToCharView (Revealed s)  = case s of
  Mine     -> "*"
  Empty    -> "."
  Hint n   -> show n

-- | Print the full true board (where the mines and hints really are)
printTrueBoard :: GameState -> IO ()
printTrueBoard gs = do
  let (w, h) = (configWidth (gameConfig gs), configHeight (gameConfig gs))
      board = trueBoard gs
  mapM_ (putStrLn . unwords . rowToStrTrue board w) [y | y <- [0..h-1]]
  
rowToStrTrue :: Map.Map Coords CellState -> Int -> Int -> [String]
rowToStrTrue board w y =
  [cellToCharTrue (Map.findWithDefault Empty (x, y) board) | x <- [0..w-1]]

cellToCharTrue :: CellState -> String
cellToCharTrue Mine      = "*"
cellToCharTrue Empty     = "."
cellToCharTrue (Hint n)  = show n