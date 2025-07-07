-- | Utility functions for creating test game states.
--   This module provides helper functions to set up specific board
--   configurations for testing purposes.

module TestUtils 
  ( createTestState
  ) where

import CoreTypes
import qualified Data.Map.Strict as Map

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