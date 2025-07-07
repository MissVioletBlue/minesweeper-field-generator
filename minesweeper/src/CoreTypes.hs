-- | Core data types for the Minesweeper game.
--   This module defines all the fundamental types used throughout the game.

module CoreTypes where

import           Data.Map.Strict (Map)

-- | Coordinate type for board positions
type Coords = (Int, Int)

-- | The actual state of a cell in the game
data CellState = Mine | Empty | Hint Int
  deriving (Show, Eq)

-- | What the player can see for each cell
data CellView = Hidden | Flagged | Revealed CellState
  deriving (Show, Eq)

-- | Overall state of the game
data GameStatus = Playing | Won | Lost
  deriving (Show, Eq)

-- | Configuration settings for the game
data GameConfig = GameConfig
  { configWidth  :: Int
  , configHeight :: Int
  , configMines  :: Int
  } deriving (Show)

-- | The master state of the game containing all information
data GameState = GameState
  { -- | The board the player sees
    gameBoard   :: Map Coords CellView
    -- | The true board with all mine locations and hints
  , trueBoard   :: Map Coords CellState
  , gameStatus  :: GameStatus
  , gameConfig  :: GameConfig
  } deriving (Show)