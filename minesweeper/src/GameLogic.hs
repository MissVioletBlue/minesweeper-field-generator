-- | Core game logic for Minesweeper.
--   This module implements the main game mechanics.

module GameLogic
  ( revealCell
  , toggleFlag
  ) where

import CoreTypes
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

-- | Checks if the game has been won. (Internal function)
--   A game is won if the number of hidden/flagged cells equals the number of mines.
checkWinCondition :: GameState -> GameState
checkWinCondition gs =
  if gameStatus gs /= Playing
  then gs -- Don't check if game is already over
  else
    let
      isUnrevealed :: CellView -> Bool
      isUnrevealed Hidden  = True
      isUnrevealed Flagged = True
      isUnrevealed _       = False

      unrevealedCount = Map.size $ Map.filter isUnrevealed (gameBoard gs)
      mineCount = configMines (gameConfig gs)
    in
      if unrevealedCount == mineCount
      then gs { gameStatus = Won }
      else gs

-- | Toggles a flag on a cell.
toggleFlag :: GameState -> Coords -> GameState
toggleFlag gs coords =
  if gameStatus gs /= Playing
  then gs
  else
    let
      updateFlag cellView = case cellView of
        Hidden  -> Flagged
        Flagged -> Hidden
        Revealed s -> Revealed s
      newBoard = Map.adjust updateFlag coords (gameBoard gs)
    in
      gs { gameBoard = newBoard }

-- | The public-facing function to handle a reveal action.
--   It now correctly chains the reveal logic with the win condition check.
revealCell :: GameState -> Coords -> GameState
revealCell gs coords =
  -- Do nothing if game is already over or cell is flagged
  if gameStatus gs /= Playing || Map.lookup coords (gameBoard gs) == Just Flagged
  then gs
  else
    -- Apply reveal logic, then check for a win on the resulting state.
    let
      newState = revealCell' gs coords
    in
      checkWinCondition newState
  where
    -- Internal reveal logic
    revealCell' :: GameState -> Coords -> GameState
    revealCell' s c =
      if Map.lookup c (gameBoard s) /= Just Hidden
      then s
      else
        case Map.lookup c (trueBoard s) of
          Nothing       -> s
          Just Mine     -> s { gameStatus = Lost }
          Just (Hint n) -> revealSingleCell s c (Hint n)
          Just Empty    -> floodFill s c

-- | Helper to reveal just one cell.
revealSingleCell :: GameState -> Coords -> CellState -> GameState
revealSingleCell gs coords cellState =
  let newBoard = Map.insert coords (Revealed cellState) (gameBoard gs)
  in gs { gameBoard = newBoard }

-- | The core flood fill logic.
floodFill :: GameState -> Coords -> GameState
floodFill gs startCoords =
  gs { gameBoard = fst $ ffHelper (gameBoard gs) Set.empty [startCoords] }
  where
    ffHelper :: Map Coords CellView -> Set.Set Coords -> [Coords] -> (Map Coords CellView, Set.Set Coords)
    ffHelper board visited [] = (board, visited)
    ffHelper board visited (c:cs) =
      if Set.member c visited
      then ffHelper board visited cs
      else
        let
          newVisited = Set.insert c visited
          cellTrueState = Map.findWithDefault Empty c (trueBoard gs)
          newBoard = Map.insert c (Revealed cellTrueState) board
        in
          case cellTrueState of
            Hint _ -> ffHelper newBoard newVisited cs
            Empty  ->
              let
                (w, h) = (configWidth (gameConfig gs), configHeight (gameConfig gs))
                neighbors = [(x,y) | dx<-[-1..1], dy<-[-1..1], let x=fst c+dx, let y=snd c+dy, (dx,dy)/=(0,0), x>=0, x<w, y>=0, y<h]
                newToVisit = filter (\n -> not (Set.member n newVisited)) neighbors
              in ffHelper newBoard newVisited (newToVisit ++ cs)
            Mine   -> (board, visited)