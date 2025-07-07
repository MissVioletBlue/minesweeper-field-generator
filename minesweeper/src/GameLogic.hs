-- | Core game logic for Minesweeper.
--   This module implements the main game mechanics, particularly the
--   cell revelation logic including flood fill for empty areas.

module GameLogic
  ( revealCell
  , toggleFlag -- Export the new function
  ) where

import CoreTypes
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

-- | Toggles a flag on a cell.
--   If the cell is hidden, it becomes flagged.
--   If it's flagged, it becomes hidden.
--   It has no effect on revealed cells or if the game is over.
toggleFlag :: GameState -> Coords -> GameState
toggleFlag gs coords =
  -- Do nothing if game is already over
  if gameStatus gs /= Playing
  then gs
  else
    let
      currentBoard = gameBoard gs
      -- A function to apply the change to the map
      updateFlag cellView = case cellView of
        Hidden  -> Flagged
        Flagged -> Hidden
        -- For revealed cells, do nothing
        Revealed s -> Revealed s
      -- `adjust` applies a function to a value at a specific key
      newBoard = Map.adjust updateFlag coords currentBoard
    in
      gs { gameBoard = newBoard }


-- | The public-facing function to handle a reveal action.
--   It checks the cell's state and delegates to the appropriate helper.
revealCell :: GameState -> Coords -> GameState
revealCell gs coords =
  -- Do nothing if game is already over or cell is not hidden
  if gameStatus gs /= Playing || Map.lookup coords (gameBoard gs) /= Just Hidden
  then gs
  else
    case Map.lookup coords (trueBoard gs) of
      Nothing         -> gs -- Clicked outside the board, do nothing
      Just Mine       -> gs { gameStatus = Lost } -- Game over
      Just (Hint n)   -> revealSingleCell gs coords (Hint n)
      Just Empty      -> floodFill gs coords

-- | Helper to reveal just one cell.
revealSingleCell :: GameState -> Coords -> CellState -> GameState
revealSingleCell gs coords cellState =
  let newBoard = Map.insert coords (Revealed cellState) (gameBoard gs)
  in gs { gameBoard = newBoard }

-- | The core flood fill logic.
--   It uses a recursive helper with a set of visited coordinates to avoid cycles.
floodFill :: GameState -> Coords -> GameState
floodFill gs startCoords =
  gs { gameBoard = fst $ ffHelper (gameBoard gs) Set.empty [startCoords] }
  where
    -- ffHelper takes the board, a set of visited coords, and a list of coords to visit.
    -- It returns the updated board and the set of visited coordinates.
    ffHelper :: Map Coords CellView -> Set.Set Coords -> [Coords] -> (Map Coords CellView, Set.Set Coords)
    ffHelper board visited [] = (board, visited) -- Base case: no more cells to visit
    ffHelper board visited (c:cs) =
      if Set.member c visited
      then ffHelper board visited cs -- Already visited, skip
      else
        let
          newVisited = Set.insert c visited
          cellTrueState = Map.findWithDefault Empty c (trueBoard gs)
          newBoard = Map.insert c (Revealed cellTrueState) board
        in
          case cellTrueState of
            -- If we hit a hint, reveal it but don't explore its neighbors
            Hint _ -> ffHelper newBoard newVisited cs
            -- If it's empty, reveal it and add its neighbors to the list to be visited
            Empty  ->
              let
                (w, h) = (configWidth (gameConfig gs), configHeight (gameConfig gs))
                neighbors = [(x,y) | dx<-[-1..1], dy<-[-1..1],
                            let x=fst c+dx, let y=snd c+dy,
                            (dx,dy)/=(0,0), x>=0, x<w, y>=0, y<h]
                -- Filter out neighbors we've already processed
                newToVisit = filter (\n -> not (Set.member n newVisited)) neighbors
              in ffHelper newBoard newVisited (newToVisit ++ cs)
            -- This case should not be reached due to the initial check in `revealCell`
            Mine   -> (board, visited)