# Minesweeper Game Logic

A Test-Driven Development (TDD) implementation of core Minesweeper game logic in Haskell.

## Project Structure

```
minesweeper/
├── minesweeper.cabal    # Cabal package configuration
├── Main.hs              # Main entry point for running tests
├── README.md            # This file
├── src/                 # Source code
│   ├── CoreTypes.hs     # Core data types and structures
│   └── GameLogic.hs     # Main game logic implementation
└── test/                # Test files
    ├── TestUtils.hs     # Utilities for creating test states
    └── GameLogicTests.hs # Test cases
```

## Building and Running

### With Cabal (Recommended)

```bash
# Build the project
cabal build

# Run the tests
cabal run minesweeper-exe

# Or run tests directly
cabal test
```

### With GHCi (Development)

```bash
# Load the main file
ghci Main.hs

# Run tests
main
```

### Loading Individual Modules

```bash
# To work with specific modules
ghci src/GameLogic.hs

# Or load everything with dependencies
ghci -isrc -itest Main.hs
```

## Modules

- **CoreTypes**: Defines all the fundamental data types used throughout the game
- **GameLogic**: Implements the core game mechanics, particularly the `revealCell` function
- **TestUtils**: Provides helper functions for creating specific board configurations for testing
- **GameLogicTests**: Contains all test cases following TDD principles

## Key Features

- **Flood Fill**: Automatically reveals connected empty areas when an empty cell is clicked
- **Mine Detection**: Proper game-over handling when mines are revealed  
- **Hint System**: Calculates and displays the number of adjacent mines
- **Test Coverage**: Comprehensive test suite covering all major game scenarios

## Tests

The test suite covers:
1. **Mine Revelation**: Clicking on a mine ends the game
2. **Hint Revelation**: Clicking on numbered cells reveals only that cell
3. **Idempotence**: Re-clicking revealed cells has no effect
4. **Flood Fill**: Empty areas are automatically revealed with proper boundary detection

Run `main` in GHCi or `cabal run` to execute all tests.