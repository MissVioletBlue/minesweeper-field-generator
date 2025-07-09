# Minesweeper Game Logic

A comprehensive Test-Driven Development (TDD) implementation of core Minesweeper game logic in Haskell.

## Project Structure

```
minesweeper/
├── README.txt           # This file
├── run.sh              # Linux/macOS run script
├── run.bat             # Windows run script
├── minesweeper.cabal   # Cabal package configuration
├── Main.hs             # Main entry point for running tests
├── src/                # Source code directory
│   ├── CoreTypes.hs    # Core data types and structures
│   └── GameLogic.hs    # Main game logic implementation
└── test/               # Test files directory
    ├── TestUtils.hs    # Utilities for creating test states
    └── GameLogicTests.hs # Comprehensive test cases
```

## Quick Start

### Option 1: Use the Run Scripts (Easiest)

**Linux/macOS:**
```bash
chmod +x run.sh
./run.sh
```

**Windows:**
```cmd
run.bat
```

### Option 2: Manual Compilation and Execution

#### Method A: Using Cabal (Recommended)

**Prerequisites:** Install GHC and Cabal
- Download from: https://www.haskell.org/ghcup/

**Commands:**
```bash
# Build the project
cabal build

# Run the main program (shows sample board + runs all tests)
cabal run minesweeper-exe

# Run tests only (if cabal test is configured)
cabal test
```

#### Method B: Direct GHC Compilation

**Prerequisites:** Install GHC compiler

**Simple compilation:**
```bash
# Compile and run in one step
ghc --make -isrc -itest Main.hs -o minesweeper
./minesweeper
```

**With optimizations:**
```bash
# Optimized build
ghc --make -isrc -itest -O2 Main.hs -o minesweeper
./minesweeper
```

#### Method C: Interactive Development with GHCi

**Load the project:**
```bash
# Start GHCi with all source paths
ghci -isrc -itest Main.hs

# In GHCi, run the main function
*Main> main
```

**For development/debugging:**
```bash
# Load specific modules for testing
ghci -isrc -itest src/GameLogic.hs

# Or just the test utilities
ghci -isrc -itest test/TestUtils.hs
```

## Required Dependencies

The project uses these Haskell libraries:
- `containers` (for Data.Map.Strict and Data.Set)
- `random-shuffle` (for shuffleM in TestUtils)
- `random` (for random number generation)

**Installing dependencies manually:**
```bash
cabal install containers random-shuffle random
```

## What the Program Does

When you run the program, it will:

1. **Generate a Sample Board**: Creates a 9x9 minesweeper board with 25 randomly placed mines, ensuring position (4,4) is safe
2. **Display the True Board**: Shows where all mines (*) and hints (numbers) are located
3. **Run All Tests**: Executes a comprehensive test suite covering:
   - Mine revelation (game over when clicking mines)
   - Hint revelation (numbered cells)
   - Flood fill algorithm (auto-revealing empty areas)
   - Flag toggling functionality
   - Win condition detection
   - Random board generation validation

## Understanding the Output

**Sample True Board Output:**
```
* 2 1 1 0 0 0 0 0
2 * 2 1 1 0 0 0 0
1 2 2 1 1 0 0 0 0
0 1 1 1 0 0 0 0 0
0 1 . 1 0 0 0 0 0
```
- `*` = Mine
- Numbers = Hint cells (count of adjacent mines)
- `.` = Empty cell

**Test Results:**
```
Running GameLogic tests...
testRevealMine: PASSED
testRevealHint: PASSED
...
All tests passed!
```

## Troubleshooting

### Common Issues

**"Module not found" errors:**
```bash
# Make sure you're in the project root directory
# Use the -i flags to specify source paths
ghci -isrc -itest Main.hs
```

**"Package not found" errors:**
```bash
# Install missing packages
cabal install containers random-shuffle

# Or let cabal handle it automatically
cabal build
```

**Permission denied on Linux/macOS:**
```bash
chmod +x run.sh
./run.sh
```

**Windows script doesn't run:**
- Right-click run.bat → "Run as administrator"
- Or use: `cmd /c run.bat`

### Building Without Cabal

If you don't have cabal installed:
```bash
# Install required packages with ghc-pkg
ghc-pkg list  # Check what's installed

# Compile directly (may need package flags)
ghc --make -package containers -package random-shuffle -isrc -itest Main.hs
```

## Development Workflow

### Adding New Tests
1. Edit `test/GameLogicTests.hs`
2. Add your test function
3. Add it to the `tests` list
4. Run `./run.sh` or `cabal run`

### Modifying Game Logic
1. Edit `src/GameLogic.hs` or `src/CoreTypes.hs`
2. Run tests to ensure nothing breaks
3. Add new tests for new functionality

### Interactive Testing
```bash
# Load in GHCi for interactive testing
ghci -isrc -itest test/TestUtils.hs

# Create test boards interactively
*TestUtils> gs <- createRandomTestState 5 5 3 (2,2)
*TestUtils> printTrueBoard gs
```

## Module Overview

- **CoreTypes.hs**: Fundamental data types (GameState, CellView, etc.)
- **GameLogic.hs**: Core game mechanics (revealCell, toggleFlag, win detection)
- **TestUtils.hs**: Helper functions for creating test scenarios and random boards
- **GameLogicTests.hs**: Comprehensive test suite with 12+ test cases
- **Main.hs**: Program entry point that demonstrates functionality and runs tests

## Performance Notes

- The flood fill algorithm uses efficient Set-based visited tracking
- Map-based board representation provides O(log n) cell access
- Random board generation ensures proper mine distribution while maintaining safe zones

## Next Steps

This implementation provides a solid foundation for building a complete Minesweeper game. You could extend it with:
- User interface (console or graphical)
- Save/load functionality
- Different difficulty levels
- Custom board sizes
- Timing and scoring systems