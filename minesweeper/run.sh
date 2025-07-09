#!/bin/bash
# run.sh - Linux/macOS run script for Minesweeper project
# Usage: chmod +x run.sh && ./run.sh

echo "=== Minesweeper Test Runner ==="
echo "Checking environment..."

# Function to check if a command exists
command_exists() {
    command -v "$1" >/dev/null 2>&1
}

# Check for GHC
if ! command_exists ghc; then
    echo "ERROR: GHC (Glasgow Haskell Compiler) not found!"
    echo "Please install GHC from https://www.haskell.org/ghcup/"
    exit 1
fi

# Check for Cabal (optional)
if command_exists cabal; then
    echo "✓ Found Cabal - using cabal build"
    CABAL_AVAILABLE=true
else
    echo "✓ Found GHC - using direct compilation"
    CABAL_AVAILABLE=false
fi

echo "Checking source files..."

# Check if required files exist
required_files=("Main.hs" "src/CoreTypes.hs" "src/GameLogic.hs" "test/TestUtils.hs" "test/GameLogicTests.hs")
for file in "${required_files[@]}"; do
    if [ ! -f "$file" ]; then
        echo "ERROR: Required file $file not found!"
        echo "Make sure you're in the project root directory."
        exit 1
    fi
done

echo "✓ All source files found"

# Clean up any previous builds
echo "Cleaning previous builds..."
rm -f Main minesweeper *.hi *.o src/*.hi src/*.o test/*.hi test/*.o 2>/dev/null

# Try cabal first if available
if [ "$CABAL_AVAILABLE" = true ] && [ -f "minesweeper.cabal" ]; then
    echo "Building with Cabal..."
    if cabal build; then
        echo "✓ Cabal build successful"
        echo "Running tests..."
        echo "===================="
        cabal run minesweeper-exe
        exit_code=$?
    else
        echo "⚠ Cabal build failed, falling back to GHC"
        CABAL_AVAILABLE=false
    fi
fi

# Fallback to direct GHC compilation
if [ "$CABAL_AVAILABLE" = false ]; then
    echo "Compiling with GHC..."
    if ghc --make -isrc -itest -package containers -package random-shuffle Main.hs -o minesweeper; then
        echo "✓ GHC compilation successful"
        echo "Running tests..."
        echo "===================="
        ./minesweeper
        exit_code=$?
    else
        echo "ERROR: Compilation failed!"
        echo ""
        echo "Common fixes:"
        echo "1. Install missing packages: cabal install containers random-shuffle"
        echo "2. Check that all source files are present"
        echo "3. Try: ghc --version (to verify GHC works)"
        exit 1
    fi
fi

# Report results
echo "===================="
if [ $exit_code -eq 0 ]; then
    echo "✓ All tests completed successfully!"
else
    echo "⚠ Tests completed with warnings or errors (exit code: $exit_code)"
fi

echo "Cleaning up build artifacts..."
rm -f minesweeper *.hi *.o src/*.hi src/*.o test/*.hi test/*.o 2>/dev/null

echo "Done!"