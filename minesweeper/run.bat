@echo off
REM run.bat - Windows run script for Minesweeper project
REM Usage: run.bat (Command Prompt) or .\run.bat (PowerShell)

echo === Minesweeper Test Runner ===
echo Note: If using PowerShell, run this as .\run.bat
echo Checking environment...

REM Check for GHC
ghc --version >nul 2>&1
if errorlevel 1 (
    echo ERROR: GHC Glasgow Haskell Compiler not found!
    echo Please install GHC from https://www.haskell.org/ghcup/
    echo Make sure GHC is in your PATH
    pause
    exit /b 1
)

REM Check for Cabal
cabal --version >nul 2>&1
if errorlevel 1 (
    echo Found GHC - using direct compilation
    set "CABAL_AVAILABLE=false"
) else (
    echo Found Cabal - using cabal build
    set "CABAL_AVAILABLE=true"
)

echo Checking source files...

REM Check if required files exist
if not exist Main.hs (
    echo ERROR: Main.hs not found!
    goto error_exit
)
if not exist src\CoreTypes.hs (
    echo ERROR: src\CoreTypes.hs not found!
    goto error_exit
)
if not exist src\GameLogic.hs (
    echo ERROR: src\GameLogic.hs not found!
    goto error_exit
)
if not exist test\TestUtils.hs (
    echo ERROR: test\TestUtils.hs not found!
    goto error_exit
)
if not exist test\GameLogicTests.hs (
    echo ERROR: test\GameLogicTests.hs not found!
    goto error_exit
)

echo All source files found

REM Clean up any previous builds
echo Cleaning previous builds...
if exist minesweeper.exe del minesweeper.exe
if exist Main.exe del Main.exe
if exist *.hi del *.hi
if exist *.o del *.o
if exist src\*.hi del src\*.hi
if exist src\*.o del src\*.o
if exist test\*.hi del test\*.hi
if exist test\*.o del test\*.o

REM Try cabal first if available
if "%CABAL_AVAILABLE%"=="true" (
    if exist minesweeper.cabal (
        echo Building with Cabal...
        cabal build
        if errorlevel 1 (
            echo Cabal build failed, falling back to GHC
            set "CABAL_AVAILABLE=false"
        ) else (
            echo Cabal build successful
            echo Running tests...
            echo ====================
            cabal run minesweeper-exe
            set "exit_code=%errorlevel%"
            goto report_results
        )
    )
)

REM Fallback to direct GHC compilation
if "%CABAL_AVAILABLE%"=="false" (
    echo Compiling with GHC...
    ghc --make -isrc -itest -package containers -package random-shuffle Main.hs -o minesweeper.exe
    if errorlevel 1 (
        echo ERROR: Compilation failed!
        echo.
        echo Common fixes:
        echo 1. Install missing packages: cabal install containers random-shuffle
        echo 2. Check that all source files are present
        echo 3. Try: ghc --version to verify GHC works
        pause
        exit /b 1
    ) else (
        echo GHC compilation successful
        echo Running tests...
        echo ====================
        minesweeper.exe
        set "exit_code=%errorlevel%"
    )
)

:report_results
echo ====================
if "%exit_code%"=="0" (
    echo All tests completed successfully!
) else (
    echo Tests completed with warnings or errors
)

echo Cleaning up build artifacts...
if exist minesweeper.exe del minesweeper.exe
if exist *.hi del *.hi
if exist *.o del *.o
if exist src\*.hi del src\*.hi
if exist src\*.o del src\*.o
if exist test\*.hi del test\*.hi
if exist test\*.o del test\*.o

echo Done!
pause
exit /b 0

:error_exit
echo Make sure you are in the project root directory.
pause
exit /b 1