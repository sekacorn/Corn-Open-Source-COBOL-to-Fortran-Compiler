@echo off
REM ============================================================================
REM Corn COBOL-to-Fortran Compiler - Build Script (Windows)
REM Banking Enterprise Edition v2.0
REM Copyright (c) 2025 sekacorn | sekacorn@gmail.com
REM ============================================================================

echo ================================================================================
echo   CORN COBOL-TO-FORTRAN COMPILER - BUILD SCRIPT
echo   Banking Enterprise Edition v2.0
echo ================================================================================
echo.

REM Check if Rust is installed
where cargo >nul 2>nul
if %errorlevel% neq 0 (
    echo ERROR: Rust/Cargo is not installed!
    echo.
    echo Please install Rust from: https://rustup.rs/
    echo Download and run: https://win.rustup.rs/
    echo.
    pause
    exit /b 1
)

echo Rust version:
rustc --version
cargo --version
echo.

REM Parse command line arguments
set BUILD_MODE=release
set CLEAN_BUILD=false

:parse_args
if "%1"=="" goto end_parse
if /i "%1"=="--debug" (
    set BUILD_MODE=debug
    shift
    goto parse_args
)
if /i "%1"=="--release" (
    set BUILD_MODE=release
    shift
    goto parse_args
)
if /i "%1"=="--clean" (
    set CLEAN_BUILD=true
    shift
    goto parse_args
)
if /i "%1"=="--help" goto show_help
if /i "%1"=="-h" goto show_help
echo Unknown option: %1
echo Use --help to see available options
pause
exit /b 1

:show_help
echo Usage: build.bat [OPTIONS]
echo.
echo OPTIONS:
echo   --debug       Build in debug mode
echo   --release     Build in release mode (optimized, default)
echo   --clean       Clean build directory before building
echo   --help, -h    Show this help message
echo.
echo EXAMPLES:
echo   build.bat                    # Build in release mode
echo   build.bat --debug            # Build in debug mode
echo   build.bat --clean --release  # Clean and build release
echo.
pause
exit /b 0

:end_parse

REM Clean build if requested
if "%CLEAN_BUILD%"=="true" (
    echo [1/3] Cleaning previous build...
    cargo clean
    echo       Clean complete!
    echo.
)

REM Build the compiler
echo [2/3] Building compiler (%BUILD_MODE% mode^)...
if "%BUILD_MODE%"=="release" (
    cargo build --release
) else (
    cargo build
)

if %errorlevel% neq 0 (
    echo.
    echo ERROR: Build failed!
    echo.
    echo TROUBLESHOOTING:
    echo   1. Check internet connection (for dependency downloads^)
    echo   2. Try: build.bat --clean --release
    echo   3. Update Rust: rustup update
    echo   4. Check error messages above
    echo.
    pause
    exit /b 1
)

echo.
echo [3/3] Build complete!
echo.

REM Show build artifacts
if "%BUILD_MODE%"=="release" (
    set BUILD_DIR=target\release
) else (
    set BUILD_DIR=target\debug
)

echo Build artifacts:
if exist "%BUILD_DIR%\corn-compiler.exe" (
    dir "%BUILD_DIR%\corn-compiler.exe" | findstr corn-compiler.exe
)
echo.

echo ================================================================================
echo BUILD SUCCESS!
echo ================================================================================
echo.
echo Compiler built successfully in %BUILD_MODE% mode
echo Binary location: %BUILD_DIR%\corn-compiler.exe
echo.
echo NEXT STEPS:
echo.
echo 1. Test the compiler:
if "%BUILD_MODE%"=="release" (
    echo    run.bat test
) else (
    echo    run.bat --debug test
)
echo.
echo 2. Compile a COBOL file:
if "%BUILD_MODE%"=="release" (
    echo    run.bat compile myprogram.cob -o myprogram.f90
) else (
    echo    run.bat --debug compile myprogram.cob -o myprogram.f90
)
echo.
echo 3. Show version:
if "%BUILD_MODE%"=="release" (
    echo    run.bat version
) else (
    echo    run.bat --debug version
)
echo.
echo 4. Start web dashboard:
echo    run-web.bat
echo.
echo For help:
if "%BUILD_MODE%"=="release" (
    echo    run.bat help
) else (
    echo    run.bat --debug help
)
echo.
echo ================================================================================
pause
