@echo off
REM ============================================================================
REM Corn COBOL-to-Fortran Compiler - Run Script (Windows)
REM Banking Enterprise Edition v2.0
REM Copyright (c) 2025 sekacorn | sekacorn@gmail.com
REM ============================================================================

REM Check if Rust is installed
where cargo >nul 2>nul
if %errorlevel% neq 0 (
    echo ERROR: Rust/Cargo is not installed!
    echo Please install Rust from: https://rustup.rs/
    pause
    exit /b 1
)

REM Default to release mode
set BUILD_MODE=release

REM Check for --debug flag
if /i "%1"=="--debug" (
    set BUILD_MODE=debug
    shift
)

set BUILD_DIR=target\%BUILD_MODE%

REM Check if compiler is built
if not exist "%BUILD_DIR%\corn-compiler.exe" (
    echo Compiler not built yet. Building now...
    if "%BUILD_MODE%"=="release" (
        call build.bat --release
    ) else (
        call build.bat --debug
    )
)

REM Run the compiler with all remaining arguments
if "%BUILD_MODE%"=="release" (
    cargo run --release --bin corn-compiler -- %*
) else (
    cargo run --bin corn-compiler -- %*
)
