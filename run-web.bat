@echo off
REM ============================================================================
REM Corn COBOL-to-Fortran Compiler - Web Dashboard Runner (Windows)
REM Banking Enterprise Edition v2.0
REM Copyright (c) 2025 sekacorn | sekacorn@gmail.com
REM ============================================================================

echo ================================================================================
echo   CORN COBOL-TO-FORTRAN COMPILER - WEB DASHBOARD
echo   Banking Enterprise Edition v2.0
echo ================================================================================
echo.

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

REM Check if web dashboard is built
if not exist "%BUILD_DIR%\web-dashboard.exe" (
    echo Web dashboard not built yet. Building now...
    if "%BUILD_MODE%"=="release" (
        cargo build --release --bin web-dashboard
    ) else (
        cargo build --bin web-dashboard
    )
)

echo Starting web dashboard...
echo.
echo Once started, open your browser to:
echo   http://127.0.0.1:8080
echo.
echo API Endpoints:
echo   GET  /              - Dashboard home page
echo   GET  /version       - Compiler version info (JSON)
echo   GET  /status        - Health status (JSON)
echo   GET  /features      - Banking features (JSON)
echo   GET  /report        - Compilation reports (auth required, JSON)
echo.
echo Press Ctrl+C to stop the server
echo.
echo ================================================================================
echo.

REM Run the web dashboard
if "%BUILD_MODE%"=="release" (
    cargo run --release --bin web-dashboard
) else (
    cargo run --bin web-dashboard
)
