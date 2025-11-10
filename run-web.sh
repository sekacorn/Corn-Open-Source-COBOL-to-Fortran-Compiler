#!/bin/bash
################################################################################
# Corn COBOL-to-Fortran Compiler - Web Dashboard Runner (Linux/Mac)
# Banking Enterprise Edition v2.0
# Copyright (c) 2025 sekacorn | sekacorn@gmail.com
################################################################################

echo "================================================================================"
echo "  CORN COBOL-TO-FORTRAN COMPILER - WEB DASHBOARD"
echo "  Banking Enterprise Edition v2.0"
echo "================================================================================"
echo ""

# Check if Rust is installed
if ! command -v cargo &> /dev/null; then
    echo "ERROR: Rust/Cargo is not installed!"
    echo "Please install Rust from: https://rustup.rs/"
    exit 1
fi

# Default to release mode
BUILD_MODE="release"

# Check for --debug flag
if [ "$1" = "--debug" ]; then
    BUILD_MODE="debug"
    shift
fi

BUILD_DIR="target/$BUILD_MODE"

# Check if web dashboard is built
if [ ! -f "$BUILD_DIR/web-dashboard" ] && [ ! -f "$BUILD_DIR/web-dashboard.exe" ]; then
    echo "Web dashboard not built yet. Building now..."
    if [ "$BUILD_MODE" = "release" ]; then
        cargo build --release --bin web-dashboard
    else
        cargo build --bin web-dashboard
    fi
fi

echo "Starting web dashboard..."
echo ""
echo "Once started, open your browser to:"
echo "  http://127.0.0.1:8080"
echo ""
echo "API Endpoints:"
echo "  GET  /              - Dashboard home page"
echo "  GET  /version       - Compiler version info (JSON)"
echo "  GET  /status        - Health status (JSON)"
echo "  GET  /features      - Banking features (JSON)"
echo "  GET  /report        - Compilation reports (auth required, JSON)"
echo ""
echo "Press Ctrl+C to stop the server"
echo ""
echo "================================================================================"
echo ""

# Run the web dashboard
if [ "$BUILD_MODE" = "release" ]; then
    cargo run --release --bin web-dashboard
else
    cargo run --bin web-dashboard
fi
