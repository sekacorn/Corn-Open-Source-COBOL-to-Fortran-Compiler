#!/bin/bash
################################################################################
# Corn COBOL-to-Fortran Compiler - Run Script (Linux/Mac)
# Banking Enterprise Edition v2.0
# Copyright (c) 2025 sekacorn | sekacorn@gmail.com
################################################################################

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

# Check if compiler is built
if [ ! -f "$BUILD_DIR/corn-compiler" ] && [ ! -f "$BUILD_DIR/corn-compiler.exe" ]; then
    echo "Compiler not built yet. Building now..."
    if [ "$BUILD_MODE" = "release" ]; then
        ./build.sh --release
    else
        ./build.sh --debug
    fi
fi

# Run the compiler with all remaining arguments
if [ "$BUILD_MODE" = "release" ]; then
    cargo run --release --bin corn-compiler -- "$@"
else
    cargo run --bin corn-compiler -- "$@"
fi
