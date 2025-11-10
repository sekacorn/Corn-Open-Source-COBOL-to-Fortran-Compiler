#!/bin/bash
################################################################################
# Corn COBOL-to-Fortran Compiler - Build Script (Linux/Mac)
# Banking Enterprise Edition v2.0
# Copyright (c) 2025 sekacorn | sekacorn@gmail.com
################################################################################

echo "================================================================================"
echo "  CORN COBOL-TO-FORTRAN COMPILER - BUILD SCRIPT"
echo "  Banking Enterprise Edition v2.0"
echo "================================================================================"
echo ""

# Check if Rust is installed
if ! command -v cargo &> /dev/null; then
    echo "ERROR: Rust/Cargo is not installed!"
    echo ""
    echo "Please install Rust from: https://rustup.rs/"
    echo "Run: curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh"
    echo ""
    exit 1
fi

echo "Rust version:"
rustc --version
cargo --version
echo ""

# Parse command line arguments
BUILD_MODE="release"
CLEAN_BUILD=false

while [[ $# -gt 0 ]]; do
    case $1 in
        --debug)
            BUILD_MODE="debug"
            shift
            ;;
        --release)
            BUILD_MODE="release"
            shift
            ;;
        --clean)
            CLEAN_BUILD=true
            shift
            ;;
        --help|-h)
            echo "Usage: ./build.sh [OPTIONS]"
            echo ""
            echo "OPTIONS:"
            echo "  --debug       Build in debug mode (default)"
            echo "  --release     Build in release mode (optimized)"
            echo "  --clean       Clean build directory before building"
            echo "  --help, -h    Show this help message"
            echo ""
            echo "EXAMPLES:"
            echo "  ./build.sh                    # Build in release mode"
            echo "  ./build.sh --debug            # Build in debug mode"
            echo "  ./build.sh --clean --release  # Clean and build release"
            echo ""
            exit 0
            ;;
        *)
            echo "Unknown option: $1"
            echo "Use --help to see available options"
            exit 1
            ;;
    esac
done

# Clean build if requested
if [ "$CLEAN_BUILD" = true ]; then
    echo "[1/3] Cleaning previous build..."
    cargo clean
    echo "      Clean complete!"
    echo ""
fi

# Build the compiler
echo "[2/3] Building compiler ($BUILD_MODE mode)..."
if [ "$BUILD_MODE" = "release" ]; then
    cargo build --release
    BUILD_STATUS=$?
else
    cargo build
    BUILD_STATUS=$?
fi

if [ $BUILD_STATUS -ne 0 ]; then
    echo ""
    echo "ERROR: Build failed!"
    echo ""
    echo "TROUBLESHOOTING:"
    echo "  1. Check internet connection (for dependency downloads)"
    echo "  2. Try: ./build.sh --clean --release"
    echo "  3. Update Rust: rustup update"
    echo "  4. Check error messages above"
    echo ""
    exit 1
fi

echo ""
echo "[3/3] Build complete!"
echo ""

# Show build artifacts
if [ "$BUILD_MODE" = "release" ]; then
    BUILD_DIR="target/release"
else
    BUILD_DIR="target/debug"
fi

echo "Build artifacts:"
if [ -f "$BUILD_DIR/corn-compiler" ] || [ -f "$BUILD_DIR/corn-compiler.exe" ]; then
    ls -lh "$BUILD_DIR/corn-compiler"* 2>/dev/null | head -2
fi
echo ""

echo "================================================================================"
echo "BUILD SUCCESS!"
echo "================================================================================"
echo ""
echo "Compiler built successfully in $BUILD_MODE mode"
echo "Binary location: $BUILD_DIR/corn-compiler"
echo ""
echo "NEXT STEPS:"
echo ""
echo "1. Test the compiler:"
if [ "$BUILD_MODE" = "release" ]; then
    echo "   ./run.sh test"
else
    echo "   ./run.sh --debug test"
fi
echo ""
echo "2. Compile a COBOL file:"
if [ "$BUILD_MODE" = "release" ]; then
    echo "   ./run.sh compile myprogram.cob -o myprogram.f90"
else
    echo "   ./run.sh --debug compile myprogram.cob -o myprogram.f90"
fi
echo ""
echo "3. Show version:"
if [ "$BUILD_MODE" = "release" ]; then
    echo "   ./run.sh version"
else
    echo "   ./run.sh --debug version"
fi
echo ""
echo "4. Start web dashboard:"
echo "   ./run-web.sh"
echo ""
echo "For help:"
if [ "$BUILD_MODE" = "release" ]; then
    echo "   ./run.sh help"
else
    echo "   ./run.sh --debug help"
fi
echo ""
echo "================================================================================"
