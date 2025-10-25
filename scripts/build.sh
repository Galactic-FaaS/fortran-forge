#!/bin/bash
# Build script for ForGE
# Supports both fpm and CMake builds

set -e

# Default values
BUILD_SYSTEM="fpm"
BUILD_TYPE="release"
BUILD_EXAMPLES="yes"
BUILD_TESTS="no"
CLEAN="no"

# Usage information
usage() {
    cat << EOF
Usage: $0 [OPTIONS]

Build ForGE library

OPTIONS:
    -s, --system SYSTEM    Build system: fpm or cmake (default: fpm)
    -t, --type TYPE        Build type: debug, dev, release (default: release)
    -e, --examples         Build examples (default: yes)
    -T, --tests            Build tests (default: no)
    -c, --clean            Clean before building
    -h, --help             Show this help message

EXAMPLES:
    $0                               # Build with fpm in release mode
    $0 --system cmake --type debug   # Build with CMake in debug mode
    $0 --clean --examples --tests    # Clean build with examples and tests

EOF
    exit 0
}

# Parse command line arguments
while [[ $# -gt 0 ]]; do
    case $1 in
        -s|--system)
            BUILD_SYSTEM="$2"
            shift 2
            ;;
        -t|--type)
            BUILD_TYPE="$2"
            shift 2
            ;;
        -e|--examples)
            BUILD_EXAMPLES="yes"
            shift
            ;;
        -T|--tests)
            BUILD_TESTS="yes"
            shift
            ;;
        -c|--clean)
            CLEAN="yes"
            shift
            ;;
        -h|--help)
            usage
            ;;
        *)
            echo "Unknown option: $1"
            usage
            ;;
    esac
done

echo "=========================================="
echo "ForGE Build Script"
echo "=========================================="
echo "Build System: $BUILD_SYSTEM"
echo "Build Type:   $BUILD_TYPE"
echo "Examples:     $BUILD_EXAMPLES"
echo "Tests:        $BUILD_TESTS"
echo "=========================================="
echo ""

# Clean if requested
if [ "$CLEAN" = "yes" ]; then
    echo "Cleaning..."
    if [ "$BUILD_SYSTEM" = "fpm" ]; then
        rm -rf build/
    elif [ "$BUILD_SYSTEM" = "cmake" ]; then
        rm -rf build/ CMakeCache.txt CMakeFiles/
    fi
    echo "Clean complete."
    echo ""
fi

# Build with selected system
if [ "$BUILD_SYSTEM" = "fpm" ]; then
    echo "Building with fpm..."
    
    FPM_OPTS="--profile $BUILD_TYPE"
    
    if [ "$BUILD_EXAMPLES" = "yes" ]; then
        FPM_OPTS="$FPM_OPTS --all-examples"
    fi
    
    fpm build $FPM_OPTS
    
    if [ "$BUILD_TESTS" = "yes" ]; then
        echo "Running tests..."
        fpm test $FPM_OPTS
    fi
    
elif [ "$BUILD_SYSTEM" = "cmake" ]; then
    echo "Building with CMake..."
    
    mkdir -p build
    cd build
    
    CMAKE_OPTS="-DCMAKE_BUILD_TYPE=$BUILD_TYPE"
    
    if [ "$BUILD_EXAMPLES" = "yes" ]; then
        CMAKE_OPTS="$CMAKE_OPTS -DFORGE_BUILD_EXAMPLES=ON"
    else
        CMAKE_OPTS="$CMAKE_OPTS -DFORGE_BUILD_EXAMPLES=OFF"
    fi
    
    if [ "$BUILD_TESTS" = "yes" ]; then
        CMAKE_OPTS="$CMAKE_OPTS -DFORGE_BUILD_TESTS=ON"
    else
        CMAKE_OPTS="$CMAKE_OPTS -DFORGE_BUILD_TESTS=OFF"
    fi
    
    cmake .. $CMAKE_OPTS
    cmake --build . --config $BUILD_TYPE
    
    if [ "$BUILD_TESTS" = "yes" ]; then
        echo "Running tests..."
        ctest -C $BUILD_TYPE --output-on-failure
    fi
    
    cd ..
else
    echo "Error: Unknown build system: $BUILD_SYSTEM"
    echo "Use 'fpm' or 'cmake'"
    exit 1
fi

echo ""
echo "=========================================="
echo "Build complete!"
echo "=========================================="

