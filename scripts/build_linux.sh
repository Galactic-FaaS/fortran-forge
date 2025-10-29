#!/bin/bash
# Linux build script for ForGE

set -e

echo "Building ForGE on Linux..."

# Check if CMake is available
if ! command -v cmake &> /dev/null; then
    echo "Error: CMake is not installed"
    exit 1
fi

# Check if Fortran compiler is available
if ! command -v gfortran &> /dev/null && ! command -v ifort &> /dev/null; then
    echo "Error: No Fortran compiler found (gfortran or ifort)"
    exit 1
fi

# Check for required dependencies
echo "Checking for dependencies..."

# Check for Cairo
if ! pkg-config --exists cairo; then
    echo "Warning: Cairo library not found. Custom backend will be disabled."
    echo "Install with: sudo apt-get install libcairo2-dev"
fi

# Check for GTK4 (optional)
if pkg-config --exists gtk4; then
    echo "GTK4 found - GTK4 backend available"
else
    echo "GTK4 not found - GTK4 backend disabled"
fi

# Check for Qt6 (optional)
if command -v qmake6 &> /dev/null; then
    echo "Qt6 found - Qt backend available"
else
    echo "Qt6 not found - Qt backend disabled"
fi

# Create build directory
mkdir -p build
cd build

# Configure with CMake
echo "Configuring with CMake..."
cmake .. \
    -DCMAKE_BUILD_TYPE=Release \
    -DFORGE_BUILD_SHARED=ON \
    -DFORGE_BUILD_EXAMPLES=ON \
    -DFORGE_BUILD_TESTS=ON \
    -DFORGE_BACKEND_CUSTOM=ON

# Build
echo "Building..."
make -j$(nproc)

# Run tests
echo "Running tests..."
ctest

# Install
echo "Installing..."
make install

echo "Build completed successfully!"
echo "Library installed to: $(pwd)/../install"