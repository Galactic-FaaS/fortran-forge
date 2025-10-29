# Building ForGE

This document provides instructions for building the ForGE (Fortran GUI Environment) library on different platforms.

## Prerequisites

### Common Requirements
- Fortran compiler (gfortran, ifort, or flang)
- CMake 3.20 or later
- Git

### Platform-Specific Requirements

#### Windows
- MinGW-w64 with gfortran or Intel Fortran Compiler
- MSYS2 or similar Unix-like environment (optional but recommended)

#### Linux
- GCC with gfortran or Intel Fortran Compiler
- Development packages for GUI libraries (if using backends)

#### macOS
- Xcode command line tools
- GCC via Homebrew (`brew install gcc`)

### Optional Dependencies

#### Custom Backend (Recommended)
- Cairo graphics library
  - Windows: Install via MSYS2 (`pacman -S mingw-w64-x86_64-cairo`)
  - Linux: `sudo apt-get install libcairo2-dev`
  - macOS: `brew install cairo`

#### GTK4 Backend
- GTK4 development libraries
  - Linux: `sudo apt-get install libgtk-4-dev`
  - macOS: `brew install gtk4`

#### Qt6 Backend
- Qt6 development libraries
  - Linux: `sudo apt-get install qt6-base-dev`
  - macOS: `brew install qt6`

## Build Methods

### Method 1: CMake (Recommended)

#### Quick Build
```bash
# Create build directory
mkdir build && cd build

# Configure
cmake .. -DCMAKE_BUILD_TYPE=Release

# Build
cmake --build . --config Release

# Install (optional)
cmake --install . --prefix ../install
```

#### Advanced CMake Options
```bash
cmake .. \
    -DCMAKE_BUILD_TYPE=Release \
    -DFORGE_BUILD_SHARED=ON \
    -DFORGE_BUILD_EXAMPLES=ON \
    -DFORGE_BUILD_TESTS=ON \
    -DFORGE_BACKEND_CUSTOM=ON \
    -DFORGE_BACKEND_QT=OFF \
    -DFORGE_BACKEND_GTK4=OFF
```

#### Available CMake Options
- `FORGE_BUILD_SHARED`: Build shared library (default: ON)
- `FORGE_BUILD_EXAMPLES`: Build example programs (default: ON)
- `FORGE_BUILD_TESTS`: Build test suite (default: ON)
- `FORGE_BACKEND_CUSTOM`: Enable custom Cairo backend (default: OFF)
- `FORGE_BACKEND_QT`: Enable Qt backend (default: OFF)
- `FORGE_BACKEND_GTK4`: Enable GTK4 backend (default: OFF)
- `FORGE_BACKEND_TCL_TK`: Enable Tcl/Tk backend (default: ON)

### Method 2: Platform-Specific Scripts

#### Windows
```batch
# Run the Windows build script
scripts\build_windows.bat
```

#### Linux
```bash
# Make script executable and run
chmod +x scripts/build_linux.sh
./scripts/build_linux.sh
```

#### macOS
```bash
# Make script executable and run
chmod +x scripts/build_macos.sh
./scripts/build_macos.sh
```

### Method 3: Fortran Package Manager (FPM)

#### Basic Build
```bash
# Build library
fpm build

# Build with release optimization
fpm build --profile release

# Run tests
fpm test

# Build examples
fpm build --examples
```

#### Platform-Specific Profiles
```bash
# Windows release build
fpm build --profile release-windows

# Linux release build
fpm build --profile release-linux

# macOS release build
fpm build --profile release-macos
```

## Testing

### CMake Build
```bash
cd build
ctest -C Release
```

### FPM Build
```bash
fpm test
```

## Installation

### CMake
```bash
cd build
cmake --install . --prefix /usr/local
```

### FPM
```bash
fpm install
```

## Troubleshooting

### Common Issues

#### CMake Cannot Find Fortran Compiler
- Ensure gfortran/ifort is in PATH
- On Windows, use MinGW-w64 distribution
- On macOS, install GCC via Homebrew

#### Missing Cairo Library
- Install Cairo development libraries for your platform
- Custom backend will be disabled if Cairo is not found

#### Linker Errors
- Ensure all dependencies are installed
- Check that library paths are correct
- On Windows, ensure MinGW libraries are in PATH

#### Permission Errors During Install
- Use `sudo` on Unix systems
- Or install to user directory: `cmake --install . --prefix ~/local`

### Getting Help
- Check the [GitHub Issues](https://github.com/your-org/fortran-forge/issues) for known problems
- Create a new issue with your platform, compiler version, and error messages

## Advanced Configuration

### Custom Compiler Flags
```bash
# CMake
cmake .. -DCMAKE_Fortran_FLAGS="-O3 -march=native"

# FPM
fpm build --flag "-O3 -march=native"
```

### Cross-Compilation
For cross-compiling, set appropriate CMake toolchain files and compiler variables.

### Static Linking
```bash
cmake .. -DFORGE_BUILD_SHARED=OFF
```

## Continuous Integration

ForGE uses GitHub Actions for CI/CD. See `.github/workflows/` for workflow configurations.