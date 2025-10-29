@echo off
REM Windows build script for ForGE

echo Building ForGE on Windows...

REM Check if CMake is available
cmake --version >nul 2>&1
if %errorlevel% neq 0 (
    echo Error: CMake is not installed or not in PATH
    exit /b 1
)

REM Check if Fortran compiler is available
gfortran --version >nul 2>&1
if %errorlevel% neq 0 (
    ifort --version >nul 2>&1
    if %errorlevel% neq 0 (
        echo Error: No Fortran compiler found (gfortran or ifort)
        exit /b 1
    )
)

REM Create build directory
if not exist build (
    mkdir build
)
cd build

REM Configure with CMake
echo Configuring with CMake...
cmake .. -G "MinGW Makefiles" ^
    -DCMAKE_BUILD_TYPE=Release ^
    -DFORGE_BUILD_SHARED=ON ^
    -DFORGE_BUILD_EXAMPLES=ON ^
    -DFORGE_BUILD_TESTS=ON ^
    -DFORGE_BACKEND_CUSTOM=ON

if %errorlevel% neq 0 (
    echo Error: CMake configuration failed
    cd ..
    exit /b 1
)

REM Build
echo Building...
cmake --build . --config Release

if %errorlevel% neq 0 (
    echo Error: Build failed
    cd ..
    exit /b 1
)

REM Run tests
echo Running tests...
ctest -C Release

if %errorlevel% neq 0 (
    echo Warning: Some tests failed
)

REM Install
echo Installing...
cmake --install . --prefix ../install

echo Build completed successfully!
echo Library installed to: %cd%\..\install
cd ..