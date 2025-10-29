# Deployment Guide

This guide covers deploying ForGE applications across different platforms, including build systems, packaging, and distribution strategies.

## Build Systems

### CMake Build System

```cmake
# CMakeLists.txt for ForGE application
cmake_minimum_required(VERSION 3.20)
project(MyForGEApp)

# Find ForGE
find_package(ForGE REQUIRED)

# Create executable
add_executable(my_app src/main.f90)

# Link ForGE
target_link_libraries(my_app ForGE::forge)

# Set Fortran standard
target_compile_options(my_app PRIVATE -std=f2018)

# Install
install(TARGETS my_app DESTINATION bin)
```

### fpm Build System

```toml
# fpm.toml
name = "my-forge-app"
version = "1.0.0"
license = "MIT"
author = "Your Name"
maintainer = "your.email@example.com"
copyright = "Copyright 2025, Your Name"

[dependencies]
forge = { git = "https://github.com/your-org/fortran-forge.git" }

[[executable]]
name = "my_app"
source-dir = "src"
main = "main.f90"

[install]
library = false
```

### Manual Build

```bash
# Manual compilation with gfortran
gfortran -c -I/path/to/forge/include src/main.f90 -o main.o
gfortran main.o -L/path/to/forge/lib -lforge -o my_app

# With OpenMP support (if needed)
gfortran -fopenmp main.o -L/path/to/forge/lib -lforge -o my_app
```

## Platform-Specific Deployment

### Windows Deployment

#### Static Linking
```cmake
# CMake for static linking
target_link_libraries(my_app ForGE::forge_static)

# Or with specific libraries
target_link_libraries(my_app
    ForGE::forge
    # Windows-specific libraries
    kernel32.lib
    user32.lib
    gdi32.lib
)
```

#### Windows Installer

```cmake
# CPack configuration for Windows installer
set(CPACK_GENERATOR "NSIS")
set(CPACK_PACKAGE_NAME "MyForGEApp")
set(CPACK_PACKAGE_VERSION "1.0.0")
set(CPACK_PACKAGE_DESCRIPTION_SUMMARY "My ForGE Application")
set(CPACK_PACKAGE_VENDOR "Your Company")

# Include required DLLs
set(CPACK_NSIS_EXTRA_INSTALL_COMMANDS "
    # Install Visual C++ Redistributables if needed
    ExecWait '\\\"$INSTDIR\\\\vcredist.exe\\\" /quiet /norestart'
")

include(CPack)
```

#### Portable Windows Application

```bash
# Create portable package
mkdir MyApp
copy my_app.exe MyApp/
copy forge.dll MyApp/  # If not statically linked
copy config.ini MyApp/
# Create zip archive
zip -r MyApp.zip MyApp/
```

### Linux Deployment

#### AppImage Creation

```bash
# Create AppImage
# 1. Build your application with static linking if possible
# 2. Create AppDir structure
mkdir -p MyApp.AppDir/usr/bin
mkdir -p MyApp.AppDir/usr/lib
mkdir -p MyApp.AppDir/usr/share/applications
mkdir -p MyApp.AppDir/usr/share/icons/hicolor/256x256/apps

# Copy files
cp my_app MyApp.AppDir/usr/bin/
cp *.so MyApp.AppDir/usr/lib/  # Shared libraries
cp my_app.desktop MyApp.AppDir/usr/share/applications/
cp my_app.png MyApp.AppDir/usr/share/icons/hicolor/256x256/apps/

# Create AppRun script
cat > MyApp.AppDir/AppRun << 'EOF'
#!/bin/bash
HERE="$(dirname "$(readlink -f "${0}")")"
export LD_LIBRARY_PATH="${HERE}/usr/lib:${LD_LIBRARY_PATH}"
exec "${HERE}/usr/bin/my_app" "$@"
EOF
chmod +x MyApp.AppDir/AppRun

# Download and run appimagetool
wget https://github.com/AppImage/AppImageKit/releases/download/continuous/appimagetool-x86_64.AppImage
chmod +x appimagetool-x86_64.AppImage
./appimagetool-x86_64.AppImage MyApp.AppDir MyApp.AppImage
```

#### Debian Package

```cmake
# CMake Debian package configuration
set(CPACK_GENERATOR "DEB")
set(CPACK_DEBIAN_PACKAGE_NAME "my-forge-app")
set(CPACK_DEBIAN_PACKAGE_VERSION "1.0.0")
set(CPACK_DEBIAN_PACKAGE_DESCRIPTION "My ForGE Application")
set(CPACK_DEBIAN_PACKAGE_MAINTAINER "Your Name <your.email@example.com>")
set(CPACK_DEBIAN_PACKAGE_SECTION "utils")
set(CPACK_DEBIAN_PACKAGE_DEPENDS "libc6 (>= 2.27), libgcc-s1 (>= 8)")

include(CPack)
```

#### RPM Package

```cmake
# CMake RPM package configuration
set(CPACK_GENERATOR "RPM")
set(CPACK_RPM_PACKAGE_NAME "my-forge-app")
set(CPACK_RPM_PACKAGE_VERSION "1.0.0")
set(CPACK_RPM_PACKAGE_DESCRIPTION "My ForGE Application")
set(CPACK_RPM_PACKAGE_MAINTAINER "Your Name <your.email@example.com>")
set(CPACK_RPM_PACKAGE_REQUIRES "glibc >= 2.27, libgcc >= 8")

include(CPack)
```

### macOS Deployment

#### macOS Application Bundle

```cmake
# CMake macOS bundle configuration
set(CPACK_GENERATOR "Bundle")
set(CPACK_BUNDLE_NAME "MyForGEApp")
set(CPACK_BUNDLE_ICON "my_app.icns")
set(CPACK_BUNDLE_PLIST "${CMAKE_SOURCE_DIR}/Info.plist")

# Bundle structure
set(CPACK_BUNDLE_STARTUP_COMMAND "MyForGEApp")
set(CPACK_BUNDLE_APPLE_CERT_APP "Developer ID Application: Your Name")

include(CPack)
```

#### Info.plist for macOS

```xml
<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0">
<dict>
    <key>CFBundleExecutable</key>
    <string>MyForGEApp</string>
    <key>CFBundleIdentifier</key>
    <string>com.yourcompany.myforgeapp</string>
    <key>CFBundleName</key>
    <string>MyForGEApp</string>
    <key>CFBundleVersion</key>
    <string>1.0.0</string>
    <key>CFBundleShortVersionString</key>
    <string>1.0</string>
    <key>LSMinimumSystemVersion</key>
    <string>10.12</string>
    <key>CFBundleIconFile</key>
    <string>my_app.icns</string>
    <key>CFBundlePackageType</key>
    <string>APPL</string>
</dict>
</plist>
```

## Cross-Platform Considerations

### Backend Selection Strategy

```fortran
program cross_platform_app
    use forge
    implicit none

    type(forge_application) :: app
    type(forge_status) :: status

    ! Try backends in order of preference for each platform
    call initialize_best_backend(app, status)

contains

    subroutine initialize_best_backend(app, status)
        type(forge_application), intent(out) :: app
        type(forge_status), intent(out) :: status

        ! Platform-specific backend selection
#if defined(__linux__)
        call try_backends(app, [BACKEND_GTK4, BACKEND_QT, BACKEND_CUSTOM], status)
#elif defined(__APPLE__)
        call try_backends(app, [BACKEND_QT, BACKEND_CUSTOM], status)
#elif defined(_WIN32)
        call try_backends(app, [BACKEND_QT, BACKEND_CUSTOM], status)
#else
        call try_backends(app, [BACKEND_CUSTOM], status)
#endif
    end subroutine initialize_best_backend

    subroutine try_backends(app, backend_list, status)
        type(forge_application), intent(out) :: app
        integer, intent(in) :: backend_list(:)
        type(forge_status), intent(out) :: status

        integer :: i

        do i = 1, size(backend_list)
            call app%init(backend_list(i), status)
            if (status%is_ok()) then
                print *, "Selected backend:", backend_list(i)
                return
            end if
        end do

        print *, "Failed to initialize any backend"
    end subroutine try_backends

end program cross_platform_app
```

### Platform-Specific Code

```fortran
module platform_utils
    implicit none

contains

    subroutine get_platform_config(config)
        type(app_config), intent(inout) :: config

#if defined(__linux__)
        config%data_dir = "~/.config/myapp/"
        config%temp_dir = "/tmp/"
#elif defined(__APPLE__)
        config%data_dir = "~/Library/Application Support/MyApp/"
        config%temp_dir = "~/Library/Caches/MyApp/"
#elif defined(_WIN32)
        config%data_dir = getenv("APPDATA") // "\MyApp\"
        config%temp_dir = getenv("TEMP") // "\"
#endif
    end subroutine get_platform_config

    function get_platform_name() result(name)
        character(len=:), allocatable :: name

#if defined(__linux__)
        name = "Linux"
#elif defined(__APPLE__)
        name = "macOS"
#elif defined(_WIN32)
        name = "Windows"
#else
        name = "Unknown"
#endif
    end function get_platform_name

end module platform_utils
```

## Dependency Management

### Static vs Dynamic Linking

```cmake
# Option 1: Static linking (larger binary, no dependencies)
option(BUILD_STATIC "Build with static linking" OFF)
if(BUILD_STATIC)
    target_link_libraries(my_app ForGE::forge_static)
else()
    target_link_libraries(my_app ForGE::forge)
endif()

# Option 2: Dynamic linking with rpath
set(CMAKE_INSTALL_RPATH "${CMAKE_INSTALL_PREFIX}/lib")
set(CMAKE_BUILD_WITH_INSTALL_RPATH TRUE)
target_link_libraries(my_app ForGE::forge)
```

### Required Libraries

```cmake
# Find and link required libraries
find_package(Threads REQUIRED)
find_package(X11 REQUIRED)  # For Linux X11 backend

target_link_libraries(my_app
    ForGE::forge
    Threads::Threads
    ${X11_LIBRARIES}
)
```

## Configuration and Settings

### Application Configuration

```fortran
module app_config
    use json_module  ! If using JSON for config
    implicit none

    type :: application_config
        character(len=:), allocatable :: app_name
        character(len=:), allocatable :: version
        character(len=:), allocatable :: data_directory
        integer :: window_width = 800
        integer :: window_height = 600
        logical :: fullscreen = .false.
        integer :: backend_preference = BACKEND_CUSTOM
    contains
        procedure :: load => config_load
        procedure :: save => config_save
        procedure :: get_default => config_get_default
    end type application_config

contains

    subroutine config_load(this, filename)
        class(application_config), intent(inout) :: this
        character(len=*), intent(in) :: filename

        ! Load from JSON, INI, or custom format
        ! Implementation depends on configuration format chosen
    end subroutine config_load

    subroutine config_save(this, filename)
        class(application_config), intent(in) :: this
        character(len=*), intent(in) :: filename

        ! Save configuration
    end subroutine config_save

    subroutine config_get_default(this)
        class(application_config), intent(inout) :: this

        this%app_name = "My ForGE Application"
        this%version = "1.0.0"
        this%window_width = 800
        this%window_height = 600
        this%fullscreen = .false.
        this%backend_preference = BACKEND_CUSTOM
    end subroutine config_get_default

end module app_config
```

### Runtime Configuration

```fortran
program configurable_app
    use forge
    use app_config
    implicit none

    type(forge_application) :: app
    type(application_config) :: config
    type(forge_window_t) :: window
    type(forge_status) :: status

    ! Load configuration
    call config%load("config.json")
    if (.not. file_exists("config.json")) then
        call config%get_default()
    end if

    ! Initialize with configured backend
    call app%init(config%backend_preference, status)

    ! Create window with configured size
    window = app%create_window(config%app_name, &
                              config%window_width, &
                              config%window_height)

    ! Apply other settings
    if (config%fullscreen) then
        call window%set_fullscreen(.true.)
    end if

    ! Run application
    call app%run()

    ! Save configuration on exit
    call config%save("config.json")

end program configurable_app
```

## Packaging and Distribution

### Docker Containerization

```dockerfile
# Dockerfile for ForGE application
FROM ubuntu:20.04

# Install dependencies
RUN apt-get update && apt-get install -y \
    gfortran \
    libforge-dev \
    && rm -rf /var/lib/apt/lists/*

# Copy application
COPY . /app
WORKDIR /app

# Build application
RUN gfortran -o my_app src/*.f90 -lforge

# Run application
CMD ["./my_app"]
```

### Flatpak for Linux

```ini
# my_app.flatpak
[Application]
name=com.yourcompany.MyApp
runtime=org.freedesktop.Platform/x86_64/21.08
sdk=org.freedesktop.Sdk/x86_64/21.08

[Build]
buildsystem=simple
build-commands=
    gfortran -o my_app src/*.f90 $(pkg-config --libs forge)

[Modules]
# Dependencies would be listed here
```

### Snap Package

```yaml
# snapcraft.yaml
name: my-forge-app
version: '1.0.0'
summary: My ForGE Application
description: A ForGE-based application

grade: stable
confinement: strict

apps:
  my-app:
    command: my_app
    plugs: [home, network]

parts:
  my-app:
    plugin: nil
    source: .
    build-packages: [gfortran, libforge-dev]
    override-build: |
      gfortran -o my_app src/*.f90 -lforge
      install -D my_app $SNAPCRAFT_PART_INSTALL/my_app
```

## Testing Deployment

### Automated Testing

```fortran
program deployment_test
    use forge
    implicit none

    type(forge_application) :: app
    type(forge_window_t) :: window
    type(forge_status) :: status

    ! Test basic functionality
    call test_initialization(app, status)
    call test_window_creation(app, window, status)
    call test_basic_interaction(window)

    print *, "All deployment tests passed!"

contains

    subroutine test_initialization(app, status)
        type(forge_application), intent(out) :: app
        type(forge_status), intent(out) :: status

        call app%init(BACKEND_CUSTOM, status)
        if (.not. status%is_ok()) then
            print *, "Initialization failed:", status%get_message()
            stop 1
        end if
    end subroutine test_initialization

    subroutine test_window_creation(app, window, status)
        type(forge_application), intent(inout) :: app
        type(forge_window_t), intent(out) :: window
        type(forge_status), intent(out) :: status

        window = app%create_window("Test Window", 400, 300)
        call window%show()
    end subroutine test_window_creation

    subroutine test_basic_interaction(window)
        type(forge_window_t), intent(in) :: window

        ! Test basic window operations
        call window%set_title("Test Complete")
        call sleep(1)  ! Brief pause to verify window appears
    end subroutine test_basic_interaction

end program deployment_test
```

## Continuous Integration

### GitHub Actions Example

```yaml
# .github/workflows/deploy.yml
name: Deploy

on:
  push:
    tags:
      - 'v*'

jobs:
  build-and-deploy:
    runs-on: ${{ matrix.os }}
    strategy:
      matrix:
        os: [ubuntu-latest, windows-latest, macos-latest]

    steps:
    - uses: actions/checkout@v2

    - name: Setup Fortran
      uses: fortran-lang/setup-fortran@v1

    - name: Build with fpm
      run: fpm build --release

    - name: Run tests
      run: fpm test

    - name: Package application
      run: |
        mkdir deploy
        cp build/gfortran_*/*/my_app deploy/
        # Copy dependencies...

    - name: Upload artifacts
      uses: actions/upload-artifact@v2
      with:
        name: my-app-${{ matrix.os }}
        path: deploy/
```

## Troubleshooting Deployment

### Common Issues

#### Missing Libraries

```bash
# Linux: Check for missing shared libraries
ldd my_app

# Install missing dependencies
sudo apt-get install libforge1  # Example package name
```

#### Backend Initialization Failure

```fortran
! Add fallback backends
call app%init(BACKEND_QT, status)
if (.not. status%is_ok()) then
    print *, "Qt failed, trying custom backend"
    call app%init(BACKEND_CUSTOM, status)
end if
```

#### Permission Issues

```bash
# Set executable permissions
chmod +x my_app

# For AppImages on Linux
chmod +x MyApp.AppImage
```

#### Path Issues

```fortran
! Use absolute paths for resources
character(len=:), allocatable :: exe_path, resource_path

exe_path = get_executable_path()
resource_path = exe_path // "/../resources/"
```

## Best Practices

### Deployment Checklist

- [ ] Test on all target platforms
- [ ] Include all required dependencies
- [ ] Test backend fallback mechanisms
- [ ] Verify configuration loading/saving
- [ ] Check file permissions
- [ ] Test with different screen resolutions
- [ ] Validate accessibility features
- [ ] Check memory usage
- [ ] Verify uninstallation process

### Distribution Strategies

1. **Direct Download**: Simple zip/tar archives
2. **Package Managers**: apt, yum, brew, etc.
3. **App Stores**: Microsoft Store, Snap Store, etc.
4. **Enterprise Deployment**: MSI, pkg installers
5. **Web Deployment**: WebAssembly (future)

### Version Management

```fortran
module version_info
    implicit none

    character(len=*), parameter :: APP_VERSION = "1.0.0"
    character(len=*), parameter :: APP_NAME = "My ForGE App"
    character(len=*), parameter :: BUILD_DATE = __DATE__
    character(len=*), parameter :: BUILD_TIME = __TIME__

contains

    subroutine print_version_info()
        print *, APP_NAME, "Version", APP_VERSION
        print *, "Built on", BUILD_DATE, "at", BUILD_TIME
        print *, "ForGE Version:", FORGE_VERSION
    end subroutine print_version_info

end module version_info
```

## Next Steps

- Read the [backend guide](backend_guide.md) for backend-specific deployment
- Explore the [examples](../../examples/) directory for deployment examples
- Learn about [performance optimization](performance_guide.md) for deployed applications
- Study [build instructions](../../docs/BUILDING.md) for compilation details