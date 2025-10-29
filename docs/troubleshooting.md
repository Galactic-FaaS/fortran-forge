# Troubleshooting Guide

This guide helps resolve common issues encountered when developing with ForGE, including build problems, runtime errors, and platform-specific challenges.

## Build Issues

### Compiler Errors

#### "Cannot find module forge"

**Problem**: Compiler cannot locate ForGE modules.

**Solutions**:

1. **Check installation**:
   ```bash
   # Verify ForGE is installed
   find /usr/local -name "forge.mod" 2>/dev/null

   # Check if ForGE is in module path
   gfortran -I/usr/local/include/forge -c test.f90
   ```

2. **Set module path**:
   ```bash
   # Add to compiler flags
   gfortran -I/path/to/forge/modules your_file.f90

   # Or set environment variable
   export FORGE_MODULE_PATH=/path/to/forge/modules
   ```

3. **Reinstall ForGE**:
   ```bash
   # With CMake
   cd build && make uninstall && make install

   # With FPM
   fpm install --prefix=/usr/local
   ```

#### "Undefined reference to forge_*"

**Problem**: Linker cannot find ForGE library functions.

**Solutions**:

1. **Link library**:
   ```bash
   # Add library path and link
   gfortran -L/usr/local/lib -lforge your_file.f90

   # With CMake
   target_link_libraries(your_app ForGE::forge)
   ```

2. **Check library installation**:
   ```bash
   # Verify library exists
   ls -la /usr/local/lib/libforge.*

   # Check if library is in linker path
   ldconfig -p | grep forge
   ```

3. **Static vs dynamic linking**:
   ```bash
   # Force static linking
   gfortran -static -lforge your_file.f90

   # Or dynamic linking
   gfortran -Wl,-rpath,/usr/local/lib -lforge your_file.f90
   ```

### CMake Issues

#### "Could NOT find ForGE"

**Problem**: CMake cannot find ForGE installation.

**Solutions**:

1. **Set CMAKE_PREFIX_PATH**:
   ```bash
   export CMAKE_PREFIX_PATH=/usr/local:$CMAKE_PREFIX_PATH
   cmake ..
   ```

2. **Specify ForGE path**:
   ```cmake
   # In CMakeLists.txt
   find_package(ForGE REQUIRED PATHS /custom/install/path)
   ```

3. **Check ForGEConfig.cmake**:
   ```bash
   find /usr/local -name "ForGEConfig.cmake"
   ```

#### Build failures

**Problem**: Compilation fails during build.

**Solutions**:

1. **Check compiler version**:
   ```bash
   gfortran --version  # Should be 9.0+
   ```

2. **Enable verbose output**:
   ```bash
   make VERBOSE=1
   # or
   cmake --build . --verbose
   ```

3. **Clean rebuild**:
   ```bash
   rm -rf build/
   mkdir build && cd build
   cmake .. && make clean && make
   ```

### FPM Issues

#### "Package not found"

**Problem**: FPM cannot find ForGE package.

**Solutions**:

1. **Check fpm.toml**:
   ```toml
   [dependencies]
   ForGE = { git = "https://github.com/your-org/fortran-forge.git" }
   ```

2. **Update fpm**:
   ```bash
   fpm update
   ```

3. **Local installation**:
   ```bash
   # Install locally
   fpm install --prefix=./local
   export PATH=./local/bin:$PATH
   ```

## Runtime Issues

### Backend Initialization

#### "Backend not initialized"

**Problem**: Application fails to start with backend error.

**Solutions**:

1. **Check backend availability**:
   ```fortran
   use forge
   type(forge_application) :: app
   type(forge_status) :: status

   ! Try different backends
   call app%init(BACKEND_CUSTOM, status)
   if (status%is_error()) then
       write(*,*) "Custom backend failed, trying Qt..."
       call app%init(BACKEND_QT, status)
   end if
   ```

2. **Verify backend dependencies**:
   ```bash
   # For Qt backend
   pkg-config --exists Qt5Widgets

   # For GTK backend
   pkg-config --exists gtk+-3.0
   ```

3. **Check platform support**:
   ```fortran
   ! Check what backends are available
   call print_available_backends()
   ```

### Window Creation

#### "Window not appearing"

**Problem**: Created window doesn't show on screen.

**Solutions**:

1. **Call show() method**:
   ```fortran
   type(forge_window_t) :: window

   window = app%create_window("Title", 800, 600)
   call window%show()  ! Don't forget this!
   ```

2. **Check event loop**:
   ```fortran
   ! Ensure event loop is running
   call app%run()  ! This blocks until app closes
   ```

3. **Platform-specific issues**:
   ```bash
   # Linux: Check X11/display
   echo $DISPLAY

   # Windows: Check console output for errors
   # macOS: Check system permissions
   ```

### Widget Issues

#### "Widget not responding"

**Problem**: Widgets don't respond to user input.

**Solutions**:

1. **Check widget state**:
   ```fortran
   ! Ensure widget is enabled
   call widget%enable()

   ! Check if widget is visible
   if (.not. widget%is_visible()) then
       call widget%show()
   end if
   ```

2. **Verify event connections**:
   ```fortran
   ! Check signal/slot connections
   call button%on_click(handle_click)

   subroutine handle_click(event)
       type(forge_event), intent(in) :: event
       write(*,*) "Button clicked!"  ! Add debug output
   end subroutine handle_click
   ```

3. **Backend compatibility**:
   ```fortran
   ! Some widgets may not be implemented in all backends
   ! Check backend documentation for supported widgets
   ```

### Memory Issues

#### Memory leaks

**Problem**: Application memory usage grows over time.

**Solutions**:

1. **Check allocations**:
   ```fortran
   ! Ensure deallocate matches allocate
   real, allocatable :: data(:)
   allocate(data(100))
   ! ... use data ...
   deallocate(data)  ! Don't forget!
   ```

2. **Use memory debugging**:
   ```bash
   # Compile with bounds checking
   gfortran -fcheck=all -fbounds-check your_app.f90

   # Use Valgrind (Linux)
   valgrind --leak-check=full ./your_app
   ```

3. **Profile memory usage**:
   ```fortran
   ! Add memory tracking
   call track_memory_usage()
   ```

### Signal/Slot Problems

#### "Signal not connected"

**Problem**: Signals aren't triggering connected slots.

**Solutions**:

1. **Verify connection syntax**:
   ```fortran
   ! Correct syntax
   call button%on_click(my_handler)

   ! Wrong syntax
   ! call button%connect(my_handler)  ! Wrong method name
   ```

2. **Check procedure signature**:
   ```fortran
   ! Correct signature
   subroutine my_handler(event)
       type(forge_event), intent(in) :: event
   end subroutine my_handler

   ! Wrong signature
   ! subroutine my_handler()  ! Missing event parameter
   ```

3. **Debug connections**:
   ```fortran
   ! Enable connection debugging
   call enable_signal_debugging(.true.)
   ```

## Platform-Specific Issues

### Linux

#### X11/display issues

**Problem**: GUI doesn't appear on Linux systems.

**Solutions**:

1. **Check display**:
   ```bash
   echo $DISPLAY  # Should show :0 or similar
   ```

2. **X11 forwarding**:
   ```bash
   # For SSH connections
   ssh -X username@host
   ```

3. **Virtual display**:
   ```bash
   # Use Xvfb for headless systems
   Xvfb :99 &
   export DISPLAY=:99
   ```

#### GTK/Qt conflicts

**Problem**: Conflicts between GTK and Qt applications.

**Solutions**:

1. **Use different backends**:
   ```fortran
   ! Use custom backend to avoid conflicts
   call app%init(BACKEND_CUSTOM, status)
   ```

2. **Set environment variables**:
   ```bash
   export GTK_MODULES=""
   export QT_QPA_PLATFORM=xcb
   ```

### Windows

#### Console window

**Problem**: Console window appears with GUI application.

**Solutions**:

1. **Use Windows subsystem**:
   ```fortran
   ! Compile with -mwindows flag
   gfortran -mwindows your_app.f90 -lforge
   ```

2. **Separate console/logic**:
   ```fortran
   ! Keep console for debugging, hide for release
   #ifdef DEBUG
       ! Show console
   #else
       ! Hide console
   #endif
   ```

#### Path issues

**Problem**: File paths don't work correctly.

**Solutions**:

1. **Use platform-independent paths**:
   ```fortran
   use forge_platform, only: get_app_data_directory

   character(len=:), allocatable :: config_path
   config_path = get_app_data_directory() // "/config.ini"
   ```

2. **Handle backslashes**:
   ```fortran
   ! Windows uses backslashes, but forward slashes work too
   config_path = "C:/Users/User/AppData/config.ini"
   ```

### macOS

#### Permissions

**Problem**: Application lacks necessary permissions.

**Solutions**:

1. **App sandbox**:
   ```xml
   <!-- Add to Info.plist -->
   <key>com.apple.security.app-sandbox</key>
   <true/>
   ```

2. **Grant permissions**:
   ```bash
   # Grant accessibility permissions
   tccutil reset Accessibility your.app.bundle.id
   ```

#### Bundle issues

**Problem**: Application doesn't run as bundle.

**Solutions**:

1. **Create proper bundle structure**:
   ```
   MyApp.app/
   ├── Contents/
   │   ├── Info.plist
   │   ├── MacOS/
   │   │   └── my_app
   │   └── Resources/
   │       └── icons/
   ```

2. **Set executable permissions**:
   ```bash
   chmod +x MyApp.app/Contents/MacOS/my_app
   ```

## Tool Issues

### Code Analyzer

#### False positives

**Problem**: Analyzer reports incorrect issues.

**Solutions**:

1. **Configure analyzer**:
   ```bash
   # Create .forge_analysis.ini
   [qt_style]
   allow_custom_naming = true

   [memory]
   strict_pointer_checks = false
   ```

2. **Exclude files**:
   ```bash
   forge_code_analyzer --exclude="generated_*" src/
   ```

### Resource Compiler

#### Resource not found

**Problem**: Embedded resources aren't accessible.

**Solutions**:

1. **Check resource names**:
   ```fortran
   ! Case-sensitive matching
   data => qresource_get('images/logo.png')  ! Correct
   data => qresource_get('Images/Logo.PNG')  ! Wrong case
   ```

2. **Verify compilation**:
   ```bash
   # Check if qrc_*.f90 was generated
   ls -la qrc_*
   ```

### Translation Tools

#### Translations not loading

**Problem**: Application doesn't show translated text.

**Solutions**:

1. **Check QM file**:
   ```bash
   # Verify QM file exists and is readable
   ls -la *.qm
   ```

2. **Load translations**:
   ```fortran
   type(forge_translator) :: translator

   call translator%load("app_es.qm")
   call app%install_translator(translator)
   ```

3. **Mark strings**:
   ```fortran
   ! Ensure strings are marked for translation
   title = tr("Application Title")
   ```

## Performance Issues

### Slow startup

**Problem**: Application takes too long to start.

**Solutions**:

1. **Profile startup**:
   ```fortran
   real :: start_time, end_time
   call cpu_time(start_time)

   ! Startup code here

   call cpu_time(end_time)
   write(*,*) "Startup time:", end_time - start_time
   ```

2. **Lazy initialization**:
   ```fortran
   ! Don't initialize everything at startup
   call defer_initialization(heavy_component)
   ```

3. **Optimize imports**:
   ```fortran
   ! Only import what you need
   use forge, only: forge_application, forge_window_t
   ```

### UI responsiveness

**Problem**: UI becomes unresponsive during operations.

**Solutions**:

1. **Use background threads**:
   ```fortran
   ! Move heavy operations to background
   call run_in_background(heavy_operation)
   ```

2. **Process events**:
   ```fortran
   ! Allow UI updates during long operations
   do i = 1, num_items
       call process_item(i)
       call app%process_events()  ! Allow UI updates
   end do
   ```

3. **Progress indicators**:
   ```fortran
   call progress_bar%set_value(current / total * 100)
   call app%process_events()  ! Update progress display
   ```

## Debugging Techniques

### Logging

Implement comprehensive logging:

```fortran
module debug_utils
contains
    subroutine debug_log(message, level)
        character(len=*), intent(in) :: message
        integer, intent(in) :: level

        if (level <= DEBUG_LEVEL) then
            write(*,*) "[DEBUG] ", message
        end if
    end subroutine debug_log
end module debug_utils

! Usage
call debug_log("Window created", DEBUG_INFO)
call debug_log("Memory allocation failed", DEBUG_ERROR)
```

### Assertions

Add debug assertions:

```fortran
subroutine debug_assert(condition, message)
    logical, intent(in) :: condition
    character(len=*), intent(in) :: message

#ifdef DEBUG
    if (.not. condition) then
        write(*,*) "ASSERTION FAILED: ", message
        call abort()
    end if
#endif
end subroutine debug_assert

! Usage
call debug_assert(allocated(data), "Data not allocated")
call debug_assert(size(data) > 0, "Data is empty")
```

### Breakpoints

Use compiler breakpoints:

```fortran
subroutine debug_breakpoint()
#ifdef DEBUG
    write(*,*) "DEBUG BREAKPOINT - Press Enter to continue"
    read(*,*)
#endif
end subroutine debug_breakpoint

! Usage
call debug_breakpoint()  ! Program pauses here in debug builds
```

## Getting Help

### Documentation

1. **Check API documentation**:
   ```bash
   # Open local docs
   xdg-open docs/index.html
   ```

2. **Search examples**:
   ```bash
   find examples/ -name "*.f90" -exec grep -l "your_issue" {} \;
   ```

### Community Support

1. **GitHub Issues**:
   - Check existing issues
   - Create new issue with minimal reproduction case

2. **Forum/Community**:
   - Post on Fortran forums
   - Ask on Qt forums for Qt-related issues

### Bug Reports

Include in bug reports:

```markdown
**System Information:**
- OS: [Linux/Windows/macOS] [version]
- Compiler: [gfortran/ifort] [version]
- ForGE version: [version/commit]
- Backend: [custom/qt/gtk/tcl]

**Steps to reproduce:**
1. [Step 1]
2. [Step 2]
3. [Step 3]

**Expected behavior:**
[What should happen]

**Actual behavior:**
[What actually happens]

**Code sample:**
```fortran
[Minimal code that reproduces the issue]
```

**Error messages:**
```
[Copy any error messages here]
```
```

This structured approach to troubleshooting will help identify and resolve issues efficiently. Start with the most common solutions and progressively investigate more complex causes.