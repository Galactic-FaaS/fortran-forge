# Troubleshooting Guide

This guide helps diagnose and resolve common issues when developing with ForGE, including build problems, runtime errors, and performance issues.

## Build Issues

### Compiler Errors

#### Module Not Found

**Problem:**
```
Error: Can't open module file 'forge.mod' for reading
```

**Solutions:**
1. **Check ForGE installation:**
   ```bash
   # Verify ForGE is installed
   find /usr/local -name "forge.mod"
   
   # Or check fpm installation
   fpm list --installed
   ```

2. **Set correct include path:**
   ```bash
   # With gfortran
   gfortran -I/path/to/forge/include your_file.f90
   
   # With fpm
   fpm build --include /path/to/forge/include
   ```

3. **Check module case sensitivity:**
   ```fortran
   ! Correct
   use forge
   
   ! Incorrect
   use Forge
   use FORGE
   ```

#### Symbol Not Found

**Problem:**
```
Error: Symbol 'forge_application' not found in module 'forge'
```

**Solutions:**
1. **Check ForGE version:**
   ```fortran
   print *, "ForGE version:", FORGE_VERSION
   ```

2. **Verify correct module usage:**
   ```fortran
   use forge  ! Includes all public symbols
   ! or
   use forge_types
   use forge_application  ! Specific modules
   ```

3. **Check for API changes:**
   - Review the [API documentation](../api/) for current symbols
   - Check [changelog](../../CHANGELOG.md) for breaking changes

### Linker Errors

#### Undefined References

**Problem:**
```
undefined reference to `forge_application_init'
```

**Solutions:**
1. **Link ForGE library:**
   ```bash
   # With gfortran
   gfortran your_file.o -lforge -L/path/to/forge/lib
   
   # With fpm
   fpm build  # Should handle linking automatically
   ```

2. **Check library order:**
   ```bash
   # Libraries must be ordered correctly
   gfortran main.o -lforge -lother_libs
   ```

3. **Verify library architecture:**
   ```bash
   # Check if library matches your system
   file /path/to/libforge.so
   ```

#### Missing Shared Libraries

**Problem:**
```
libforge.so: cannot open shared object file: No such file or directory
```

**Solutions:**
1. **Set library path:**
   ```bash
   export LD_LIBRARY_PATH=/path/to/forge/lib:$LD_LIBRARY_PATH
   ```

2. **Install in system location:**
   ```bash
   sudo make install  # If building from source
   ```

3. **Use static linking:**
   ```cmake
   target_link_libraries(your_app forge_static)
   ```

### CMake Issues

#### Package Not Found

**Problem:**
```
CMake Error: Could not find ForGE
```

**Solutions:**
1. **Install ForGE with CMake support:**
   ```bash
   cd fortran-forge
   mkdir build && cd build
   cmake .. -DCMAKE_INSTALL_PREFIX=/usr/local
   make install
   ```

2. **Set CMake module path:**
   ```cmake
   list(APPEND CMAKE_MODULE_PATH "/path/to/forge/cmake")
   find_package(ForGE REQUIRED)
   ```

3. **Check CMake version:**
   ```cmake
   cmake_minimum_required(VERSION 3.20)  # ForGE requires 3.20+
   ```

## Runtime Issues

### Backend Initialization

#### Backend Not Available

**Problem:**
```
Backend not implemented
```

**Solutions:**
1. **Check available backends:**
   ```fortran
   ! Try different backends
   call app%init(BACKEND_CUSTOM, status)
   if (.not. status%is_ok()) then
       call app%init(BACKEND_QT, status)
   end if
   ```

2. **Verify backend dependencies:**
   - Custom backend: Usually no external dependencies
   - Qt backend: Requires Qt libraries
   - GTK backend: Requires GTK libraries

3. **Check platform compatibility:**
   ```fortran
   print *, "Platform:", get_platform_name()
   ```

#### Window Creation Fails

**Problem:**
```
Failed to create window
```

**Solutions:**
1. **Check display connection:**
   ```bash
   # Linux
   echo $DISPLAY
   
   # Windows - should work automatically
   ```

2. **Verify window parameters:**
   ```fortran
   ! Check dimensions are reasonable
   if (width <= 0 .or. height <= 0) then
       print *, "Invalid window dimensions"
   end if
   ```

3. **Try different window configurations:**
   ```fortran
   ! Try smaller window first
   window = app%create_window("Test", 400, 300)
   ```

### Widget Issues

#### Widget Not Appearing

**Problem:**
Widgets are created but not visible.

**Solutions:**
1. **Check widget visibility:**
   ```fortran
   call widget%show()
   ```

2. **Verify layout computation:**
   ```fortran
   call layout%set_parent_size(width, height)
   call layout%compute()
   ```

3. **Check widget positioning:**
   ```fortran
   type(forge_position) :: pos
   pos = widget%get_position()
   print *, "Widget at:", pos%x, pos%y
   ```

#### Event Not Firing

**Problem:**
Button clicks or other events are not handled.

**Solutions:**
1. **Verify event registration:**
   ```fortran
   call button%on_click(handle_click)
   
   subroutine handle_click(event)
       type(forge_event), intent(in) :: event
       print *, "Button clicked!"  ! Add debug output
   end subroutine handle_click
   ```

2. **Check event loop:**
   ```fortran
   ! Ensure event loop is running
   call app%run()  ! Blocking
   ! or
   call app%process_events()  ! Non-blocking
   ```

3. **Test with simple callback:**
   ```fortran
   call button%on_click(lambda event: print("Click"))
   ```

### Layout Problems

#### Widgets Overlapping

**Problem:**
Widgets are drawn on top of each other.

**Solutions:**
1. **Check layout computation:**
   ```fortran
   call layout%compute()
   ```

2. **Verify layout size:**
   ```fortran
   call layout%set_parent_size(window_width, window_height)
   ```

3. **Use different layout:**
   ```fortran
   ! Try grid layout for structured arrangement
   type(forge_grid_layout) :: grid
   call grid%set_dimensions(2, 2)
   ```

#### Layout Not Updating

**Problem:**
Layout doesn't update when widgets are added/removed.

**Solutions:**
1. **Call invalidate:**
   ```fortran
   call layout%invalidate()
   call layout%compute()
   ```

2. **Recompute after changes:**
   ```fortran
   call layout%add_widget(new_widget)
   call layout%compute()  ! Required
   ```

## Performance Issues

### Slow Startup

**Problem:**
Application takes too long to start.

**Solutions:**
1. **Profile initialization:**
   ```fortran
   real :: start_time, end_time
   call cpu_time(start_time)
   call app%init(BACKEND_CUSTOM, status)
   call cpu_time(end_time)
   print *, "Init time:", end_time - start_time
   ```

2. **Delay expensive operations:**
   ```fortran
   ! Load data in background or on demand
   call load_data_async()
   ```

3. **Use faster backend:**
   ```fortran
   call app%init(BACKEND_CUSTOM, status)  ! Usually fastest
   ```

### UI Responsiveness

**Problem:**
Interface becomes unresponsive during operations.

**Solutions:**
1. **Move work off main thread:**
   ```fortran
   ! Use background threads for heavy computation
   call perform_calculation_async(callback)
   ```

2. **Show progress indicators:**
   ```fortran
   call progress_bar%set_value(current_progress)
   call app%process_events()  ! Allow UI updates
   ```

3. **Break up long operations:**
   ```fortran
   do i = 1, total_items, batch_size
       call process_batch(items(i:i+batch_size-1))
       call app%process_events()  ! Allow UI updates
   end do
   ```

### Memory Leaks

**Problem:**
Memory usage grows over time.

**Solutions:**
1. **Check object cleanup:**
   ```fortran
   ! Explicitly clean up
   call window%close()
   call app%shutdown()
   ```

2. **Monitor allocations:**
   ```fortran
   ! Use custom allocator with tracking
   call track_allocation(size)
   ```

3. **Profile memory usage:**
   ```fortran
   ! Check memory stats periodically
   call print_memory_stats()
   ```

## Platform-Specific Issues

### Windows Issues

#### Console Window Appears

**Problem:**
Console window appears with GUI application.

**Solutions:**
1. **Use Windows subsystem:**
   ```cmake
   set(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} /SUBSYSTEM:WINDOWS")
   ```

2. **Hide console at runtime:**
   ```fortran
   ! Windows-specific code
   call ShowWindow(GetConsoleWindow(), SW_HIDE)
   ```

#### High DPI Issues

**Problem:**
Interface appears too small on high-DPI displays.

**Solutions:**
1. **Enable DPI awareness:**
   ```fortran
   ! Windows-specific
   call SetProcessDPIAware()
   ```

2. **Use appropriate scaling:**
   ```fortran
   ! Scale fonts and sizes based on DPI
   dpi_scale = get_dpi_scale()
   font_size = base_font_size * dpi_scale
   ```

### Linux Issues

#### Display Not Set

**Problem:**
```
Cannot connect to display
```

**Solutions:**
1. **Check DISPLAY variable:**
   ```bash
   echo $DISPLAY
   export DISPLAY=:0  # Or appropriate display
   ```

2. **Use X11 forwarding:**
   ```bash
   ssh -X username@host
   ```

#### Missing X11 Libraries

**Problem:**
X11-related linking errors.

**Solutions:**
1. **Install X11 development packages:**
   ```bash
   sudo apt-get install libx11-dev libxext-dev
   ```

2. **Link X11 libraries:**
   ```cmake
   find_package(X11 REQUIRED)
   target_link_libraries(app ${X11_LIBRARIES})
   ```

### macOS Issues

#### Bundle Issues

**Problem:**
Application doesn't run as bundle.

**Solutions:**
1. **Create proper bundle structure:**
   ```
   MyApp.app/
   ├── Contents/
   │   ├── Info.plist
   │   ├── MacOS/
   │   │   └── my_app
   │   └── Resources/
   │       └── my_app.icns
   ```

2. **Set executable permissions:**
   ```bash
   chmod +x MyApp.app/Contents/MacOS/my_app
   ```

#### Gatekeeper Issues

**Problem:**
macOS blocks unsigned applications.

**Solutions:**
1. **Sign application:**
   ```bash
   codesign -s "Developer ID Application: Your Name" MyApp.app
   ```

2. **Notarize for distribution:**
   ```bash
   xcrun altool --notarize-app --primary-bundle-id "com.yourcompany.myapp" --username "your@email.com" --password "@keychain:AC_PASSWORD" --file MyApp.app
   ```

## Debugging Techniques

### Debug Output

```fortran
module debug_utils
    implicit none

contains

    subroutine debug_widget(widget, label)
        class(forge_widget), intent(in) :: widget
        character(len=*), intent(in) :: label

        type(forge_size) :: size
        type(forge_position) :: pos

        size = widget%get_size()
        pos = widget%get_position()

        print *, label, "widget:"
        print *, "  Position:", pos%x, pos%y
        print *, "  Size:", size%width, size%height
        print *, "  Visible:", widget%is_visible()
        print *, "  Enabled:", widget%is_enabled()
    end subroutine debug_widget

    subroutine debug_layout(layout, label)
        class(forge_layout_base), intent(in) :: layout
        character(len=*), intent(in) :: label

        print *, label, "layout:"
        print *, "  Spacing:", layout%get_spacing()
        print *, "  Padding:", layout%get_padding()
        print *, "  Parent size:", layout%get_parent_width(), layout%get_parent_height()
    end subroutine debug_layout

end module debug_utils
```

### Event Logging

```fortran
module event_logger
    implicit none

contains

    subroutine log_event(event, context)
        type(forge_event), intent(in) :: event
        character(len=*), intent(in) :: context

        print *, context, "event:"
        print *, "  Type:", event%type
        print *, "  Timestamp:", event%timestamp
        ! Log additional event data...
    end subroutine log_event

    ! Use as event filter
    subroutine logging_event_handler(event)
        type(forge_event), intent(in) :: event

        call log_event(event, "Widget")
        ! Continue with normal processing
    end subroutine logging_event_handler

end module event_logger
```

### Memory Debugging

```fortran
module memory_debugger
    implicit none

    type :: allocation_info
        integer :: line_number
        character(len=:), allocatable :: file_name
        integer(c_size_t) :: size
    end type allocation_info

contains

    subroutine track_allocation(size, file, line)
        integer(c_size_t), intent(in) :: size
        character(len=*), intent(in) :: file
        integer, intent(in) :: line

        type(allocation_info) :: info

        info%file_name = file
        info%line_number = line
        info%size = size

        ! Store allocation info for leak detection
        call store_allocation_info(info)
    end subroutine track_allocation

    subroutine check_for_leaks()
        ! Report any unfreed allocations
        call report_leaked_allocations()
    end subroutine check_for_leaks

end module memory_debugger
```

## Common Error Messages

### "Backend not initialized"

**Cause:** Calling GUI functions before `app%init()`

**Solution:**
```fortran
type(forge_application) :: app
type(forge_status) :: status

call app%init(BACKEND_CUSTOM, status)  ! Initialize first
window = app%create_window("Title", 800, 600)  ! Then create windows
```

### "Invalid window handle"

**Cause:** Using window after `close()` or with invalid handle

**Solution:**
```fortran
if (window%is_valid()) then  ! Check validity
    call window%set_title("New Title")
end if
```

### "Layout not computed"

**Cause:** Forgetting to call `compute()` after layout changes

**Solution:**
```fortran
call layout%add_widget(button)
call layout%set_parent_size(400, 300)
call layout%compute()  ! Required!
```

### "Event callback not set"

**Cause:** Event handler not properly registered

**Solution:**
```fortran
call button%on_click(my_callback)

subroutine my_callback(event)
    type(forge_event), intent(in) :: event
    ! Handle event
end subroutine my_callback
```

## Getting Help

### Debug Information

```fortran
subroutine print_debug_info()
    print *, "ForGE Version:", FORGE_VERSION
    print *, "Platform:", get_platform_name()
    print *, "Compiler:", compiler_version()
    print *, "Build date:", __DATE__, __TIME__
end subroutine print_debug_info
```

### System Information

```fortran
subroutine print_system_info()
    print *, "OS:", get_os_name()
    print *, "Architecture:", get_architecture()
    print *, "Available memory:", get_available_memory()
    print *, "Display:", get_display_info()
end subroutine print_system_info
```

### Creating Test Cases

```fortran
program minimal_test
    ! Minimal test case to isolate issues
    use forge
    implicit none

    type(forge_application) :: app
    type(forge_status) :: status

    call app%init(BACKEND_CUSTOM, status)
    if (.not. status%is_ok()) then
        print *, "Failed:", status%get_message()
        stop 1
    end if

    print *, "Basic initialization works"
end program minimal_test
```

## Best Practices

### Error Handling

```fortran
subroutine robust_app_init()
    type(forge_application) :: app
    type(forge_status) :: status

    ! Try multiple backends
    call app%init(BACKEND_QT, status)
    if (.not. status%is_ok()) then
        call app%init(BACKEND_CUSTOM, status)
    end if

    if (.not. status%is_ok()) then
        print *, "Failed to initialize any backend"
        print *, "Error:", status%get_message()
        stop 1
    end if
end subroutine robust_app_init
```

### Logging

```fortran
module app_logger
    implicit none

contains

    subroutine log_info(message)
        character(len=*), intent(in) :: message
        print *, "[INFO]", message
    end subroutine log_info

    subroutine log_error(message)
        character(len=*), intent(in) :: message
        print *, "[ERROR]", message
    end subroutine log_error

    subroutine log_debug(message)
        character(len=*), intent(in) :: message
#ifdef DEBUG
        print *, "[DEBUG]", message
#endif
    end subroutine log_debug

end module app_logger
```

### Assertions

```fortran
module assertions
    implicit none

contains

    subroutine assert(condition, message)
        logical, intent(in) :: condition
        character(len=*), intent(in) :: message

        if (.not. condition) then
            print *, "Assertion failed:", message
            call print_backtrace()
            stop 1
        end if
    end subroutine assert

    subroutine assert_widget_valid(widget, name)
        class(forge_widget), intent(in) :: widget
        character(len=*), intent(in) :: name

        call assert(widget%is_visible(), name // " should be visible")
        call assert(widget%is_enabled(), name // " should be enabled")
    end subroutine assert_widget_valid

end module assertions
```

## Next Steps

- Read the [API documentation](../api/) for detailed function references
- Explore the [examples](../../examples/) directory for working code
- Check the [backend guide](backend_guide.md) for backend-specific issues
- Review [performance guide](performance_guide.md) for optimization tips