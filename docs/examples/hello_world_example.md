# Hello World Example

The classic "Hello World" example demonstrating basic ForGE application setup and window creation.

## Overview

This example shows how to:
- Initialize a ForGE application
- Create a window using the builder pattern
- Display the window
- Properly clean up resources

## Code

```fortran
!> @brief Hello World example for ForGE
!> @details Demonstrates creating a simple window with a label
!> @author ForGE Contributors
!> @license GPL-3.0-or-later

program hello_world_example
    use forge
    use forge_stub_backend
    implicit none
    
    type(forge_application) :: app
    type(forge_window_t) :: window
    type(forge_status) :: status
    type(forge_stub_backend_t), target :: stub_backend
    
    ! Print ForGE version
    print '(A,A)', "ForGE Version: ", FORGE_VERSION
    print '(A)', ""
    print '(A)', "=== Hello World Example ==="
    print '(A)', ""
    
    ! Note: Using stub backend for now since real backends not yet implemented
    print '(A)', "NOTE: Using stub backend (no actual GUI will appear)"
    print '(A)', "      Real GUI backends (Tcl/Tk, GTK4, Qt) coming soon!"
    print '(A)', ""
    
    ! Initialize stub backend manually
    call stub_backend%init(status)
    if (status%is_error()) then
        print '(A)', "ERROR: Failed to initialize stub backend"
        call status%print()
        stop 1
    end if
    
    ! Create window using builder pattern
    print '(A)', "Creating window..."
    block
        type(forge_window_builder) :: builder
        call builder%set_title("Hello, ForGE!")
        call builder%set_size(400, 300)
        call builder%set_position(100, 100)
        call builder%set_backend(stub_backend)
        window = builder%build(status)
    end block
    
    if (status%is_error()) then
        print '(A)', "ERROR: Failed to create window"
        call status%print()
        call stub_backend%shutdown()
        stop 1
    end if
    
    ! Show the window
    print '(A)', "Showing window..."
    call window%show()
    
    ! In a real application, this would run the event loop
    ! For now with stub backend, it just prints and exits
    print '(A)', ""
    print '(A)', "With a real backend, the window would appear and the event loop would run."
    print '(A)', "Press Ctrl+C to close (or the window close button in real backends)."
    print '(A)', ""
    
    ! "Run" the event loop (stub version just returns immediately)
    call stub_backend%run()
    
    ! Cleanup
    print '(A)', ""
    print '(A)', "Cleaning up..."
    call window%close()
    call stub_backend%shutdown()
    
    print '(A)', ""
    print '(A)', "=== Example Complete ==="
    
end program hello_world_example
```

## Explanation

### Module Imports

```fortran
use forge
use forge_stub_backend
```

- `forge`: Main ForGE module with all public APIs
- `forge_stub_backend`: Stub backend for demonstration (no real GUI)

### Application Initialization

```fortran
type(forge_application) :: app
type(forge_status) :: status
type(forge_stub_backend_t), target :: stub_backend

call stub_backend%init(status)
```

- `forge_application`: Main application class
- `forge_status`: Error handling and status reporting
- `forge_stub_backend_t`: Backend implementation (stub for demo)

### Window Creation

```fortran
type(forge_window_builder) :: builder
call builder%set_title("Hello, ForGE!")
call builder%set_size(400, 300)
call builder%set_position(100, 100)
call builder%set_backend(stub_backend)
window = builder%build(status)
```

The builder pattern provides a fluent API for window configuration:
- `set_title()`: Window title bar text
- `set_size()`: Window dimensions in pixels
- `set_position()`: Window position on screen
- `set_backend()`: GUI backend to use
- `build()`: Create the window object

### Window Display

```fortran
call window%show()
```

Makes the window visible on screen.

### Event Loop

```fortran
call stub_backend%run()
```

Runs the application's event loop. With the stub backend, this returns immediately. With real backends (Qt, GTK, etc.), this would block until the application closes.

### Cleanup

```fortran
call window%close()
call stub_backend%shutdown()
```

Properly releases resources and shuts down the backend.

## Running the Example

### With fpm

```bash
fpm run hello_world
```

### With CMake

```bash
cd build
cmake --build . --target hello_world_example
./examples/hello_world/hello_world_example
```

### Manual Build

```bash
gfortran -I/path/to/forge/include examples/hello_world/hello_world.f90 -lforge -o hello_world
./hello_world
```

## Expected Output

```
ForGE Version: 1.0.0

=== Hello World Example ===

NOTE: Using stub backend (no actual GUI will appear)
      Real GUI backends (Tcl/Tk, GTK4, Qt) coming soon!

Creating window...
Showing window...

With a real backend, the window would appear and the event loop would run.
Press Ctrl+C to close (or the window close button in real backends).

Cleaning up...

=== Example Complete ===
```

## Key Concepts Demonstrated

1. **Application Lifecycle**: Initialize → Create → Show → Run → Cleanup
2. **Builder Pattern**: Fluent API for object construction
3. **Error Handling**: Status checking and error reporting
4. **Backend Abstraction**: Separation of GUI logic from implementation
5. **Resource Management**: Proper cleanup of resources

## Next Steps

- Try the [button demo](../examples/button_demo.md) for interactive widgets
- Learn about [layouts](../tutorials/layout_managers.md) for widget arrangement
- Explore [event handling](../tutorials/event_handling.md) for user interaction
- See the [widget gallery](../tutorials/widget_gallery.md) for all available widgets