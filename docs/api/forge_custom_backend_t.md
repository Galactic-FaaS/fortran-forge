# forge_custom_backend_t

Custom GUI backend implementation providing platform-specific window and rendering capabilities.

## Synopsis

```fortran
type, extends(forge_backend_base) :: forge_custom_backend_t
    private
    class(forge_platform_base), allocatable :: platform
    integer :: window_count = 0
contains
    procedure :: init => custom_init
    procedure :: shutdown => custom_shutdown
    procedure :: run => custom_run
    procedure :: process_events => custom_process_events
    procedure :: create_window => custom_create_window
    procedure :: destroy_window => custom_destroy_window
    procedure :: show_window => custom_show_window
    procedure :: hide_window => custom_hide_window
    procedure :: create_widget => custom_create_widget
    procedure :: destroy_widget => custom_destroy_widget
    procedure :: get_name => custom_get_name
end type forge_custom_backend_t
```

## Description

The `forge_custom_backend_t` provides a custom GUI backend that implements ForGE's backend interface using platform-specific APIs. It currently supports:

- **Windows**: Win32 API integration
- **Linux**: X11 window management (planned)
- **macOS**: Cocoa framework (planned)
- Custom rendering pipeline
- Platform abstraction layer

This backend is the primary implementation for ForGE and provides full control over GUI rendering and platform integration.

## Properties

- `platform`: Platform-specific implementation (forge_platform_windows, etc.)
- `window_count`: Number of windows created

## Methods

### Initialization

#### init(status)
Initializes the custom backend and platform layer.

**Parameters:**
- `status` (forge_status, out): Operation result

**Example:**
```fortran
type(forge_custom_backend_t) :: backend
type(forge_status) :: status

call backend%init(status)
if (status%is_error()) then
    print *, "Backend initialization failed"
end if
```

#### shutdown()
Shuts down the backend and cleans up platform resources.

**Example:**
```fortran
call backend%shutdown()
```

### Event Loop

#### run()
Starts the main event loop (blocking).

**Example:**
```fortran
call backend%run()  ! Blocks until application exits
```

#### process_events()
Processes pending events without blocking.

**Example:**
```fortran
! In a game loop
do while (.not. done)
    call backend%process_events()
    call update_game()
end do
```

### Window Management

#### create_window(handle, title, width, height, status)
Creates a new window.

**Parameters:**
- `handle` (forge_window_handle, out): Window handle
- `title` (character): Window title
- `width` (integer): Window width
- `height` (integer): Window height
- `status` (forge_status, out): Operation result

**Example:**
```fortran
type(forge_window_handle) :: handle
type(forge_status) :: status

call backend%create_window(handle, "My Window", 800, 600, status)
```

#### destroy_window(handle)
Destroys a window.

**Parameters:**
- `handle` (forge_window_handle): Window to destroy

**Example:**
```fortran
call backend%destroy_window(handle)
```

#### show_window(handle)
Makes a window visible.

**Parameters:**
- `handle` (forge_window_handle): Window to show

**Example:**
```fortran
call backend%show_window(handle)
```

#### hide_window(handle)
Hides a window.

**Parameters:**
- `handle` (forge_window_handle): Window to hide

**Example:**
```fortran
call backend%hide_window(handle)
```

### Widget Management

#### create_widget(handle, widget_type, parent, status)
Creates a widget (currently placeholder).

**Parameters:**
- `handle` (forge_widget_handle, out): Widget handle
- `widget_type` (character): Type of widget
- `parent` (forge_window_handle): Parent window
- `status` (forge_status, out): Operation result

**Note:** Widget creation is not yet implemented in the custom backend.

#### destroy_widget(handle)
Destroys a widget.

**Parameters:**
- `handle` (forge_widget_handle): Widget to destroy

### Information

#### get_name()
Gets the backend name.

**Returns:** character - "Custom GUI Framework (Windows)"

**Example:**
```fortran
character(len=:), allocatable :: name

name = backend%get_name()
print *, "Using backend: ", name
```

## Platform Support

### Windows Platform
The custom backend uses `forge_platform_windows` for Windows integration:

- Win32 API for window management
- Windows message loop for events
- Direct2D/DirectWrite for rendering (planned)
- Windows theming integration

### Linux Platform (Planned)
Future support for Linux using X11:

- Xlib for window management
- X11 event handling
- Cairo for rendering
- GTK integration option

### macOS Platform (Planned)
Future support for macOS using Cocoa:

- NSWindow for window management
- Cocoa event loop
- Core Graphics/Core Text for rendering

## Rendering Pipeline

The custom backend provides a custom rendering system:

- Hardware-accelerated rendering (planned)
- Vector graphics support via Cairo
- Font rendering with platform fonts
- Custom widget rendering
- Theme-able appearance

## Event Handling

Custom backend provides comprehensive event handling:

- Windows messages (Windows)
- X11 events (Linux, planned)
- Cocoa events (macOS, planned)
- Mouse and keyboard input
- Window management events
- Custom event dispatching

## Performance

The custom backend is optimized for performance:

- Direct platform API calls
- Minimal abstraction overhead
- Hardware acceleration support
- Efficient event processing
- Memory-efficient resource management

## Thread Safety

Backend operations are generally not thread-safe:

- GUI operations must occur on main thread
- Some platforms support background operations
- Check platform documentation for threading details

## Example Usage

```fortran
program custom_backend_demo
    use forge
    implicit none

    type(forge_application) :: app
    type(forge_window_t) :: window
    type(forge_status) :: status

    ! Initialize with custom backend
    call app%init(BACKEND_CUSTOM, status)
    if (.not. status%is_ok()) then
        print *, "Failed to initialize custom backend"
        stop
    end if

    ! Create window
    window = app%create_window("Custom Backend Demo", 800, 600)

    ! Configure window
    call window%set_position(100, 100)

    ! Show window
    call window%show()

    ! Run event loop
    call app%run()

    ! Cleanup
    call app%shutdown()

end program custom_backend_demo
```

## Advanced Features

### Custom Rendering
```fortran
! Access to custom rendering context (planned)
type(forge_rendering_context) :: context

call backend%get_rendering_context(window_handle, context)
call context%draw_rectangle(x, y, width, height)
call context%draw_text(x, y, "Hello World")
```

### Platform-Specific Features
```fortran
! Windows-specific features
if (backend%get_platform_type() == PLATFORM_WINDOWS) then
    ! Use Windows-specific APIs
    call backend%set_windows_style(window_handle, WS_OVERLAPPEDWINDOW)
end if
```

### Custom Event Loop
```fortran
! Custom event processing
logical :: running = .true.

do while (running)
    ! Process events
    call backend%process_events()

    ! Custom logic
    call update_animation()
    call process_network()

    ! Check exit condition
    if (should_exit()) running = .false.
end do
```

## Limitations

Current limitations of the custom backend:

- Widget creation not yet implemented (renders custom widgets)
- Limited platform support (Windows only currently)
- Some advanced features still in development
- No native control integration

## Future Enhancements

Planned improvements:

- Complete widget system implementation
- Linux and macOS platform support
- Hardware-accelerated rendering
- Advanced theming system
- Accessibility features
- Internationalization support

## Troubleshooting

### Backend Initialization Fails
```fortran
type(forge_status) :: status

call backend%init(status)
if (status%is_error()) then
    select case (status%get_code())
    case (FORGE_ERROR_BACKEND)
        print *, "Platform initialization failed - check platform support"
    case (FORGE_ERROR_NOT_IMPLEMENTED)
        print *, "Feature not implemented on this platform"
    end select
end if
```

### Window Creation Fails
```fortran
call backend%create_window(handle, title, width, height, status)
if (status%is_error()) then
    print *, "Window creation failed: ", status%get_message()
    ! Possible causes: invalid dimensions, platform limitations
end if
```

### Performance Issues
```fortran
! Enable performance monitoring (planned)
call backend%enable_performance_monitoring(.true.)
call backend%get_performance_stats(stats)
print *, "FPS: ", stats%frames_per_second
```

## See Also

- [forge_backend_base](forge_backend_base.md) - Backend interface
- [forge_platform_windows](forge_platform_windows.md) - Windows platform implementation
- [forge_application](forge_application.md) - Application management
- [forge_window_t](forge_window_t.md) - Window operations