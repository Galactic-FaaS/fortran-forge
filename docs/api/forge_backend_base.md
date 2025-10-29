# forge_backend_base

Abstract base class defining the interface for GUI backend implementations in ForGE.

## Synopsis

```fortran
type, abstract :: forge_backend_base
    private
    logical :: initialized = .false.
    type(forge_backend_type) :: backend_type
contains
    procedure :: init => forge_backend_init
    procedure :: shutdown => forge_backend_shutdown
    procedure :: run => forge_backend_run
    procedure :: process_events => forge_backend_process_events
    procedure :: create_window => forge_backend_create_window
    procedure :: destroy_window => forge_backend_destroy_window
    procedure :: show_window => forge_backend_show_window
    procedure :: hide_window => forge_backend_hide_window
    procedure :: create_widget => forge_backend_create_widget
    procedure :: destroy_widget => forge_backend_destroy_widget
    procedure :: get_name => forge_backend_get_name
    procedure :: is_initialized => forge_backend_is_initialized
end type forge_backend_base
```

## Description

The `forge_backend_base` class defines the contract that all GUI backends must implement. It provides:

- Abstract interface for GUI operations
- Backend initialization and lifecycle management
- Window and widget management
- Event loop abstraction
- Cross-platform compatibility layer

Concrete implementations include:
- `forge_custom_backend_t`: Custom rendering backend
- `forge_qt_backend_t`: Qt framework backend (planned)
- `forge_gtk4_backend_t`: GTK4 backend (planned)
- `forge_tcl_tk_backend_t`: Tcl/Tk backend (planned)

## Properties

- `initialized`: Whether backend is ready for use
- `backend_type`: Backend identification information

## Methods

### Initialization

#### init(status)
Initializes the backend and prepares it for use.

**Parameters:**
- `status` (forge_status, out): Operation result

**Example:**
```fortran
type(forge_status) :: status
call backend%init(status)
if (status%is_error()) then
    ! Handle initialization failure
end if
```

#### shutdown()
Cleans up backend resources and shuts down.

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

#### destroy_window(handle)
Destroys a window.

**Parameters:**
- `handle` (forge_window_handle): Window to destroy

#### show_window(handle)
Makes a window visible.

**Parameters:**
- `handle` (forge_window_handle): Window to show

#### hide_window(handle)
Hides a window.

**Parameters:**
- `handle` (forge_window_handle): Window to hide

### Widget Management

#### create_widget(handle, widget_type, parent, status)
Creates a widget.

**Parameters:**
- `handle` (forge_widget_handle, out): Widget handle
- `widget_type` (character): Type of widget
- `parent` (forge_window_handle): Parent window
- `status` (forge_status, out): Operation result

#### destroy_widget(handle)
Destroys a widget.

**Parameters:**
- `handle` (forge_widget_handle): Widget to destroy

### Information

#### get_name()
Gets the backend name.

**Returns:** character - Backend name

**Example:**
```fortran
character(len=:), allocatable :: name
name = backend%get_name()
print *, "Using backend: ", name
```

#### is_initialized()
Checks if backend is initialized.

**Returns:** logical - Initialization status

## Backend Types

ForGE supports multiple backend types:

### BACKEND_CUSTOM
Custom rendering backend using platform APIs directly.

**Features:**
- Windows: Win32 API
- Linux: X11
- macOS: Cocoa (planned)
- Lightweight and fast
- Full control over rendering

### BACKEND_QT (Planned)
Qt framework backend.

**Features:**
- Cross-platform Qt widgets
- Rich widget set
- Theming support
- Accessibility features

### BACKEND_GTK4 (Planned)
GTK4 backend.

**Features:**
- Native Linux integration
- GNOME desktop integration
- Accessibility support

### BACKEND_TCL_TK (Planned)
Tcl/Tk backend.

**Features:**
- Lightweight and simple
- Good for simple applications
- Cross-platform

## Backend Selection

Backends are selected at application initialization:

```fortran
type(forge_application) :: app

! Use custom backend
call app%init(BACKEND_CUSTOM)

! Use Qt backend (when available)
call app%init(BACKEND_QT)
```

## Handle Types

Backends use opaque handles to abstract platform differences:

```fortran
type :: forge_window_handle
    integer(c_int) :: window_id = 0
    type(c_ptr) :: ptr = c_null_ptr
end type forge_window_handle

type :: forge_widget_handle
    integer(c_int) :: widget_id = 0
    type(c_ptr) :: ptr = c_null_ptr
end type forge_widget_handle
```

## Error Handling

All backend operations return status information:

```fortran
type(forge_status) :: status

call backend%create_window(handle, "Title", 800, 600, status)
if (status%is_error()) then
    print *, "Window creation failed: ", status%get_message()
end if
```

## Thread Safety

Backend operations are generally not thread-safe:

- GUI operations must occur on main thread
- Some backends may support background operations
- Check backend documentation for threading details

## Implementing a Backend

To create a new backend:

```fortran
type, extends(forge_backend_base) :: my_backend
contains
    procedure :: init => my_init
    procedure :: create_window => my_create_window
    ! Implement all abstract methods...
end type my_backend
```

## Performance Considerations

Different backends have different performance characteristics:

- **Custom Backend**: Fastest for simple applications
- **Qt Backend**: Good all-around performance
- **GTK Backend**: Good for complex UIs
- **Tcl/Tk Backend**: Lightweight but limited features

## Platform Support

Backend availability depends on platform:

| Backend | Windows | Linux | macOS |
|---------|---------|-------|-------|
| Custom | ✓ | ✓ | Planned |
| Qt | Planned | Planned | Planned |
| GTK | N/A | ✓ | N/A |
| Tcl/Tk | ✓ | ✓ | ✓ |

## Example Usage

```fortran
program backend_demo
    use forge
    implicit none

    type(forge_application) :: app
    type(forge_window_t) :: window
    type(forge_status) :: status

    ! Initialize with custom backend
    call app%init(BACKEND_CUSTOM, status)
    if (.not. status%is_ok()) then
        print *, "Backend initialization failed"
        stop
    end if

    ! Backend is now ready for window creation
    window = app%create_window("Backend Demo", 400, 300)
    call window%show()

    ! Run event loop
    call app%run()

    ! Cleanup
    call app%shutdown()

end program backend_demo
```

## See Also

- [forge_application](forge_application.md) - Application management
- [forge_window_t](forge_window_t.md) - Window operations
- [forge_custom_backend_t](forge_custom_backend_t.md) - Custom backend implementation