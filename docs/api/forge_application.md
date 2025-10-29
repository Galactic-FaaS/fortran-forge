# forge_application

The main application class that manages the ForGE GUI framework lifecycle, backend initialization, and event loop.

## Synopsis

```fortran
type :: forge_application
    private
    class(forge_backend_base), allocatable :: backend
    logical :: initialized = .false.
    type(forge_backend_type) :: backend_type
    type(forge_error) :: error
contains
    procedure :: init => forge_application_init
    procedure :: shutdown => forge_application_shutdown
    procedure :: run => forge_application_run
    procedure :: process_events => forge_application_process_events
    procedure :: is_initialized => forge_application_is_initialized
    procedure :: get_backend_type => forge_application_get_backend_type
    procedure :: create_window => forge_application_create_window
end type forge_application
```

## Description

The `forge_application` class is the central component of any ForGE application. It manages:

- Backend initialization and selection
- Application lifecycle (startup/shutdown)
- Event loop management
- Window creation through backend abstraction

## Methods

### init(backend_id, status)

Initializes the application with the specified backend.

**Parameters:**
- `backend_id` (integer): Backend type identifier (BACKEND_TCL_TK, BACKEND_GTK4, BACKEND_QT, BACKEND_CUSTOM)
- `status` (forge_status, optional): Output status for error reporting

**Example:**
```fortran
type(forge_application) :: app
type(forge_status) :: status

call app%init(BACKEND_CUSTOM, status)
if (status%is_error()) then
    ! Handle initialization error
end if
```

### shutdown()

Shuts down the application and cleans up resources.

**Example:**
```fortran
call app%shutdown()
```

### run()

Runs the main event loop (blocking until application closes).

**Example:**
```fortran
call app%run()  ! Blocks until app closes
```

### process_events()

Processes pending events without blocking (non-blocking event processing).

**Example:**
```fortran
! In a game loop or custom event processing
do while (.not. should_quit)
    call app%process_events()
    ! Do other work...
end do
```

### is_initialized()

Checks if the application has been successfully initialized.

**Returns:** logical - True if initialized, false otherwise

**Example:**
```fortran
if (.not. app%is_initialized()) then
    call app%init(BACKEND_CUSTOM)
end if
```

### get_backend_type()

Gets the current backend type information.

**Returns:** forge_backend_type - Backend type information

**Example:**
```fortran
type(forge_backend_type) :: backend_info

backend_info = app%get_backend_type()
print *, "Backend ID:", backend_info%id
```

### create_window(title, width, height)

Creates a new window using the application's backend.

**Parameters:**
- `title` (character): Window title
- `width` (integer): Window width in pixels
- `height` (integer): Window height in pixels

**Returns:** forge_window_t - Newly created window

**Example:**
```fortran
type(forge_window_t) :: window

window = app%create_window("My Application", 800, 600)
call window%show()
```

## Backend Types

ForGE supports multiple GUI backends:

- `BACKEND_TCL_TK`: Tcl/Tk backend (planned)
- `BACKEND_GTK4`: GTK4 backend (planned)
- `BACKEND_QT`: Qt backend (planned)
- `BACKEND_CUSTOM`: Custom GUI framework backend (implemented)

## Error Handling

All methods that can fail accept an optional `forge_status` parameter for error reporting:

```fortran
type(forge_status) :: status

call app%init(BACKEND_INVALID, status)
if (status%is_error()) then
    print *, "Error:", status%get_message()
end if
```

## Thread Safety

The `forge_application` class is not thread-safe. All GUI operations should be performed from the main thread.

## Example Usage

```fortran
program hello_world
    use forge
    implicit none

    type(forge_application) :: app
    type(forge_window_t) :: window
    type(forge_status) :: status

    ! Initialize with custom backend
    call app%init(BACKEND_CUSTOM, status)
    call forge_check_status(status, abort_on_error=.true.)

    ! Create and show window
    window = app%create_window("Hello ForGE", 400, 300)
    call window%show()

    ! Run event loop
    call app%run()

    ! Cleanup
    call app%shutdown()
end program hello_world
```

## See Also

- [forge_window_t](forge_window_t.md) - Window management
- [forge_backend_base](forge_backend_base.md) - Backend abstraction
- [forge_status](forge_status.md) - Error handling