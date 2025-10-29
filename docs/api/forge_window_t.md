# forge_window_t

Window management class providing high-level window creation, configuration, and lifecycle management.

## Synopsis

```fortran
type :: forge_window_t
    private
    type(forge_window_handle) :: handle
    type(forge_string) :: title
    type(forge_size) :: size
    type(forge_position) :: position
    logical :: visible = .false.
    logical :: resizable = .true.
    logical :: decorated = .true.
    class(forge_backend_base), pointer :: backend => null()
    type(forge_event_handler) :: close_handler
contains
    procedure :: show => forge_window_show
    procedure :: hide => forge_window_hide
    procedure :: close => forge_window_close
    procedure :: set_title => forge_window_set_title
    procedure :: set_size => forge_window_set_size
    procedure :: set_position => forge_window_set_position
    procedure :: get_size => forge_window_get_size
    procedure :: get_position => forge_window_get_position
    procedure :: is_visible => forge_window_is_visible
    procedure :: set_resizable => forge_window_set_resizable
    procedure :: on_close => forge_window_on_close
    procedure :: get_handle => forge_window_get_handle
end type forge_window_t
```

## Description

The `forge_window_t` class represents a top-level GUI window. It provides methods for:

- Window lifecycle management (show/hide/close)
- Window properties (title, size, position)
- Window configuration (resizable, decorated)
- Event handling (close events)
- Backend abstraction for cross-platform compatibility

## Methods

### show()

Makes the window visible on screen.

**Example:**
```fortran
type(forge_window_t) :: window

! Create window...
call window%show()
```

### hide()

Hides the window from the screen without closing it.

**Example:**
```fortran
call window%hide()  ! Window becomes invisible but still exists
```

### close()

Closes and destroys the window, releasing all associated resources.

**Example:**
```fortran
call window%close()  ! Window is destroyed
```

### set_title(title)

Sets the window title displayed in the title bar.

**Parameters:**
- `title` (character): New window title

**Example:**
```fortran
call window%set_title("My Application v1.0")
```

### set_size(width, height)

Sets the window size in pixels.

**Parameters:**
- `width` (integer): Window width in pixels
- `height` (integer): Window height in pixels

**Example:**
```fortran
call window%set_size(1024, 768)
```

### set_position(x, y)

Sets the window position on screen.

**Parameters:**
- `x` (integer): X coordinate from left edge of screen
- `y` (integer): Y coordinate from top edge of screen

**Example:**
```fortran
call window%set_position(100, 50)  ! Position at (100, 50)
```

### get_size()

Gets the current window size.

**Returns:** forge_size - Current window dimensions

**Example:**
```fortran
type(forge_size) :: size

size = window%get_size()
print *, "Width:", size%width, "Height:", size%height
```

### get_position()

Gets the current window position.

**Returns:** forge_position - Current window coordinates

**Example:**
```fortran
type(forge_position) :: pos

pos = window%get_position()
print *, "X:", pos%x, "Y:", pos%y
```

### is_visible()

Checks if the window is currently visible.

**Returns:** logical - True if visible, false otherwise

**Example:**
```fortran
if (window%is_visible()) then
    print *, "Window is visible"
end if
```

### set_resizable(resizable)

Sets whether the window can be resized by the user.

**Parameters:**
- `resizable` (logical): True to allow resizing, false to prevent

**Example:**
```fortran
call window%set_resizable(.false.)  ! Fixed-size window
```

### on_close(callback)

Registers a callback for window close events.

**Parameters:**
- `callback` (procedure): Event callback subroutine

**Example:**
```fortran
call window%on_close(on_window_close)

! Callback subroutine
subroutine on_window_close(event)
    type(forge_event), intent(in) :: event
    print *, "Window is closing"
    ! Perform cleanup...
end subroutine on_window_close
```

### get_handle()

Gets the internal window handle for backend operations.

**Returns:** forge_window_handle - Backend-specific window handle

**Example:**
```fortran
type(forge_window_handle) :: handle

handle = window%get_handle()
! Use handle for backend-specific operations
```

## Window Builder Pattern

Windows are typically created using the `forge_window_builder` for fluent configuration:

```fortran
type(forge_window_builder) :: builder
type(forge_window_t) :: window
type(forge_status) :: status

window = builder%set_title("My Window") &
               %set_size(800, 600) &
               %set_position(100, 100) &
               %set_resizable(.true.) &
               %set_backend(app%get_backend()) &
               %build(status)
```

## Window States

Windows can be in different states:

- **Hidden**: Created but not visible (`visible = .false.`)
- **Visible**: Displayed on screen (`visible = .true.`)
- **Closed**: Destroyed and resources freed

## Event Handling

Windows support close events through the observer pattern:

```fortran
! Register close handler
call window%on_close(handle_close)

! Event callback
subroutine handle_close(event)
    type(forge_event), intent(in) :: event

    select case (event%type)
    case (EVENT_WINDOW_CLOSED)
        ! User clicked close button or pressed Alt+F4
        ! Perform cleanup, save data, etc.
        call cleanup_resources()
    end select
end subroutine handle_close
```

## Platform Considerations

Window behavior may vary across platforms:

- **Windows**: Standard window decorations, taskbar integration
- **macOS**: Unified title bar, different close behavior
- **Linux**: Window manager dependent decorations and behavior

## Memory Management

Windows automatically manage their resources:

- Window handles are managed by the backend
- Fortran objects are deallocated when `close()` is called
- Event handlers are automatically cleaned up

## Thread Safety

Window operations are not thread-safe. All window manipulation should occur on the main GUI thread.

## Example Usage

```fortran
program window_demo
    use forge
    implicit none

    type(forge_application) :: app
    type(forge_window_t) :: window
    type(forge_status) :: status

    ! Initialize application
    call app%init(BACKEND_CUSTOM, status)

    ! Create window
    window = app%create_window("Demo Window", 640, 480)

    ! Configure window
    call window%set_position(200, 150)
    call window%set_resizable(.false.)

    ! Register event handler
    call window%on_close(on_close_handler)

    ! Show window
    call window%show()

    ! Run application
    call app%run()

    ! Cleanup
    call app%shutdown()

contains

    subroutine on_close_handler(event)
        type(forge_event), intent(in) :: event
        print *, "Window closed by user"
    end subroutine on_close_handler

end program window_demo
```

## See Also

- [forge_window_builder](forge_window_builder.md) - Window creation
- [forge_application](forge_application.md) - Application management
- [forge_event](forge_event.md) - Event handling