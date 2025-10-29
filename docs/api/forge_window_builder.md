# forge_window_builder

Fluent API builder for creating and configuring ForGE windows with method chaining.

## Synopsis

```fortran
type :: forge_window_builder
    private
    type(forge_string) :: title
    type(forge_size) :: size
    type(forge_position) :: position
    logical :: resizable = .true.
    logical :: decorated = .true.
    class(forge_backend_base), pointer :: backend => null()
contains
    procedure :: set_title => forge_window_builder_set_title
    procedure :: set_size => forge_window_builder_set_size
    procedure :: set_position => forge_window_builder_set_position
    procedure :: set_resizable => forge_window_builder_set_resizable
    procedure :: set_decorated => forge_window_builder_set_decorated
    procedure :: set_backend => forge_window_builder_set_backend
    procedure :: build => forge_window_builder_build
end type forge_window_builder
```

## Description

The `forge_window_builder` provides a fluent, chainable API for window creation. It follows the builder pattern to allow:

- Method chaining for readable code
- Optional parameter configuration
- Validation of window parameters
- Backend abstraction for cross-platform compatibility

The builder pattern makes window creation more readable and less error-prone than passing many parameters to a constructor.

## Properties

- `title`: Window title text
- `size`: Window dimensions (width, height)
- `position`: Window position on screen
- `resizable`: Whether window can be resized
- `decorated`: Whether window has decorations (title bar, borders)
- `backend`: GUI backend for window creation

## Methods

### set_title(title)

Sets the window title.

**Parameters:**
- `title` (character): Window title text

**Returns:** forge_window_builder - Self reference for chaining

**Example:**
```fortran
builder = builder%set_title("My Application")
```

### set_size(width, height)

Sets the window size.

**Parameters:**
- `width` (integer): Window width in pixels
- `height` (integer): Window height in pixels

**Returns:** forge_window_builder - Self reference for chaining

**Example:**
```fortran
builder = builder%set_size(1024, 768)
```

### set_position(x, y)

Sets the window position.

**Parameters:**
- `x` (integer): X coordinate from screen left
- `y` (integer): Y coordinate from screen top

**Returns:** forge_window_builder - Self reference for chaining

**Example:**
```fortran
builder = builder%set_position(100, 50)
```

### set_resizable(resizable)

Sets whether the window can be resized by the user.

**Parameters:**
- `resizable` (logical): True for resizable, false for fixed size

**Returns:** forge_window_builder - Self reference for chaining

**Example:**
```fortran
builder = builder%set_resizable(.false.)  ! Fixed-size window
```

### set_decorated(decorated)

Sets whether the window has decorations (title bar, borders).

**Parameters:**
- `decorated` (logical): True for decorated, false for undecorated

**Returns:** forge_window_builder - Self reference for chaining

**Example:**
```fortran
builder = builder%set_decorated(.false.)  ! Borderless window
```

### set_backend(backend)

Sets the GUI backend for window creation.

**Parameters:**
- `backend` (forge_backend_base): Backend instance

**Returns:** forge_window_builder - Self reference for chaining

**Example:**
```fortran
builder = builder%set_backend(app%get_backend())
```

### build(status)

Creates the window with the configured parameters.

**Parameters:**
- `status` (forge_status, optional): Operation result

**Returns:** forge_window_t - Created window

**Example:**
```fortran
type(forge_window_t) :: window
type(forge_status) :: status

window = builder%build(status)
if (status%is_error()) then
    ! Handle creation failure
end if
```

## Fluent API Usage

The builder supports method chaining for readable window creation:

### Basic Window
```fortran
type(forge_window_builder) :: builder
type(forge_window_t) :: window

window = builder%set_title("Hello World") &
               %set_size(400, 300) &
               %set_backend(backend) &
               %build()
```

### Full Configuration
```fortran
window = builder%set_title("Advanced Window") &
               %set_size(800, 600) &
               %set_position(200, 150) &
               %set_resizable(.true.) &
               %set_decorated(.true.) &
               %set_backend(backend) &
               %build(status)
```

### Application Integration
```fortran
type(forge_application) :: app

call app%init(BACKEND_CUSTOM)
window = app%create_window("App Window", 640, 480)
! create_window uses builder internally
```

## Parameter Validation

The builder validates parameters during `build()`:

- **Size**: Must be positive dimensions
- **Position**: Can be any valid screen coordinates
- **Title**: Can be empty but not null
- **Backend**: Must be initialized and valid

Invalid parameters result in error status:

```fortran
window = builder%set_size(-100, 0)%build(status)
! status will indicate invalid size
```

## Default Values

Builder provides sensible defaults:

- **Size**: 800x600 pixels
- **Position**: (0,0) - let window manager position
- **Resizable**: True
- **Decorated**: True
- **Title**: Empty string

## Window Types

Different window configurations:

### Standard Window
```fortran
window = builder%set_title("Document") &
               %set_size(1000, 700) &
               %build()
```

### Dialog Window
```fortran
dialog = builder%set_title("Save Changes?") &
               %set_size(300, 150) &
               %set_resizable(.false.) &
               %build()
```

### Fullscreen Window
```fortran
fullscreen = builder%set_size(1920, 1080) &
                   %set_position(0, 0) &
                   %set_decorated(.false.) &
                   %build()
```

### Tool Window
```fortran
tool = builder%set_title("Tools") &
             %set_size(200, 400) &
             %set_resizable(.false.) &
             %build()
```

## Backend Integration

The builder works with any ForGE backend:

```fortran
! Custom backend
call app%init(BACKEND_CUSTOM)
window = builder%set_backend(app%get_backend())%build()

! Qt backend (when available)
call app%init(BACKEND_QT)
window = builder%set_backend(app%get_backend())%build()
```

## Error Handling

Builder operations can fail:

```fortran
type(forge_status) :: status

window = builder%set_backend(invalid_backend)%build(status)
if (status%is_error()) then
    select case (status%get_code())
    case (FORGE_ERROR_NULL_PTR)
        print *, "Backend not set"
    case (FORGE_ERROR_BACKEND)
        print *, "Backend not initialized"
    end select
end if
```

## Performance

Builder is lightweight:

- Minimal memory allocation
- Fast parameter setting
- Efficient validation
- No overhead for unused parameters

## Thread Safety

Builder is not thread-safe. Window creation should occur on the main GUI thread.

## Example Usage

```fortran
program builder_demo
    use forge
    implicit none

    type(forge_application) :: app
    type(forge_window_t) :: main_window, dialog_window
    type(forge_window_builder) :: builder
    type(forge_status) :: status

    ! Initialize application
    call app%init(BACKEND_CUSTOM, status)
    call forge_check_status(status)

    ! Create main window
    main_window = builder%set_title("Main Application") &
                        %set_size(800, 600) &
                        %set_position(100, 100) &
                        %set_backend(app%get_backend()) &
                        %build(status)

    if (status%is_error()) then
        print *, "Main window creation failed"
        stop
    end if

    ! Create dialog window
    dialog_window = builder%set_title("Settings") &
                          %set_size(400, 300) &
                          %set_resizable(.false.) &
                          %set_backend(app%get_backend()) &
                          %build(status)

    ! Show windows
    call main_window%show()
    call dialog_window%show()

    ! Run application
    call app%run()

    ! Cleanup
    call app%shutdown()

end program builder_demo
```

## Advanced Usage

### Builder Reuse
```fortran
type(forge_window_builder) :: builder

! Create multiple similar windows
do i = 1, 3
    write(title, "('Window ', I0)") i
    window(i) = builder%set_title(trim(title)) &
                      %set_size(300, 200) &
                      %set_position(50 + i*50, 50 + i*50) &
                      %build()
end do
```

### Configuration Objects
```fortran
type :: window_config
    character(len=:), allocatable :: title
    integer :: width, height
    logical :: resizable
end type window_config

function create_window_from_config(builder, config, backend) result(window)
    type(forge_window_builder), intent(inout) :: builder
    type(window_config), intent(in) :: config
    class(forge_backend_base), intent(in) :: backend
    type(forge_window_t) :: window

    window = builder%set_title(config%title) &
                   %set_size(config%width, config%height) &
                   %set_resizable(config%resizable) &
                   %set_backend(backend) &
                   %build()
end function create_window_from_config
```

### Validation Extensions
```fortran
function build_with_validation(builder, min_width, min_height) result(window)
    type(forge_window_builder), intent(inout) :: builder
    integer, intent(in) :: min_width, min_height
    type(forge_window_t) :: window
    type(forge_status) :: status

    ! Add custom validation
    window = builder%build(status)
    if (status%is_ok()) then
        if (window%get_size()%get_width() < min_width .or. &
            window%get_size()%get_height() < min_height) then
            call status%set(FORGE_ERROR_INVALID_ARG, "Window too small")
        end if
    end if

    ! Handle validation failure...
end function build_with_validation
```

## See Also

- [forge_window_t](forge_window_t.md) - Window class
- [forge_application](forge_application.md) - Application window creation
- [forge_backend_base](forge_backend_base.md) - Backend abstraction