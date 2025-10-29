# forge_types

Fundamental type definitions and utilities used throughout the ForGE library.

## Synopsis

```fortran
! String handling
type :: forge_string
    private
    character(len=:), allocatable :: data
contains
    procedure :: set => forge_string_set
    procedure :: get => forge_string_get
    procedure :: length => forge_string_length
    procedure :: clear => forge_string_clear
end type forge_string

! Color representation
type :: forge_color
    integer(c_int) :: red = 0
    integer(c_int) :: green = 0
    integer(c_int) :: blue = 0
    integer(c_int) :: alpha = 255
contains
    procedure :: set_rgb => forge_color_set_rgb
    procedure :: set_rgba => forge_color_set_rgba
    procedure :: get_red => forge_color_get_red
    procedure :: get_green => forge_color_get_green
    procedure :: get_blue => forge_color_get_blue
    procedure :: get_alpha => forge_color_get_alpha
end type forge_color

! Size representation
type :: forge_size
    integer(c_int) :: width = 0
    integer(c_int) :: height = 0
contains
    procedure :: set => forge_size_set
    procedure :: get_width => forge_size_get_width
    procedure :: get_height => forge_size_get_height
end type forge_size

! Position representation
type :: forge_position
    integer(c_int) :: x = 0
    integer(c_int) :: y = 0
contains
    procedure :: set => forge_position_set
    procedure :: get_x => forge_position_get_x
    procedure :: get_y => forge_position_get_y
end type forge_position

! Rectangle representation
type :: forge_rect
    type(forge_position) :: position
    type(forge_size) :: size
contains
    procedure :: set => forge_rect_set
    procedure :: get_position => forge_rect_get_position
    procedure :: get_size => forge_rect_get_size
    procedure :: contains => forge_rect_contains
end type forge_rect
```

## Description

The `forge_types` module provides fundamental data types and utilities that are used throughout ForGE. These types provide:

- Type-safe geometric operations
- Memory-efficient string handling
- Color representation
- Consistent API across all ForGE components

## forge_string

Efficient string handling with C interop support.

### Methods

#### set(value)
Sets the string value.

**Parameters:**
- `value` (character): String to set

**Example:**
```fortran
type(forge_string) :: str

call str%set("Hello, ForGE!")
```

#### get()
Gets the string value.

**Returns:** character - String content

**Example:**
```fortran
character(len=:), allocatable :: text

text = str%get()
print *, "String: ", text
```

#### length()
Gets the string length.

**Returns:** integer - String length

**Example:**
```fortran
integer :: len

len = str%length()
print *, "Length: ", len
```

#### clear()
Clears the string content.

**Example:**
```fortran
call str%clear()  ! String is now empty
```

## forge_color

RGBA color representation.

### Methods

#### set_rgb(red, green, blue)
Sets RGB color with full alpha.

**Parameters:**
- `red` (integer): Red component (0-255)
- `green` (integer): Green component (0-255)
- `blue` (integer): Blue component (0-255)

**Example:**
```fortran
type(forge_color) :: color

call color%set_rgb(255, 0, 0)  ! Red color
```

#### set_rgba(red, green, blue, alpha)
Sets RGBA color.

**Parameters:**
- `red` (integer): Red component (0-255)
- `green` (integer): Green component (0-255)
- `blue` (integer): Blue component (0-255)
- `alpha` (integer): Alpha component (0-255)

**Example:**
```fortran
call color%set_rgba(0, 255, 0, 128)  ! Semi-transparent green
```

#### Component Accessors
- `get_red()`: Returns red component
- `get_green()`: Returns green component
- `get_blue()`: Returns blue component
- `get_alpha()`: Returns alpha component

## forge_size

Two-dimensional size representation.

### Methods

#### set(width, height)
Sets the size dimensions.

**Parameters:**
- `width` (integer): Width in pixels
- `height` (integer): Height in pixels

**Example:**
```fortran
type(forge_size) :: size

call size%set(800, 600)
```

#### get_width()
Gets the width.

**Returns:** integer - Width in pixels

#### get_height()
Gets the height.

**Returns:** integer - Height in pixels

## forge_position

Two-dimensional position representation.

### Methods

#### set(x, y)
Sets the position coordinates.

**Parameters:**
- `x` (integer): X coordinate
- `y` (integer): Y coordinate

**Example:**
```fortran
type(forge_position) :: pos

call pos%set(100, 50)
```

#### get_x()
Gets the X coordinate.

**Returns:** integer - X position

#### get_y()
Gets the Y coordinate.

**Returns:** integer - Y position

## forge_rect

Rectangle representation combining position and size.

### Methods

#### set(x, y, width, height)
Sets rectangle bounds.

**Parameters:**
- `x` (integer): X position
- `y` (integer): Y position
- `width` (integer): Width
- `height` (integer): Height

**Example:**
```fortran
type(forge_rect) :: rect

call rect%set(10, 20, 200, 100)
```

#### get_position()
Gets the position component.

**Returns:** forge_position - Rectangle position

#### get_size()
Gets the size component.

**Returns:** forge_size - Rectangle size

#### contains(x, y)
Checks if a point is inside the rectangle.

**Parameters:**
- `x` (integer): X coordinate to test
- `y` (integer): Y coordinate to test

**Returns:** logical - True if point is inside

**Example:**
```fortran
if (rect%contains(mouse_x, mouse_y)) then
    print *, "Point is inside rectangle"
end if
```

## Type-bound Procedures

All types provide convenient type-bound procedures:

```fortran
! forge_size
size = forge_size(800, 600)

! forge_position
pos = forge_position(100, 200)

! forge_color
color = forge_color(255, 0, 0, 255)  ! Red with full alpha

! forge_rect
rect = forge_rect(0, 0, 640, 480)  ! Full screen rectangle
```

## Memory Management

Types handle memory automatically:

- `forge_string` uses allocatable character arrays
- All types are lightweight value types
- No manual memory management required

## C Interoperability

Types are designed for C interop:

- Integer components use `c_int`
- Compatible with C struct layouts
- Suitable for passing to backend libraries

## Performance

Types are optimized for performance:

- Minimal memory overhead
- Fast copy operations
- Efficient storage

## Thread Safety

All types are thread-safe for reading. Write operations should be synchronized if shared between threads.

## Example Usage

```fortran
program types_demo
    use forge_types
    implicit none

    type(forge_string) :: title
    type(forge_size) :: window_size
    type(forge_position) :: window_pos
    type(forge_color) :: background
    type(forge_rect) :: viewport

    ! String operations
    call title%set("ForGE Application")
    print *, "Title: ", title%get()
    print *, "Length: ", title%length()

    ! Size operations
    call window_size%set(1024, 768)
    print *, "Size: ", window_size%get_width(), "x", window_size%get_height()

    ! Position operations
    call window_pos%set(100, 50)
    print *, "Position: (", window_pos%get_x(), ",", window_pos%get_y(), ")"

    ! Color operations
    call background%set_rgba(240, 240, 240, 255)  ! Light gray
    print *, "Background: RGB(", background%get_red(), ",", &
             background%get_green(), ",", background%get_blue(), ")"

    ! Rectangle operations
    call viewport%set(0, 0, 800, 600)
    if (viewport%contains(400, 300)) then
        print *, "Center point is inside viewport"
    end if

end program types_demo
```

## Constants

Common type constants:

```fortran
! Colors
type(forge_color), parameter :: COLOR_BLACK = forge_color(0, 0, 0, 255)
type(forge_color), parameter :: COLOR_WHITE = forge_color(255, 255, 255, 255)
type(forge_color), parameter :: COLOR_RED = forge_color(255, 0, 0, 255)
type(forge_color), parameter :: COLOR_GREEN = forge_color(0, 255, 0, 255)
type(forge_color), parameter :: COLOR_BLUE = forge_color(0, 0, 255, 255)

! Common sizes
type(forge_size), parameter :: SIZE_ZERO = forge_size(0, 0)
type(forge_size), parameter :: SIZE_DEFAULT = forge_size(100, 30)
```

## See Also

- [forge_widget](forge_widget.md) - Uses forge_types for properties
- [forge_layout_base](forge_layout_base.md) - Uses geometric types
- [forge_color](forge_color.md) - Color utilities (if available)