# forge_widget

Abstract base class for all GUI widgets in ForGE, providing common functionality and interface.

## Synopsis

```fortran
type, abstract :: forge_widget
    private
    type(forge_widget_handle) :: handle
    type(forge_string) :: name
    type(forge_size) :: size
    type(forge_size) :: minimum_size
    type(forge_size) :: maximum_size
    type(forge_size) :: size_hint
    type(forge_position) :: position
    type(forge_size_policy) :: size_policy
    logical :: visible = .true.
    logical :: enabled = .true.
    logical :: size_hint_valid = .false.
    class(forge_backend_base), pointer :: backend => null()
contains
    procedure :: show => forge_widget_show
    procedure :: hide => forge_widget_hide
    procedure :: enable => forge_widget_enable
    procedure :: disable => forge_widget_disable
    procedure :: set_size => forge_widget_set_size
    procedure :: set_position => forge_widget_set_position
    procedure :: set_minimum_size => forge_widget_set_minimum_size
    procedure :: set_maximum_size => forge_widget_set_maximum_size
    procedure :: set_size_hint => forge_widget_set_size_hint
    procedure :: set_size_policy => forge_widget_set_size_policy
    procedure :: get_size => forge_widget_get_size
    procedure :: get_position => forge_widget_get_position
    procedure :: get_minimum_size => forge_widget_get_minimum_size
    procedure :: get_maximum_size => forge_widget_get_maximum_size
    procedure :: get_size_hint => forge_widget_get_size_hint
    procedure :: get_size_policy => forge_widget_get_size_policy
    procedure :: is_visible => forge_widget_is_visible
    procedure :: is_enabled => forge_widget_is_enabled
    procedure :: set_name => forge_widget_set_name
    procedure :: get_name => forge_widget_get_name
    procedure :: get_handle => forge_widget_get_handle
    procedure :: invalidate_size_hint => forge_widget_invalidate_size_hint
    procedure :: update_size_hint => forge_widget_update_size_hint
    procedure(update_size_hint_interface), deferred :: calculate_size_hint
end type forge_widget
```

## Description

The `forge_widget` class is the abstract base class for all GUI widgets in ForGE. It provides:

- Common widget properties (size, position, visibility, enabled state)
- Size management and layout hints
- Event handling infrastructure
- Backend abstraction for cross-platform compatibility
- Memory management and cleanup

All concrete widgets (buttons, labels, text fields, etc.) extend this base class.

## Common Properties

### Size and Position
- `size`: Current widget dimensions
- `position`: Current widget coordinates
- `minimum_size`: Minimum allowed dimensions
- `maximum_size`: Maximum allowed dimensions
- `size_hint`: Preferred size for layout managers

### State Properties
- `visible`: Whether widget is displayed
- `enabled`: Whether widget responds to user input
- `name`: Optional widget identifier for debugging

### Layout Properties
- `size_policy`: How widget should resize in layouts
- `size_hint_valid`: Whether size hint needs recalculation

## Methods

### Visibility Control

#### show()
Makes the widget visible.

```fortran
call widget%show()
```

#### hide()
Hides the widget without destroying it.

```fortran
call widget%hide()
```

### State Control

#### enable()
Enables the widget for user interaction.

```fortran
call widget%enable()
```

#### disable()
Disables the widget, preventing user interaction.

```fortran
call widget%disable()
```

#### is_enabled()
Checks if the widget is enabled.

**Returns:** logical - True if enabled

```fortran
if (widget%is_enabled()) then
    ! Widget is interactive
end if
```

### Size Management

#### set_size(width, height)
Sets the widget's size in pixels.

**Parameters:**
- `width` (integer): Width in pixels
- `height` (integer): Height in pixels

```fortran
call widget%set_size(200, 50)
```

#### get_size()
Gets the current widget size.

**Returns:** forge_size - Current dimensions

```fortran
type(forge_size) :: size
size = widget%get_size()
```

#### set_minimum_size(width, height)
Sets the minimum allowed size.

```fortran
call widget%set_minimum_size(100, 30)
```

#### set_maximum_size(width, height)
Sets the maximum allowed size.

```fortran
call widget%set_maximum_size(500, 200)
```

#### set_size_hint(width, height)
Sets the preferred size for layout managers.

```fortran
call widget%set_size_hint(150, 40)
```

### Position Management

#### set_position(x, y)
Sets the widget's position relative to its parent.

**Parameters:**
- `x` (integer): X coordinate
- `y` (integer): Y coordinate

```fortran
call widget%set_position(10, 20)
```

#### get_position()
Gets the current widget position.

**Returns:** forge_position - Current coordinates

```fortran
type(forge_position) :: pos
pos = widget%get_position()
```

### Size Policy

#### set_size_policy(horizontal_policy, vertical_policy, horizontal_stretch, vertical_stretch)
Sets how the widget should behave in layouts.

**Parameters:**
- `horizontal_policy` (integer): QSizePolicy_Fixed, QSizePolicy_Minimum, etc.
- `vertical_policy` (integer): Same as horizontal
- `horizontal_stretch` (integer, optional): Stretch factor
- `vertical_stretch` (integer, optional): Stretch factor

```fortran
call widget%set_size_policy(QSizePolicy_Expanding, QSizePolicy_Preferred, 1, 0)
```

### Naming and Identification

#### set_name(name)
Sets an optional name for the widget (useful for debugging).

```fortran
call widget%set_name("submit_button")
```

#### get_name()
Gets the widget's name.

**Returns:** character - Widget name

```fortran
character(len=:), allocatable :: name
name = widget%get_name()
```

### Size Hint Management

#### invalidate_size_hint()
Marks the size hint as invalid, forcing recalculation.

```fortran
call widget%invalidate_size_hint()
```

#### update_size_hint()
Recalculates the size hint if invalid.

```fortran
call widget%update_size_hint()
```

#### calculate_size_hint() [Deferred]
Abstract method that concrete widgets must implement to calculate their preferred size.

```fortran
subroutine calculate_size_hint(this)
    class(forge_widget), intent(inout) :: this
    ! Implementation specific to widget type
end subroutine calculate_size_hint
```

## Size Policies

ForGE uses Qt-style size policies:

- `QSizePolicy_Fixed`: Widget has fixed size
- `QSizePolicy_Minimum`: Widget can grow, has minimum size
- `QSizePolicy_Maximum`: Widget can shrink, has maximum size
- `QSizePolicy_Preferred`: Widget has preferred size
- `QSizePolicy_Expanding`: Widget wants to expand
- `QSizePolicy_MinimumExpanding`: Widget has minimum size but wants to expand
- `QSizePolicy_Ignored`: Widget ignores size constraints

## Layout Integration

Widgets work with layout managers:

```fortran
type(forge_box_layout) :: layout
type(forge_button) :: button

! Add widget to layout
call layout%add_widget(button)

! Layout will position and size the widget
call layout%compute()
```

## Event Handling

Widgets support event handling through derived classes:

```fortran
type(forge_button) :: button

call button%on_click(handle_click)

subroutine handle_click(event)
    type(forge_event), intent(in) :: event
    print *, "Button clicked!"
end subroutine handle_click
```

## Memory Management

Widgets manage their own resources:

- String properties use `forge_string` for efficient memory handling
- Handles are managed by the backend
- Size hints are cached and invalidated when properties change

## Thread Safety

Widget operations are not thread-safe. All widget manipulation should occur on the main GUI thread.

## Extending forge_widget

To create a new widget type:

```fortran
type, extends(forge_widget) :: my_custom_widget
    private
    ! Custom properties...
contains
    procedure :: calculate_size_hint => my_calculate_size_hint
    ! Custom methods...
end type my_custom_widget
```

## Example Usage

```fortran
program widget_demo
    use forge
    implicit none

    type(forge_application) :: app
    type(forge_window_t) :: window
    type(forge_button) :: button
    type(forge_status) :: status

    ! Initialize
    call app%init(BACKEND_CUSTOM, status)

    ! Create window
    window = app%create_window("Widget Demo", 400, 300)

    ! Configure button
    call button%set_label("Click Me")
    call button%set_size_hint(100, 40)
    call button%set_position(150, 130)
    call button%on_click(button_clicked)

    ! Show window
    call window%show()
    call app%run()

contains

    subroutine button_clicked(event)
        type(forge_event), intent(in) :: event
        print *, "Button was clicked!"
    end subroutine button_clicked

end program widget_demo
```

## See Also

- [forge_button](forge_button.md) - Button widget
- [forge_label](forge_label.md) - Label widget
- [forge_layout_base](forge_layout_base.md) - Layout management
- [forge_size_policy](forge_size_policy.md) - Size policy details