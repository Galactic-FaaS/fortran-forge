# forge_layout_base

Abstract base class for all layout managers in ForGE, providing automatic widget positioning and sizing.

## Synopsis

```fortran
type, abstract :: forge_layout_base
    private
    integer :: spacing = 5
    integer :: padding = 5
    logical :: needs_recalc = .true.
    integer(c_int) :: parent_width = 400
    integer(c_int) :: parent_height = 300
contains
    procedure :: set_spacing => forge_layout_set_spacing
    procedure :: set_padding => forge_layout_set_padding
    procedure :: get_spacing => forge_layout_get_spacing
    procedure :: get_padding => forge_layout_get_padding
    procedure :: invalidate => forge_layout_invalidate
    procedure :: set_parent_size => forge_layout_set_parent_size
    procedure :: get_parent_width => forge_layout_get_parent_width
    procedure :: get_parent_height => forge_layout_get_parent_height
    procedure(layout_add_widget_interface), deferred :: add_widget
    procedure(layout_remove_widget_interface), deferred :: remove_widget
    procedure(layout_compute_interface), deferred :: compute
end type forge_layout_base
```

## Description

The `forge_layout_base` class is the abstract foundation for all layout managers in ForGE. Layout managers automatically:

- Position widgets within containers
- Calculate widget sizes based on available space
- Handle widget resizing and container size changes
- Apply spacing and padding between widgets

Concrete layout implementations include:
- `forge_box_layout`: Horizontal or vertical linear arrangement
- `forge_grid_layout`: Table-like grid arrangement
- `forge_stack_layout`: Widgets stacked on top of each other
- `forge_form_layout`: Two-column label-field arrangement

## Core Concepts

### Spacing vs Padding
- **Spacing**: Distance between widgets
- **Padding**: Distance between layout edge and widgets

### Size Calculation
Layouts use size hints, policies, and constraints to determine widget sizes.

### Automatic Recalculation
Layouts automatically recalculate when:
- Widgets are added or removed
- Container size changes
- Widget properties change

## Methods

### Configuration

#### set_spacing(spacing)
Sets the spacing between widgets in pixels.

**Parameters:**
- `spacing` (integer): Pixels between widgets

**Example:**
```fortran
type(forge_box_layout) :: layout

call layout%set_spacing(10)  ! 10 pixels between widgets
```

#### set_padding(padding)
Sets the padding around the layout edge in pixels.

**Parameters:**
- `padding` (integer): Pixels from layout edge to widgets

**Example:**
```fortran
call layout%set_padding(15)  ! 15 pixels padding on all sides
```

#### get_spacing()
Gets the current spacing value.

**Returns:** integer - Spacing in pixels

#### get_padding()
Gets the current padding value.

**Returns:** integer - Padding in pixels

### Size Management

#### set_parent_size(width, height)
Sets the available space for the layout.

**Parameters:**
- `width` (integer): Available width in pixels
- `height` (integer): Available height in pixels

**Example:**
```fortran
call layout%set_parent_size(800, 600)
```

#### get_parent_width()
Gets the available width.

**Returns:** integer - Available width

#### get_parent_height()
Gets the available height.

**Returns:** integer - Available height

### Layout Control

#### invalidate()
Marks the layout as needing recalculation.

**Example:**
```fortran
call layout%invalidate()  ! Force recalculation on next compute
```

#### compute() [Deferred]
Calculates and applies widget positions and sizes. This is the core layout algorithm implemented by each concrete layout class.

### Widget Management [Deferred]

#### add_widget(widget, row, col, row_span, col_span)
Adds a widget to the layout. Parameters vary by layout type.

#### remove_widget(widget)
Removes a widget from the layout.

## Layout Types

### Box Layout
```fortran
type(forge_box_layout) :: hbox, vbox

! Horizontal layout
call hbox%set_orientation(LAYOUT_HORIZONTAL)
call hbox%add_widget(button1)
call hbox%add_widget(button2)

! Vertical layout
call vbox%set_orientation(LAYOUT_VERTICAL)
call vbox%add_widget(label1)
call vbox%add_widget(label2)
```

### Grid Layout
```fortran
type(forge_grid_layout) :: grid

call grid%set_dimensions(3, 2)  ! 3 rows, 2 columns
call grid%add_widget(widget1, 1, 1)  ! Row 1, Column 1
call grid%add_widget(widget2, 1, 2)  ! Row 1, Column 2
call grid%add_widget(widget3, 2, 1, 2, 1)  ! Row 2-3, Column 1 (spanning)
```

### Stack Layout
```fortran
type(forge_stack_layout) :: stack

call stack%add_widget(page1)
call stack%add_widget(page2)
call stack%set_current(2)  ! Show page2
```

### Form Layout
```fortran
type(forge_form_layout) :: form

call form%add_row(name_label, name_entry)
call form%add_row(email_label, email_entry)
```

## Size Policy Integration

Layouts respect widget size policies:

```fortran
! Widget that should expand to fill space
call widget%set_size_policy(QSizePolicy_Expanding, QSizePolicy_Expanding)

! Widget with fixed size
call widget%set_size_policy(QSizePolicy_Fixed, QSizePolicy_Fixed)

! Widget with preferred size
call widget%set_size_policy(QSizePolicy_Preferred, QSizePolicy_Preferred)
```

## Automatic Layout

Layouts automatically recalculate when needed:

```fortran
type(forge_box_layout) :: layout

! Add widgets
call layout%add_widget(button1)
call layout%add_widget(button2)

! Layout automatically positions widgets
call layout%compute()

! Later, when window resizes
call layout%set_parent_size(new_width, new_height)
call layout%compute()  ! Widgets reposition automatically
```

## Nested Layouts

Layouts can be nested for complex UIs:

```fortran
type(forge_box_layout) :: main_layout, button_layout
type(forge_grid_layout) :: form_layout

! Main vertical layout
call main_layout%set_orientation(LAYOUT_VERTICAL)

! Form section
call main_layout%add_widget(title_label)
call form_layout%set_dimensions(2, 2)
call form_layout%add_widget(name_label, 1, 1)
call form_layout%add_widget(name_entry, 1, 2)
! ... more form widgets

! Button section
call button_layout%set_orientation(LAYOUT_HORIZONTAL)
call button_layout%add_widget(ok_button)
call button_layout%add_widget(cancel_button)

call main_layout%add_widget(button_layout)  ! Nested layout
```

## Performance Considerations

- Layouts cache calculations and only recalculate when invalidated
- Avoid calling `compute()` unnecessarily
- Use appropriate size policies to reduce calculation complexity

## Thread Safety

Layout operations are not thread-safe. All layout modifications should occur on the main GUI thread.

## Example Usage

```fortran
program layout_demo
    use forge
    implicit none

    type(forge_application) :: app
    type(forge_window_t) :: window
    type(forge_button) :: button1, button2, button3
    type(forge_box_layout) :: hbox
    type(forge_status) :: status

    ! Initialize
    call app%init(BACKEND_CUSTOM, status)
    window = app%create_window("Layout Demo", 400, 200)

    ! Create buttons
    call button1%set_label("Button 1")
    call button2%set_label("Button 2")
    call button3%set_label("Button 3")

    ! Create horizontal layout
    call hbox%set_orientation(LAYOUT_HORIZONTAL)
    call hbox%set_spacing(10)
    call hbox%set_padding(20)

    ! Add buttons to layout
    call hbox%add_widget(button1)
    call hbox%add_widget(button2)
    call hbox%add_widget(button3)

    ! Set layout size and compute
    call hbox%set_parent_size(400, 200)
    call hbox%compute()

    ! Show window
    call window%show()
    call app%run()

end program layout_demo
```

## Advanced Features

### Custom Layouts
Create custom layouts by extending `forge_layout_base`:

```fortran
type, extends(forge_layout_base) :: custom_layout
contains
    procedure :: add_widget => custom_add_widget
    procedure :: remove_widget => custom_remove_widget
    procedure :: compute => custom_compute
end type custom_layout
```

### Layout Solvers
ForGE uses layout solvers for complex positioning calculations:

- `forge_box_layout_solver`: Linear arrangement
- `forge_grid_layout_solver`: Grid positioning
- `forge_form_layout_solver`: Label-field pairing

## See Also

- [forge_box_layout](forge_box_layout.md) - Horizontal/vertical layouts
- [forge_grid_layout](forge_grid_layout.md) - Grid-based layouts
- [forge_size_policy](forge_size_policy.md) - Widget sizing policies
- [forge_widget](forge_widget.md) - Widget size hints