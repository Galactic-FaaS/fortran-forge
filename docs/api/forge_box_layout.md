# forge_box_layout

Layout manager that arranges widgets in a horizontal or vertical line.

## Synopsis

```fortran
type, extends(forge_layout_base) :: forge_box_layout
    private
    type(QList_widget) :: widgets
    integer :: orientation = LAYOUT_HORIZONTAL
contains
    procedure :: set_orientation => forge_box_layout_set_orientation
    procedure :: add_widget => forge_box_layout_add_widget
    procedure :: remove_widget => forge_box_layout_remove_widget
    procedure :: compute => forge_box_layout_compute
end type forge_box_layout
```

## Description

The `forge_box_layout` arranges widgets in a single row (horizontal) or column (vertical). It provides:

- Linear widget arrangement
- Configurable orientation (horizontal/vertical)
- Automatic spacing and sizing
- Size policy respect
- Equal distribution options

This is one of the most commonly used layouts for simple interfaces.

## Properties

- `widgets`: List of managed widgets
- `orientation`: LAYOUT_HORIZONTAL or LAYOUT_VERTICAL

## Methods

### set_orientation(orientation)

Sets the layout direction.

**Parameters:**
- `orientation` (integer): LAYOUT_HORIZONTAL or LAYOUT_VERTICAL

**Example:**
```fortran
type(forge_box_layout) :: layout

! Horizontal layout (default)
call layout%set_orientation(LAYOUT_HORIZONTAL)

! Vertical layout
call layout%set_orientation(LAYOUT_VERTICAL)
```

### add_widget(widget, row, col, row_span, col_span)

Adds a widget to the layout. For box layouts, position parameters are ignored.

**Parameters:**
- `widget` (forge_widget): Widget to add

**Example:**
```fortran
type(forge_box_layout) :: layout
type(forge_button) :: button

call layout%add_widget(button)
```

### remove_widget(widget)

Removes a widget from the layout.

**Parameters:**
- `widget` (forge_widget): Widget to remove

**Example:**
```fortran
call layout%remove_widget(button)
```

### compute()

Calculates and applies widget positions and sizes.

**Example:**
```fortran
call layout%compute()
```

## Layout Behavior

### Horizontal Layout
Widgets are arranged left to right:

```
[Widget1] [Widget2] [Widget3]
```

### Vertical Layout
Widgets are arranged top to bottom:

```
[Widget1]
[Widget2]
[Widget3]
```

## Size Distribution

Box layouts distribute space based on widget size policies:

- **Fixed widgets**: Keep their size hint
- **Expanding widgets**: Share extra space proportionally
- **Minimum widgets**: Grow from minimum size
- **Preferred widgets**: Use preferred size when possible

## Spacing and Padding

```fortran
type(forge_box_layout) :: layout

call layout%set_spacing(10)  ! 10 pixels between widgets
call layout%set_padding(15)  ! 15 pixels from container edges
```

## Common Patterns

### Button Row
```fortran
type(forge_box_layout) :: button_row

call button_row%set_orientation(LAYOUT_HORIZONTAL)
call button_row%set_spacing(5)

call button_row%add_widget(ok_button)
call button_row%add_widget(cancel_button)
call button_row%add_widget(help_button)
```

### Form Controls
```fortran
type(forge_box_layout) :: form_row

call form_row%set_orientation(LAYOUT_HORIZONTAL)
call form_row%set_spacing(10)

call form_row%add_widget(name_label)
call form_row%add_widget(name_entry)
```

### Vertical Stack
```fortran
type(forge_box_layout) :: main_layout

call main_layout%set_orientation(LAYOUT_VERTICAL)
call main_layout%set_spacing(15)

call main_layout%add_widget(title_label)
call main_layout%add_widget(content_area)
call main_layout%add_widget(button_row)
```

## Qt Compatibility

ForGE provides Qt-style convenience classes:

```fortran
type(QHBoxLayout) :: hbox  ! Horizontal box layout
type(QVBoxLayout) :: vbox  ! Vertical box layout

! These automatically set the correct orientation
```

## Size Policy Examples

### Equal Width Buttons
```fortran
type(forge_button) :: button1, button2, button3

! All buttons expand equally
call button1%set_size_policy(QSizePolicy_Expanding, QSizePolicy_Fixed)
call button2%set_size_policy(QSizePolicy_Expanding, QSizePolicy_Fixed)
call button3%set_size_policy(QSizePolicy_Expanding, QSizePolicy_Fixed)

call button1%set_size_hint(0, 30)  ! Height hint only
call button2%set_size_hint(0, 30)
call button3%set_size_hint(0, 30)
```

### Mixed Sizing
```fortran
! Label stays at preferred size, entry expands
call label%set_size_policy(QSizePolicy_Preferred, QSizePolicy_Preferred)
call entry%set_size_policy(QSizePolicy_Expanding, QSizePolicy_Preferred)
```

## Nested Layouts

Box layouts work well with other layouts:

```fortran
type(forge_box_layout) :: main_layout, button_layout
type(forge_grid_layout) :: form_layout

! Main vertical layout
call main_layout%set_orientation(LAYOUT_VERTICAL)

! Form section
call main_layout%add_widget(form_layout)

! Button section
call button_layout%set_orientation(LAYOUT_HORIZONTAL)
call button_layout%add_widget(ok_button)
call button_layout%add_widget(cancel_button)

call main_layout%add_widget(button_layout)
```

## Performance

Box layouts are efficient:

- O(n) computation time where n is widget count
- Minimal memory overhead
- Fast recalculation

## Thread Safety

Layout operations are not thread-safe. Modifications should occur on the main GUI thread.

## Example Usage

```fortran
program box_layout_demo
    use forge
    implicit none

    type(forge_application) :: app
    type(forge_window_t) :: window
    type(forge_button) :: button1, button2, button3
    type(forge_box_layout) :: hbox
    type(forge_status) :: status

    ! Initialize
    call app%init(BACKEND_CUSTOM, status)
    window = app%create_window("Box Layout Demo", 400, 100)

    ! Create buttons
    call button1%set_label("OK")
    call button2%set_label("Cancel")
    call button3%set_label("Help")

    ! Create horizontal layout
    call hbox%set_orientation(LAYOUT_HORIZONTAL)
    call hbox%set_spacing(10)
    call hbox%set_padding(20)

    ! Add buttons
    call hbox%add_widget(button1)
    call hbox%add_widget(button2)
    call hbox%add_widget(button3)

    ! Set layout size and compute
    call hbox%set_parent_size(400, 100)
    call hbox%compute()

    ! Show window
    call window%show()
    call app%run()

end program box_layout_demo
```

## Advanced Usage

### Dynamic Layouts
```fortran
type(forge_box_layout) :: dynamic_layout

! Add widgets based on conditions
if (show_advanced) then
    call dynamic_layout%add_widget(advanced_button)
end if

call dynamic_layout%add_widget(basic_button)
```

### Responsive Layouts
```fortran
subroutine adjust_for_size(layout, width, height)
    type(forge_box_layout), intent(inout) :: layout
    integer, intent(in) :: width, height

    if (width > height) then
        ! Wide layout - horizontal
        call layout%set_orientation(LAYOUT_HORIZONTAL)
    else
        ! Tall layout - vertical
        call layout%set_orientation(LAYOUT_VERTICAL)
    end if

    call layout%set_parent_size(width, height)
    call layout%compute()
end subroutine adjust_for_size
```

## See Also

- [forge_layout_base](forge_layout_base.md) - Base layout class
- [forge_grid_layout](forge_grid_layout.md) - Grid-based layouts
- [forge_size_policy](forge_size_policy.md) - Widget sizing policies
- [QHBoxLayout](qhboxlayout.md) - Qt-style horizontal layout
- [QVBoxLayout](qvboxlayout.md) - Qt-style vertical layout