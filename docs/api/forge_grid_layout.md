# forge_grid_layout

Layout manager that arranges widgets in a table-like grid with rows and columns.

## Synopsis

```fortran
type, extends(forge_layout_base) :: forge_grid_layout
    private
    type(QList_widget) :: widgets
    integer, allocatable :: row_indices(:)
    integer, allocatable :: col_indices(:)
    integer, allocatable :: row_spans(:)
    integer, allocatable :: col_spans(:)
    integer :: rows = 1
    integer :: columns = 1
    logical :: homogeneous = .false.
contains
    procedure :: set_dimensions => forge_grid_layout_set_dimensions
    procedure :: set_homogeneous => forge_grid_layout_set_homogeneous
    procedure :: add_widget => forge_grid_layout_add_widget
    procedure :: remove_widget => forge_grid_layout_remove_widget
    procedure :: compute => forge_grid_layout_compute
end type forge_grid_layout
```

## Description

The `forge_grid_layout` arranges widgets in a grid pattern similar to a spreadsheet. It provides:

- Table-like widget arrangement
- Configurable rows and columns
- Widget spanning across multiple cells
- Homogeneous spacing option
- Automatic size calculation

Grid layouts are ideal for forms, data entry screens, and structured interfaces.

## Properties

- `widgets`: List of managed widgets
- `row_indices`: Row positions for each widget
- `col_indices`: Column positions for each widget
- `row_spans`: Row span for each widget
- `col_spans`: Column span for each widget
- `rows`: Total number of rows
- `columns`: Total number of columns
- `homogeneous`: Whether all cells have equal size

## Methods

### set_dimensions(rows, columns)

Sets the grid dimensions.

**Parameters:**
- `rows` (integer): Number of rows
- `columns` (integer): Number of columns

**Example:**
```fortran
type(forge_grid_layout) :: grid

call grid%set_dimensions(3, 2)  ! 3 rows, 2 columns
```

### set_homogeneous(homogeneous)

Sets whether all cells should have equal size.

**Parameters:**
- `homogeneous` (logical): True for equal cell sizes

**Example:**
```fortran
call grid%set_homogeneous(.true.)  ! All cells same size
```

### add_widget(widget, row, col, row_span, col_span)

Adds a widget to the grid at specified position.

**Parameters:**
- `widget` (forge_widget): Widget to add
- `row` (integer, optional): Row position (1-based)
- `col` (integer, optional): Column position (1-based)
- `row_span` (integer, optional): Rows to span (default 1)
- `col_span` (integer, optional): Columns to span (default 1)

**Example:**
```fortran
! Add widget at row 1, column 1
call grid%add_widget(button1, 1, 1)

! Add widget spanning 2 columns
call grid%add_widget(title_label, 1, 1, 1, 2)
```

### remove_widget(widget)

Removes a widget from the grid.

**Parameters:**
- `widget` (forge_widget): Widget to remove

**Example:**
```fortran
call grid%remove_widget(button1)
```

### compute()

Calculates and applies widget positions and sizes.

**Example:**
```fortran
call grid%compute()
```

## Grid Arrangement

Widgets are positioned in a table layout:

```
Row 1: [Widget A] [Widget B]
Row 2: [Widget C] [Widget D]
Row 3: [Widget E]         [Widget F spans 2 columns]
```

## Spanning Cells

Widgets can span multiple cells:

```fortran
! Normal widget
call grid%add_widget(button, 1, 1)

! Widget spanning 2 rows and 3 columns
call grid%add_widget(large_widget, 2, 1, 2, 3)
```

## Size Calculation

Grid layouts calculate cell sizes based on:

- Widget size hints and policies
- Available space
- Homogeneous setting
- Spacing and padding

## Common Patterns

### Form Layout
```fortran
type(forge_grid_layout) :: form_grid
type(forge_label) :: name_label, email_label
type(forge_entry) :: name_entry, email_entry

call form_grid%set_dimensions(2, 2)
call form_grid%set_spacing(10)

! Row 1: Name label and entry
call form_grid%add_widget(name_label, 1, 1)
call form_grid%add_widget(name_entry, 1, 2)

! Row 2: Email label and entry
call form_grid%add_widget(email_label, 2, 1)
call form_grid%add_widget(email_entry, 2, 2)
```

### Button Grid
```fortran
type(forge_grid_layout) :: button_grid

call button_grid%set_dimensions(2, 3)
call button_grid%set_homogeneous(.true.)

call button_grid%add_widget(button1, 1, 1)
call button_grid%add_widget(button2, 1, 2)
call button_grid%add_widget(button3, 1, 3)
call button_grid%add_widget(button4, 2, 1)
call button_grid%add_widget(button5, 2, 2)
call button_grid%add_widget(button6, 2, 3)
```

### Complex Layout
```fortran
type(forge_grid_layout) :: main_grid

call main_grid%set_dimensions(4, 3)

! Title spans full width
call main_grid%add_widget(title_label, 1, 1, 1, 3)

! Form fields
call main_grid%add_widget(name_label, 2, 1)
call main_grid%add_widget(name_entry, 2, 2, 1, 2)

call main_grid%add_widget(email_label, 3, 1)
call main_grid%add_widget(email_entry, 3, 2, 1, 2)

! Buttons in last row
call main_grid%add_widget(ok_button, 4, 2)
call main_grid%add_widget(cancel_button, 4, 3)
```

## Qt Compatibility

ForGE provides Qt-style grid layout:

```fortran
type(QGridLayout) :: grid

! Same interface as forge_grid_layout
call grid%set_dimensions(3, 3)
call grid%add_widget(widget, 1, 1)
```

## Size Policies in Grids

Grid layouts respect size policies for cell sizing:

```fortran
! Column expands, row stays preferred
call widget%set_size_policy(QSizePolicy_Expanding, QSizePolicy_Preferred)
```

## Homogeneous Grids

When homogeneous is true:

- All cells have equal width and height
- Spacing is regular
- Good for button arrays and simple forms

When homogeneous is false:

- Cell sizes based on widget content
- More flexible but potentially uneven

## Performance

Grid layouts have O(n) performance where n is widget count:

- Efficient for most applications
- Scales well to dozens of widgets
- Memory usage proportional to widget count

## Thread Safety

Grid layout operations are not thread-safe. All modifications should occur on the main GUI thread.

## Example Usage

```fortran
program grid_layout_demo
    use forge
    implicit none

    type(forge_application) :: app
    type(forge_window_t) :: window
    type(forge_label) :: name_label, age_label
    type(forge_entry) :: name_entry, age_entry
    type(forge_button) :: ok_button, cancel_button
    type(forge_grid_layout) :: grid
    type(forge_status) :: status

    ! Initialize
    call app%init(BACKEND_CUSTOM, status)
    window = app%create_window("Grid Layout Demo", 400, 200)

    ! Create form elements
    call name_label%set_text("Name:")
    call name_entry%set_placeholder("Enter name")

    call age_label%set_text("Age:")
    call age_entry%set_placeholder("Enter age")

    call ok_button%set_label("OK")
    call cancel_button%set_label("Cancel")

    ! Create 3x3 grid layout
    call grid%set_dimensions(3, 3)
    call grid%set_spacing(10)
    call grid%set_padding(15)

    ! Add widgets to grid
    call grid%add_widget(name_label, 1, 1)
    call grid%add_widget(name_entry, 1, 2, 1, 2)  ! Span 2 columns

    call grid%add_widget(age_label, 2, 1)
    call grid%add_widget(age_entry, 2, 2, 1, 2)   ! Span 2 columns

    ! Buttons in last row
    call grid%add_widget(ok_button, 3, 2)
    call grid%add_widget(cancel_button, 3, 3)

    ! Set layout size and compute
    call grid%set_parent_size(400, 200)
    call grid%compute()

    ! Show window
    call window%show()
    call app%run()

end program grid_layout_demo
```

## Advanced Usage

### Dynamic Grids
```fortran
subroutine resize_grid(grid, new_rows, new_cols)
    type(forge_grid_layout), intent(inout) :: grid
    integer, intent(in) :: new_rows, new_cols

    call grid%set_dimensions(new_rows, new_cols)
    ! Re-add widgets with new positions...
    call grid%compute()
end subroutine resize_grid
```

### Responsive Grids
```fortran
subroutine adjust_grid_for_size(grid, width, height)
    type(forge_grid_layout), intent(inout) :: grid
    integer, intent(in) :: width, height

    if (width > height) then
        ! Wide layout - more columns
        call grid%set_dimensions(2, 4)
    else
        ! Tall layout - more rows
        call grid%set_dimensions(4, 2)
    end if

    call grid%set_parent_size(width, height)
    call grid%compute()
end subroutine adjust_grid_for_size
```

## See Also

- [forge_layout_base](forge_layout_base.md) - Base layout class
- [forge_box_layout](forge_box_layout.md) - Linear layouts
- [forge_form_layout](forge_form_layout.md) - Two-column layouts
- [forge_size_policy](forge_size_policy.md) - Widget sizing policies
- [QGridLayout](qgridlayout.md) - Qt-style grid layout