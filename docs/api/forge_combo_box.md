# forge_combo_box

Drop-down selection widget for choosing from a list of options.

## Synopsis

```fortran
type, extends(forge_widget) :: forge_combo_box
    private
    type(forge_string), allocatable :: items(:)
    integer :: selected_index = 0
    logical :: editable = .false.
    type(forge_event_handler) :: selection_changed_handler
contains
    procedure :: add_item => forge_combo_box_add_item
    procedure :: remove_item => forge_combo_box_remove_item
    procedure :: clear_items => forge_combo_box_clear_items
    procedure :: set_selected_index => forge_combo_box_set_selected_index
    procedure :: get_selected_index => forge_combo_box_get_selected_index
    procedure :: get_selected_text => forge_combo_box_get_selected_text
    procedure :: set_editable => forge_combo_box_set_editable
    procedure :: on_selection_changed => forge_combo_box_on_selection_changed
end type forge_combo_box
```

## Description

The `forge_combo_box` widget provides a drop-down list for selecting from predefined options. It supports:

- Dynamic item lists
- Single selection
- Optional text editing
- Keyboard navigation
- Search/filtering (backend dependent)
- Selection change events

Combo boxes are commonly used for:
- Option selection
- Settings choices
- Form dropdowns
- Category selection
- Mode switching

## Properties

- `items`: Array of selectable text items
- `selected_index`: Currently selected item (0 = none, 1-based)
- `editable`: Whether users can type custom text
- `selection_changed_handler`: Event handler for selection changes

## Methods

### add_item(item)

Adds an item to the combo box list.

**Parameters:**
- `item` (character): Text for the new item

**Example:**
```fortran
type(forge_combo_box) :: combo

call combo%add_item("Option 1")
call combo%add_item("Option 2")
call combo%add_item("Option 3")
```

### remove_item(index)

Removes an item from the list.

**Parameters:**
- `index` (integer): Item index to remove (1-based)

**Example:**
```fortran
call combo%remove_item(2)  ! Remove second item
```

### clear_items()

Removes all items from the combo box.

**Example:**
```fortran
call combo%clear_items()
```

### set_selected_index(index)

Sets the currently selected item.

**Parameters:**
- `index` (integer): Item index (0 = none selected, 1-based)

**Example:**
```fortran
call combo%set_selected_index(1)  ! Select first item
call combo%set_selected_index(0)  ! Clear selection
```

### get_selected_index()

Gets the index of the currently selected item.

**Returns:** integer - Selected index (0 = none, 1-based)

**Example:**
```fortran
integer :: selected

selected = combo%get_selected_index()
if (selected > 0) then
    print *, "Selected item:", selected
end if
```

### get_selected_text()

Gets the text of the currently selected item.

**Returns:** character - Selected item text (empty if none selected)

**Example:**
```fortran
character(len=:), allocatable :: selected_text

selected_text = combo%get_selected_text()
if (len(selected_text) > 0) then
    print *, "Selected:", selected_text
end if
```

### set_editable(editable)

Sets whether users can type custom text.

**Parameters:**
- `editable` (logical): True to allow custom text input

**Example:**
```fortran
! Allow custom input
call combo%set_editable(.true.)
```

### on_selection_changed(callback)

Registers a callback for selection change events.

**Parameters:**
- `callback` (procedure): Event callback subroutine

**Example:**
```fortran
call combo%on_selection_changed(handle_selection)

subroutine handle_selection(event)
    type(forge_event), intent(in) :: event
    character(len=:), allocatable :: selected

    selected = combo%get_selected_text()
    print *, "Selected:", selected
    call apply_setting(selected)
end subroutine handle_selection
```

## Item Management

Combo boxes support dynamic item lists:

```fortran
! Add items individually
call combo%add_item("Red")
call combo%add_item("Green")
call combo%add_item("Blue")

! Bulk loading
character(len=*), parameter :: colors(*) = ["Red", "Green", "Blue", "Yellow"]
integer :: i

do i = 1, size(colors)
    call combo%add_item(colors(i))
end do

! Remove items
call combo%remove_item(4)  ! Remove "Yellow"

! Clear all
call combo%clear_items()
```

## Selection Handling

Combo boxes handle selection state:

```fortran
! Set selection
call combo%set_selected_index(2)  ! Select second item

! Check selection
if (combo%get_selected_index() > 0) then
    ! Item is selected
    selected_text = combo%get_selected_text()
end if

! Clear selection
call combo%set_selected_index(0)
```

## Events

Combo boxes generate selection change events:

```fortran
subroutine selection_handler(event)
    type(forge_event), intent(in) :: event

    select case (event%type)
    case (12)  ! EVENT_SELECTION_CHANGED
        ! Selection was modified
        call update_ui_for_selection()
    end select
end subroutine selection_handler
```

## Editable Mode

When editable, combo boxes allow custom input:

```fortran
call combo%set_editable(.true.)

! Add predefined options
call combo%add_item("Common Option 1")
call combo%add_item("Common Option 2")

! User can also type custom text
! Selection events still fire for both dropdown and typed input
```

## Common Patterns

### Settings Selection
```fortran
type(forge_combo_box) :: theme_combo

call theme_combo%add_item("Light")
call theme_combo%add_item("Dark")
call theme_combo%add_item("Auto")
call theme_combo%set_selected_index(1)  ! Default to Light
call theme_combo%on_selection_changed(apply_theme)
```

### Country Selection
```fortran
type(forge_combo_box) :: country_combo

call country_combo%add_item("United States")
call country_combo%add_item("Canada")
call country_combo%add_item("United Kingdom")
call country_combo%add_item("Australia")
call country_combo%set_editable(.true.)  ! Allow custom countries
```

### File Format Selection
```fortran
type(forge_combo_box) :: format_combo

call format_combo%add_item("PDF")
call format_combo%add_item("HTML")
call format_combo%add_item("Text")
call format_combo%add_item("JSON")
call format_combo%set_selected_index(1)  ! Default to PDF
```

### Priority Levels
```fortran
type(forge_combo_box) :: priority_combo

call priority_combo%add_item("Low")
call priority_combo%add_item("Medium")
call priority_combo%add_item("High")
call priority_combo%add_item("Urgent")
call priority_combo%set_selected_index(2)  ! Default to Medium
```

## Size and Layout

Combo boxes work well with layout managers:

```fortran
! Default size is usually adequate
type(forge_combo_box) :: combo

! Or set custom size hint
call combo%set_size_hint(150, 25)
```

## Visual Appearance

Combo box appearance varies by backend:

- **Custom Backend**: Simple dropdown with arrow
- **Qt Backend**: QComboBox with platform styling
- **GTK Backend**: GtkComboBox with theme support
- **Tcl/Tk Backend**: Tk combobox widgets

## Accessibility

Combo boxes support accessibility features:

- Screen reader item announcements
- Keyboard navigation (arrow keys, Enter)
- Dropdown list navigation
- High contrast mode support

## Performance

Combo boxes handle item lists efficiently:

- Fast item lookup
- Memory efficient storage
- Quick dropdown rendering
- Responsive user interaction

## Thread Safety

Combo box operations are not thread-safe. Item and selection modifications should occur on the main GUI thread.

## Example Usage

```fortran
program combo_box_demo
    use forge
    implicit none

    type(forge_application) :: app
    type(forge_window_t) :: window
    type(forge_combo_box) :: color_combo
    type(forge_label) :: selection_label
    type(forge_box_layout) :: layout
    type(forge_status) :: status

    ! Initialize
    call app%init(BACKEND_CUSTOM, status)
    window = app%create_window("Combo Box Demo", 300, 150)

    ! Create combo box
    call color_combo%add_item("Red")
    call color_combo%add_item("Green")
    call color_combo%add_item("Blue")
    call color_combo%add_item("Yellow")
    call color_combo%set_selected_index(1)  ! Select Red
    call color_combo%on_selection_changed(update_selection)

    ! Create label
    call selection_label%set_text("Selected: Red")

    ! Layout widgets
    call layout%set_orientation(LAYOUT_VERTICAL)
    call layout%set_spacing(10)
    call layout%add_widget(color_combo)
    call layout%add_widget(selection_label)

    ! Show window
    call window%show()
    call app%run()

contains

    subroutine update_selection(event)
        type(forge_event), intent(in) :: event
        character(len=:), allocatable :: selected

        selected = color_combo%get_selected_text()
        call selection_label%set_text("Selected: " // selected)
    end subroutine update_selection

end program combo_box_demo
```

## Advanced Usage

### Dynamic Item Loading
```fortran
subroutine load_items_from_file(combo, filename)
    type(forge_combo_box), intent(inout) :: combo
    character(len=*), intent(in) :: filename

    integer :: unit, ios
    character(len=100) :: line

    call combo%clear_items()
    open(newunit=unit, file=filename, status='old', iostat=ios)
    if (ios == 0) then
        do
            read(unit, '(A)', iostat=ios) line
            if (ios /= 0) exit
            call combo%add_item(trim(line))
        end do
        close(unit)
    end if
end subroutine load_items_from_file
```

### Filtered Combo Box
```fortran
type(forge_combo_box) :: filtered_combo
character(len=:), allocatable :: filter_text

call filtered_combo%set_editable(.true.)
call filtered_combo%on_selection_changed(filter_items)

subroutine filter_items(event)
    character(len=:), allocatable :: current_text
    integer :: i

    current_text = filtered_combo%get_selected_text()

    ! Clear and repopulate based on filter
    call filtered_combo%clear_items()
    do i = 1, size(all_items)
        if (index(all_items(i), current_text) > 0) then
            call filtered_combo%add_item(all_items(i))
        end if
    end do
end subroutine filter_items
```

### Cascading Combo Boxes
```fortran
type(forge_combo_box) :: country_combo, city_combo

! Country selection affects city options
call country_combo%on_selection_changed(update_cities)

subroutine update_cities(event)
    character(len=:), allocatable :: country

    country = country_combo%get_selected_text()
    call city_combo%clear_items()

    select case (country)
    case ("USA")
        call city_combo%add_item("New York")
        call city_combo%add_item("Los Angeles")
        call city_combo%add_item("Chicago")
    case ("Canada")
        call city_combo%add_item("Toronto")
        call city_combo%add_item("Vancouver")
        call city_combo%add_item("Montreal")
    end select
end subroutine update_cities
```

### Combo Box with Icons
```fortran
! Planned: Icon support
call combo%add_item_with_icon("Save", "save_icon.png")
call combo%add_item_with_icon("Open", "open_icon.png")
call combo%add_item_with_icon("Exit", "exit_icon.png")
```

## See Also

- [forge_widget](forge_widget.md) - Base widget class
- [forge_listview](forge_listview.md) - Multi-selection lists
- [forge_check_box](forge_check_box.md) - Boolean selection
- [forge_radio_button](forge_radio_button.md) - Single-choice options
- [forge_event](forge_event.md) - Event handling