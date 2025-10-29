# Widget Gallery

This guide showcases all available widgets in ForGE with examples and usage patterns.

## Basic Widgets

### Button

Interactive button for triggering actions.

```fortran
type(forge_button) :: button

call button%set_label("Click Me")
call button%on_click(handle_click)

subroutine handle_click(event)
    type(forge_event), intent(in) :: event
    print *, "Button clicked!"
end subroutine handle_click
```

**Common Uses:**
- Form submission
- Dialog actions (OK, Cancel, Apply)
- Navigation
- Tool actions

### Label

Non-editable text display.

```fortran
type(forge_label) :: label

call label%set_text("Welcome to ForGE!")
```

**Common Uses:**
- Form labels
- Status messages
- Instructions
- Section headers

### Entry

Single-line text input field.

```fortran
type(forge_entry) :: entry

call entry%set_placeholder("Enter your name")
call entry%set_max_length(50)
call entry%on_change(validate_input)

subroutine validate_input(event)
    character(len=:), allocatable :: text
    text = entry%get_text()
    ! Validate input...
end subroutine validate_input
```

**Common Uses:**
- Name fields
- Email addresses
- Search boxes
- Single-line data entry

## Numeric Input Widgets

### Slider

Visual range selector with draggable handle.

```fortran
type(forge_slider) :: slider

call slider%set_range(0.0_c_double, 100.0_c_double)
call slider%set_value(50.0_c_double)
call slider%set_step(1.0_c_double)
call slider%on_value_changed(update_value)

subroutine update_value(event)
    real(c_double) :: value
    value = slider%get_value()
    ! Use value...
end subroutine update_value
```

**Common Uses:**
- Volume controls
- Brightness adjustment
- Progress indication
- Parameter tuning

### Spin Button

Numeric input with up/down buttons.

```fortran
type(forge_spin_button) :: spin

call spin%set_range(0.0_c_double, 100.0_c_double)
call spin%set_step(1.0_c_double)
call spin%set_digits(0)  ! Integer values
call spin%set_value(50.0_c_double)

! For currency
call spin%set_range(0.0_c_double, 999.99_c_double)
call spin%set_step(0.01_c_double)
call spin%set_digits(2)
```

**Common Uses:**
- Quantity selection
- Age input
- Price entry
- Precision numeric input

## Selection Widgets

### Combo Box

Drop-down list for option selection.

```fortran
type(forge_combo_box) :: combo

call combo%add_item("Option 1")
call combo%add_item("Option 2")
call combo%add_item("Option 3")
call combo%set_selected_index(1)
call combo%on_selection_changed(handle_selection)

subroutine handle_selection(event)
    character(len=:), allocatable :: selected
    selected = combo%get_selected_text()
    ! Use selection...
end subroutine handle_selection
```

**Common Uses:**
- Country selection
- Priority levels
- File format choice
- Settings options

### Check Box

Boolean on/off toggle.

```fortran
type(forge_check_box) :: checkbox

call checkbox%set_label("Enable feature")
call checkbox%set_checked(.true.)
call checkbox%on_toggled(handle_toggle)

subroutine handle_toggle(event)
    logical :: checked
    checked = checkbox%get_checked()
    if (checked) then
        call enable_feature()
    else
        call disable_feature()
    end if
end subroutine handle_toggle
```

**Common Uses:**
- Feature toggles
- Option selection
- Agreement checkboxes
- Filter options

### Radio Button

Exclusive selection within a group.

```fortran
type(forge_radio_button) :: radio1, radio2, radio3

call radio1%set_label("Small")
call radio2%set_label("Medium")
call radio3%set_label("Large")

call radio2%set_checked(.true.)  ! Default selection

call radio1%on_toggled(handle_size_change)
call radio2%on_toggled(handle_size_change)
call radio3%on_toggled(handle_size_change)

subroutine handle_size_change(event)
    if (radio1%get_checked()) then
        call set_size("small")
    else if (radio2%get_checked()) then
        call set_size("medium")
    else if (radio3%get_checked()) then
        call set_size("large")
    end if
end subroutine handle_size_change
```

**Common Uses:**
- Size selection (S/M/L)
- Mode selection
- Exclusive options
- Rating systems

## Text Widgets

### Text View

Multi-line text display and editing.

```fortran
type(forge_text_view) :: text_view

call text_view%set_text("Multi-line\ntext content\nhere")
call text_view%set_editable(.true.)
call text_view%set_word_wrap(.true.)
call text_view%on_change(handle_text_change)

subroutine handle_text_change(event)
    character(len=:), allocatable :: content
    content = text_view%get_text()
    ! Process text...
end subroutine handle_text_change
```

**Common Uses:**
- Document editing
- Log display
- Code editing
- Form text areas
- README display

## Progress Widgets

### Progress Bar

Visual progress indication.

```fortran
type(forge_progress_bar) :: progress

call progress%set_value(0.5_c_double)  ! 50%
call progress%set_show_text(.true.)
call progress%set_text("Loading...")

! Update progress
call progress%set_value(current_progress / total_work)
```

**Common Uses:**
- File loading
- Data processing
- Installation progress
- Task completion status

## Layout Widgets

### Separator

Visual divider between sections.

```fortran
type(forge_separator) :: separator

call separator%set_vertical(.false.)  ! Horizontal line
```

**Common Uses:**
- Section dividers
- Visual grouping
- Menu separators

## Advanced Widgets (Planned)

### Table Widget
Multi-column data display with sorting and selection.

### Tree Widget
Hierarchical data display.

### Calendar Widget
Date selection and display.

### Graphics View
Custom drawing and visualization.

## Widget Properties

All widgets inherit common properties from `forge_widget`:

### Size and Position
```fortran
call widget%set_size(200, 50)
call widget%set_position(100, 50)
call widget%set_size_hint(150, 40)
```

### Size Policies
```fortran
! Fixed size
call widget%set_size_policy(QSizePolicy_Fixed, QSizePolicy_Fixed)

! Expand to fill space
call widget%set_size_policy(QSizePolicy_Expanding, QSizePolicy_Expanding)

! Preferred size
call widget%set_size_policy(QSizePolicy_Preferred, QSizePolicy_Preferred)
```

### State Control
```fortran
call widget%show()
call widget%hide()
call widget%enable()
call widget%disable()
```

### Naming
```fortran
call widget%set_name("submit_button")
```

## Widget Event Handling

### Button Events
```fortran
call button%on_click(handle_click)
```

### Text Events
```fortran
call entry%on_change(handle_text_change)
call text_view%on_change(handle_text_change)
```

### Value Events
```fortran
call slider%on_value_changed(handle_value_change)
call spin%on_value_changed(handle_value_change)
```

### Selection Events
```fortran
call combo%on_selection_changed(handle_selection)
call checkbox%on_toggled(handle_toggle)
call radio%on_toggled(handle_toggle)
```

### Window Events
```fortran
call window%on_close(handle_close)
```

## Complete Example

```fortran
program widget_showcase
    use forge
    implicit none

    type(forge_application) :: app
    type(forge_window_t) :: window

    ! Basic widgets
    type(forge_button) :: submit_button
    type(forge_label) :: title_label
    type(forge_entry) :: name_entry

    ! Numeric widgets
    type(forge_slider) :: volume_slider
    type(forge_spin_button) :: quantity_spin

    ! Selection widgets
    type(forge_combo_box) :: priority_combo
    type(forge_check_box) :: agree_checkbox
    type(forge_radio_button) :: small_radio, medium_radio, large_radio

    ! Text widgets
    type(forge_text_view) :: description_text

    ! Progress widgets
    type(forge_progress_bar) :: progress_bar

    ! Layout
    type(forge_box_layout) :: main_layout
    type(forge_form_layout) :: form_layout

    type(forge_status) :: status

    ! Initialize application
    call app%init(BACKEND_CUSTOM, status)
    window = app%create_window("Widget Showcase", 500, 600)

    ! Set up widgets
    call title_label%set_text("ForGE Widget Gallery")

    call name_entry%set_placeholder("Enter your name")

    call submit_button%set_label("Submit")
    call submit_button%on_click(handle_submit)

    call volume_slider%set_range(0.0_c_double, 100.0_c_double)
    call volume_slider%set_value(75.0_c_double)

    call quantity_spin%set_range(1.0_c_double, 10.0_c_double)
    call quantity_spin%set_value(1.0_c_double)

    call priority_combo%add_item("Low")
    call priority_combo%add_item("Medium")
    call priority_combo%add_item("High")
    call priority_combo%set_selected_index(2)

    call agree_checkbox%set_label("I agree to terms")

    call small_radio%set_label("Small")
    call medium_radio%set_label("Medium")
    call large_radio%set_label("Large")
    call medium_radio%set_checked(.true.)

    call description_text%set_text("Enter description here...")
    call description_text%set_editable(.true.)

    call progress_bar%set_value(0.3_c_double)

    ! Set up layout
    call main_layout%set_orientation(LAYOUT_VERTICAL)
    call main_layout%set_spacing(10)
    call main_layout%set_padding(20)

    call main_layout%add_widget(title_label)

    ! Add form fields
    call form_layout%add_row(name_entry, submit_button)
    call main_layout%add_widget(form_layout)

    ! Add other widgets
    call main_layout%add_widget(volume_slider)
    call main_layout%add_widget(quantity_spin)
    call main_layout%add_widget(priority_combo)
    call main_layout%add_widget(agree_checkbox)
    call main_layout%add_widget(small_radio)
    call main_layout%add_widget(medium_radio)
    call main_layout%add_widget(large_radio)
    call main_layout%add_widget(description_text)
    call main_layout%add_widget(progress_bar)

    ! Compute layout
    call main_layout%set_parent_size(500, 600)
    call main_layout%compute()

    ! Show window
    call window%show()
    call app%run()
    call app%shutdown()

contains

    subroutine handle_submit(event)
        type(forge_event), intent(in) :: event

        character(len=:), allocatable :: name
        logical :: agreed

        name = name_entry%get_text()
        agreed = agree_checkbox%get_checked()

        if (len(name) > 0 .and. agreed) then
            print *, "Form submitted by:", name
        else
            print *, "Form validation failed"
        end if
    end subroutine handle_submit

end program widget_showcase
```

## Widget Compatibility

| Widget | Windows | Linux | macOS | Qt Backend | GTK Backend |
|--------|---------|-------|-------|------------|-------------|
| Button | ✓ | ✓ | Planned | Planned | Planned |
| Label | ✓ | ✓ | Planned | Planned | Planned |
| Entry | ✓ | ✓ | Planned | Planned | Planned |
| Slider | ✓ | ✓ | Planned | Planned | Planned |
| Spin Button | ✓ | ✓ | Planned | Planned | Planned |
| Combo Box | ✓ | ✓ | Planned | Planned | Planned |
| Check Box | ✓ | ✓ | Planned | Planned | Planned |
| Radio Button | ✓ | ✓ | Planned | Planned | Planned |
| Text View | ✓ | ✓ | Planned | Planned | Planned |
| Progress Bar | ✓ | ✓ | Planned | Planned | Planned |
| Separator | ✓ | ✓ | Planned | Planned | Planned |

## Performance Notes

- Basic widgets (Button, Label) have minimal overhead
- Text widgets (Entry, Text View) use more memory for content
- Selection widgets (Combo Box) cache item lists
- Numeric widgets (Slider, Spin) handle real-time updates efficiently

## Accessibility

All widgets support accessibility features:

- Keyboard navigation (Tab order)
- Screen reader compatibility
- High contrast mode support
- Focus indicators

## Next Steps

- Read the [API documentation](../api/) for detailed widget method references
- Explore the [examples](../../examples/) directory for more widget usage examples
- Learn about [layout managers](layout_managers.md) for organizing widgets
- Study [event handling](event_handling.md) for interactive applications