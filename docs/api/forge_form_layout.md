# forge_form_layout

Layout manager that arranges widgets in a two-column label-field format typical of forms.

## Synopsis

```fortran
type, extends(forge_layout_base) :: forge_form_layout
    private
    type(QList_widget) :: labels
    type(QList_widget) :: fields
contains
    procedure :: add_row => forge_form_layout_add_row
    procedure :: add_widget => forge_form_layout_add_widget
    procedure :: remove_widget => forge_form_layout_remove_widget
    procedure :: compute => forge_form_layout_compute
end type forge_form_layout
```

## Description

The `forge_form_layout` arranges widgets in a two-column layout where:

- **Left column**: Labels (typically text labels)
- **Right column**: Fields (input widgets like entries, combos, etc.)

This layout is ideal for:
- Data entry forms
- Settings dialogs
- Configuration panels
- Login screens
- Any structured label-field arrangements

## Properties

- `labels`: List of label widgets (left column)
- `fields`: List of field widgets (right column)

## Methods

### add_row(label, field)

Adds a label-field pair to the form.

**Parameters:**
- `label` (forge_widget): Label widget (left column)
- `field` (forge_widget): Field widget (right column)

**Example:**
```fortran
type(forge_form_layout) :: form
type(forge_label) :: name_label
type(forge_entry) :: name_entry

call name_label%set_text("Name:")
call form%add_row(name_label, name_entry)
```

### add_widget(widget, row, col, row_span, col_span)

Adds a widget to the form at specified position.

**Parameters:**
- `widget` (forge_widget): Widget to add
- `row` (integer, optional): Row position (1-based)
- `col` (integer, optional): Column position (1=label, 2=field)
- `row_span` (integer, optional): Rows to span
- `col_span` (integer, optional): Columns to span

**Example:**
```fortran
! Add as label (column 1)
call form%add_widget(label_widget, 1, 1)

! Add as field (column 2)
call form%add_widget(entry_widget, 1, 2)

! Span both columns
call form%add_widget(button_widget, 2, 1, 1, 2)
```

### remove_widget(widget)

Removes a widget from the form.

**Parameters:**
- `widget` (forge_widget): Widget to remove

**Example:**
```fortran
call form%remove_widget(name_entry)
```

### compute()

Calculates and applies widget positions and sizes.

**Example:**
```fortran
call form%compute()
```

## Layout Structure

Form layouts create a table structure:

```
Row 1: [Label 1    ] [Field 1              ]
Row 2: [Label 2    ] [Field 2              ]
Row 3: [Label 3    ] [Field 3              ]
Row 4: [            ] [Full Width Button    ]
```

## Size Distribution

Form layouts handle sizing automatically:

- **Labels**: Use preferred width, align to right
- **Fields**: Expand to fill remaining space
- **Rows**: Height based on tallest widget in row
- **Spacing**: Consistent gaps between rows and columns

## Common Patterns

### Basic Form
```fortran
type(forge_form_layout) :: form
type(forge_label) :: name_label, email_label
type(forge_entry) :: name_entry, email_entry

call name_label%set_text("Name:")
call email_label%set_text("Email:")

call form%add_row(name_label, name_entry)
call form%add_row(email_label, email_entry)
```

### Advanced Form
```fortran
type(forge_form_layout) :: form
type(forge_label) :: name_label, age_label, country_label
type(forge_entry) :: name_entry, age_entry
type(forge_combo_box) :: country_combo
type(forge_button) :: submit_button

! Add text fields
call form%add_row(name_label, name_entry)
call form%add_row(age_label, age_entry)

! Add combo box
call form%add_row(country_label, country_combo)

! Add full-width button
call form%add_widget(submit_button, 4, 1, 1, 2)
```

### Grouped Forms
```fortran
type(forge_box_layout) :: main_layout
type(forge_form_layout) :: personal_form, address_form

! Personal information section
call personal_form%add_row(name_label, name_entry)
call personal_form%add_row(phone_label, phone_entry)

! Address section
call address_form%add_row(street_label, street_entry)
call address_form%add_row(city_label, city_entry)

! Combine in main layout
call main_layout%set_orientation(LAYOUT_VERTICAL)
call main_layout%add_widget(personal_form)
call main_layout%add_widget(address_form)
```

## Qt Compatibility

ForGE provides Qt-style form layout:

```fortran
type(QFormLayout) :: form

! Same interface as forge_form_layout
call form%add_row(label, field)
```

## Field Types

Common field widgets used in forms:

### Text Input
```fortran
type(forge_entry) :: text_field

call text_field%set_placeholder("Enter text here")
call form%add_row(label, text_field)
```

### Numeric Input
```fortran
type(forge_spin_button) :: number_field

call number_field%set_range(0.0_c_double, 100.0_c_double)
call form%add_row(label, number_field)
```

### Selection
```fortran
type(forge_combo_box) :: choice_field

call choice_field%add_item("Option 1")
call choice_field%add_item("Option 2")
call form%add_row(label, choice_field)
```

### Boolean
```fortran
type(forge_check_box) :: bool_field

call bool_field%set_label("Enable feature")
call form%add_widget(bool_field, row, 2)  ! Full row for checkbox
```

## Validation

Form layouts work well with validation:

```fortran
subroutine validate_form(form)
    type(forge_form_layout), intent(in) :: form
    logical :: valid = .true.

    ! Check required fields
    if (len(name_entry%get_text()) == 0) then
        valid = .false.
        call highlight_error(name_entry)
    end if

    if (.not. valid) then
        call show_validation_message()
    end if
end subroutine validate_form
```

## Spacing and Alignment

Form layouts provide consistent spacing:

- **Row spacing**: Configurable via `set_spacing()`
- **Column spacing**: Fixed ratio between labels and fields
- **Label alignment**: Right-aligned for better readability
- **Field alignment**: Left-aligned, expand to fill space

## Performance

Form layouts are efficient:

- O(n) computation where n is number of widgets
- Minimal memory overhead
- Fast layout recalculation
- Scales well to dozens of form fields

## Thread Safety

Form layout operations are not thread-safe. Modifications should occur on the main GUI thread.

## Example Usage

```fortran
program form_layout_demo
    use forge
    implicit none

    type(forge_application) :: app
    type(forge_window_t) :: window
    type(forge_form_layout) :: form
    type(forge_label) :: name_label, email_label, age_label
    type(forge_entry) :: name_entry, email_entry
    type(forge_spin_button) :: age_spin
    type(forge_button) :: submit_button
    type(forge_status) :: status

    ! Initialize
    call app%init(BACKEND_CUSTOM, status)
    window = app%create_window("Form Layout Demo", 400, 300)

    ! Create form elements
    call name_label%set_text("Name:")
    call name_entry%set_placeholder("Enter your name")

    call email_label%set_text("Email:")
    call email_entry%set_placeholder("Enter your email")

    call age_label%set_text("Age:")
    call age_spin%set_range(0.0_c_double, 120.0_c_double)

    call submit_button%set_label("Submit")

    ! Build form layout
    call form%set_spacing(10)
    call form%set_padding(20)

    call form%add_row(name_label, name_entry)
    call form%add_row(email_label, email_entry)
    call form%add_row(age_label, age_spin)
    call form%add_widget(submit_button, 4, 1, 1, 2)  ! Span both columns

    ! Set layout size and compute
    call form%set_parent_size(400, 300)
    call form%compute()

    ! Show window
    call window%show()
    call app%run()

contains

    ! Handle form submission
    subroutine on_submit(event)
        type(forge_event), intent(in) :: event

        character(len=:), allocatable :: name, email
        real(c_double) :: age

        name = name_entry%get_text()
        email = email_entry%get_text()
        age = age_spin%get_value()

        print *, "Form submitted:"
        print *, "Name: ", name
        print *, "Email: ", email
        print *, "Age: ", age
    end subroutine on_submit

end program form_layout_demo
```

## Advanced Usage

### Dynamic Forms
```fortran
subroutine add_form_field(form, label_text, field_type)
    type(forge_form_layout), intent(inout) :: form
    character(len=*), intent(in) :: label_text
    integer, intent(in) :: field_type

    type(forge_label) :: label
    ! Create appropriate field based on type...

    call label%set_text(label_text)
    call form%add_row(label, field)
end subroutine add_form_field
```

### Form Sections
```fortran
subroutine create_section(form, title, fields)
    type(forge_form_layout), intent(inout) :: form
    character(len=*), intent(in) :: title
    type(form_field), intent(in) :: fields(:)

    type(forge_label) :: section_title

    ! Add section title spanning both columns
    call section_title%set_text(title)
    call form%add_widget(section_title, current_row, 1, 1, 2)

    ! Add fields...
end subroutine create_section
```

### Form Validation
```fortran
function validate_form_layout(form) result(valid)
    type(forge_form_layout), intent(in) :: form
    logical :: valid

    integer :: i
    class(forge_widget), pointer :: field

    valid = .true.
    do i = 1, form%fields%size()
        field => form%fields%at(i)
        if (.not. validate_field(field)) then
            valid = .false.
            call highlight_invalid_field(field)
        end if
    end do
end function validate_form_layout
```

## See Also

- [forge_layout_base](forge_layout_base.md) - Base layout class
- [forge_box_layout](forge_box_layout.md) - Linear layouts
- [forge_grid_layout](forge_grid_layout.md) - Grid-based layouts
- [forge_entry](forge_entry.md) - Text input fields
- [forge_combo_box](forge_combo_box.md) - Selection fields