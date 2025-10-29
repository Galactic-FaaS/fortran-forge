# forge_label

Non-interactive text display widget for showing static text in GUI applications.

## Synopsis

```fortran
type, extends(forge_widget) :: forge_label
    private
    type(forge_string) :: text
contains
    procedure :: set_text => forge_label_set_text
    procedure :: get_text => forge_label_get_text
    procedure :: calculate_size_hint => forge_label_calculate_size_hint
end type forge_label
```

## Description

The `forge_label` class provides a simple widget for displaying text. It is:

- Non-interactive (users cannot edit the text)
- Automatically sizes based on text content
- Supports basic text formatting
- Lightweight and efficient

Labels are commonly used for:
- Form field labels
- Status messages
- Instructions and help text
- Section headers

## Properties

- `text`: The text content to display

## Methods

### set_text(text)

Sets the text content of the label.

**Parameters:**
- `text` (character): Text to display

**Example:**
```fortran
type(forge_label) :: label

call label%set_text("Welcome to ForGE!")
```

### get_text()

Gets the current text content of the label.

**Returns:** character - Label text

**Example:**
```fortran
character(len=:), allocatable :: content

content = label%get_text()
print *, "Label shows:", content
```

### calculate_size_hint()

Calculates the preferred size based on the text content. This method is called automatically by the layout system.

**Implementation Details:**
- Width: `len(text) * 8` (approximate character width)
- Height: `16` (approximate character height)

## Text Formatting

Labels support basic text formatting:

```fortran
! Multi-line text
call label%set_text("Line 1\nLine 2\nLine 3")

! Long text (will wrap if word wrap is enabled in container)
call label%set_text("This is a very long label that might need to wrap to multiple lines")
```

## Size and Layout

Labels automatically size themselves to fit their content:

```fortran
type(forge_label) :: title_label, description_label

! Title label
call title_label%set_text("Application Title")
! Will size to fit "Application Title"

! Description label
call description_label%set_text("This application demonstrates ForGE widgets")
! Will size to fit the longer description
```

You can also set custom size hints:

```fortran
! Force label to be wider
call label%set_size_hint(300, 50)
```

## Visual Appearance

Label appearance depends on the backend:

- **Custom Backend**: Simple text rendering
- **Qt Backend**: QLabel with platform styling
- **GTK Backend**: GtkLabel with theme support
- **Tcl/Tk Backend**: Tk label widgets

## Common Use Cases

### Form Labels
```fortran
type(forge_label) :: name_label, email_label

call name_label%set_text("Full Name:")
call email_label%set_text("Email Address:")
```

### Status Messages
```fortran
type(forge_label) :: status_label

call status_label%set_text("Ready")
! Later...
call status_label%set_text("Processing...")
! Later...
call status_label%set_text("Complete!")
```

### Instructions
```fortran
type(forge_label) :: instruction_label

call instruction_label%set_text("Please enter your information below:")
```

### Section Headers
```fortran
type(forge_label) :: header_label

call header_label%set_text("Personal Information")
! Make it stand out with larger size
call header_label%set_size_hint(200, 30)
```

## Layout Integration

Labels work well with layout managers:

```fortran
type(forge_form_layout) :: form_layout
type(forge_label) :: name_label
type(forge_entry) :: name_entry

call name_label%set_text("Name:")
call form_layout%add_row(name_label, name_entry)
```

## Accessibility

Labels support accessibility features:

- Screen reader text-to-speech
- Keyboard navigation association with form fields
- High contrast mode support

## Performance

Labels are lightweight widgets:

- Minimal memory footprint
- Fast rendering
- No event handling overhead

## Thread Safety

Label operations are not thread-safe. Text updates should occur on the main GUI thread.

## Example Usage

```fortran
program label_demo
    use forge
    implicit none

    type(forge_application) :: app
    type(forge_window_t) :: window
    type(forge_label) :: title_label, status_label
    type(forge_box_layout) :: layout
    type(forge_status) :: status

    ! Initialize application
    call app%init(BACKEND_CUSTOM, status)

    ! Create window
    window = app%create_window("Label Demo", 400, 300)

    ! Create labels
    call title_label%set_text("ForGE Label Demo")
    call title_label%set_size_hint(200, 40)

    call status_label%set_text("Application started successfully")
    call status_label%set_size_hint(300, 30)

    ! Add to layout
    call layout%set_orientation(LAYOUT_VERTICAL)
    call layout%add_widget(title_label)
    call layout%add_widget(status_label)

    ! Show window
    call window%show()
    call app%run()

end program label_demo
```

## Advanced Usage

### Dynamic Text Updates
```fortran
type(forge_label) :: counter_label
integer :: counter = 0

call counter_label%set_text("Count: 0")

! Later, update the text
counter = counter + 1
call counter_label%set_text("Count: " // int_to_string(counter))
```

### Multi-line Labels
```fortran
type(forge_label) :: multiline_label

call multiline_label%set_text("This is line 1\nThis is line 2\nThis is line 3")
call multiline_label%set_size_hint(200, 60)  ! Accommodate multiple lines
```

## See Also

- [forge_widget](forge_widget.md) - Base widget class
- [forge_entry](forge_entry.md) - Editable text input
- [forge_text_view](forge_text_view.md) - Multi-line text display