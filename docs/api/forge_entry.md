# forge_entry

Single-line text input widget for user data entry and editing.

## Synopsis

```fortran
type, extends(forge_widget) :: forge_entry
    private
    type(forge_string) :: text
    type(forge_string) :: placeholder
    logical :: read_only = .false.
    integer :: max_length = 0
    type(forge_event_handler) :: change_handler
contains
    procedure :: set_text => forge_entry_set_text
    procedure :: get_text => forge_entry_get_text
    procedure :: set_placeholder => forge_entry_set_placeholder
    procedure :: set_read_only => forge_entry_set_read_only
    procedure :: set_max_length => forge_entry_set_max_length
    procedure :: on_change => forge_entry_on_change
end type forge_entry
```

## Description

The `forge_entry` class provides a single-line text input field. It supports:

- Text editing and display
- Placeholder text
- Read-only mode
- Maximum length limits
- Change event notifications
- Standard text editing operations (cut, copy, paste)

Entry widgets are essential for forms and data input in GUI applications.

## Properties

- `text`: The current text content
- `placeholder`: Hint text shown when field is empty
- `read_only`: Whether text can be edited
- `max_length`: Maximum allowed characters (0 = unlimited)
- `change_handler`: Event handler for text changes

## Methods

### set_text(text)

Sets the text content of the entry field.

**Parameters:**
- `text` (character): Text to set

**Example:**
```fortran
type(forge_entry) :: entry

call entry%set_text("Default value")
```

### get_text()

Gets the current text content.

**Returns:** character - Current text

**Example:**
```fortran
character(len=:), allocatable :: user_input

user_input = entry%get_text()
print *, "User entered:", user_input
```

### set_placeholder(placeholder)

Sets placeholder text shown when the field is empty.

**Parameters:**
- `placeholder` (character): Placeholder text

**Example:**
```fortran
call entry%set_placeholder("Enter your name...")
```

### set_read_only(read_only)

Sets whether the text can be edited by the user.

**Parameters:**
- `read_only` (logical): True to make read-only, false for editable

**Example:**
```fortran
! Make field read-only for display purposes
call entry%set_read_only(.true.)
```

### set_max_length(max_length)

Sets the maximum number of characters allowed.

**Parameters:**
- `max_length` (integer): Maximum length (0 = unlimited)

**Example:**
```fortran
! Limit to 50 characters
call entry%set_max_length(50)
```

### on_change(callback)

Registers a callback for text change events.

**Parameters:**
- `callback` (procedure): Event callback subroutine

**Example:**
```fortran
call entry%on_change(handle_text_change)

subroutine handle_text_change(event)
    type(forge_event), intent(in) :: event
    character(len=:), allocatable :: new_text

    new_text = entry%get_text()
    print *, "Text changed to:", new_text
    ! Validate input, update UI, etc.
end subroutine handle_text_change
```

## Events

Entry widgets generate `EVENT_TEXT_CHANGED` events:

```fortran
subroutine text_change_handler(event)
    type(forge_event), intent(in) :: event

    select case (event%type)
    case (EVENT_TEXT_CHANGED)
        ! Text was modified
        call validate_input()
        call update_preview()
    end select
end subroutine text_change_handler
```

## Text Validation

Common validation patterns:

```fortran
subroutine validate_email(event)
    type(forge_event), intent(in) :: event
    character(len=:), allocatable :: email

    email = entry%get_text()
    if (.not. is_valid_email(email)) then
        call entry%set_text("")  ! Clear invalid input
        call show_error("Invalid email format")
    end if
end subroutine validate_email
```

## Input Types

### Plain Text
```fortran
type(forge_entry) :: name_entry

call name_entry%set_placeholder("Enter your full name")
```

### Numeric Input
```fortran
type(forge_entry) :: age_entry

call age_entry%set_placeholder("Age")
call age_entry%set_max_length(3)
call age_entry%on_change(validate_numeric)
```

### Password Input
```fortran
type(forge_entry) :: password_entry

call password_entry%set_placeholder("Enter password")
! Note: Actual password masking depends on backend support
```

## Size and Layout

Entry widgets have reasonable default sizes:

```fortran
! Default size is usually adequate
type(forge_entry) :: entry

! Or set custom size
call entry%set_size_hint(200, 25)
```

## Visual Appearance

Entry appearance varies by backend:

- **Custom Backend**: Simple text input box
- **Qt Backend**: QLineEdit with platform styling
- **GTK Backend**: GtkEntry with theme support
- **Tcl/Tk Backend**: Tk entry widgets

## Accessibility

Entry widgets support accessibility features:

- Screen reader announcements
- Keyboard navigation (Tab key)
- High contrast mode
- Form field labeling association

## Common Patterns

### Form Input with Validation
```fortran
type(forge_entry) :: username_entry, password_entry
type(forge_label) :: error_label

call username_entry%set_placeholder("Username")
call username_entry%on_change(validate_username)

call password_entry%set_placeholder("Password")
call password_entry%on_change(validate_password)

subroutine validate_username(event)
    type(forge_event), intent(in) :: event
    character(len=:), allocatable :: username

    username = username_entry%get_text()
    if (len(username) < 3) then
        call error_label%set_text("Username too short")
    else
        call error_label%set_text("")
    end if
end subroutine validate_username
```

### Search Field
```fortran
type(forge_entry) :: search_entry

call search_entry%set_placeholder("Search...")
call search_entry%on_change(perform_search)

subroutine perform_search(event)
    type(forge_event), intent(in) :: event
    character(len=:), allocatable :: query

    query = search_entry%get_text()
    call update_search_results(query)
end subroutine perform_search
```

### Auto-complete
```fortran
type(forge_entry) :: autocomplete_entry

call autocomplete_entry%set_placeholder("Type to search...")
call autocomplete_entry%on_change(show_suggestions)

subroutine show_suggestions(event)
    type(forge_event), intent(in) :: event
    character(len=:), allocatable :: partial

    partial = autocomplete_entry%get_text()
    call display_matching_items(partial)
end subroutine show_suggestions
```

## Thread Safety

Entry operations are not thread-safe. Text updates and event handling should occur on the main GUI thread.

## Example Usage

```fortran
program entry_demo
    use forge
    implicit none

    type(forge_application) :: app
    type(forge_window_t) :: window
    type(forge_entry) :: name_entry, email_entry
    type(forge_label) :: name_label, email_label, result_label
    type(forge_button) :: submit_button
    type(forge_form_layout) :: layout
    type(forge_status) :: status

    ! Initialize application
    call app%init(BACKEND_CUSTOM, status)

    ! Create window
    window = app%create_window("Entry Demo", 400, 300)

    ! Create form elements
    call name_label%set_text("Name:")
    call name_entry%set_placeholder("Enter your name")

    call email_label%set_text("Email:")
    call email_entry%set_placeholder("Enter your email")

    call submit_button%set_label("Submit")
    call submit_button%on_click(submit_form)

    call result_label%set_text("")

    ! Set up form layout
    call layout%add_row(name_label, name_entry)
    call layout%add_row(email_label, email_entry)
    call layout%add_widget(submit_button)
    call layout%add_widget(result_label)

    ! Show window
    call window%show()
    call app%run()

contains

    subroutine submit_form(event)
        type(forge_event), intent(in) :: event
        character(len=:), allocatable :: name, email

        name = name_entry%get_text()
        email = email_entry%get_text()

        if (len(name) > 0 .and. len(email) > 0) then
            call result_label%set_text("Hello " // name // "! We'll contact you at " // email)
        else
            call result_label%set_text("Please fill in all fields")
        end if
    end subroutine submit_form

end program entry_demo
```

## See Also

- [forge_widget](forge_widget.md) - Base widget class
- [forge_text_view](forge_text_view.md) - Multi-line text input
- [forge_label](forge_label.md) - Non-editable text display
- [forge_form_layout](forge_form_layout.md) - Form layout management