# forge_button

Interactive button widget that responds to user clicks and displays text labels.

## Synopsis

```fortran
type, extends(forge_widget) :: forge_button
    private
    type(forge_string) :: label
    type(forge_event_handler) :: click_handler
contains
    procedure :: set_label => forge_button_set_label
    procedure :: get_label => forge_button_get_label
    procedure :: on_click => forge_button_on_click
    procedure :: calculate_size_hint => forge_button_calculate_size_hint
end type forge_button
```

## Description

The `forge_button` class represents a clickable button widget. It provides:

- Text label display
- Click event handling
- Size hint calculation based on label text
- Visual feedback for user interaction

Buttons are one of the most commonly used widgets for triggering actions in GUI applications.

## Properties

- `label`: The text displayed on the button
- `click_handler`: Event handler for click events

## Methods

### set_label(label)

Sets the text displayed on the button.

**Parameters:**
- `label` (character): Button text

**Example:**
```fortran
type(forge_button) :: button

call button%set_label("Save File")
```

### get_label()

Gets the current button label text.

**Returns:** character - Button label

**Example:**
```fortran
character(len=:), allocatable :: text

text = button%get_label()
print *, "Button says:", text
```

### on_click(callback)

Registers a callback function to be called when the button is clicked.

**Parameters:**
- `callback` (procedure): Event callback subroutine

**Example:**
```fortran
call button%on_click(handle_button_click)

subroutine handle_button_click(event)
    type(forge_event), intent(in) :: event
    print *, "Button was clicked!"
    ! Perform action...
end subroutine handle_button_click
```

### calculate_size_hint()

Calculates the preferred size based on the button label text. This method is called automatically by the layout system.

**Implementation Details:**
- Width: `len(label) * 8 + 20` (approximate character width + padding)
- Height: `16 + 10` (approximate character height + padding)

## Events

Buttons generate `EVENT_BUTTON_CLICKED` events when clicked:

```fortran
subroutine button_callback(event)
    type(forge_event), intent(in) :: event

    select case (event%type)
    case (EVENT_BUTTON_CLICKED)
        ! Handle button click
        call perform_action()
    end select
end subroutine button_callback
```

## Size and Layout

Buttons automatically calculate their preferred size based on their label:

```fortran
! Button will size itself appropriately
call button%set_label("Submit Form")

! Or set custom size hint
call button%set_size_hint(120, 35)
```

## Visual Appearance

Button appearance depends on the backend:

- **Custom Backend**: Simple rectangular button with text
- **Qt Backend**: Native platform button styling
- **GTK Backend**: GTK theme button styling
- **Tcl/Tk Backend**: Tk button widgets

## Accessibility

Buttons support accessibility features:

- Keyboard navigation (Tab key)
- Screen reader support (when available in backend)
- High contrast mode support

## Common Patterns

### Action Button
```fortran
type(forge_button) :: save_button

call save_button%set_label("Save")
call save_button%on_click(save_document)
```

### Toggle Button
```fortran
type(forge_button) :: toggle_button
logical :: is_active = .false.

call toggle_button%set_label("Start")
call toggle_button%on_click(toggle_action)

subroutine toggle_action(event)
    type(forge_event), intent(in) :: event

    is_active = .not. is_active
    if (is_active) then
        call toggle_button%set_label("Stop")
        call start_process()
    else
        call toggle_button%set_label("Start")
        call stop_process()
    end if
end subroutine toggle_action
```

### Form Submission
```fortran
type(forge_button) :: submit_button

call submit_button%set_label("Submit")
call submit_button%on_click(validate_and_submit)

subroutine validate_and_submit(event)
    type(forge_event), intent(in) :: event

    if (validate_form()) then
        call submit_form()
        call show_success_message()
    else
        call show_error_message()
    end if
end subroutine validate_and_submit
```

## Thread Safety

Button operations are not thread-safe. Event callbacks should not perform long-running operations that block the GUI thread.

## Example Usage

```fortran
program button_demo
    use forge
    implicit none

    type(forge_application) :: app
    type(forge_window_t) :: window
    type(forge_button) :: hello_button, quit_button
    type(forge_box_layout) :: layout
    type(forge_status) :: status

    ! Initialize application
    call app%init(BACKEND_CUSTOM, status)

    ! Create window
    window = app%create_window("Button Demo", 300, 200)

    ! Create buttons
    call hello_button%set_label("Say Hello")
    call hello_button%on_click(say_hello)

    call quit_button%set_label("Quit")
    call quit_button%on_click(quit_app)

    ! Add buttons to layout
    call layout%set_orientation(LAYOUT_VERTICAL)
    call layout%add_widget(hello_button)
    call layout%add_widget(quit_button)

    ! Show window
    call window%show()
    call app%run()

contains

    subroutine say_hello(event)
        type(forge_event), intent(in) :: event
        print *, "Hello, ForGE!"
    end subroutine say_hello

    subroutine quit_app(event)
        type(forge_event), intent(in) :: event
        call app%shutdown()
    end subroutine quit_app

end program button_demo
```

## See Also

- [forge_widget](forge_widget.md) - Base widget class
- [forge_event](forge_event.md) - Event handling
- [forge_box_layout](forge_box_layout.md) - Layout management