# Button Demo Example

Demonstrates button creation, configuration, and event handling in ForGE applications.

## Overview

This example shows how to:
- Create multiple buttons with different labels
- Attach click event handlers to buttons
- Handle button events with callbacks
- Use button naming for identification

## Code

```fortran
!> @brief Button demonstration for ForGE
!> @details Shows button creation and click event handling
!> @author ForGE Contributors
!> @license GPL-3.0-or-later

program button_demo_example
    use forge
    use forge_stub_backend
    implicit none
    
    type(forge_application) :: app
    type(forge_window_t) :: window
    type(forge_button) :: button1, button2, button3
    type(forge_status) :: status
    type(forge_stub_backend_t), target :: stub_backend
    
    print '(A)', "=== Button Demo Example ==="
    print '(A)', ""
    print '(A)', "This example demonstrates:"
    print '(A)', "  - Creating multiple buttons"
    print '(A)', "  - Attaching click event handlers"
    print '(A)', "  - Button layout and positioning"
    print '(A)', ""
    print '(A)', "NOTE: Using stub backend - events won't fire in demo mode"
    print '(A)', ""
    
    ! Initialize backend
    call stub_backend%init(status)
    call forge_check_status(status, abort_on_error=.true.)
    
    ! Create window
    block
        type(forge_window_builder) :: builder
        call builder%set_title("Button Demo")
        call builder%set_size(500, 400)
        call builder%set_backend(stub_backend)
        window = builder%build(status)
    end block
    call forge_check_status(status, abort_on_error=.true.)
    
    ! Create buttons (backend integration would happen here)
    print '(A)', "Creating buttons..."
    call button1%set_name("btn_click_me")
    call button1%set_label("Click Me!")
    call button1%on_click(on_button1_clicked)
    
    call button2%set_name("btn_increment")
    call button2%set_label("Increment Counter")
    call button2%on_click(on_button2_clicked)
    
    call button3%set_name("btn_quit")
    call button3%set_label("Quit")
    call button3%on_click(on_quit_clicked)
    
    print '(A)', "  Button 1: ", trim(button1%get_label())
    print '(A)', "  Button 2: ", trim(button2%get_label())
    print '(A)', "  Button 3: ", trim(button3%get_label())
    print '(A)', ""
    
    ! Show window and run
    call window%show()
    call stub_backend%run()
    
    ! Cleanup
    call window%close()
    call stub_backend%shutdown()
    
    print '(A)', ""
    print '(A)', "=== Demo Complete ==="
    
contains

    !> @brief Callback for button 1 click
    subroutine on_button1_clicked(event)
        type(forge_event), intent(in) :: event
        print '(A)', "[EVENT] Button 1 clicked!"
    end subroutine on_button1_clicked
    
    !> @brief Callback for button 2 click
    subroutine on_button2_clicked(event)
        type(forge_event), intent(in) :: event
        integer, save :: counter = 0
        counter = counter + 1
        print '(A,I0)', "[EVENT] Button 2 clicked! Counter: ", counter
    end subroutine on_button2_clicked
    
    !> @brief Callback for quit button
    subroutine on_quit_clicked(event)
        type(forge_event), intent(in) :: event
        print '(A)', "[EVENT] Quit button clicked - application would exit"
    end subroutine on_quit_clicked

end program button_demo_example
```

## Explanation

### Button Creation and Configuration

```fortran
type(forge_button) :: button1, button2, button3

call button1%set_name("btn_click_me")
call button1%set_label("Click Me!")
```

- `set_name()`: Internal identifier for the button (useful for debugging)
- `set_label()`: Text displayed on the button

### Event Handler Registration

```fortran
call button1%on_click(on_button1_clicked)
```

Registers a callback subroutine to be called when the button is clicked.

### Event Callbacks

```fortran
subroutine on_button1_clicked(event)
    type(forge_event), intent(in) :: event
    print '(A)', "[EVENT] Button 1 clicked!"
end subroutine on_button1_clicked
```

- Event callbacks receive a `forge_event` parameter
- Callbacks should be quick to avoid blocking the UI
- Use the `save` attribute for persistent state between calls

### State Persistence

```fortran
subroutine on_button2_clicked(event)
    type(forge_event), intent(in) :: event
    integer, save :: counter = 0  ! Persists between calls
    counter = counter + 1
    print '(A,I0)', "[EVENT] Button 2 clicked! Counter: ", counter
end subroutine on_button2_clicked
```

The `save` attribute makes variables retain their values between subroutine calls.

## Running the Example

### With fpm

```bash
fpm run button_demo
```

### With CMake

```bash
cd build
cmake --build . --target button_demo_example
./examples/button_demo/button_demo_example
```

### Manual Build

```bash
gfortran -I/path/to/forge/include examples/button_demo/button_demo.f90 -lforge -o button_demo
./button_demo
```

## Expected Output

```
=== Button Demo Example ===

This example demonstrates:
  - Creating multiple buttons
  - Attaching click event handlers
  - Button layout and positioning

NOTE: Using stub backend - events won't fire in demo mode

Creating buttons...
  Button 1: Click Me!
  Button 2: Increment Counter
  Button 3: Quit

=== Demo Complete ===
```

## Key Concepts Demonstrated

1. **Widget Creation**: Instantiating and configuring button widgets
2. **Event Handling**: Registering and implementing event callbacks
3. **State Management**: Using `save` variables for persistent state
4. **Error Handling**: Using `forge_check_status` for robust error checking
5. **Resource Management**: Proper cleanup of windows and backends

## Button Properties

Buttons support various configuration options:

```fortran
! Basic properties
call button%set_label("Button Text")
call button%set_name("unique_id")

! Size and position (inherited from forge_widget)
call button%set_size(100, 30)
call button%set_position(10, 10)

! State
call button%enable()   ! Default state
call button%disable()  ! Grayed out, non-responsive

! Visibility
call button%show()     ! Default state
call button%hide()     ! Invisible but still exists
```

## Event Types

Buttons generate `EVENT_BUTTON_CLICKED` events:

```fortran
subroutine handle_click(event)
    type(forge_event), intent(in) :: event

    select case (event%type)
    case (EVENT_BUTTON_CLICKED)
        ! Handle button click
        call perform_action()
    end select
end subroutine handle_click
```

## Advanced Button Usage

### Toggle Buttons

```fortran
type :: toggle_button
    type(forge_button) :: button
    logical :: is_pressed = .false.
contains
    procedure :: toggle
end type toggle_button

subroutine toggle(this)
    class(toggle_button), intent(inout) :: this
    this%is_pressed = .not. this%is_pressed
    if (this%is_pressed) then
        call this%button%set_label("ON")
    else
        call this%button%set_label("OFF")
    end if
end subroutine toggle
```

### Button Groups

```fortran
subroutine create_button_group(buttons, layout)
    type(forge_button), intent(inout) :: buttons(:)
    type(forge_box_layout), intent(inout) :: layout

    integer :: i

    call layout%set_orientation(LAYOUT_HORIZONTAL)
    do i = 1, size(buttons)
        call layout%add_widget(buttons(i))
    end do
end subroutine create_button_group
```

## Common Patterns

### Action Buttons

```fortran
! OK/Cancel pattern
call ok_button%on_click(handle_ok)
call cancel_button%on_click(handle_cancel)

subroutine handle_ok(event)
    call save_changes()
    call close_dialog()
end subroutine handle_ok

subroutine handle_cancel(event)
    call discard_changes()
    call close_dialog()
end subroutine handle_cancel
```

### Counter Buttons

```fortran
subroutine increment_counter(event)
    integer, save :: count = 0
    count = count + 1
    call update_display(count)
end subroutine increment_counter
```

### Toggle Functionality

```fortran
subroutine toggle_feature(event)
    logical, save :: enabled = .false.
    enabled = .not. enabled

    if (enabled) then
        call enable_feature()
        call button%set_label("Disable Feature")
    else
        call disable_feature()
        call button%set_label("Enable Feature")
    end if
end subroutine toggle_feature
```

## Next Steps

- Try the [interactive button demo](../examples/interactive_button.md) for real event handling
- Learn about [layouts](../tutorials/layout_managers.md) for button arrangement
- Explore the [widget gallery](../tutorials/widget_gallery.md) for other interactive widgets
- See the [calculator demo](../examples/calculator.md) for a complete application