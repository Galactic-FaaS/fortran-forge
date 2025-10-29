# Event Handling Tutorial

This tutorial covers ForGE's event system, which enables responsive user interfaces through event-driven programming.

## Overview

Events in ForGE represent user interactions and system notifications. The event system follows the observer pattern, allowing widgets and windows to notify your code when interesting things happen.

## Event Types

ForGE defines several standard event types:

- `EVENT_BUTTON_CLICKED`: Button pressed
- `EVENT_WINDOW_CLOSED`: Window close requested
- `EVENT_KEY_PRESSED`: Keyboard key pressed
- `EVENT_MOUSE_MOVED`: Mouse movement
- `EVENT_TEXT_CHANGED`: Text input modified
- `EVENT_VALUE_CHANGED`: Widget value changed

## Basic Event Handling

### Button Click Events

```fortran
program button_events_demo
    use forge
    implicit none

    type(forge_application) :: app
    type(forge_window_t) :: window
    type(forge_button) :: click_button
    type(forge_label) :: message_label
    type(forge_box_layout) :: layout
    type(forge_status) :: status

    ! Initialize
    call app%init(BACKEND_CUSTOM, status)
    window = app%create_window("Button Events", 300, 150)

    ! Create widgets
    call click_button%set_label("Click Me!")
    call message_label%set_text("Button not clicked yet")

    ! Register event handler
    call click_button%on_click(handle_button_click)

    ! Layout
    call layout%set_orientation(LAYOUT_VERTICAL)
    call layout%add_widget(click_button)
    call layout%add_widget(message_label)
    call layout%set_parent_size(300, 150)
    call layout%compute()

    ! Show window and run
    call window%show()
    call app%run()
    call app%shutdown()

contains

    subroutine handle_button_click(event)
        type(forge_event), intent(in) :: event

        ! Update message when button is clicked
        call message_label%set_text("Button was clicked!")

        ! Could perform any action here:
        ! - Save data
        ! - Open dialog
        ! - Update other widgets
        ! - Call external functions
    end subroutine handle_button_click

end program button_events_demo
```

### Window Events

```fortran
program window_events_demo
    use forge
    implicit none

    type(forge_application) :: app
    type(forge_window_t) :: window
    type(forge_label) :: status_label
    type(forge_status) :: status

    ! Initialize
    call app%init(BACKEND_CUSTOM, status)
    window = app%create_window("Window Events", 300, 100)

    ! Create status label
    call status_label%set_text("Window is open")

    ! Register window event handlers
    call window%on_close(handle_window_close)

    ! Layout
    call status_label%set_position(50, 30)

    ! Show window
    call window%show()
    call app%run()
    call app%shutdown()

contains

    subroutine handle_window_close(event)
        type(forge_event), intent(in) :: event

        print *, "Window is closing - perform cleanup here"

        ! Perform cleanup:
        ! - Save user data
        ! - Close database connections
        ! - Stop background tasks
        ! - Free resources

        call status_label%set_text("Window closed")
    end subroutine handle_window_close

end program window_events_demo
```

## Text Input Events

### Entry Field Changes

```fortran
program text_events_demo
    use forge
    implicit none

    type(forge_application) :: app
    type(forge_window_t) :: window
    type(forge_entry) :: name_entry
    type(forge_label) :: greeting_label
    type(forge_form_layout) :: form
    type(forge_status) :: status

    ! Initialize
    call app%init(BACKEND_CUSTOM, status)
    window = app%create_window("Text Events", 350, 150)

    ! Create widgets
    call name_entry%set_placeholder("Enter your name")
    call greeting_label%set_text("Hello, stranger!")

    ! Register text change handler
    call name_entry%on_change(handle_text_change)

    ! Layout
    call form%add_row(name_entry, greeting_label)
    call form%set_parent_size(350, 150)
    call form%compute()

    ! Show window
    call window%show()
    call app%run()
    call app%shutdown()

contains

    subroutine handle_text_change(event)
        type(forge_event), intent(in) :: event
        character(len=:), allocatable :: name

        ! Get current text
        name = name_entry%get_text()

        ! Update greeting
        if (len(trim(name)) > 0) then
            call greeting_label%set_text("Hello, " // trim(name) // "!")
        else
            call greeting_label%set_text("Hello, stranger!")
        end if
    end subroutine handle_text_change

end program text_events_demo
```

### Text Validation

```fortran
subroutine handle_email_change(event)
    type(forge_event), intent(in) :: event
    character(len=:), allocatable :: email
    logical :: is_valid

    email = email_entry%get_text()
    is_valid = validate_email(email)

    if (.not. is_valid .and. len(email) > 0) then
        call error_label%set_text("Invalid email format")
        call email_entry%set_background_color(COLOR_LIGHT_RED)
    else
        call error_label%set_text("")
        call email_entry%set_background_color(COLOR_WHITE)
    end if
end subroutine handle_email_change

function validate_email(email) result(valid)
    character(len=*), intent(in) :: email
    logical :: valid

    ! Simple email validation
    integer :: at_pos, dot_pos

    at_pos = index(email, '@')
    dot_pos = index(email, '.', back=.true.)

    valid = at_pos > 1 .and. dot_pos > at_pos + 1 .and. dot_pos < len(email)
end function validate_email
```

## Value Change Events

### Slider Events

```fortran
program slider_events_demo
    use forge
    implicit none

    type(forge_application) :: app
    type(forge_window_t) :: window
    type(forge_slider) :: volume_slider
    type(forge_label) :: volume_label
    type(forge_box_layout) :: layout
    type(forge_status) :: status

    ! Initialize
    call app%init(BACKEND_CUSTOM, status)
    window = app%create_window("Slider Events", 300, 120)

    ! Create slider
    call volume_slider%set_range(0.0_c_double, 100.0_c_double)
    call volume_slider%set_value(50.0_c_double)
    call volume_slider%on_value_changed(handle_volume_change)

    ! Create label
    call volume_label%set_text("Volume: 50%")

    ! Layout
    call layout%set_orientation(LAYOUT_VERTICAL)
    call layout%add_widget(volume_slider)
    call layout%add_widget(volume_label)
    call layout%set_parent_size(300, 120)
    call layout%compute()

    ! Show window
    call window%show()
    call app%run()
    call app%shutdown()

contains

    subroutine handle_volume_change(event)
        type(forge_event), intent(in) :: event
        real(c_double) :: volume
        character(len=20) :: text

        volume = volume_slider%get_value()
        write(text, "('Volume: ', I0, '%')") int(volume)
        call volume_label%set_text(trim(text))

        ! Apply volume change
        call set_audio_volume(volume / 100.0_c_double)
    end subroutine handle_volume_change

end program slider_events_demo
```

### Spin Button Events

```fortran
program spin_events_demo
    use forge
    implicit none

    type(forge_application) :: app
    type(forge_window_t) :: window
    type(forge_spin_button) :: quantity_spin
    type(forge_label) :: total_label
    type(forge_form_layout) :: form
    type(forge_status) :: status
    real(c_double) :: price_per_item = 19.99_c_double

    ! Initialize
    call app%init(BACKEND_CUSTOM, status)
    window = app%create_window("Spin Events", 300, 120)

    ! Create spin button
    call quantity_spin%set_range(1.0_c_double, 99.0_c_double)
    call quantity_spin%set_value(1.0_c_double)
    call quantity_spin%on_value_changed(update_total)

    ! Create total label
    call total_label%set_text("Total: $19.99")

    ! Layout
    call form%add_row(quantity_spin, total_label)
    call form%set_parent_size(300, 120)
    call form%compute()

    ! Show window
    call window%show()
    call app%run()
    call app%shutdown()

contains

    subroutine update_total(event)
        type(forge_event), intent(in) :: event
        real(c_double) :: quantity, total
        character(len=20) :: text

        quantity = quantity_spin%get_value()
        total = quantity * price_per_item

        write(text, "('Total: $', F0.2)") total
        call total_label%set_text(trim(text))
    end subroutine update_total

end program spin_events_demo
```

## Selection Events

### Combo Box Events

```fortran
program combo_events_demo
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
    window = app%create_window("Combo Events", 300, 120)

    ! Create combo box
    call color_combo%add_item("Red")
    call color_combo%add_item("Green")
    call color_combo%add_item("Blue")
    call color_combo%set_selected_index(1)
    call color_combo%on_selection_changed(handle_color_change)

    ! Create label
    call selection_label%set_text("Selected: Red")

    ! Layout
    call layout%set_orientation(LAYOUT_VERTICAL)
    call layout%add_widget(color_combo)
    call layout%add_widget(selection_label)
    call layout%set_parent_size(300, 120)
    call layout%compute()

    ! Show window
    call window%show()
    call app%run()
    call app%shutdown()

contains

    subroutine handle_color_change(event)
        type(forge_event), intent(in) :: event
        character(len=:), allocatable :: color

        color = color_combo%get_selected_text()
        call selection_label%set_text("Selected: " // color)

        ! Apply color change
        call set_background_color(color)
    end subroutine handle_color_change

end program combo_events_demo
```

## Multiple Event Handlers

Widgets can have multiple event handlers for the same event type.

```fortran
program multiple_handlers_demo
    use forge
    implicit none

    type(forge_application) :: app
    type(forge_window_t) :: window
    type(forge_button) :: action_button
    type(forge_status) :: status

    ! Initialize
    call app%init(BACKEND_CUSTOM, status)
    window = app%create_window("Multiple Handlers", 200, 100)

    ! Create button with multiple handlers
    call action_button%set_label("Do It")
    call action_button%on_click(log_click)
    call action_button%on_click(update_counter)
    call action_button%on_click(play_sound)

    ! Layout
    call action_button%set_position(50, 30)

    ! Show window
    call window%show()
    call app%run()
    call app%shutdown()

contains

    subroutine log_click(event)
        type(forge_event), intent(in) :: event
        print *, "Button clicked - logging action"
    end subroutine log_click

    subroutine update_counter(event)
        type(forge_event), intent(in) :: event
        integer, save :: click_count = 0

        click_count = click_count + 1
        print *, "Click count:", click_count
    end subroutine update_counter

    subroutine play_sound(event)
        type(forge_event), intent(in) :: event
        print *, "Playing click sound"
        ! call play_click_sound()
    end subroutine play_sound

end program multiple_handlers_demo
```

## Event Data

Some events carry additional data.

```fortran
subroutine handle_key_press(event)
    type(forge_event), intent(in) :: event

    select case (event%type)
    case (EVENT_KEY_PRESSED)
        ! Access key code from event data
        select case (event%data%key_code)
        case (KEY_ENTER)
            call submit_form()
        case (KEY_ESCAPE)
            call cancel_operation()
        case (KEY_F1)
            call show_help()
        end select
    end select
end subroutine handle_key_press
```

## Event Propagation

Events flow from the widget that generated them through the system.

```fortran
! Widget-level event
call button%on_click(handle_button_event)

! Window-level event
call window%on_close(handle_window_event)

! Application-level event (planned)
! call app%on_quit(handle_app_event)
```

## Error Handling in Events

Handle errors gracefully in event handlers.

```fortran
subroutine safe_event_handler(event)
    type(forge_event), intent(in) :: event

    ! Use error handling in event callbacks
    call risky_operation()
    if (error_occurred) then
        call show_error_dialog("Operation failed")
        return
    end if

    call update_ui()
end subroutine safe_event_handler
```

## Performance Considerations

Keep event handlers fast and responsive.

```fortran
! Good: Quick response
subroutine quick_handler(event)
    type(forge_event), intent(in) :: event
    call update_status_indicator()
end subroutine quick_handler

! Bad: Slow operation blocks UI
subroutine slow_handler(event)
    type(forge_event), intent(in) :: event
    call perform_expensive_calculation()  ! Blocks UI!
end subroutine slow_handler

! Better: Start background task
subroutine async_handler(event)
    type(forge_event), intent(in) :: event
    call start_background_calculation()
    call show_progress_indicator()
end subroutine async_handler
```

## Thread Safety

Event handlers execute on the main GUI thread. Avoid thread-safety issues:

```fortran
! Safe: Update UI from event handler
subroutine update_ui_handler(event)
    call label%set_text("Updated")  ! OK - main thread
end subroutine update_ui_handler

! Unsafe: Access shared data without synchronization
subroutine unsafe_handler(event)
    shared_counter = shared_counter + 1  ! Race condition!
end subroutine unsafe_handler
```

## Event Filtering

Create event filters for complex logic.

```fortran
subroutine create_event_filter(widget, filter_proc)
    type(forge_widget), intent(inout) :: widget
    procedure(event_filter_interface) :: filter_proc

    ! Install filter that can modify or block events
    call widget%install_event_filter(filter_proc)
end subroutine create_event_filter

subroutine my_event_filter(event, accept)
    type(forge_event), intent(inout) :: event
    logical, intent(out) :: accept

    ! Filter logic
    if (event%type == EVENT_KEY_PRESSED .and. &
        event%data%key_code == KEY_DELETE) then

        ! Ask user to confirm deletion
        accept = confirm_deletion()
    else
        accept = .true.  ! Accept other events
    end if
end subroutine my_event_filter
```

## Custom Events

Generate custom events for application-specific notifications.

```fortran
! Define custom event type
integer, parameter :: EVENT_DATA_LOADED = 1000

! Create and send custom event
subroutine notify_data_loaded()
    type(forge_event) :: custom_event

    custom_event%type = EVENT_DATA_LOADED
    ! Set custom_event%data as needed

    call send_custom_event(custom_event)
end subroutine notify_data_loaded

! Handle custom event
subroutine handle_custom_event(event)
    type(forge_event), intent(in) :: event

    select case (event%type)
    case (EVENT_DATA_LOADED)
        call update_data_display()
    end select
end subroutine handle_custom_event
```

## Best Practices

### Use Meaningful Handler Names

```fortran
! Good: Descriptive names
call submit_button%on_click(handle_form_submission)
call cancel_button%on_click(handle_operation_cancel)

! Bad: Generic names
call submit_button%on_click(button_click)
call cancel_button%on_click(button_click2)
```

### Separate Event Logic

```fortran
! Good: Separate event handling from business logic
subroutine handle_save_click(event)
    type(forge_event), intent(in) :: event

    ! Event handling
    call disable_save_button()

    ! Business logic
    call perform_save_operation()
end subroutine handle_save_click

! Bad: Mix event and business logic
subroutine handle_save_click(event)
    call save_button%disable()
    ! ... 50 lines of save logic ...
end subroutine handle_save_click
```

### Handle Errors Gracefully

```fortran
subroutine robust_event_handler(event)
    type(forge_event), intent(in) :: event

    ! Use try-catch or status checking
    call perform_operation()
    if (operation_failed) then
        call show_user_friendly_error()
        return
    end if

    call update_ui_success()
end subroutine robust_event_handler
```

### Avoid Infinite Loops

```fortran
! Bad: Event triggers another event
subroutine bad_handler(event)
    call text_field%set_text("new text")  ! Triggers text change event!
end subroutine bad_handler

! Good: Prevent recursive events
logical, save :: updating = .false.

subroutine good_handler(event)
    if (updating) return
    updating = .true

    call text_field%set_text("new text")
    updating = .false
end subroutine good_handler
```

## Common Patterns

### Form Validation

```fortran
logical :: form_valid = .true.

subroutine validate_field(event)
    type(forge_event), intent(in) :: event

    if (.not. is_valid_input()) then
        form_valid = .false.
        call highlight_invalid_field()
    else
        form_valid = .true.
        call clear_field_highlight()
    end if

    call update_submit_button_state()
end subroutine validate_field
```

### Master-Detail Updates

```fortran
subroutine handle_master_selection(event)
    ! Update detail view based on master selection
    call load_detail_data(selected_id)
    call update_detail_display()
end subroutine handle_master_selection
```

### Progress Updates

```fortran
subroutine handle_progress_update(event)
    real :: progress

    progress = get_current_progress()
    call progress_bar%set_value(progress)

    if (progress >= 1.0) then
        call show_completion_message()
    end if
end subroutine handle_progress_update
```

## Next Steps

- Read the [API documentation](../api/) for detailed event type references
- Explore the [examples](../../examples/) directory for more event examples
- Learn about [layout managers](layout_managers.md) for organizing widgets
- Study [widget gallery](widget_gallery.md) to see all available widgets