# forge_spin_button

Numeric input widget with up/down buttons for precise value selection.

## Synopsis

```fortran
type, extends(forge_widget) :: forge_spin_button
    private
    real(c_double) :: value = 0.0_c_double
    real(c_double) :: minimum = 0.0_c_double
    real(c_double) :: maximum = 100.0_c_double
    real(c_double) :: step = 1.0_c_double
    integer :: digits = 0
    type(forge_event_handler) :: value_changed_handler
contains
    procedure :: set_value => forge_spin_button_set_value
    procedure :: get_value => forge_spin_button_get_value
    procedure :: set_range => forge_spin_button_set_range
    procedure :: set_step => forge_spin_button_set_step
    procedure :: set_digits => forge_spin_button_set_digits
    procedure :: on_value_changed => forge_spin_button_on_value_changed
end type forge_spin_button
```

## Description

The `forge_spin_button` widget provides a text field with up/down buttons for entering numeric values. It supports:

- Precise numeric input
- Configurable value ranges
- Step increments
- Decimal precision control
- Keyboard and mouse navigation
- Value validation

Spin buttons are ideal for:
- Numeric parameter input
- Quantity selection
- Precision value entry
- Settings configuration
- Form numeric fields

## Properties

- `value`: Current numeric value
- `minimum`: Minimum allowed value
- `maximum`: Maximum allowed value
- `step`: Increment/decrement step size
- `digits`: Decimal places to display
- `value_changed_handler`: Event handler for value changes

## Methods

### set_value(value)

Sets the current value.

**Parameters:**
- `value` (real): New value (clamped to range)

**Example:**
```fortran
type(forge_spin_button) :: spin

call spin%set_value(42.5_c_double)
```

### get_value()

Gets the current value.

**Returns:** real - Current numeric value

**Example:**
```fortran
real(c_double) :: current

current = spin%get_value()
print *, "Value:", current
```

### set_range(minimum, maximum)

Sets the allowed value range.

**Parameters:**
- `minimum` (real): Minimum value
- `maximum` (real): Maximum value

**Example:**
```fortran
! Age input (0-120)
call spin%set_range(0.0_c_double, 120.0_c_double)
```

### set_step(step)

Sets the increment/decrement step size.

**Parameters:**
- `step` (real): Step size

**Example:**
```fortran
! Integer steps
call spin%set_step(1.0_c_double)

! Fine adjustment
call spin%set_step(0.1_c_double)
```

### set_digits(digits)

Sets the number of decimal places to display.

**Parameters:**
- `digits` (integer): Decimal places (0 for integers)

**Example:**
```fortran
! Currency (2 decimal places)
call spin%set_digits(2)

! Integer values
call spin%set_digits(0)
```

### on_value_changed(callback)

Registers a callback for value change events.

**Parameters:**
- `callback` (procedure): Event callback subroutine

**Example:**
```fortran
call spin%on_value_changed(handle_value_change)

subroutine handle_value_change(event)
    type(forge_event), intent(in) :: event
    real(c_double) :: new_value

    new_value = spin%get_value()
    print *, "New value:", new_value
    call update_calculation(new_value)
end subroutine handle_value_change
```

## Value Management

Spin buttons handle value constraints:

```fortran
! Set range and step
call spin%set_range(0.0_c_double, 100.0_c_double)
call spin%set_step(5.0_c_double)

! Value is clamped and stepped
call spin%set_value(47.3_c_double)  ! Becomes 45.0 (nearest step)
call spin%set_value(150.0_c_double) ! Becomes 100.0 (range max)
```

## User Interaction

Spin buttons support multiple input methods:

- **Direct typing**: Enter value in text field
- **Up/Down buttons**: Increment/decrement by step
- **Arrow keys**: Keyboard navigation
- **Page Up/Down**: Larger steps
- **Mouse wheel**: Scroll to change value

## Events

Spin buttons generate `EVENT_VALUE_CHANGED` events:

```fortran
subroutine value_changed_handler(event)
    type(forge_event), intent(in) :: event

    select case (event%type)
    case (EVENT_VALUE_CHANGED)
        ! Value was modified
        call recalculate_total()
    end select
end subroutine value_changed_handler
```

## Display Formatting

Spin buttons format values based on digits setting:

```fortran
call spin%set_digits(2)
call spin%set_value(123.456_c_double)
! Displays: "123.46"

call spin%set_digits(0)
call spin%set_value(123.456_c_double)
! Displays: "123"
```

## Common Patterns

### Integer Input
```fortran
type(forge_spin_button) :: quantity_spin

call quantity_spin%set_range(1.0_c_double, 99.0_c_double)
call quantity_spin%set_step(1.0_c_double)
call quantity_spin%set_digits(0)
call quantity_spin%set_value(1.0_c_double)
```

### Decimal Input
```fortran
type(forge_spin_button) :: price_spin

call price_spin%set_range(0.0_c_double, 999.99_c_double)
call price_spin%set_step(0.01_c_double)
call price_spin%set_digits(2)
call price_spin%set_value(19.99_c_double)
```

### Percentage Input
```fortran
type(forge_spin_button) :: percent_spin

call percent_spin%set_range(0.0_c_double, 100.0_c_double)
call percent_spin%set_step(1.0_c_double)
call percent_spin%set_digits(0)
call percent_spin%set_value(75.0_c_double)
```

### Scientific Input
```fortran
type(forge_spin_button) :: precision_spin

call precision_spin%set_range(0.001_c_double, 1000.0_c_double)
call precision_spin%set_step(0.001_c_double)
call precision_spin%set_digits(3)
call precision_spin%set_value(1.000_c_double)
```

## Size and Layout

Spin buttons have reasonable default sizes:

```fortran
! Default size is usually adequate
type(forge_spin_button) :: spin

! Or set custom size hint
call spin%set_size_hint(100, 25)
```

## Validation

Spin buttons provide automatic validation:

- **Range checking**: Values clamped to min/max
- **Step enforcement**: Values rounded to nearest step
- **Type checking**: Only numeric input allowed
- **Format validation**: Proper decimal formatting

## Accessibility

Spin buttons support accessibility features:

- Screen reader value announcements
- Keyboard navigation
- High contrast mode
- Form field labeling

## Performance

Spin buttons are efficient widgets:

- Fast value updates
- Minimal memory usage
- Efficient text rendering
- Responsive user interaction

## Thread Safety

Spin button operations are not thread-safe. Value updates should occur on the main GUI thread.

## Example Usage

```fortran
program spin_button_demo
    use forge
    implicit none

    type(forge_application) :: app
    type(forge_window_t) :: window
    type(forge_spin_button) :: quantity_spin, price_spin
    type(forge_label) :: quantity_label, price_label, total_label
    type(forge_form_layout) :: form
    type(forge_status) :: status

    ! Initialize
    call app%init(BACKEND_CUSTOM, status)
    window = app%create_window("Spin Button Demo", 300, 200)

    ! Create quantity input
    call quantity_label%set_text("Quantity:")
    call quantity_spin%set_range(1.0_c_double, 100.0_c_double)
    call quantity_spin%set_step(1.0_c_double)
    call quantity_spin%set_digits(0)
    call quantity_spin%set_value(1.0_c_double)
    call quantity_spin%on_value_changed(update_total)

    ! Create price input
    call price_label%set_text("Price:")
    call price_spin%set_range(0.0_c_double, 999.99_c_double)
    call price_spin%set_step(0.01_c_double)
    call price_spin%set_digits(2)
    call price_spin%set_value(19.99_c_double)
    call price_spin%on_value_changed(update_total)

    ! Create total display
    call total_label%set_text("Total: $19.99")

    ! Set up form
    call form%add_row(quantity_label, quantity_spin)
    call form%add_row(price_label, price_spin)
    call form%add_widget(total_label, 3, 1, 1, 2)

    ! Show window
    call window%show()
    call app%run()

contains

    subroutine update_total(event)
        type(forge_event), intent(in) :: event
        real(c_double) :: quantity, price, total
        character(len=20) :: total_text

        quantity = quantity_spin%get_value()
        price = price_spin%get_value()
        total = quantity * price

        write(total_text, "('Total: $', F0.2)") total
        call total_label%set_text(trim(total_text))
    end subroutine update_total

end program spin_button_demo
```

## Advanced Usage

### Custom Validation
```fortran
subroutine set_custom_validation(spin, validator)
    type(forge_spin_button), intent(inout) :: spin
    procedure(validator_interface) :: validator

    call spin%on_value_changed(custom_validator)

contains

    subroutine custom_validator(event)
        real(c_double) :: value

        value = spin%get_value()
        if (.not. validator(value)) then
            ! Invalid value - could revert or show error
            call show_validation_error()
        end if
    end subroutine custom_validator

end subroutine set_custom_validation
```

### Linked Spin Buttons
```fortran
subroutine link_spin_buttons(master, slave, multiplier)
    type(forge_spin_button), intent(inout) :: master, slave
    real(c_double), intent(in) :: multiplier

    call master%on_value_changed(link_update)

contains

    subroutine link_update(event)
        real(c_double) :: master_value

        master_value = master%get_value()
        call slave%set_value(master_value * multiplier)
    end subroutine link_update

end subroutine link_spin_buttons
```

### Unit Conversion
```fortran
type(forge_spin_button) :: celsius_spin, fahrenheit_spin

call celsius_spin%set_range(-273.15_c_double, 1000.0_c_double)
call fahrenheit_spin%set_range(-459.67_c_double, 1832.0_c_double)

call celsius_spin%on_value_changed(convert_c_to_f)
call fahrenheit_spin%on_value_changed(convert_f_to_c)

subroutine convert_c_to_f(event)
    real(c_double) :: celsius, fahrenheit

    celsius = celsius_spin%get_value()
    fahrenheit = celsius * 9.0_c_double / 5.0_c_double + 32.0_c_double
    call fahrenheit_spin%set_value(fahrenheit)
end subroutine convert_c_to_f
```

### Time Input
```fortran
type(forge_spin_button) :: hour_spin, minute_spin

! Hours (0-23)
call hour_spin%set_range(0.0_c_double, 23.0_c_double)
call hour_spin%set_step(1.0_c_double)
call hour_spin%set_digits(0)

! Minutes (0-59)
call minute_spin%set_range(0.0_c_double, 59.0_c_double)
call minute_spin%set_step(1.0_c_double)
call minute_spin%set_digits(0)
```

## See Also

- [forge_widget](forge_widget.md) - Base widget class
- [forge_slider](forge_slider.md) - Range selection slider
- [forge_entry](forge_entry.md) - General text input
- [forge_event](forge_event.md) - Event handling