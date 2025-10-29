# forge_slider

Interactive slider widget for selecting values from a continuous range.

## Synopsis

```fortran
type, extends(forge_widget) :: forge_slider
    private
    real(c_double) :: value = 0.0_c_double
    real(c_double) :: minimum = 0.0_c_double
    real(c_double) :: maximum = 100.0_c_double
    real(c_double) :: step = 1.0_c_double
    logical :: vertical = .false.
    type(forge_event_handler) :: value_changed_handler
contains
    procedure :: set_value => forge_slider_set_value
    procedure :: get_value => forge_slider_get_value
    procedure :: set_range => forge_slider_set_range
    procedure :: set_step => forge_slider_set_step
    procedure :: set_vertical => forge_slider_set_vertical
    procedure :: on_value_changed => forge_slider_on_value_changed
end type forge_slider
```

## Description

The `forge_slider` widget provides a visual control for selecting numeric values within a range. It supports:

- Horizontal and vertical orientations
- Configurable value ranges
- Step increments
- Visual feedback during interaction
- Value change event notifications

Sliders are commonly used for:
- Volume controls
- Brightness adjustment
- Progress indication
- Parameter tuning
- Range selection

## Properties

- `value`: Current slider value
- `minimum`: Minimum allowed value
- `maximum`: Maximum allowed value
- `step`: Increment step size
- `vertical`: Orientation flag
- `value_changed_handler`: Event handler for value changes

## Methods

### set_value(value)

Sets the current slider value.

**Parameters:**
- `value` (real): New value (clamped to range)

**Example:**
```fortran
type(forge_slider) :: slider

call slider%set_value(50.0_c_double)
```

### get_value()

Gets the current slider value.

**Returns:** real - Current value

**Example:**
```fortran
real(c_double) :: current_value

current_value = slider%get_value()
print *, "Slider value:", current_value
```

### set_range(minimum, maximum)

Sets the minimum and maximum values.

**Parameters:**
- `minimum` (real): Minimum value
- `maximum` (real): Maximum value

**Example:**
```fortran
! Range from 0 to 255 (for color values)
call slider%set_range(0.0_c_double, 255.0_c_double)
```

### set_step(step)

Sets the step increment for value changes.

**Parameters:**
- `step` (real): Step size

**Example:**
```fortran
! Integer steps
call slider%set_step(1.0_c_double)

! Fine control
call slider%set_step(0.1_c_double)
```

### set_vertical(vertical)

Sets the slider orientation.

**Parameters:**
- `vertical` (logical): True for vertical, false for horizontal

**Example:**
```fortran
! Vertical slider
call slider%set_vertical(.true.)
```

### on_value_changed(callback)

Registers a callback for value change events.

**Parameters:**
- `callback` (procedure): Event callback subroutine

**Example:**
```fortran
call slider%on_value_changed(handle_value_change)

subroutine handle_value_change(event)
    type(forge_event), intent(in) :: event
    real(c_double) :: new_value

    new_value = slider%get_value()
    print *, "Slider changed to:", new_value
    call update_display(new_value)
end subroutine handle_value_change
```

## Value Management

Sliders handle value constraints automatically:

```fortran
! Set range first
call slider%set_range(0.0_c_double, 100.0_c_double)

! Value is clamped to range
call slider%set_value(150.0_c_double)  ! Becomes 100.0
call slider%set_value(-10.0_c_double)  ! Becomes 0.0
```

## Events

Slider widgets generate `EVENT_VALUE_CHANGED` events:

```fortran
subroutine value_changed_handler(event)
    type(forge_event), intent(in) :: event

    select case (event%type)
    case (EVENT_VALUE_CHANGED)
        ! Slider value was modified
        call apply_setting(slider%get_value())
    end select
end subroutine value_changed_handler
```

## Visual Appearance

Slider appearance depends on backend:

- **Custom Backend**: Simple slider with track and thumb
- **Qt Backend**: Native platform slider styling
- **GTK Backend**: GTK scale widget
- **Tcl/Tk Backend**: Tk scale widget

## Common Patterns

### Volume Control
```fortran
type(forge_slider) :: volume_slider

call volume_slider%set_range(0.0_c_double, 100.0_c_double)
call volume_slider%set_value(75.0_c_double)
call volume_slider%set_step(1.0_c_double)
call volume_slider%on_value_changed(update_volume)

subroutine update_volume(event)
    real(c_double) :: volume

    volume = volume_slider%get_value()
    call set_audio_volume(volume / 100.0_c_double)
end subroutine update_volume
```

### Color Picker
```fortran
type(forge_slider) :: red_slider, green_slider, blue_slider

! Red component (0-255)
call red_slider%set_range(0.0_c_double, 255.0_c_double)
call red_slider%on_value_changed(update_color)

! Similar for green and blue...
call green_slider%set_range(0.0_c_double, 255.0_c_double)
call blue_slider%set_range(0.0_c_double, 255.0_c_double)

subroutine update_color(event)
    integer :: red, green, blue

    red = int(red_slider%get_value())
    green = int(green_slider%get_value())
    blue = int(blue_slider%get_value())

    call set_background_color(red, green, blue)
end subroutine update_color
```

### Progress Indicator
```fortran
type(forge_slider) :: progress_slider

call progress_slider%set_range(0.0_c_double, 100.0_c_double)
call progress_slider%set_value(0.0_c_double)
call progress_slider%set_step(0.1_c_double)

! Update during operation
call progress_slider%set_value(current_progress * 100.0_c_double)
```

### Parameter Control
```fortran
type(forge_slider) :: sensitivity_slider

call sensitivity_slider%set_range(0.1_c_double, 10.0_c_double)
call sensitivity_slider%set_value(1.0_c_double)
call sensitivity_slider%set_step(0.1_c_double)
call sensitivity_slider%on_value_changed(update_sensitivity)
```

## Size and Layout

Sliders work well with layout managers:

```fortran
! Horizontal slider expands horizontally
call slider%set_size_policy(QSizePolicy_Expanding, QSizePolicy_Fixed)
call slider%set_size_hint(200, 30)

! Vertical slider expands vertically
call vertical_slider%set_vertical(.true.)
call vertical_slider%set_size_policy(QSizePolicy_Fixed, QSizePolicy_Expanding)
call vertical_slider%set_size_hint(30, 200)
```

## Accessibility

Slider widgets support accessibility features:

- Keyboard navigation (arrow keys, Page Up/Down)
- Screen reader value announcements
- High contrast mode support
- Focus indicators

## Performance

Sliders are lightweight widgets:

- Minimal memory footprint
- Fast value updates
- Efficient event handling
- Smooth visual feedback

## Thread Safety

Slider operations are not thread-safe. Value updates should occur on the main GUI thread.

## Example Usage

```fortran
program slider_demo
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
    window = app%create_window("Slider Demo", 300, 150)

    ! Create volume slider
    call volume_slider%set_range(0.0_c_double, 100.0_c_double)
    call volume_slider%set_value(50.0_c_double)
    call volume_slider%set_step(1.0_c_double)
    call volume_slider%on_value_changed(update_volume_display)

    ! Create label
    call volume_label%set_text("Volume: 50%")

    ! Layout widgets
    call layout%set_orientation(LAYOUT_VERTICAL)
    call layout%set_spacing(10)
    call layout%add_widget(volume_label)
    call layout%add_widget(volume_slider)

    ! Show window
    call window%show()
    call app%run()

contains

    subroutine update_volume_display(event)
        type(forge_event), intent(in) :: event
        real(c_double) :: volume
        character(len=20) :: text

        volume = volume_slider%get_value()
        write(text, "('Volume: ', I0, '%')") int(volume)
        call volume_label%set_text(trim(text))
    end subroutine update_volume_display

end program slider_demo
```

## Advanced Usage

### Multi-range Sliders
```fortran
! Planned: Range sliders with min/max handles
type(forge_range_slider) :: range_slider

call range_slider%set_range(0.0_c_double, 100.0_c_double)
call range_slider%set_values(25.0_c_double, 75.0_c_double)  ! Min and max
```

### Non-linear Scales
```fortran
subroutine set_logarithmic_scale(slider)
    type(forge_slider), intent(inout) :: slider

    ! Set up for logarithmic mapping
    call slider%set_range(0.01_c_double, 100.0_c_double)
    call slider%set_step(0.01_c_double)
    ! Value mapping handled in callback
end subroutine set_logarithmic_scale

subroutine logarithmic_callback(event)
    real(c_double) :: linear_value, log_value

    linear_value = slider%get_value()
    log_value = 10.0_c_double ** (linear_value / 20.0_c_double)  ! dB to linear
    call apply_logarithmic_setting(log_value)
end subroutine logarithmic_callback
```

### Slider Groups
```fortran
subroutine link_sliders(slider1, slider2, ratio)
    type(forge_slider), intent(inout) :: slider1, slider2
    real(c_double), intent(in) :: ratio

    call slider1%on_value_changed(link_callback)

    subroutine link_callback(event)
        real(c_double) :: value1

        value1 = slider1%get_value()
        call slider2%set_value(value1 * ratio)
    end subroutine link_callback
end subroutine link_sliders
```

### Custom Ticks
```fortran
! Planned: Custom tick marks and labels
call slider%add_tick(25.0_c_double, "Low")
call slider%add_tick(50.0_c_double, "Medium")
call slider%add_tick(75.0_c_double, "High")
call slider%set_tick_visible(.true.)
```

## See Also

- [forge_widget](forge_widget.md) - Base widget class
- [forge_spin_button](forge_spin_button.md) - Numeric input with buttons
- [forge_progress_bar](forge_progress_bar.md) - Progress display
- [forge_event](forge_event.md) - Event handling