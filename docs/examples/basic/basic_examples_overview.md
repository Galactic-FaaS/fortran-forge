# Basic Examples

This section contains fundamental examples demonstrating the core concepts and basic usage patterns of ForGE Qt.

## Hello World Example

**File:** `examples/hello_world/hello_world.f90`

A simple "Hello World" application that demonstrates:
- Creating a basic window
- Setting window properties (title, size, position)
- Using the builder pattern for window construction
- Basic application lifecycle

```fortran
program hello_world_example
    use forge
    use forge_stub_backend
    implicit none

    type(forge_window_t) :: window
    type(forge_status) :: status

    ! Create window using builder pattern
    window = forge_window_builder() &
        %set_title("Hello, ForGE!") &
        %set_size(400, 300) &
        %build(status)

    ! Show the window
    call window%show()
end program hello_world_example
```

## Basic Widget Examples

### Label Widget

**File:** `examples/basic_label/basic_label.f90`

Demonstrates:
- Creating label widgets
- Setting text content
- Basic text formatting
- Label positioning

### Entry Widget

**File:** `examples/basic_entry/basic_entry.f90`

Demonstrates:
- Text input fields
- Placeholder text
- Password mode
- Input validation

### Checkbox Widget

**File:** `examples/basic_checkbox/basic_checkbox.f90`

Demonstrates:
- Boolean input controls
- Checked/unchecked states
- Tri-state checkboxes
- State change handling

### Radio Button Widget

**File:** `examples/basic_radio/basic_radio.f90`

Demonstrates:
- Exclusive selection groups
- Radio button groups
- State management

### Combo Box Widget

**File:** `examples/basic_combo/basic_combo.f90`

Demonstrates:
- Dropdown selection lists
- Editable vs read-only combos
- Item management
- Selection handling

### Slider Widget

**File:** `examples/basic_slider/basic_slider.f90`

Demonstrates:
- Range input controls
- Value selection
- Horizontal/vertical orientations
- Step sizes

### Spin Button Widget

**File:** `examples/basic_spin/basic_spin.f90`

Demonstrates:
- Numeric input with increment/decrement
- Range limits
- Step values
- Decimal precision

### Text View Widget

**File:** `examples/basic_textview/basic_textview.f90`

Demonstrates:
- Multi-line text display
- Read-only vs editable modes
- Text formatting
- Scrolling

## Basic Layout Examples

### Vertical Layout

**File:** `examples/basic_layout_vertical/basic_layout_vertical.f90`

Demonstrates:
- Vertical box layout
- Widget arrangement
- Spacing and margins
- Layout properties

### Horizontal Layout

**File:** `examples/basic_layout_horizontal/basic_layout_horizontal.f90`

Demonstrates:
- Horizontal box layout
- Side-by-side widget arrangement
- Layout alignment

### Grid Layout

**File:** `examples/basic_layout_grid/basic_layout_grid.f90`

Demonstrates:
- Grid-based widget positioning
- Row and column spanning
- Grid sizing

## Running the Examples

All basic examples can be built and run using the standard ForGE build process:

```bash
# Build all examples
cmake --build build --target all

# Run a specific example
./build/examples/hello_world/hello_world

# Or run a basic widget example
./build/examples/basic_label/basic_label
```

## Key Concepts Covered

- **Widget Creation:** Basic instantiation and configuration
- **Property Setting:** Text, size, position, state
- **Event Handling:** Basic user interaction
- **Layout Management:** Arranging widgets in containers
- **Application Structure:** Program organization and cleanup

These examples provide a foundation for understanding ForGE Qt's basic functionality before moving to more advanced features.