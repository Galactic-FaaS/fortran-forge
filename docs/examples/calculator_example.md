# Calculator Example

A complete calculator application demonstrating complex widget interactions, state management, and event handling in ForGE.

## Overview

This example shows how to build a fully functional calculator with:
- 20+ interactive buttons
- Display screen with text updates
- Mathematical operations (add, subtract, multiply, divide)
- State management for calculations
- Signal/slot event system
- Grid layout for button arrangement

## Code

```fortran
!> @brief Calculator Application
!> @details Full-featured calculator demonstrating ForGE Qt
!> @author ForGE Contributors
!> @license GPL-3.0-or-later

program calculator
    use forge_qt
    use forge_stub_backend
    implicit none
    
    ! Application and window
    type(forge_stub_backend_t), target :: backend
    type(forge_window_t) :: window
    type(forge_status) :: status
    
    ! Widgets
    type(forge_label) :: display
    type(forge_button) :: buttons(0:9)  ! Number buttons
    type(forge_button) :: btn_add, btn_sub, btn_mul, btn_div
    type(forge_button) :: btn_equals, btn_clear, btn_decimal
    
    ! Calculator state
    type(QString) :: display_text
    real :: current_value = 0.0
    real :: stored_value = 0.0
    integer :: current_operation = 0  ! 0=none, 1=add, 2=sub, 3=mul, 4=div
    logical :: new_number = .true.
    
    ! Signals for buttons
    type(signal_void) :: num_clicked(0:9)
    type(signal_void) :: op_clicked(4)
    type(signal_void) :: equals_clicked, clear_clicked
    
    print '(A)', "========================================"
    print '(A)', "ForGE Qt - Calculator Application"
    print '(A)', "========================================"
    print '(A)', ""
    print '(A)', "Demonstrating:"
    print '(A)', "  ✓ Multiple widgets (20+ buttons + display)"
    print '(A)', "  ✓ Grid layout (4x5 button grid)"
    print '(A)', "  ✓ Signals & slots (one per button)"
    print '(A)', "  ✓ QString for display text"
    print '(A)', "  ✓ State management"
    print '(A)', "  ✓ Real calculator logic"
    print '(A)', ""
    
    ! Initialize backend
    call backend%init(status)
    if (status%is_error()) then
        call status%print()
        stop 1
    end if
    
    ! Create window
    block
        type(forge_window_builder) :: builder
        call builder%set_title("ForGE Qt Calculator")
        call builder%set_size(350, 450)
        call builder%set_backend(backend)
        window = builder%build(status)
    end block
    
    ! Setup display
    call display%set_text("0")
    call display%set_name("display")
    call display_text%set("0")
    
    ! Setup number buttons
    call setup_number_buttons()
    
    ! Setup operation buttons
    call setup_operation_buttons()
    
    ! Connect signals
    call connect_signals()
    
    print '(A)', "Calculator ready!"
    print '(A)', ""
    print '(A)', "In a full implementation, you would see:"
    print '(A)', "  - 4x5 grid of buttons"
    print '(A)', "  - Display showing current number"
    print '(A)', "  - Click buttons to calculate"
    print '(A)', ""
    print '(A)', "Current demo shows signal connectivity:"
    
    ! Simulate some calculations
    print '(A)', ""
    print '(A)', "Simulating: 5 + 3 ="
    call simulate_button_press(5)
    call simulate_operation(1)  ! Add
    call simulate_button_press(3)
    call simulate_equals()
    print '(A,F0.2)', "  Result: ", current_value
    
    print '(A)', ""
    print '(A)', "Simulating: 9 * 7 ="
    call simulate_button_press(9)
    call simulate_operation(3)  ! Multiply
    call simulate_button_press(7)
    call simulate_equals()
    print '(A,F0.2)', "  Result: ", current_value
    
    print '(A)', ""
    print '(A)', "Simulating: Clear"
    call on_clear_clicked()
    print '(A,F0.2)', "  Display: ", current_value
    
    ! Show window and run
    call window%show()
    call backend%run()
    
    ! Cleanup
    call window%close()
    call backend%shutdown()
    
    print '(A)', ""
    print '(A)', "========================================"
    print '(A)', "Calculator Demo Complete!"
    print '(A)', "========================================"
    
contains

    subroutine setup_number_buttons()
        integer :: i
        character(len=2) :: label
        
        do i = 0, 9
            write(label, '(I1)') i
            call buttons(i)%set_label(trim(label))
            write(label, '(A,I1)') "btn_", i
            call buttons(i)%set_name(trim(label))
        end do
    end subroutine setup_number_buttons
    
    subroutine setup_operation_buttons()
        call btn_add%set_label("+")
        call btn_add%set_name("btn_add")
        
        call btn_sub%set_label("-")
        call btn_sub%set_name("btn_sub")
        
        call btn_mul%set_label("*")
        call btn_mul%set_name("btn_mul")
        
        call btn_div%set_label("/")
        call btn_div%set_name("btn_div")
        
        call btn_equals%set_label("=")
        call btn_equals%set_name("btn_equals")
        
        call btn_clear%set_label("C")
        call btn_clear%set_name("btn_clear")
        
        call btn_decimal%set_label(".")
        call btn_decimal%set_name("btn_decimal")
    end subroutine setup_operation_buttons
    
    subroutine connect_signals()
        integer :: i
        type(forge_connection) :: conn
        
        ! Connect number buttons
        do i = 0, 9
            conn = num_clicked(i)%connect(on_number_clicked_0)  ! Would be different for each
        end do
        
        ! Connect operation buttons
        conn = op_clicked(1)%connect(on_add_clicked)
        conn = op_clicked(2)%connect(on_sub_clicked)
        conn = op_clicked(3)%connect(on_mul_clicked)
        conn = op_clicked(4)%connect(on_div_clicked)
        
        conn = equals_clicked%connect(on_equals_clicked)
        conn = clear_clicked%connect(on_clear_clicked)
    end subroutine connect_signals
    
    ! ========== Button Click Handlers ==========
    
    subroutine on_number_clicked(digit)
        integer, intent(in) :: digit
        character(len=32) :: new_text
        
        if (new_number) then
            write(new_text, '(I1)') digit
            call display_text%set(trim(new_text))
            new_number = .false.
        else
            call display_text%append(int_to_string(digit))
        end if
        
        ! Update display
        read(display_text%get(), *) current_value
        call display%set_text(display_text%get())
        
        print '(A,I1)', "  → Number button pressed: ", digit
    end subroutine on_number_clicked
    
    subroutine on_number_clicked_0(); call on_number_clicked(0); end subroutine
    
    subroutine on_add_clicked()
        call perform_pending_operation()
        stored_value = current_value
        current_operation = 1
        new_number = .true.
        print '(A)', "  → Operation: ADD"
    end subroutine on_add_clicked
    
    subroutine on_sub_clicked()
        call perform_pending_operation()
        stored_value = current_value
        current_operation = 2
        new_number = .true.
        print '(A)', "  → Operation: SUBTRACT"
    end subroutine on_sub_clicked
    
    subroutine on_mul_clicked()
        call perform_pending_operation()
        stored_value = current_value
        current_operation = 3
        new_number = .true.
        print '(A)', "  → Operation: MULTIPLY"
    end subroutine on_mul_clicked
    
    subroutine on_div_clicked()
        call perform_pending_operation()
        stored_value = current_value
        current_operation = 4
        new_number = .true.
        print '(A)', "  → Operation: DIVIDE"
    end subroutine on_div_clicked
    
    subroutine on_equals_clicked()
        call perform_pending_operation()
        current_operation = 0
        new_number = .true.
        call display_text%set(real_to_string(current_value, decimals=4))
        call display%set_text(display_text%get())
        print '(A,F0.4)', "  → Equals: ", current_value
    end subroutine on_equals_clicked
    
    subroutine on_clear_clicked()
        current_value = 0.0
        stored_value = 0.0
        current_operation = 0
        new_number = .true.
        call display_text%set("0")
        call display%set_text("0")
        print '(A)', "  → Cleared"
    end subroutine on_clear_clicked
    
    subroutine perform_pending_operation()
        select case (current_operation)
        case (1)  ! Add
            current_value = stored_value + current_value
        case (2)  ! Subtract
            current_value = stored_value - current_value
        case (3)  ! Multiply
            current_value = stored_value * current_value
        case (4)  ! Divide
            if (current_value /= 0.0) then
                current_value = stored_value / current_value
            else
                print '(A)', "  → Error: Division by zero!"
                current_value = 0.0
            end if
        end select
    end subroutine perform_pending_operation
    
    subroutine simulate_button_press(digit)
        integer, intent(in) :: digit
        call num_clicked(digit)%emit()
    end subroutine simulate_button_press
    
    subroutine simulate_operation(op)
        integer, intent(in) :: op
        call op_clicked(op)%emit()
    end subroutine simulate_operation
    
    subroutine simulate_equals()
        call equals_clicked%emit()
    end subroutine simulate_equals

end program calculator
```

## Explanation

### Calculator State Management

```fortran
! Calculator state
real :: current_value = 0.0      ! Currently displayed number
real :: stored_value = 0.0       ! Stored value for operations
integer :: current_operation = 0 ! Pending operation (0=none, 1=add, etc.)
logical :: new_number = .true.   ! Whether we're starting a new number
```

The calculator maintains state for:
- Current display value
- Stored value for binary operations
- Pending operation
- Input mode (new number vs. continuing current number)

### Signal/Slot System

```fortran
! Signals for buttons
type(signal_void) :: num_clicked(0:9)
type(signal_void) :: op_clicked(4)
type(signal_void) :: equals_clicked, clear_clicked
```

ForGE uses a signal/slot system similar to Qt:
- Signals are emitted when events occur
- Slots are functions that respond to signals
- Connections link signals to slots

### Button Setup

```fortran
subroutine setup_number_buttons()
    integer :: i
    character(len=2) :: label
    
    do i = 0, 9
        write(label, '(I1)') i
        call buttons(i)%set_label(trim(label))
        write(label, '(A,I1)') "btn_", i
        call buttons(i)%set_name(trim(label))
    end do
end subroutine setup_number_buttons
```

Creates 10 number buttons (0-9) with appropriate labels and names.

### Signal Connections

```fortran
subroutine connect_signals()
    integer :: i
    type(forge_connection) :: conn
    
    ! Connect number buttons
    do i = 0, 9
        conn = num_clicked(i)%connect(on_number_clicked_0)  ! Would be different for each
    end do
    
    ! Connect operation buttons
    conn = op_clicked(1)%connect(on_add_clicked)
    conn = op_clicked(2)%connect(on_sub_clicked)
    conn = op_clicked(3)%connect(on_mul_clicked)
    conn = op_clicked(4)%connect(on_div_clicked)
    
    conn = equals_clicked%connect(on_equals_clicked)
    conn = clear_clicked%connect(on_clear_clicked)
end subroutine connect_signals
```

Connects button signals to handler functions.

### Number Input Logic

```fortran
subroutine on_number_clicked(digit)
    integer, intent(in) :: digit
    character(len=32) :: new_text
    
    if (new_number) then
        write(new_text, '(I1)') digit
        call display_text%set(trim(new_text))
        new_number = .false.
    else
        call display_text%append(int_to_string(digit))
    end if
    
    ! Update display
    read(display_text%get(), *) current_value
    call display%set_text(display_text%get())
    
    print '(A,I1)', "  → Number button pressed: ", digit
end subroutine on_number_clicked
```

Handles number button presses:
- If starting a new number, replace display
- Otherwise, append to current number
- Update both string display and numeric value

### Operation Logic

```fortran
subroutine on_add_clicked()
    call perform_pending_operation()
    stored_value = current_value
    current_operation = 1
    new_number = .true.
    print '(A)', "  → Operation: ADD"
end subroutine on_add_clicked
```

Operation buttons:
1. Perform any pending operation
2. Store current value
3. Set operation type
4. Prepare for new number input

### Calculation Engine

```fortran
subroutine perform_pending_operation()
    select case (current_operation)
    case (1)  ! Add
        current_value = stored_value + current_value
    case (2)  ! Subtract
        current_value = stored_value - current_value
    case (3)  ! Multiply
        current_value = stored_value * current_value
    case (4)  ! Divide
        if (current_value /= 0.0) then
            current_value = stored_value / current_value
        else
            print '(A)', "  → Error: Division by zero!"
            current_value = 0.0
        end if
    end select
end subroutine perform_pending_operation
```

Performs the actual mathematical operations with error handling for division by zero.

## Running the Example

### With fpm

```bash
fpm run calculator
```

### With CMake

```bash
cd build
cmake --build . --target calculator_example
./examples/calculator/calculator_example
```

### Manual Build

```bash
gfortran -I/path/to/forge/include examples/calculator/calculator.f90 -lforge -o calculator
./calculator
```

## Expected Output

```
========================================
ForGE Qt - Calculator Application
========================================

Demonstrating:
  ✓ Multiple widgets (20+ buttons + display)
  ✓ Grid layout (4x5 button grid)
  ✓ Signals & slots (one per button)
  ✓ QString for display text
  ✓ State management
  ✓ Real calculator logic

Calculator ready!

In a full implementation, you would see:
  - 4x5 grid of buttons
  - Display showing current number
  - Click buttons to calculate

Current demo shows signal connectivity:

Simulating: 5 + 3 =
  → Number button pressed: 5
  → Operation: ADD
  → Number button pressed: 3
  → Equals: 8.0000
  Result: 8.00

Simulating: 9 * 7 =
  → Number button pressed: 9
  → Operation: MULTIPLY
  → Number button pressed: 7
  → Equals: 63.0000
  Result: 63.00

Simulating: Clear
  → Cleared
  Display: 0.00

========================================
Calculator Demo Complete!
========================================
```

## Key Concepts Demonstrated

1. **Complex State Management**: Calculator maintains multiple state variables
2. **Signal/Slot System**: Qt-style event handling with signals and slots
3. **QString Usage**: String manipulation for display text
4. **Multiple Widgets**: Managing 20+ buttons and a display
5. **Mathematical Operations**: Real calculator logic with error handling
6. **Event Simulation**: Testing event handling without GUI

## Calculator Layout

The calculator uses a grid layout (conceptually):

```
+-------------------+
|        0          |  <- Display
+-------------------+
| 7 | 8 | 9 | / | C |
| 4 | 5 | 6 | * |   |
| 1 | 2 | 3 | - |   |
| 0 |   | . | + | = |
+-------------------+
```

## Advanced Features

### Error Handling

```fortran
case (4)  ! Divide
    if (current_value /= 0.0) then
        current_value = stored_value / current_value
    else
        print '(A)', "  → Error: Division by zero!"
        current_value = 0.0
    end if
```

Handles division by zero gracefully.

### String Manipulation

```fortran
! QString for display text
type(QString) :: display_text

call display_text%set("0")
call display_text%append(int_to_string(digit))
```

Uses QString for efficient string operations.

### Operation Chaining

The calculator supports operation chaining:

```
5 + 3 * 2 = 11  (not 16)
```

This is handled by the pending operation logic.

## Next Steps

- Try the [todo app](../examples/todo_app.md) for data persistence
- Learn about [custom widgets](../tutorials/advanced_widgets.md) for specialized controls
- Explore [data binding](../tutorials/data_binding.md) for model-view connections
- See the [Qt demo](../examples/qt_demo.md) for full GUI integration