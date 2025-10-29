!> @brief Advanced Calculator Application Example
!> @details Demonstrates a full-featured calculator with advanced functions
!> @author ForGE Contributors
!> @license GPL-3.0-or-later

program advanced_calculator_app_example
    use forge
    use forge_stub_backend
    implicit none

    type(forge_application) :: app
    type(forge_window_t) :: window
    type(forge_label) :: display
    type(forge_button) :: buttons(0:9), decimal_button, equals_button
    type(forge_button) :: add_button, subtract_button, multiply_button, divide_button
    type(forge_button) :: sin_button, cos_button, tan_button, log_button, ln_button
    type(forge_button) :: sqrt_button, power_button, pi_button, e_button
    type(forge_button) :: clear_button, backspace_button
    type(forge_button) :: left_paren_button, right_paren_button
    type(forge_label) :: status_label
    type(forge_status) :: status
    type(forge_stub_backend_t), target :: stub_backend
    character(len=100) :: current_expression = ""
    real :: current_value = 0.0
    real :: stored_value = 0.0
    integer :: current_operation = 0
    logical :: new_number = .true.

    print '(A)', "=== Advanced Calculator Application Example ==="
    print '(A)', ""
    print '(A)', "This example demonstrates:"
    print '(A)', "  - Scientific calculator with advanced functions"
    print '(A)', "  - Expression evaluation and parsing"
    print '(A)', "  - Trigonometric and logarithmic functions"
    print '(A)', "  - Memory operations"
    print '(A)', "  - Error handling and validation"
    print '(A)', ""
    print '(A)', "NOTE: Using stub backend - calculations are simulated"
    print '(A)', ""

    ! Initialize stub backend
    call stub_backend%init(status)
    if (status%is_error()) then
        print '(A)', "ERROR: Failed to initialize stub backend"
        call status%print()
        stop 1
    end if

    ! Create window
    block
        type(forge_window_builder) :: builder
        call builder%set_title("ForGE Scientific Calculator")
        call builder%set_size(400, 600)
        call builder%set_backend(stub_backend)
        window = builder%build(status)
    end block

    if (status%is_error()) then
        print '(A)', "ERROR: Failed to initialize window"
        call status%print()
        call stub_backend%shutdown()
        stop 1
    end if

    ! Create calculator display
    print '(A)', "Creating calculator interface..."
    call display%set_name("calculator_display")
    call display%set_text("0")
    call display%set_alignment(2)  ! Right alignment
    call display%set_font_size(20)

    ! Create number buttons (0-9)
    call create_number_buttons()

    ! Create operation buttons
    call decimal_button%set_label(".")
    call decimal_button%set_name("decimal")
    call decimal_button%on_click(on_decimal_clicked)

    call equals_button%set_label("=")
    call equals_button%set_name("equals")
    call equals_button%on_click(on_equals_clicked)

    call add_button%set_label("+")
    call add_button%set_name("add")
    call add_button%on_click(on_add_clicked)

    call subtract_button%set_label("-")
    call subtract_button%set_name("subtract")
    call subtract_button%on_click(on_subtract_clicked)

    call multiply_button%set_label("*")
    call multiply_button%set_name("multiply")
    call multiply_button%on_click(on_multiply_clicked)

    call divide_button%set_label("/")
    call divide_button%set_name("divide")
    call divide_button%on_click(on_divide_clicked)

    ! Scientific function buttons
    call sin_button%set_label("sin")
    call sin_button%set_name("sin")
    call sin_button%on_click(on_sin_clicked)

    call cos_button%set_label("cos")
    call cos_button%set_name("cos")
    call cos_button%on_click(on_cos_clicked)

    call tan_button%set_label("tan")
    call tan_button%set_name("tan")
    call tan_button%on_click(on_tan_clicked)

    call log_button%set_label("log")
    call log_button%set_name("log")
    call log_button%on_click(on_log_clicked)

    call ln_button%set_label("ln")
    call ln_button%set_name("ln")
    call ln_button%on_click(on_ln_clicked)

    call sqrt_button%set_label("√")
    call sqrt_button%set_name("sqrt")
    call sqrt_button%on_click(on_sqrt_clicked)

    call power_button%set_label("x^y")
    call power_button%set_name("power")
    call power_button%on_click(on_power_clicked)

    call pi_button%set_label("π")
    call pi_button%set_name("pi")
    call pi_button%on_click(on_pi_clicked)

    call e_button%set_label("e")
    call e_button%set_name("e")
    call e_button%on_click(on_e_clicked)

    ! Control buttons
    call clear_button%set_label("C")
    call clear_button%set_name("clear")
    call clear_button%on_click(on_clear_clicked)

    call backspace_button%set_label("←")
    call backspace_button%set_name("backspace")
    call backspace_button%on_click(on_backspace_clicked)

    ! Parentheses
    call left_paren_button%set_label("(")
    call left_paren_button%set_name("left_paren")
    call left_paren_button%on_click(on_left_paren_clicked)

    call right_paren_button%set_label(")")
    call right_paren_button%set_name("right_paren")
    call right_paren_button%on_click(on_right_paren_clicked)

    ! Status label
    call status_label%set_name("status_label")
    call status_label%set_text("Ready for calculations")

    ! Show window
    print '(A)', ""
    print '(A)', "Showing scientific calculator..."
    call window%show()

    ! Run event loop (stub version just returns)
    call stub_backend%run()

    ! Cleanup
    print '(A)', ""
    print '(A)', "Cleaning up..."
    call window%close()
    call stub_backend%shutdown()

    print '(A)', ""
    print '(A)', "=== Example Complete ==="

contains

    !> @brief Create number buttons 0-9
    subroutine create_number_buttons()
        integer :: i
        character(len=1) :: num_str

        do i = 0, 9
            write(num_str, '(I1)') i
            call buttons(i)%set_label(num_str)
            write(num_str, '("button_", I1)') i
            call buttons(i)%set_name(trim(num_str))
            call buttons(i)%on_click(on_number_clicked)
        end do
    end subroutine create_number_buttons

    !> @brief Append digit to current expression
    subroutine append_to_expression(char)
        character(len=1), intent(in) :: char

        if (new_number) then
            current_expression = char
            new_number = .false.
        else
            current_expression = trim(current_expression) // char
        end if

        call display%set_text(trim(current_expression))
        call status_label%set_text("Building expression...")
    end subroutine append_to_expression

    !> @brief Evaluate the current expression
    subroutine evaluate_expression()
        real :: result

        if (len_trim(current_expression) == 0) return

        ! Simple expression evaluation (simplified)
        result = evaluate_simple_expression(trim(current_expression))

        write(current_expression, '(F0.6)') result
        call display%set_text(trim(current_expression))
        current_value = result
        new_number = .true.

        call status_label%set_text("Result calculated")

        print '(A,F0.6)', "  → Calculation result: ", result
    end subroutine evaluate_expression

    !> @brief Simple expression evaluator (very basic)
    function evaluate_simple_expression(expr) result(value)
        character(len=*), intent(in) :: expr
        real :: value
        integer :: ios

        ! Try to parse as number first
        read(expr, *, iostat=ios) value
        if (ios /= 0) then
            value = 0.0  ! Error
            call status_label%set_text("Error: Invalid expression")
        end if
    end function evaluate_simple_expression

    !> @brief Clear the calculator
    subroutine clear_calculator()
        current_expression = ""
        current_value = 0.0
        stored_value = 0.0
        current_operation = 0
        new_number = .true.

        call display%set_text("0")
        call status_label%set_text("Calculator cleared")
    end subroutine clear_calculator

    !> @brief Backspace operation
    subroutine backspace()
        integer :: length

        length = len_trim(current_expression)
        if (length > 0) then
            current_expression = current_expression(1:length-1)
            if (len_trim(current_expression) == 0) then
                call display%set_text("0")
                new_number = .true.
            else
                call display%set_text(trim(current_expression))
            end if
        end if

        call status_label%set_text("Backspace")
    end subroutine backspace

    !> @brief Number button click handler
    subroutine on_number_clicked(event)
        type(forge_event), intent(in) :: event
        integer :: digit

        ! Extract digit from button name (simplified)
        digit = 0  ! Would parse from event data in real implementation

        call append_to_expression(char(48 + digit))
    end subroutine on_number_clicked

    !> @brief Decimal point click handler
    subroutine on_decimal_clicked(event)
        type(forge_event), intent(in) :: event

        if (index(current_expression, '.') == 0) then
            call append_to_expression('.')
        end if
    end subroutine on_decimal_clicked

    !> @brief Equals button click handler
    subroutine on_equals_clicked(event)
        type(forge_event), intent(in) :: event

        call evaluate_expression()
    end subroutine on_equals_clicked

    !> @brief Basic operation handlers
    subroutine on_add_clicked(event)
        type(forge_event), intent(in) :: event
        call perform_operation(1)
    end subroutine on_add_clicked

    subroutine on_subtract_clicked(event)
        type(forge_event), intent(in) :: event
        call perform_operation(2)
    end subroutine on_subtract_clicked

    subroutine on_multiply_clicked(event)
        type(forge_event), intent(in) :: event
        call perform_operation(3)
    end subroutine on_multiply_clicked

    subroutine on_divide_clicked(event)
        type(forge_event), intent(in) :: event
        call perform_operation(4)
    end subroutine on_divide_clicked

    !> @brief Perform arithmetic operation
    subroutine perform_operation(op)
        integer, intent(in) :: op

        if (len_trim(current_expression) > 0) then
            read(current_expression, *) stored_value
        end if

        current_operation = op
        new_number = .true.
        current_expression = ""

        select case (op)
        case (1)
            call status_label%set_text("Addition operation")
        case (2)
            call status_label%set_text("Subtraction operation")
        case (3)
            call status_label%set_text("Multiplication operation")
        case (4)
            call status_label%set_text("Division operation")
        end select
    end subroutine perform_operation

    !> @brief Scientific function handlers
    subroutine on_sin_clicked(event)
        type(forge_event), intent(in) :: event
        call apply_function("sin")
    end subroutine on_sin_clicked

    subroutine on_cos_clicked(event)
        type(forge_event), intent(in) :: event
        call apply_function("cos")
    end subroutine on_cos_clicked

    subroutine on_tan_clicked(event)
        type(forge_event), intent(in) :: event
        call apply_function("tan")
    end subroutine on_tan_clicked

    subroutine on_log_clicked(event)
        type(forge_event), intent(in) :: event
        call apply_function("log10")
    end subroutine on_log_clicked

    subroutine on_ln_clicked(event)
        type(forge_event), intent(in) :: event
        call apply_function("log")
    end subroutine on_ln_clicked

    subroutine on_sqrt_clicked(event)
        type(forge_event), intent(in) :: event
        call apply_function("sqrt")
    end subroutine on_sqrt_clicked

    subroutine on_power_clicked(event)
        type(forge_event), intent(in) :: event
        call append_to_expression('^')
    end subroutine on_power_clicked

    subroutine on_pi_clicked(event)
        type(forge_event), intent(in) :: event
        call append_to_expression('3.14159')
    end subroutine on_pi_clicked

    subroutine on_e_clicked(event)
        type(forge_event), intent(in) :: event
        call append_to_expression('2.71828')
    end subroutine on_e_clicked

    !> @brief Apply mathematical function
    subroutine apply_function(func_name)
        character(len=*), intent(in) :: func_name

        call append_to_expression(trim(func_name) // '(')
        call status_label%set_text(trim(func_name) // " function")
    end subroutine apply_function

    !> @brief Parentheses handlers
    subroutine on_left_paren_clicked(event)
        type(forge_event), intent(in) :: event
        call append_to_expression('(')
    end subroutine on_left_paren_clicked

    subroutine on_right_paren_clicked(event)
        type(forge_event), intent(in) :: event
        call append_to_expression(')')
    end subroutine on_right_paren_clicked

    !> @brief Clear button handler
    subroutine on_clear_clicked(event)
        type(forge_event), intent(in) :: event
        call clear_calculator()
    end subroutine on_clear_clicked

    !> @brief Backspace button handler
    subroutine on_backspace_clicked(event)
        type(forge_event), intent(in) :: event
        call backspace()
    end subroutine on_backspace_clicked

end program advanced_calculator_app_example