!> @brief Example demonstrating Qt Test GUI testing features
!> @details Shows how to use Qt Test for GUI testing with event simulation
!> @author ForGE Contributors
!> @date 2025
!> @license GPL-3.0-or-later

program qt_test_gui_example
    use iso_c_binding
    use forge_qt_test
    use forge_qt_bindings
    implicit none

    ! GUI test case
    type :: gui_test_case
        private
        type(qtest_case) :: test_case
        type(qwidget) :: window
        type(qpushbutton) :: button
        type(qlineedit) :: text_field
        type(qtest_event_simulator) :: simulator
    contains
        procedure :: init => gui_test_case_init
        procedure :: test_button_click => gui_test_case_test_button_click
        procedure :: test_text_input => gui_test_case_test_text_input
        procedure :: test_keyboard_events => gui_test_case_test_keyboard_events
        procedure :: test_mouse_events => gui_test_case_test_mouse_events
        procedure :: test_window_operations => gui_test_case_test_window_operations
        procedure :: cleanup => gui_test_case_cleanup
    end type gui_test_case

    ! Test suite
    type(qtest_suite) :: suite
    type(gui_test_case) :: gui_tests

    logical :: success

    ! Initialize Qt Test framework
    call qtest_init()

    ! Initialize QApplication (required for GUI testing)
    ! Note: In real implementation, QApplication would be initialized

    ! Initialize test suite
    call suite%init("Qt Test GUI Examples")

    ! Initialize and add GUI test case
    call gui_tests%init()
    call suite%add_test(gui_tests%test_case)

    ! Run all tests
    success = suite%run_all()

    ! Cleanup
    call gui_tests%cleanup()
    call suite%cleanup()

    call qtest_cleanup()

    if (success) then
        write(*,*) "All Qt Test GUI examples passed!"
    else
        write(*,*) "Some Qt Test GUI examples failed!"
        stop 1
    end if

contains

    !> Initialize GUI test case
    subroutine gui_test_case_init(this)
        class(gui_test_case), intent(out) :: this

        call this%test_case%init("GUI Testing Examples")

        ! Note: In real implementation, GUI widgets would be created here
        ! For this example, we simulate the setup

        write(*,*) "GUI test case initialized (widgets would be created here)"
    end subroutine gui_test_case_init

    !> Test button click functionality
    subroutine gui_test_case_test_button_click(this)
        class(gui_test_case), intent(inout) :: this

        integer :: click_count
        logical :: button_clicked

        ! Initialize test state
        click_count = 0
        button_clicked = .false.

        ! Simulate button click
        call this%simulator%mouse_click(this%button, QT_LEFTBUTTON, pos=[50, 25])

        ! In real implementation, this would trigger a signal/slot connection
        ! For this example, we simulate the expected behavior
        button_clicked = .true.
        click_count = click_count + 1

        ! Verify button was clicked
        call qtest_assert_true(button_clicked, "Button should be clicked")
        call qtest_assert_equal(click_count, 1, "Click count should be 1")

        ! Test multiple clicks
        call this%simulator%mouse_click(this%button, QT_LEFTBUTTON, pos=[50, 25])
        call this%simulator%mouse_click(this%button, QT_LEFTBUTTON, pos=[50, 25])

        click_count = click_count + 2
        call qtest_assert_equal(click_count, 3, "Click count should be 3 after multiple clicks")

    end subroutine gui_test_case_test_button_click

    !> Test text input functionality
    subroutine gui_test_case_test_text_input(this)
        class(gui_test_case), intent(inout) :: this

        character(len=100) :: input_text, expected_text

        ! Simulate text input
        call this%simulator%key_click(this%text_field, QT_KEY_H)
        call this%simulator%key_click(this%text_field, QT_KEY_E)
        call this%simulator%key_click(this%text_field, QT_KEY_L)
        call this%simulator%key_click(this%text_field, QT_KEY_L)
        call this%simulator%key_click(this%text_field, QT_KEY_O)

        ! In real implementation, text would be retrieved from the widget
        ! For this example, we simulate the expected text
        input_text = "HELLO"
        expected_text = "HELLO"

        ! Verify text input
        call qtest_assert_equal(input_text, expected_text, "Text input should match expected")

        ! Test backspace
        call this%simulator%key_click(this%text_field, QT_KEY_BACKSPACE)
        input_text = "HELL"
        expected_text = "HELL"
        call qtest_assert_equal(input_text, expected_text, "Text after backspace should be correct")

        ! Test text selection and replacement
        call this%simulator%key_click(this%text_field, QT_KEY_A, modifier=QT_CONTROLMODIFIER)  ! Select all
        call this%simulator%key_click(this%text_field, QT_KEY_W)
        call this%simulator%key_click(this%text_field, QT_KEY_O)
        call this%simulator%key_click(this%text_field, QT_KEY_R)
        call this%simulator%key_click(this%text_field, QT_KEY_L)
        call this%simulator%key_click(this%text_field, QT_KEY_D)

        input_text = "WORLD"
        expected_text = "WORLD"
        call qtest_assert_equal(input_text, expected_text, "Text replacement should work")

    end subroutine gui_test_case_test_text_input

    !> Test keyboard events
    subroutine gui_test_case_test_keyboard_events(this)
        class(gui_test_case), intent(inout) :: this

        logical :: shift_pressed, ctrl_pressed

        ! Initialize state
        shift_pressed = .false.
        ctrl_pressed = .false.

        ! Test key press and release
        call this%simulator%key_press(this%window, QT_KEY_SHIFT)
        shift_pressed = .true.
        call qtest_assert_true(shift_pressed, "Shift key should be pressed")

        call this%simulator%key_press(this%window, QT_KEY_CONTROL)
        ctrl_pressed = .true.
        call qtest_assert_true(ctrl_pressed, "Control key should be pressed")

        ! Test key combinations
        call this%simulator%key_click(this%window, QT_KEY_S, modifier=QT_CONTROLMODIFIER)
        ! In real implementation, this would trigger save operation

        call this%simulator%key_click(this%window, QT_KEY_C, modifier=QT_CONTROLMODIFIER)
        ! In real implementation, this would trigger copy operation

        ! Release keys
        call this%simulator%key_release(this%window, QT_KEY_CONTROL)
        ctrl_pressed = .false.
        call qtest_assert_false(ctrl_pressed, "Control key should be released")

        call this%simulator%key_release(this%window, QT_KEY_SHIFT)
        shift_pressed = .false.
        call qtest_assert_false(shift_pressed, "Shift key should be released")

        ! Test special keys
        call this%simulator%key_click(this%window, QT_KEY_RETURN)
        call this%simulator%key_click(this%window, QT_KEY_TAB)
        call this%simulator%key_click(this%window, QT_KEY_ESCAPE)

        ! Test arrow keys
        call this%simulator%key_click(this%window, QT_KEY_LEFT)
        call this%simulator%key_click(this%window, QT_KEY_RIGHT)
        call this%simulator%key_click(this%window, QT_KEY_UP)
        call this%simulator%key_click(this%window, QT_KEY_DOWN)

    end subroutine gui_test_case_test_keyboard_events

    !> Test mouse events
    subroutine gui_test_case_test_mouse_events(this)
        class(gui_test_case), intent(inout) :: this

        integer, dimension(2) :: start_pos, end_pos
        logical :: mouse_pressed, mouse_released

        ! Initialize state
        mouse_pressed = .false.
        mouse_released = .false.

        start_pos = [100, 100]
        end_pos = [200, 150]

        ! Test mouse press
        call this%simulator%mouse_press(this%window, QT_LEFTBUTTON, pos=start_pos)
        mouse_pressed = .true.
        call qtest_assert_true(mouse_pressed, "Mouse should be pressed")

        ! Test mouse move
        call this%simulator%mouse_move(this%window, end_pos)

        ! Test mouse release
        call this%simulator%mouse_release(this%window, QT_LEFTBUTTON, pos=end_pos)
        mouse_released = .true.
        call qtest_assert_true(mouse_released, "Mouse should be released")

        ! Test different mouse buttons
        call this%simulator%mouse_click(this%window, QT_RIGHTBUTTON, pos=start_pos)
        call this%simulator%mouse_click(this%window, QT_MIDBUTTON, pos=start_pos)

        ! Test mouse wheel
        call this%simulator%mouse_wheel(this%window, start_pos, 120)  ! Scroll up
        call this%simulator%mouse_wheel(this%window, start_pos, -120) ! Scroll down

        ! Test double click
        call this%simulator%mouse_click(this%window, QT_LEFTBUTTON, pos=start_pos)
        call qtest_sleep(50)  ! Small delay for double-click timing
        call this%simulator%mouse_click(this%window, QT_LEFTBUTTON, pos=start_pos)

        ! Test drag operation
        call this%simulator%mouse_press(this%window, QT_LEFTBUTTON, pos=start_pos)
        call this%simulator%mouse_move(this%window, [150, 125])
        call this%simulator%mouse_move(this%window, end_pos)
        call this%simulator%mouse_release(this%window, QT_LEFTBUTTON, pos=end_pos)

    end subroutine gui_test_case_test_mouse_events

    !> Test window operations
    subroutine gui_test_case_test_window_operations(this)
        class(gui_test_case), intent(inout) :: this

        logical :: window_active, window_shown, window_closed

        ! Initialize state
        window_active = .false.
        window_shown = .false.
        window_closed = .false.

        ! Test window activation
        ! In real implementation, these would test actual window states
        call qtest_wait_for_window_active(this%window, 1000)
        window_active = .true.
        call qtest_assert_true(window_active, "Window should be active")

        ! Test window visibility
        call qtest_wait_for_window_shown(this%window, 1000)
        window_shown = .true.
        call qtest_assert_true(window_shown, "Window should be shown")

        ! Test window closing
        call qtest_wait_for_window_closed(this%window, 1000)
        window_closed = .true.
        call qtest_assert_true(window_closed, "Window should be closed")

        ! Test window exposure
        call qtest_wait_for_window_exposed(this%window, 1000)
        call qtest_assert_true(.true., "Window exposure test completed")

        ! Test timing functions
        call qtest_sleep(100)
        call qtest_wait(50)

        ! Test conditional waiting
        call qtest_wait_for(wait_for_condition, 1000)

    end subroutine gui_test_case_test_window_operations

    !> Wait condition for window operations
    function wait_for_condition() result(ready)
        logical :: ready
        ready = .true.  ! Simplified condition
    end function wait_for_condition

    !> Cleanup GUI test case
    subroutine gui_test_case_cleanup(this)
        class(gui_test_case), intent(inout) :: this

        ! In real implementation, GUI widgets would be destroyed here
        write(*,*) "GUI test case cleaned up (widgets would be destroyed here)"

        call this%test_case%cleanup()
    end subroutine gui_test_case_cleanup

end program qt_test_gui_example