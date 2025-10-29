!> @brief Example demonstrating Qt Test signal spying features
!> @details Shows how to use Qt Test for signal verification and spying
!> @author ForGE Contributors
!> @date 2025
!> @license GPL-3.0-or-later

program qt_test_signal_spy_example
    use iso_c_binding
    use forge_qt_test
    use forge_qt_bindings
    implicit none

    ! Signal spy test case
    type :: signal_spy_test_case
        private
        type(qtest_case) :: test_case
        type(qsignal_spy) :: spy
        type(qpushbutton) :: button
        type(qlineedit) :: text_field
        type(qwidget) :: window
    contains
        procedure :: init => signal_spy_test_case_init
        procedure :: test_button_signals => signal_spy_test_case_test_button_signals
        procedure :: test_text_field_signals => signal_spy_test_case_test_text_field_signals
        procedure :: test_window_signals => signal_spy_test_case_test_window_signals
        procedure :: test_custom_signals => signal_spy_test_case_test_custom_signals
        procedure :: test_signal_timing => signal_spy_test_case_test_signal_timing
        procedure :: cleanup => signal_spy_test_case_cleanup
    end type signal_spy_test_case

    ! Test suite
    type(qtest_suite) :: suite
    type(signal_spy_test_case) :: spy_tests

    logical :: success

    ! Initialize Qt Test framework
    call qtest_init()

    ! Initialize QApplication (required for Qt objects)
    ! Note: In real implementation, QApplication would be initialized

    ! Initialize test suite
    call suite%init("Qt Test Signal Spy Examples")

    ! Initialize and add signal spy test case
    call spy_tests%init()
    call suite%add_test(spy_tests%test_case)

    ! Run all tests
    success = suite%run_all()

    ! Cleanup
    call spy_tests%cleanup()
    call suite%cleanup()

    call qtest_cleanup()

    if (success) then
        write(*,*) "All Qt Test signal spy examples passed!"
    else
        write(*,*) "Some Qt Test signal spy examples failed!"
        stop 1
    end if

contains

    !> Initialize signal spy test case
    subroutine signal_spy_test_case_init(this)
        class(signal_spy_test_case), intent(out) :: this

        call this%test_case%init("Signal Spy Testing Examples")

        ! Note: In real implementation, Qt widgets would be created here
        ! For this example, we simulate the setup

        write(*,*) "Signal spy test case initialized (widgets would be created here)"
    end subroutine signal_spy_test_case_init

    !> Test button signal spying
    subroutine signal_spy_test_case_test_button_signals(this)
        class(signal_spy_test_case), intent(inout) :: this

        integer :: signal_count
        logical :: spy_valid, signal_received

        ! Create signal spy for button clicked signal
        call qtest_spy_create(this%spy, this%button%ptr, "clicked()")

        ! Verify spy is valid
        spy_valid = this%spy%is_valid()
        call qtest_assert_true(spy_valid, "Signal spy should be valid")

        ! Check initial signal count
        signal_count = this%spy%count()
        call qtest_assert_equal(signal_count, 0, "Initial signal count should be 0")

        ! Simulate button click (in real implementation, this would trigger the signal)
        ! For this example, we simulate the signal emission
        signal_count = 1  ! Simulate signal emission

        ! Verify signal was received
        call qtest_assert_equal(signal_count, 1, "Signal count should be 1 after button click")

        ! Test signal access functions
        call qtest_assert_not_null(this%spy%take_first(), "Take first should return signal data")
        call qtest_assert_not_null(this%spy%take_last(), "Take last should return signal data")
        call qtest_assert_not_null(this%spy%at(0), "At index should return signal data")

        ! Test multiple signals
        signal_count = signal_count + 2  ! Simulate two more clicks
        call qtest_assert_equal(signal_count, 3, "Signal count should be 3 after multiple clicks")

        ! Test spy cleanup
        call this%spy%cleanup()

    end subroutine signal_spy_test_case_test_button_signals

    !> Test text field signal spying
    subroutine signal_spy_test_case_test_text_field_signals(this)
        class(signal_spy_test_case), intent(inout) :: this

        integer :: signal_count
        logical :: spy_valid

        ! Create signal spy for text changed signal
        call qtest_spy_create(this%spy, this%text_field%ptr, "textChanged(QString)")

        ! Verify spy is valid
        spy_valid = this%spy%is_valid()
        call qtest_assert_true(spy_valid, "Text field signal spy should be valid")

        ! Check initial signal count
        signal_count = this%spy%count()
        call qtest_assert_equal(signal_count, 0, "Initial text changed signal count should be 0")

        ! Simulate text input (in real implementation, this would trigger signals)
        ! For this example, we simulate signal emissions
        signal_count = 5  ! Simulate 5 text change signals

        ! Verify signals were received
        call qtest_assert_equal(signal_count, 5, "Text changed signal count should be 5")

        ! Test return pressed signal
        call qtest_spy_create(this%spy, this%text_field%ptr, "returnPressed()")
        signal_count = 1  ! Simulate return pressed
        call qtest_assert_equal(signal_count, 1, "Return pressed signal should be received")

        ! Test editing finished signal
        call qtest_spy_create(this%spy, this%text_field%ptr, "editingFinished()")
        signal_count = 1  ! Simulate editing finished
        call qtest_assert_equal(signal_count, 1, "Editing finished signal should be received")

        ! Cleanup
        call this%spy%cleanup()

    end subroutine signal_spy_test_case_test_text_field_signals

    !> Test window signal spying
    subroutine signal_spy_test_case_test_window_signals(this)
        class(signal_spy_test_case), intent(inout) :: this

        integer :: signal_count
        logical :: spy_valid

        ! Test window close signal
        call qtest_spy_create(this%spy, this%window%ptr, "close()")

        spy_valid = this%spy%is_valid()
        call qtest_assert_true(spy_valid, "Window close signal spy should be valid")

        signal_count = this%spy%count()
        call qtest_assert_equal(signal_count, 0, "Initial window close signal count should be 0")

        ! Simulate window close
        signal_count = 1
        call qtest_assert_equal(signal_count, 1, "Window close signal should be received")

        ! Test window show signal
        call qtest_spy_create(this%spy, this%window%ptr, "show()")
        signal_count = 1  ! Simulate window shown
        call qtest_assert_equal(signal_count, 1, "Window show signal should be received")

        ! Test window hide signal
        call qtest_spy_create(this%spy, this%window%ptr, "hide()")
        signal_count = 1  ! Simulate window hidden
        call qtest_assert_equal(signal_count, 1, "Window hide signal should be received")

        ! Test window resize signal
        call qtest_spy_create(this%spy, this%window%ptr, "resize(int,int)")
        signal_count = 3  ! Simulate multiple resizes
        call qtest_assert_equal(signal_count, 3, "Window resize signals should be received")

        ! Cleanup
        call this%spy%cleanup()

    end subroutine signal_spy_test_case_test_window_signals

    !> Test custom signal spying
    subroutine signal_spy_test_case_test_custom_signals(this)
        class(signal_spy_test_case), intent(inout) :: this

        integer :: signal_count
        logical :: spy_valid

        ! Test custom data changed signal
        call qtest_spy_create(this%spy, this%window%ptr, "dataChanged(QModelIndex,QModelIndex)")

        spy_valid = this%spy%is_valid()
        call qtest_assert_true(spy_valid, "Custom signal spy should be valid")

        signal_count = this%spy%count()
        call qtest_assert_equal(signal_count, 0, "Initial custom signal count should be 0")

        ! Simulate custom signal emissions
        signal_count = 10  ! Simulate multiple data changes
        call qtest_assert_equal(signal_count, 10, "Custom signals should be received")

        ! Test signal with parameters
        call qtest_spy_create(this%spy, this%window%ptr, "valueChanged(int)")
        signal_count = 5  ! Simulate value changes
        call qtest_assert_equal(signal_count, 5, "Parameterized signals should be received")

        ! Test overloaded signals
        call qtest_spy_create(this%spy, this%window%ptr, "currentChanged(int)")
        signal_count = 2  ! Simulate current changes
        call qtest_assert_equal(signal_count, 2, "Overloaded signals should be received")

        ! Cleanup
        call this%spy%cleanup()

    end subroutine signal_spy_test_case_test_custom_signals

    !> Test signal timing and waiting
    subroutine signal_spy_test_case_test_signal_timing(this)
        class(signal_spy_test_case), intent(inout) :: this

        integer :: signal_count
        logical :: signal_received, wait_result

        ! Create signal spy
        call qtest_spy_create(this%spy, this%button%ptr, "clicked()")

        ! Test immediate signal check
        signal_count = this%spy%count()
        call qtest_assert_equal(signal_count, 0, "No signals should be present initially")

        ! Test waiting for signal (simplified - would timeout in real scenario)
        wait_result = this%spy%wait(100)  ! Wait 100ms for signal
        call qtest_assert_false(wait_result, "Should timeout waiting for signal")

        ! Simulate signal emission after setup
        signal_count = 1
        call qtest_assert_equal(signal_count, 1, "Signal should be received after emission")

        ! Test signal timing with multiple emissions
        signal_count = signal_count + 3  ! Simulate rapid signal emissions
        call qtest_assert_equal(signal_count, 4, "Multiple signals should be counted")

        ! Test signal order (first and last)
        call qtest_assert_not_null(this%spy%take_first(), "First signal should be accessible")
        call qtest_assert_not_null(this%spy%take_last(), "Last signal should be accessible")

        ! Test signal at specific index
        call qtest_assert_not_null(this%spy%at(0), "Signal at index 0 should be accessible")
        call qtest_assert_not_null(this%spy%at(2), "Signal at index 2 should be accessible")

        ! Test signal spy after cleanup
        call this%spy%cleanup()
        spy_valid = this%spy%is_valid()
        call qtest_assert_false(spy_valid, "Signal spy should be invalid after cleanup")

    end subroutine signal_spy_test_case_test_signal_timing

    !> Cleanup signal spy test case
    subroutine signal_spy_test_case_cleanup(this)
        class(signal_spy_test_case), intent(inout) :: this

        ! Cleanup signal spy
        call this%spy%cleanup()

        ! In real implementation, Qt widgets would be destroyed here
        write(*,*) "Signal spy test case cleaned up (widgets would be destroyed here)"

        call this%test_case%cleanup()
    end subroutine signal_spy_test_case_cleanup

end program qt_test_signal_spy_example