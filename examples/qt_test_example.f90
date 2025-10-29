!> @brief Example demonstrating Qt Test framework usage in Fortran
!> @details Shows how to use the Qt Test integration for comprehensive testing
!> @author ForGE Contributors
!> @date 2025
!> @license GPL-3.0-or-later

program qt_test_example
    use iso_c_binding
    use forge_qt_test
    implicit none

    ! Test case for basic assertions
    type :: basic_test_case
        private
        type(qtest_case) :: test_case
    contains
        procedure :: init => basic_test_case_init
        procedure :: test_basic_assertions => basic_test_case_test_basic_assertions
        procedure :: test_string_operations => basic_test_case_test_string_operations
        procedure :: test_math_operations => basic_test_case_test_math_operations
        procedure :: cleanup => basic_test_case_cleanup
    end type basic_test_case

    ! Test case for data-driven testing
    type :: data_driven_test_case
        private
        type(qtest_case) :: test_case
        type(qtest_data_table) :: data_table
    contains
        procedure :: init => data_driven_test_case_init
        procedure :: test_data_driven => data_driven_test_case_test_data_driven
        procedure :: cleanup => data_driven_test_case_cleanup
    end type data_driven_test_case

    ! Test case for benchmarking
    type :: benchmark_test_case
        private
        type(qtest_case) :: test_case
        type(qbenchmark_measurement) :: measurement
    contains
        procedure :: init => benchmark_test_case_init
        procedure :: test_benchmark => benchmark_test_case_test_benchmark
        procedure :: cleanup => benchmark_test_case_cleanup
    end type benchmark_test_case

    ! Test case for signal spying
    type :: signal_spy_test_case
        private
        type(qtest_case) :: test_case
        type(qsignal_spy) :: spy
    contains
        procedure :: init => signal_spy_test_case_init
        procedure :: test_signal_spying => signal_spy_test_case_test_signal_spying
        procedure :: cleanup => signal_spy_test_case_cleanup
    end type signal_spy_test_case

    ! Test suite
    type(qtest_suite) :: suite
    type(basic_test_case) :: basic_tests
    type(data_driven_test_case) :: data_tests
    type(benchmark_test_case) :: bench_tests
    type(signal_spy_test_case) :: spy_tests

    logical :: success

    ! Initialize Qt Test framework
    call qtest_init()

    ! Initialize test suite
    call suite%init("Qt Test Framework Examples")

    ! Initialize and add test cases
    call basic_tests%init()
    call suite%add_test(basic_tests%test_case)

    call data_tests%init()
    call suite%add_test(data_tests%test_case)

    call bench_tests%init()
    call suite%add_test(bench_tests%test_case)

    call spy_tests%init()
    call suite%add_test(spy_tests%test_case)

    ! Run all tests
    success = suite%run_all()

    ! Cleanup
    call spy_tests%cleanup()
    call bench_tests%cleanup()
    call data_tests%cleanup()
    call basic_tests%cleanup()
    call suite%cleanup()

    call qtest_cleanup()

    if (success) then
        write(*,*) "All Qt Test examples passed!"
    else
        write(*,*) "Some Qt Test examples failed!"
        stop 1
    end if

contains

    !> Initialize basic test case
    subroutine basic_test_case_init(this)
        class(basic_test_case), intent(out) :: this

        call this%test_case%init("Basic Assertion Tests")
    end subroutine basic_test_case_init

    !> Test basic assertions
    subroutine basic_test_case_test_basic_assertions(this)
        class(basic_test_case), intent(inout) :: this

        integer :: a, b
        real :: x, y
        logical :: flag
        character(len=20) :: str1, str2

        ! Integer comparisons
        a = 42
        b = 42
        call qtest_assert_equal(a, b, "Integer equality")

        a = 10
        b = 20
        call qtest_assert_not_equal(a, b, "Integer inequality")

        ! Real comparisons
        x = 3.14159
        y = 3.14159
        call qtest_assert_equal(x, y, "Real equality")

        x = 1.0
        y = 2.0
        call qtest_assert_not_equal(x, y, "Real inequality")

        ! Boolean assertions
        flag = .true.
        call qtest_assert_true(flag, "Boolean true assertion")

        flag = .false.
        call qtest_assert_false(flag, "Boolean false assertion")

        ! String comparisons
        str1 = "hello"
        str2 = "hello"
        call qtest_assert_equal(str1, str2, "String equality")

        str1 = "hello"
        str2 = "world"
        call qtest_assert_not_equal(str1, str2, "String inequality")

        ! Null pointer checks
        call qtest_assert_null(c_null_ptr, "Null pointer assertion")
        call qtest_assert_not_null(c_loc(a), "Non-null pointer assertion")

        ! Skip and fail tests
        call qtest_skip("This test is intentionally skipped")
        ! call qtest_fail("This test intentionally fails")  ! Uncomment to see failure

        ! Warning
        call qtest_warn("This is a test warning message")

    end subroutine basic_test_case_test_basic_assertions

    !> Test string operations
    subroutine basic_test_case_test_string_operations(this)
        class(basic_test_case), intent(inout) :: this

        character(len=50) :: result, expected

        ! Test string concatenation
        result = "Hello" // " " // "World"
        expected = "Hello World"
        call qtest_assert_equal(result, expected, "String concatenation")

        ! Test string length
        call qtest_assert_equal(len_trim("Fortran"), 7, "String length")

        ! Test substring operations
        result = "Fortran"(1:4)
        expected = "Fort"
        call qtest_assert_equal(result, expected, "Substring operation")

    end subroutine basic_test_case_test_string_operations

    !> Test math operations
    subroutine basic_test_case_test_math_operations(this)
        class(basic_test_case), intent(inout) :: this

        real :: result, expected
        integer :: iresult, iexpected

        ! Basic arithmetic
        result = 2.0 + 3.0
        expected = 5.0
        call qtest_assert_equal(result, expected, "Addition")

        result = 10.0 - 4.0
        expected = 6.0
        call qtest_assert_equal(result, expected, "Subtraction")

        result = 3.0 * 4.0
        expected = 12.0
        call qtest_assert_equal(result, expected, "Multiplication")

        result = 15.0 / 3.0
        expected = 5.0
        call qtest_assert_equal(result, expected, "Division")

        ! Integer operations
        iresult = 2**3
        iexpected = 8
        call qtest_assert_equal(iresult, iexpected, "Exponentiation")

        iresult = 17 / 5
        iexpected = 3
        call qtest_assert_equal(iresult, iexpected, "Integer division")

        iresult = mod(17, 5)
        iexpected = 2
        call qtest_assert_equal(iresult, iexpected, "Modulo operation")

    end subroutine basic_test_case_test_math_operations

    !> Cleanup basic test case
    subroutine basic_test_case_cleanup(this)
        class(basic_test_case), intent(inout) :: this

        call this%test_case%cleanup()
    end subroutine basic_test_case_cleanup

    !> Initialize data-driven test case
    subroutine data_driven_test_case_init(this)
        class(data_driven_test_case), intent(out) :: this

        call this%test_case%init("Data-Driven Tests")
        call this%data_table%init("Math Operations Data")

        ! Add columns to data table
        call this%data_table%add_column("operation", QTEST_TYPE_STRING)
        call this%data_table%add_column("input1", QTEST_TYPE_DOUBLE)
        call this%data_table%add_column("input2", QTEST_TYPE_DOUBLE)
        call this%data_table%add_column("expected", QTEST_TYPE_DOUBLE)

        ! Add test data rows
        call this%data_table%new_row("Addition: 2 + 3 = 5")
        call this%data_table%new_row("Subtraction: 10 - 4 = 6")
        call this%data_table%new_row("Multiplication: 3 * 4 = 12")
        call this%data_table%new_row("Division: 15 / 3 = 5")

    end subroutine data_driven_test_case_init

    !> Test data-driven operations
    subroutine data_driven_test_case_test_data_driven(this)
        class(data_driven_test_case), intent(inout) :: this

        character(len=:), allocatable :: operation
        real :: input1, input2, expected, result

        ! In a real implementation, this would be called for each data row
        ! For this example, we'll simulate with hardcoded values

        ! Test addition
        input1 = 2.0
        input2 = 3.0
        expected = 5.0
        result = input1 + input2
        call qtest_assert_equal(result, expected, "Data-driven addition")

        ! Test subtraction
        input1 = 10.0
        input2 = 4.0
        expected = 6.0
        result = input1 - input2
        call qtest_assert_equal(result, expected, "Data-driven subtraction")

        ! Test multiplication
        input1 = 3.0
        input2 = 4.0
        expected = 12.0
        result = input1 * input2
        call qtest_assert_equal(result, expected, "Data-driven multiplication")

        ! Test division
        input1 = 15.0
        input2 = 3.0
        expected = 5.0
        result = input1 / input2
        call qtest_assert_equal(result, expected, "Data-driven division")

    end subroutine data_driven_test_case_test_data_driven

    !> Cleanup data-driven test case
    subroutine data_driven_test_case_cleanup(this)
        class(data_driven_test_case), intent(inout) :: this

        call this%data_table%cleanup()
        call this%test_case%cleanup()
    end subroutine data_driven_test_case_cleanup

    !> Initialize benchmark test case
    subroutine benchmark_test_case_init(this)
        class(benchmark_test_case), intent(out) :: this

        call this%test_case%init("Benchmark Tests")
        call this%measurement%init()
    end subroutine benchmark_test_case_init

    !> Test benchmarking functionality
    subroutine benchmark_test_case_test_benchmark(this)
        class(benchmark_test_case), intent(inout) :: this

        integer :: i
        integer(c_long_long) :: elapsed_time
        integer :: iterations

        ! Start benchmark measurement
        call this%measurement%start()

        ! Perform some work to benchmark
        do i = 1, 100000
            ! Simulate some computation
            call random_number()
        end do

        ! Stop benchmark measurement
        call this%measurement%stop()

        ! Get benchmark results
        elapsed_time = this%measurement%elapsed()
        iterations = this%measurement%iterations()

        ! Verify benchmark results are reasonable
        call qtest_assert_true(elapsed_time >= 0, "Benchmark elapsed time should be non-negative")
        call qtest_assert_true(iterations >= 0, "Benchmark iterations should be non-negative")

        ! Test QBENCHMARK macro (simplified)
        call qtest_benchmark(benchmark_work)

        ! Test QBENCHMARK_ONCE macro (simplified)
        call qtest_benchmark_once(benchmark_work_once)

    end subroutine benchmark_test_case_test_benchmark

    !> Benchmark work function
    subroutine benchmark_work()
        integer :: i
        real :: x
        do i = 1, 1000
            x = sin(real(i)) * cos(real(i))
        end do
    end subroutine benchmark_work

    !> Benchmark work once function
    subroutine benchmark_work_once()
        integer :: i
        real :: x
        do i = 1, 1000
            x = sqrt(real(i)) + log(real(i + 1))
        end do
    end subroutine benchmark_work_once

    !> Cleanup benchmark test case
    subroutine benchmark_test_case_cleanup(this)
        class(benchmark_test_case), intent(inout) :: this

        call this%measurement%cleanup()
        call this%test_case%cleanup()
    end subroutine benchmark_test_case_cleanup

    !> Initialize signal spy test case
    subroutine signal_spy_test_case_init(this)
        class(signal_spy_test_case), intent(out) :: this

        call this%test_case%init("Signal Spy Tests")
        ! Note: Signal spying requires actual Qt objects with signals
        ! This is a simplified demonstration
    end subroutine signal_spy_test_case_init

    !> Test signal spying functionality
    subroutine signal_spy_test_case_test_signal_spying(this)
        class(signal_spy_test_case), intent(inout) :: this

        type(c_ptr) :: dummy_object
        integer :: count

        ! Create signal spy (would need real Qt object with signals)
        call qtest_spy_create(this%spy, dummy_object, "testSignal")

        ! Test spy validity
        call qtest_assert_true(this%spy%is_valid(), "Signal spy should be valid")

        ! Test signal count (initially 0)
        count = this%spy%count()
        call qtest_assert_equal(count, 0, "Initial signal count should be 0")

        ! Test wait for signal (simplified - would timeout in real scenario)
        call qtest_assert_false(this%spy%wait(100), "Should not receive signal within timeout")

        ! Test signal access functions
        call qtest_assert_not_null(this%spy%take_first(), "Take first should return pointer")
        call qtest_assert_not_null(this%spy%take_last(), "Take last should return pointer")
        call qtest_assert_not_null(this%spy%at(0), "At should return pointer")

    end subroutine signal_spy_test_case_test_signal_spying

    !> Cleanup signal spy test case
    subroutine signal_spy_test_case_cleanup(this)
        class(signal_spy_test_case), intent(inout) :: this

        call this%spy%cleanup()
        call this%test_case%cleanup()
    end subroutine signal_spy_test_case_cleanup

end program qt_test_example