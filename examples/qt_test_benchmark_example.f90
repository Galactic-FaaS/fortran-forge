!> @brief Example demonstrating Qt Test benchmarking features
!> @details Shows how to use Qt Test for performance benchmarking
!> @author ForGE Contributors
!> @date 2025
!> @license GPL-3.0-or-later

program qt_test_benchmark_example
    use iso_c_binding
    use forge_qt_test
    implicit none

    ! Benchmark test case
    type :: benchmark_test_case
        private
        type(qtest_case) :: test_case
        type(qbenchmark_measurement) :: measurement
    contains
        procedure :: init => benchmark_test_case_init
        procedure :: test_array_operations => benchmark_test_case_test_array_operations
        procedure :: test_matrix_operations => benchmark_test_case_test_matrix_operations
        procedure :: test_string_operations => benchmark_test_case_test_string_operations
        procedure :: test_file_operations => benchmark_test_case_test_file_operations
        procedure :: test_memory_operations => benchmark_test_case_test_memory_operations
        procedure :: cleanup => benchmark_test_case_cleanup
    end type benchmark_test_case

    ! Test suite
    type(qtest_suite) :: suite
    type(benchmark_test_case) :: bench_tests

    logical :: success

    ! Initialize Qt Test framework
    call qtest_init()

    ! Initialize test suite
    call suite%init("Qt Test Benchmark Examples")

    ! Initialize and add benchmark test case
    call bench_tests%init()
    call suite%add_test(bench_tests%test_case)

    ! Run all tests
    success = suite%run_all()

    ! Cleanup
    call bench_tests%cleanup()
    call suite%cleanup()

    call qtest_cleanup()

    if (success) then
        write(*,*) "All Qt Test benchmark examples passed!"
    else
        write(*,*) "Some Qt Test benchmark examples failed!"
        stop 1
    end if

contains

    !> Initialize benchmark test case
    subroutine benchmark_test_case_init(this)
        class(benchmark_test_case), intent(out) :: this

        call this%test_case%init("Benchmark Testing Examples")
        call this%measurement%init()
    end subroutine benchmark_test_case_init

    !> Test array operations performance
    subroutine benchmark_test_case_test_array_operations(this)
        class(benchmark_test_case), intent(inout) :: this

        integer, parameter :: ARRAY_SIZE = 100000
        real, dimension(:), allocatable :: array1, array2, result_array
        integer(c_long_long) :: elapsed_time
        integer :: iterations, i

        ! Allocate arrays
        allocate(array1(ARRAY_SIZE))
        allocate(array2(ARRAY_SIZE))
        allocate(result_array(ARRAY_SIZE))

        ! Initialize arrays
        do i = 1, ARRAY_SIZE
            array1(i) = real(i)
            array2(i) = real(i * 2)
        end do

        ! Start benchmark measurement
        call this%measurement%start()

        ! Perform array addition benchmark
        do iterations = 1, 1000
            result_array = array1 + array2
        end do

        ! Stop benchmark measurement
        call this%measurement%stop()

        ! Get benchmark results
        elapsed_time = this%measurement%elapsed()
        iterations = this%measurement%iterations()

        ! Verify results
        call qtest_assert_true(elapsed_time >= 0, "Benchmark elapsed time should be non-negative")
        call qtest_assert_true(iterations >= 0, "Benchmark iterations should be non-negative")

        ! Verify computation correctness
        do i = 1, min(10, ARRAY_SIZE)  ! Check first 10 elements
            call qtest_assert_equal(result_array(i), array1(i) + array2(i), "Array addition result should be correct")
        end do

        ! Test QBENCHMARK macro with array operations
        call qtest_benchmark(benchmark_array_addition)

        ! Cleanup
        deallocate(result_array)
        deallocate(array2)
        deallocate(array1)

    end subroutine benchmark_test_case_test_array_operations

    !> Benchmark array addition function
    subroutine benchmark_array_addition()
        integer, parameter :: SIZE = 10000
        real, dimension(SIZE) :: a, b, c
        integer :: i

        ! Initialize
        do i = 1, SIZE
            a(i) = real(i)
            b(i) = real(i * 2)
        end do

        ! Benchmark operation
        c = a + b
    end subroutine benchmark_array_addition

    !> Test matrix operations performance
    subroutine benchmark_test_case_test_matrix_operations(this)
        class(benchmark_test_case), intent(inout) :: this

        integer, parameter :: MATRIX_SIZE = 500
        real, dimension(:,:), allocatable :: matrix1, matrix2, result_matrix
        integer(c_long_long) :: elapsed_time
        integer :: iterations, i, j

        ! Allocate matrices
        allocate(matrix1(MATRIX_SIZE, MATRIX_SIZE))
        allocate(matrix2(MATRIX_SIZE, MATRIX_SIZE))
        allocate(result_matrix(MATRIX_SIZE, MATRIX_SIZE))

        ! Initialize matrices
        do j = 1, MATRIX_SIZE
            do i = 1, MATRIX_SIZE
                matrix1(i,j) = real(i + j)
                matrix2(i,j) = real(i * j)
            end do
        end do

        ! Start benchmark measurement
        call this%measurement%start()

        ! Perform matrix multiplication benchmark
        do iterations = 1, 10  ! Fewer iterations for matrix operations
            result_matrix = matmul(matrix1, matrix2)
        end do

        ! Stop benchmark measurement
        call this%measurement%stop()

        ! Get benchmark results
        elapsed_time = this%measurement%elapsed()
        iterations = this%measurement%iterations()

        ! Verify results
        call qtest_assert_true(elapsed_time >= 0, "Matrix benchmark elapsed time should be non-negative")
        call qtest_assert_true(iterations >= 0, "Matrix benchmark iterations should be non-negative")

        ! Test QBENCHMARK macro with matrix operations
        call qtest_benchmark(benchmark_matrix_multiply)

        ! Cleanup
        deallocate(result_matrix)
        deallocate(matrix2)
        deallocate(matrix1)

    end subroutine benchmark_test_case_test_matrix_operations

    !> Benchmark matrix multiplication function
    subroutine benchmark_matrix_multiply()
        integer, parameter :: SIZE = 100
        real, dimension(SIZE, SIZE) :: a, b, c

        ! Initialize
        a = 1.0
        b = 2.0

        ! Benchmark operation
        c = matmul(a, b)
    end subroutine benchmark_matrix_multiply

    !> Test string operations performance
    subroutine benchmark_test_case_test_string_operations(this)
        class(benchmark_test_case), intent(inout) :: this

        character(len=:), allocatable :: result_string
        character(len=1000) :: base_string
        integer(c_long_long) :: elapsed_time
        integer :: iterations, i

        ! Initialize base string
        base_string = "This is a test string for benchmarking string operations. "

        ! Start benchmark measurement
        call this%measurement%start()

        ! Perform string concatenation benchmark
        do iterations = 1, 1000
            result_string = ""
            do i = 1, 10
                result_string = result_string // base_string
            end do
        end do

        ! Stop benchmark measurement
        call this%measurement%stop()

        ! Get benchmark results
        elapsed_time = this%measurement%elapsed()
        iterations = this%measurement%iterations()

        ! Verify results
        call qtest_assert_true(elapsed_time >= 0, "String benchmark elapsed time should be non-negative")
        call qtest_assert_true(iterations >= 0, "String benchmark iterations should be non-negative")
        call qtest_assert_true(len(result_string) > 0, "Result string should not be empty")

        ! Test QBENCHMARK macro with string operations
        call qtest_benchmark(benchmark_string_concatenation)

    end subroutine benchmark_test_case_test_string_operations

    !> Benchmark string concatenation function
    subroutine benchmark_string_concatenation()
        character(len=:), allocatable :: result
        character(len=100) :: str
        integer :: i

        str = "test string "
        result = ""

        do i = 1, 100
            result = result // str
        end do
    end subroutine benchmark_string_concatenation

    !> Test file operations performance
    subroutine benchmark_test_case_test_file_operations(this)
        class(benchmark_test_case), intent(inout) :: this

        character(len=*), parameter :: TEST_FILE = "benchmark_test.tmp"
        character(len=1000) :: buffer
        integer(c_long_long) :: elapsed_time
        integer :: iterations, i, unit
        logical :: file_exists

        ! Create test data
        buffer = "This is test data for file I/O benchmarking. " // &
                "It contains some sample text that will be written to and read from a file. " // &
                "The purpose is to measure file operation performance."

        ! Start benchmark measurement for file writing
        call this%measurement%start()

        ! Perform file write benchmark
        do iterations = 1, 100
            open(newunit=unit, file=TEST_FILE, status='replace', action='write')
            do i = 1, 10
                write(unit, '(A)') buffer
            end do
            close(unit)
        end do

        ! Stop benchmark measurement
        call this%measurement%stop()

        ! Get benchmark results
        elapsed_time = this%measurement%elapsed()
        iterations = this%measurement%iterations()

        ! Verify results
        call qtest_assert_true(elapsed_time >= 0, "File write benchmark elapsed time should be non-negative")
        call qtest_assert_true(iterations >= 0, "File write benchmark iterations should be non-negative")

        ! Check file was created
        inquire(file=TEST_FILE, exist=file_exists)
        call qtest_assert_true(file_exists, "Benchmark test file should exist")

        ! Test QBENCHMARK macro with file operations
        call qtest_benchmark(benchmark_file_write)

        ! Cleanup test file
        if (file_exists) then
            open(newunit=unit, file=TEST_FILE, status='old')
            close(unit, status='delete')
        end if

    end subroutine benchmark_test_case_test_file_operations

    !> Benchmark file write function
    subroutine benchmark_file_write()
        character(len=*), parameter :: FILE_NAME = "bench_file.tmp"
        character(len=1000) :: data
        integer :: unit, i

        data = "Benchmark test data for file I/O operations. " // &
               "This data is written repeatedly to test performance."

        open(newunit=unit, file=FILE_NAME, status='replace', action='write')
        do i = 1, 50
            write(unit, '(A)') data
        end do
        close(unit, status='delete')  ! Delete after writing
    end subroutine benchmark_file_write

    !> Test memory operations performance
    subroutine benchmark_test_case_test_memory_operations(this)
        class(benchmark_test_case), intent(inout) :: this

        integer, parameter :: NUM_ARRAYS = 1000
        integer, parameter :: ARRAY_SIZE = 1000
        real, dimension(:), allocatable :: arrays(:)
        integer(c_long_long) :: elapsed_time
        integer :: iterations, i, j

        ! Start benchmark measurement for memory allocation
        call this%measurement%start()

        ! Perform memory allocation/deallocation benchmark
        do iterations = 1, 100
            allocate(arrays(NUM_ARRAYS))
            do i = 1, NUM_ARRAYS
                allocate(arrays(i)%data(ARRAY_SIZE))
                do j = 1, ARRAY_SIZE
                    arrays(i)%data(j) = real(j)
                end do
            end do

            ! Use the allocated memory
            do i = 1, NUM_ARRAYS
                arrays(i)%data = arrays(i)%data * 2.0
            end do

            ! Deallocate
            do i = 1, NUM_ARRAYS
                deallocate(arrays(i)%data)
            end do
            deallocate(arrays)
        end do

        ! Stop benchmark measurement
        call this%measurement%stop()

        ! Get benchmark results
        elapsed_time = this%measurement%elapsed()
        iterations = this%measurement%iterations()

        ! Verify results
        call qtest_assert_true(elapsed_time >= 0, "Memory benchmark elapsed time should be non-negative")
        call qtest_assert_true(iterations >= 0, "Memory benchmark iterations should be non-negative")

        ! Test QBENCHMARK macro with memory operations
        call qtest_benchmark(benchmark_memory_allocation)

    end subroutine benchmark_test_case_test_memory_operations

    !> Benchmark memory allocation function
    subroutine benchmark_memory_allocation()
        integer, parameter :: SIZE = 10000
        real, dimension(:), allocatable :: array
        integer :: i

        allocate(array(SIZE))
        do i = 1, SIZE
            array(i) = real(i) * 3.14159
        end do
        array = array * 2.0
        deallocate(array)
    end subroutine benchmark_memory_allocation

    !> Cleanup benchmark test case
    subroutine benchmark_test_case_cleanup(this)
        class(benchmark_test_case), intent(inout) :: this

        call this%measurement%cleanup()
        call this%test_case%cleanup()
    end subroutine benchmark_test_case_cleanup

end program qt_test_benchmark_example