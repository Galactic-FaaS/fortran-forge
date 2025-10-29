!> @brief Test suite for Qt Concurrent framework
!> @author ForGE Contributors
!> @date 2025

program test_concurrent
    use forge_concurrent
    use iso_c_binding
    implicit none

    ! Test variables
    type(c_ptr), allocatable :: test_sequence(:)
    integer :: i
    type(QFuture) :: future
    type(QFutureSynchronizer) :: synchronizer

    ! Initialize test sequence
    allocate(test_sequence(10))
    do i = 1, 10
        ! In real implementation, would store actual data
        test_sequence(i) = c_null_ptr
    end do

    write(*,*) "Testing Qt Concurrent Framework"
    write(*,*) "================================"

    ! Test QtConcurrent::run
    write(*,*) "Testing QtConcurrent::run..."
    future = QtConcurrent%run(test_function)
    call synchronizer%add_future(future)
    call synchronizer%wait_for_finished()
    write(*,*) "QtConcurrent::run test completed"

    ! Test QtConcurrent::map
    write(*,*) "Testing QtConcurrent::map..."
    call QtConcurrent%map(test_sequence, test_map_function)
    write(*,*) "QtConcurrent::map test completed"

    ! Test QtConcurrent::filter
    write(*,*) "Testing QtConcurrent::filter..."
    call QtConcurrent%filter(test_sequence, test_filter_function)
    write(*,*) "QtConcurrent::filter test completed"

    ! Test QtConcurrent::mapped
    write(*,*) "Testing QtConcurrent::mapped..."
    future = QtConcurrent%mapped(test_sequence, test_map_function)
    call future%wait_for_finished()
    write(*,*) "QtConcurrent::mapped test completed"

    ! Test blocking versions
    write(*,*) "Testing blocking versions..."
    call QtConcurrent%blockingMap(test_sequence, test_map_function)
    call QtConcurrent%blockingFilter(test_sequence, test_filter_function)
    write(*,*) "Blocking versions test completed"

    ! Test progress tracking
    write(*,*) "Testing progress tracking..."
    call test_progress_tracking()
    write(*,*) "Progress tracking test completed"

    ! Test exception handling
    write(*,*) "Testing exception handling..."
    call test_exception_handling()
    write(*,*) "Exception handling test completed"

    ! Cleanup
    deallocate(test_sequence)
    call synchronizer%clear_futures()

    write(*,*) "All tests completed successfully!"

contains

    subroutine test_function()
        write(*,*) "Test function executed in thread"
        call sleep_ms(100)  ! Simulate work
    end subroutine test_function

    subroutine test_map_function(item)
        type(c_ptr), intent(inout) :: item
        ! Simulate mapping operation
        write(*,*) "Mapping item"
        call sleep_ms(50)
    end subroutine test_map_function

    function test_filter_function(item) result(keep)
        type(c_ptr), intent(in) :: item
        logical :: keep
        ! Simulate filtering - keep all for test
        keep = .true.
        write(*,*) "Filtering item"
        call sleep_ms(25)
    end function test_filter_function

    subroutine test_progress_tracking()
        type(QFuture) :: progress_future
        type(QPromise) :: promise
        integer :: i

        progress_future = promise%get_future()

        ! Simulate progress updates
        call promise%set_progress_range(0, 100)
        do i = 0, 100, 10
            call promise%set_progress_value(i)
            call sleep_ms(20)
        end do

        call promise%finish()
        call progress_future%wait_for_finished()

        write(*,*) "Progress reached:", progress_future%progress_value()
    end subroutine test_progress_tracking

    subroutine test_exception_handling()
        type(ConcurrentException) :: ex

        ex%message = "Test exception"
        ex%error_code = 42
        call handle_concurrent_exception(ex)
    end subroutine test_exception_handling

    subroutine sleep_ms(milliseconds)
        integer, intent(in) :: milliseconds

        interface
            subroutine Sleep(dwMilliseconds) bind(C, name="Sleep")
                import :: c_int
                integer(c_int), value :: dwMilliseconds
            end subroutine Sleep
        end interface

        call Sleep(milliseconds)
    end subroutine sleep_ms

end program test_concurrent