!> @brief Future/Promise pattern for async operations
!> @details Qt-style async/await with Future and Promise
!> @author ForGE Contributors
!> @date 2025
!> @license GPL-3.0-or-later

module forge_future
    use iso_c_binding
    use forge_thread
    use forge_signals
    implicit none
    private

    public :: QFuture, QPromise, QFutureWatcher
    public :: FutureState, Pending, Running, Finished, Canceled

    !> Future states
    integer, parameter :: Pending = 0
    integer, parameter :: Running = 1
    integer, parameter :: Finished = 2
    integer, parameter :: Canceled = 3

    type :: FutureState
        integer :: value = Pending
    end type FutureState

    !> @brief Future for async operations
    type :: QFuture
        private
        type(FutureState) :: state
        logical :: result_ready = .false.
        logical :: canceled_flag = .false.
        integer :: progress_value = 0
        integer :: progress_minimum = 0
        integer :: progress_maximum = 0
        type(signal_void) :: finished
        type(signal_void) :: canceled
        type(signal_void) :: result_ready_signal
        type(signal_int) :: progress_value_changed
        type(signal_int_int) :: progress_range_changed
        type(QMutex) :: mutex
    contains
        procedure :: wait_for_finished => future_wait_finished
        procedure :: is_finished => future_is_finished
        procedure :: is_running => future_is_running
        procedure :: is_started => future_is_started
        procedure :: is_canceled => future_is_canceled
        procedure :: cancel => future_cancel
        procedure :: is_result_ready => future_is_result_ready
        procedure :: wait_for_result => future_wait_result
        procedure :: progress_value => future_progress_value
        procedure :: progress_minimum => future_progress_minimum
        procedure :: progress_maximum => future_progress_maximum
        procedure :: set_progress_value => future_set_progress_value
        procedure :: set_progress_range => future_set_progress_range
    end type QFuture

    !> @brief Promise for async operations
    type :: QPromise
        private
        type(QFuture) :: future
        logical :: finished = .false.
    contains
        procedure :: start => promise_start
        procedure :: finish => promise_finish
        procedure :: cancel => promise_cancel
        procedure :: report_result => promise_report_result
        procedure :: get_future => promise_get_future
    end type QPromise

    !> @brief Future watcher for monitoring completion
    type :: QFutureWatcher
        private
        type(QFuture), pointer :: watched_future => null()
        type(signal_void) :: finished
        type(signal_void) :: canceled
        type(signal_void) :: result_ready
    contains
        procedure :: set_future => watcher_set_future
        procedure :: future => watcher_get_future
        procedure :: is_finished => watcher_is_finished
        procedure :: is_running => watcher_is_running
        procedure :: is_started => watcher_is_started
        procedure :: is_canceled => watcher_is_canceled
        procedure :: wait_for_finished => watcher_wait_for_finished
    end type QFutureWatcher

contains

    ! ========== QFuture Implementation ==========

    subroutine future_wait_finished(this, msecs)
        class(QFuture), intent(inout) :: this
        integer, intent(in), optional :: msecs
        integer :: timeout, elapsed
        type(QWaitCondition) :: condition

        if (this%state%value == Finished) return

        timeout = 30000  ! 30 seconds default
        if (present(msecs)) timeout = msecs

        call this%mutex%lock()
        elapsed = 0
        do while (elapsed < timeout .and. this%state%value /= Finished)
            call condition%wait(this%mutex, 10)
            elapsed = elapsed + 10
        end do
        call this%mutex%unlock()
    end subroutine future_wait_finished

    function future_is_finished(this) result(finished)
        class(QFuture), intent(in) :: this
        logical :: finished
        finished = (this%state%value == Finished)
    end function future_is_finished

    function future_is_running(this) result(running)
        class(QFuture), intent(in) :: this
        logical :: running
        running = (this%state%value == Running)
    end function future_is_running

    function future_is_started(this) result(started)
        class(QFuture), intent(in) :: this
        logical :: started
        started = (this%state%value == Running .or. this%state%value == Finished)
    end function future_is_started

    function future_is_canceled(this) result(canceled)
        class(QFuture), intent(in) :: this
        logical :: canceled
        canceled = (this%state%value == Canceled)
    end function future_is_canceled

    subroutine future_cancel(this)
        class(QFuture), intent(inout) :: this
        call this%mutex%lock()
        this%state%value = Canceled
        call this%mutex%unlock()
        call this%canceled%emit()
    end subroutine future_cancel

    function future_is_result_ready(this) result(ready)
        class(QFuture), intent(in) :: this
        logical :: ready
        ready = this%result_ready
    end function future_is_result_ready

    subroutine future_wait_result(this, msecs)
        class(QFuture), intent(inout) :: this
        integer, intent(in), optional :: msecs

        call this%wait_for_finished(msecs)

        call this%mutex%lock()
        if (this%is_finished()) then
            this%result_ready = .true.
            call this%mutex%unlock()
            call this%result_ready_signal%emit()
        else
            call this%mutex%unlock()
        end if
    end subroutine future_wait_result

    function future_progress_value(this) result(value)
        class(QFuture), intent(in) :: this
        integer :: value
        value = this%progress_value
    end function future_progress_value

    function future_progress_minimum(this) result(minimum)
        class(QFuture), intent(in) :: this
        integer :: minimum
        minimum = this%progress_minimum
    end function future_progress_minimum

    function future_progress_maximum(this) result(maximum)
        class(QFuture), intent(in) :: this
        integer :: maximum
        maximum = this%progress_maximum
    end function future_progress_maximum

    subroutine future_set_progress_value(this, value)
        class(QFuture), intent(inout) :: this
        integer, intent(in) :: value

        call this%mutex%lock()
        this%progress_value = value
        call this%mutex%unlock()
        call this%progress_value_changed%emit(value)
    end subroutine future_set_progress_value

    subroutine future_set_progress_range(this, minimum, maximum)
        class(QFuture), intent(inout) :: this
        integer, intent(in) :: minimum, maximum

        call this%mutex%lock()
        this%progress_minimum = minimum
        this%progress_maximum = maximum
        call this%mutex%unlock()
        call this%progress_range_changed%emit(minimum, maximum)
    end subroutine future_set_progress_range

    ! ========== QPromise Implementation ==========

    subroutine promise_start(this)
        class(QPromise), intent(inout) :: this
        call this%future%mutex%lock()
        this%future%state%value = Running
        call this%future%mutex%unlock()
    end subroutine promise_start

    subroutine promise_finish(this)
        class(QPromise), intent(inout) :: this
        call this%future%mutex%lock()
        this%finished = .true.
        this%future%state%value = Finished
        call this%future%mutex%unlock()
        call this%future%finished%emit()
    end subroutine promise_finish

    subroutine promise_cancel(this)
        class(QPromise), intent(inout) :: this
        call this%future%mutex%lock()
        this%future%state%value = Canceled
        call this%future%mutex%unlock()
        call this%future%canceled%emit()
    end subroutine promise_cancel

    subroutine promise_report_result(this)
        class(QPromise), intent(inout) :: this
        call this%future%mutex%lock()
        this%future%result_ready = .true.
        call this%future%mutex%unlock()
        call this%future%result_ready_signal%emit()
    end subroutine promise_report_result

    function promise_get_future(this) result(future)
        class(QPromise), intent(in) :: this
        type(QFuture) :: future
        future = this%future
    end function promise_get_future

    ! ========== QFutureWatcher Implementation ==========

    subroutine watcher_set_future(this, future)
        class(QFutureWatcher), intent(inout) :: this
        type(QFuture), target, intent(in) :: future

        this%watched_future => future

        ! Connect to future signals
        call future%finished%connect(this%finished%emit)
        call future%canceled%connect(this%canceled%emit)
        call future%result_ready_signal%connect(this%result_ready%emit)
    end subroutine watcher_set_future

    function watcher_get_future(this) result(future)
        class(QFutureWatcher), intent(in) :: this
        type(QFuture), pointer :: future
        future => this%watched_future
    end function watcher_get_future

    function watcher_is_finished(this) result(finished)
        class(QFutureWatcher), intent(in) :: this
        logical :: finished
        if (associated(this%watched_future)) then
            finished = this%watched_future%is_finished()
        else
            finished = .false.
        end if
    end function watcher_is_finished

    function watcher_is_running(this) result(running)
        class(QFutureWatcher), intent(in) :: this
        logical :: running
        if (associated(this%watched_future)) then
            running = this%watched_future%is_running()
        else
            running = .false.
        end if
    end function watcher_is_running

    function watcher_is_started(this) result(started)
        class(QFutureWatcher), intent(in) :: this
        logical :: started
        if (associated(this%watched_future)) then
            started = this%watched_future%is_started()
        else
            started = .false.
        end if
    end function watcher_is_started

    function watcher_is_canceled(this) result(canceled)
        class(QFutureWatcher), intent(in) :: this
        logical :: canceled
        if (associated(this%watched_future)) then
            canceled = this%watched_future%is_canceled()
        else
            canceled = .false.
        end if
    end function watcher_is_canceled

    subroutine watcher_wait_for_finished(this, msecs)
        class(QFutureWatcher), intent(inout) :: this
        integer, intent(in), optional :: msecs
        if (associated(this%watched_future)) then
            call this%watched_future%wait_for_finished(msecs)
        end if
    end subroutine watcher_wait_for_finished

    ! ========== Helper Functions ==========

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

end module forge_future

