!> @brief Thread-local storage support
!> @details Cross-platform thread-local storage (TLS) implementation
!> @author ForGE Contributors
!> @date 2025
!> @license GPL-3.0-or-later

module forge_thread_local
    use iso_c_binding
    use forge_errors
    implicit none
    private

    public :: QThreadStorage

    !> @brief Thread-local storage container
    type :: QThreadStorage
        private
        type(c_ptr) :: key_handle = c_null_ptr
        logical :: initialized = .false.
    contains
        procedure :: set_value => tls_set_value
        procedure :: get_value => tls_get_value
        procedure :: has_value => tls_has_value
        procedure :: remove => tls_remove
        procedure :: init => tls_init
        procedure :: destroy => tls_destroy
    end type QThreadStorage

contains

    !> @brief Initialize thread-local storage
    subroutine tls_init(this)
#ifdef _WIN32
        use forge_thread_windows, only: win_thread_key_create
#endif
#ifndef _WIN32
        use forge_thread_posix, only: posix_thread_key_create
#endif
        class(QThreadStorage), intent(inout) :: this

        if (this%initialized) return

#ifdef _WIN32
        ! Windows TLS implementation would go here
        ! For now, placeholder
        this%key_handle = c_null_ptr
#endif
#ifndef _WIN32
        this%key_handle = posix_thread_key_create(c_null_funptr)
#endif

        this%initialized = c_associated(this%key_handle)
    end subroutine tls_init

    !> @brief Set thread-local value
    subroutine tls_set_value(this, value)
#ifdef _WIN32
        use forge_thread_windows, only: win_thread_setspecific
#endif
#ifndef _WIN32
        use forge_thread_posix, only: posix_thread_setspecific
#endif
        class(QThreadStorage), intent(inout) :: this
        type(c_ptr), intent(in) :: value

        if (.not. this%initialized) call this%init()

#ifdef _WIN32
        ! Windows implementation
#endif
#ifndef _WIN32
        call posix_thread_setspecific(transfer(this%key_handle, 0), value)
#endif
    end subroutine tls_set_value

    !> @brief Get thread-local value
    function tls_get_value(this) result(value)
#ifdef _WIN32
        use forge_thread_windows, only: win_thread_getspecific
#endif
#ifndef _WIN32
        use forge_thread_posix, only: posix_thread_getspecific
#endif
        class(QThreadStorage), intent(in) :: this
        type(c_ptr) :: value

        if (.not. this%initialized) then
            value = c_null_ptr
            return
        end if

#ifdef _WIN32
        ! Windows implementation
        value = c_null_ptr
#endif
#ifndef _WIN32
        value = posix_thread_getspecific(transfer(this%key_handle, 0))
#endif
    end function tls_get_value

    !> @brief Check if thread-local value exists
    function tls_has_value(this) result(has_value)
        class(QThreadStorage), intent(in) :: this
        logical :: has_value

        has_value = c_associated(this%get_value())
    end function tls_has_value

    !> @brief Remove thread-local value
    subroutine tls_remove(this)
        class(QThreadStorage), intent(inout) :: this

        if (.not. this%initialized) return

        call this%set_value(c_null_ptr)
    end subroutine tls_remove

    !> @brief Destroy thread-local storage
    subroutine tls_destroy(this)
#ifdef _WIN32
        use forge_thread_windows, only: win_thread_key_delete
#endif
#ifndef _WIN32
        use forge_thread_posix, only: posix_thread_key_delete
#endif
        class(QThreadStorage), intent(inout) :: this

        if (.not. this%initialized) return

#ifdef _WIN32
        ! Windows implementation
#endif
#ifndef _WIN32
        call posix_thread_key_delete(transfer(this%key_handle, 0))
#endif

        this%key_handle = c_null_ptr
        this%initialized = .false.
    end subroutine tls_destroy

end module forge_thread_local