!> @brief Error handling for ForGE
!> @details Provides error codes, messages, and handling utilities
!> @author ForGE Contributors
!> @date 2025
!> @license GPL-3.0-or-later

module forge_errors
    use iso_fortran_env, only: error_unit
    implicit none
    private

    public :: forge_error, forge_status
    public :: FORGE_SUCCESS, FORGE_ERROR_GENERIC, FORGE_ERROR_NULL_PTR
    public :: FORGE_ERROR_INVALID_ARG, FORGE_ERROR_BACKEND, FORGE_ERROR_NOT_IMPLEMENTED
    public :: forge_check_status, forge_error_message

    !> Error codes
    integer, parameter :: FORGE_SUCCESS = 0
    integer, parameter :: FORGE_ERROR_GENERIC = 1
    integer, parameter :: FORGE_ERROR_NULL_PTR = 2
    integer, parameter :: FORGE_ERROR_INVALID_ARG = 3
    integer, parameter :: FORGE_ERROR_BACKEND = 4
    integer, parameter :: FORGE_ERROR_NOT_IMPLEMENTED = 5

    !> @brief Error status type
    type :: forge_status
        integer :: code = FORGE_SUCCESS
        character(len=256) :: message = ""
    contains
        procedure :: is_ok => forge_status_is_ok
        procedure :: is_error => forge_status_is_error
        procedure :: set => forge_status_set
        procedure :: clear => forge_status_clear
        procedure :: print => forge_status_print
    end type forge_status

    !> @brief Error handling type
    type :: forge_error
        type(forge_status) :: status
    contains
        procedure :: raise => forge_error_raise
        procedure :: clear => forge_error_clear
    end type forge_error

contains

    !> @brief Check if status is OK
    pure function forge_status_is_ok(this) result(ok)
        class(forge_status), intent(in) :: this
        logical :: ok
        ok = (this%code == FORGE_SUCCESS)
    end function forge_status_is_ok

    !> @brief Check if status is an error
    pure function forge_status_is_error(this) result(err)
        class(forge_status), intent(in) :: this
        logical :: err
        err = (this%code /= FORGE_SUCCESS)
    end function forge_status_is_error

    !> @brief Set error status
    subroutine forge_status_set(this, code, message)
        class(forge_status), intent(inout) :: this
        integer, intent(in) :: code
        character(len=*), intent(in), optional :: message
        
        this%code = code
        if (present(message)) then
            this%message = message
        else
            this%message = forge_error_message(code)
        end if
    end subroutine forge_status_set

    !> @brief Clear error status
    subroutine forge_status_clear(this)
        class(forge_status), intent(inout) :: this
        this%code = FORGE_SUCCESS
        this%message = ""
    end subroutine forge_status_clear

    !> @brief Print error status
    subroutine forge_status_print(this)
        class(forge_status), intent(in) :: this
        if (this%is_error()) then
            write(error_unit, '(A,I0,A,A)') "ForGE Error [", this%code, "]: ", trim(this%message)
        end if
    end subroutine forge_status_print

    !> @brief Raise an error
    subroutine forge_error_raise(this, code, message)
        class(forge_error), intent(inout) :: this
        integer, intent(in) :: code
        character(len=*), intent(in), optional :: message
        
        call this%status%set(code, message)
        call this%status%print()
    end subroutine forge_error_raise

    !> @brief Clear error
    subroutine forge_error_clear(this)
        class(forge_error), intent(inout) :: this
        call this%status%clear()
    end subroutine forge_error_clear

    !> @brief Check status and print if error
    subroutine forge_check_status(status, abort_on_error)
        type(forge_status), intent(in) :: status
        logical, intent(in), optional :: abort_on_error
        logical :: do_abort
        
        do_abort = .false.
        if (present(abort_on_error)) do_abort = abort_on_error
        
        if (status%is_error()) then
            call status%print()
            if (do_abort) then
                error stop "ForGE: Fatal error occurred"
            end if
        end if
    end subroutine forge_check_status

    !> @brief Get error message for code
    function forge_error_message(code) result(message)
        integer, intent(in) :: code
        character(len=256) :: message
        
        select case (code)
        case (FORGE_SUCCESS)
            message = "Success"
        case (FORGE_ERROR_GENERIC)
            message = "Generic error"
        case (FORGE_ERROR_NULL_PTR)
            message = "Null pointer encountered"
        case (FORGE_ERROR_INVALID_ARG)
            message = "Invalid argument"
        case (FORGE_ERROR_BACKEND)
            message = "Backend error"
        case (FORGE_ERROR_NOT_IMPLEMENTED)
            message = "Feature not implemented"
        case default
            message = "Unknown error"
        end select
    end function forge_error_message

end module forge_errors

