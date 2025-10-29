! ForGE Multimedia Error Handling
! Comprehensive error management for multimedia operations
!
! This module provides error handling for:
! - Audio device errors
! - Video device errors
! - Codec errors
! - Playback errors
! - Recording errors

module forge_multimedia_errors
  use iso_c_binding
  use forge_types
  implicit none

  ! Error codes for multimedia operations
  integer(c_int), parameter :: MULTIMEDIA_SUCCESS = 0
  integer(c_int), parameter :: MULTIMEDIA_ERROR_GENERIC = -1
  integer(c_int), parameter :: MULTIMEDIA_ERROR_DEVICE_NOT_FOUND = -2
  integer(c_int), parameter :: MULTIMEDIA_ERROR_DEVICE_BUSY = -3
  integer(c_int), parameter :: MULTIMEDIA_ERROR_INVALID_FORMAT = -4
  integer(c_int), parameter :: MULTIMEDIA_ERROR_UNSUPPORTED_FORMAT = -5
  integer(c_int), parameter :: MULTIMEDIA_ERROR_BUFFER_OVERFLOW = -6
  integer(c_int), parameter :: MULTIMEDIA_ERROR_BUFFER_UNDERFLOW = -7
  integer(c_int), parameter :: MULTIMEDIA_ERROR_IO_ERROR = -8
  integer(c_int), parameter :: MULTIMEDIA_ERROR_PERMISSION_DENIED = -9
  integer(c_int), parameter :: MULTIMEDIA_ERROR_NOT_INITIALIZED = -10
  integer(c_int), parameter :: MULTIMEDIA_ERROR_ALREADY_INITIALIZED = -11
  integer(c_int), parameter :: MULTIMEDIA_ERROR_INVALID_STATE = -12
  integer(c_int), parameter :: MULTIMEDIA_ERROR_CODEC_NOT_FOUND = -13
  integer(c_int), parameter :: MULTIMEDIA_ERROR_DECODING_FAILED = -14
  integer(c_int), parameter :: MULTIMEDIA_ERROR_ENCODING_FAILED = -15
  integer(c_int), parameter :: MULTIMEDIA_ERROR_FILE_NOT_FOUND = -16
  integer(c_int), parameter :: MULTIMEDIA_ERROR_FILE_CORRUPT = -17
  integer(c_int), parameter :: MULTIMEDIA_ERROR_NETWORK_ERROR = -18
  integer(c_int), parameter :: MULTIMEDIA_ERROR_TIMEOUT = -19
  integer(c_int), parameter :: MULTIMEDIA_ERROR_RESOURCE_EXHAUSTED = -20

  ! Multimedia error structure
  type :: multimedia_error
    private
    integer(c_int) :: code = MULTIMEDIA_SUCCESS
    character(len=:), allocatable :: message
    character(len=:), allocatable :: details
    character(len=:), allocatable :: context
  contains
    procedure :: set_error => multimedia_error_set
    procedure :: clear => multimedia_error_clear
    procedure :: get_code => multimedia_error_get_code
    procedure :: get_message => multimedia_error_get_message
    procedure :: get_details => multimedia_error_get_details
    procedure :: get_context => multimedia_error_get_context
    procedure :: is_error => multimedia_error_is_error
    procedure :: is_success => multimedia_error_is_success
    procedure :: to_string => multimedia_error_to_string
  end type multimedia_error

  ! Global error instance for convenience
  type(multimedia_error), save :: global_multimedia_error

contains

  ! ============================================================================
  ! Multimedia Error Implementation
  ! ============================================================================

  subroutine multimedia_error_set(this, code, message, details, context)
    class(multimedia_error), intent(inout) :: this
    integer(c_int), intent(in) :: code
    character(len=*), intent(in) :: message
    character(len=*), intent(in), optional :: details
    character(len=*), intent(in), optional :: context

    this%code = code
    this%message = trim(message)

    if (present(details)) then
      this%details = trim(details)
    else
      this%details = ""
    end if

    if (present(context)) then
      this%context = trim(context)
    else
      this%context = ""
    end if
  end subroutine multimedia_error_set

  subroutine multimedia_error_clear(this)
    class(multimedia_error), intent(inout) :: this
    this%code = MULTIMEDIA_SUCCESS
    if (allocated(this%message)) deallocate(this%message)
    if (allocated(this%details)) deallocate(this%details)
    if (allocated(this%context)) deallocate(this%context)
  end subroutine multimedia_error_clear

  function multimedia_error_get_code(this) result(code)
    class(multimedia_error), intent(in) :: this
    integer(c_int) :: code
    code = this%code
  end function multimedia_error_get_code

  function multimedia_error_get_message(this) result(message)
    class(multimedia_error), intent(in) :: this
    character(len=:), allocatable :: message
    if (allocated(this%message)) then
      message = this%message
    else
      message = ""
    end if
  end function multimedia_error_get_message

  function multimedia_error_get_details(this) result(details)
    class(multimedia_error), intent(in) :: this
    character(len=:), allocatable :: details
    if (allocated(this%details)) then
      details = this%details
    else
      details = ""
    end if
  end function multimedia_error_get_details

  function multimedia_error_get_context(this) result(context)
    class(multimedia_error), intent(in) :: this
    character(len=:), allocatable :: context
    if (allocated(this%context)) then
      context = this%context
    else
      context = ""
    end if
  end function multimedia_error_get_context

  function multimedia_error_is_error(this) result(is_error)
    class(multimedia_error), intent(in) :: this
    logical :: is_error
    is_error = (this%code /= MULTIMEDIA_SUCCESS)
  end function multimedia_error_is_error

  function multimedia_error_is_success(this) result(is_success)
    class(multimedia_error), intent(in) :: this
    logical :: is_success
    is_success = (this%code == MULTIMEDIA_SUCCESS)
  end function multimedia_error_is_success

  function multimedia_error_to_string(this) result(str)
    class(multimedia_error), intent(in) :: this
    character(len=:), allocatable :: str
    character(len=20) :: code_str

    write(code_str, '(I0)') this%code
    str = "MultimediaError(code=" // trim(code_str) // ", message='" // &
          this%get_message() // "'"

    if (len_trim(this%get_details()) > 0) then
      str = str // ", details='" // this%get_details() // "'"
    end if

    if (len_trim(this%get_context()) > 0) then
      str = str // ", context='" // this%get_context() // "'"
    end if

    str = str // ")"
  end function multimedia_error_to_string

  ! ============================================================================
  ! Convenience Functions
  ! ============================================================================

  subroutine set_multimedia_error(code, message, details, context)
    integer(c_int), intent(in) :: code
    character(len=*), intent(in) :: message
    character(len=*), intent(in), optional :: details
    character(len=*), intent(in), optional :: context

    call global_multimedia_error%set_error(code, message, details, context)
  end subroutine set_multimedia_error

  subroutine clear_multimedia_error()
    call global_multimedia_error%clear()
  end subroutine clear_multimedia_error

  function get_multimedia_error() result(error)
    type(multimedia_error) :: error
    error = global_multimedia_error
  end function get_multimedia_error

  function multimedia_error_code_to_string(code) result(str)
    integer(c_int), intent(in) :: code
    character(len=:), allocatable :: str

    select case (code)
    case (MULTIMEDIA_SUCCESS)
      str = "Success"
    case (MULTIMEDIA_ERROR_GENERIC)
      str = "Generic error"
    case (MULTIMEDIA_ERROR_DEVICE_NOT_FOUND)
      str = "Device not found"
    case (MULTIMEDIA_ERROR_DEVICE_BUSY)
      str = "Device busy"
    case (MULTIMEDIA_ERROR_INVALID_FORMAT)
      str = "Invalid format"
    case (MULTIMEDIA_ERROR_UNSUPPORTED_FORMAT)
      str = "Unsupported format"
    case (MULTIMEDIA_ERROR_BUFFER_OVERFLOW)
      str = "Buffer overflow"
    case (MULTIMEDIA_ERROR_BUFFER_UNDERFLOW)
      str = "Buffer underflow"
    case (MULTIMEDIA_ERROR_IO_ERROR)
      str = "I/O error"
    case (MULTIMEDIA_ERROR_PERMISSION_DENIED)
      str = "Permission denied"
    case (MULTIMEDIA_ERROR_NOT_INITIALIZED)
      str = "Not initialized"
    case (MULTIMEDIA_ERROR_ALREADY_INITIALIZED)
      str = "Already initialized"
    case (MULTIMEDIA_ERROR_INVALID_STATE)
      str = "Invalid state"
    case (MULTIMEDIA_ERROR_CODEC_NOT_FOUND)
      str = "Codec not found"
    case (MULTIMEDIA_ERROR_DECODING_FAILED)
      str = "Decoding failed"
    case (MULTIMEDIA_ERROR_ENCODING_FAILED)
      str = "Encoding failed"
    case (MULTIMEDIA_ERROR_FILE_NOT_FOUND)
      str = "File not found"
    case (MULTIMEDIA_ERROR_FILE_CORRUPT)
      str = "File corrupt"
    case (MULTIMEDIA_ERROR_NETWORK_ERROR)
      str = "Network error"
    case (MULTIMEDIA_ERROR_TIMEOUT)
      str = "Timeout"
    case (MULTIMEDIA_ERROR_RESOURCE_EXHAUSTED)
      str = "Resource exhausted"
    case default
      str = "Unknown error"
    end select
  end function multimedia_error_code_to_string

end module forge_multimedia_errors