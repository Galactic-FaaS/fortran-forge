# forge_errors

Error handling system for ForGE applications, providing structured error reporting and status management.

## Synopsis

```fortran
! Error status type
type :: forge_status
    private
    integer :: code = FORGE_SUCCESS
    character(len=:), allocatable :: message
contains
    procedure :: set => forge_status_set
    procedure :: clear => forge_status_clear
    procedure :: is_ok => forge_status_is_ok
    procedure :: is_error => forge_status_is_error
    procedure :: get_code => forge_status_get_code
    procedure :: get_message => forge_status_get_message
    procedure :: print => forge_status_print
end type forge_status

! Error type
type :: forge_error
    private
    integer :: code = FORGE_SUCCESS
    character(len=:), allocatable :: message
    character(len=:), allocatable :: context
contains
    procedure :: raise => forge_error_raise
    procedure :: clear => forge_error_clear
    procedure :: get_code => forge_error_get_code
    procedure :: get_message => forge_error_get_message
    procedure :: get_context => forge_error_get_context
end type forge_error
```

## Description

The ForGE error handling system provides:

- Structured error reporting
- Status codes for different error conditions
- Contextual error information
- Consistent error handling across the library
- Easy integration with application error handling

## Error Codes

ForGE defines standard error codes:

```fortran
integer, parameter, public :: FORGE_SUCCESS = 0
integer, parameter, public :: FORGE_ERROR_GENERIC = -1
integer, parameter, public :: FORGE_ERROR_NULL_PTR = -2
integer, parameter, public :: FORGE_ERROR_INVALID_ARG = -3
integer, parameter, public :: FORGE_ERROR_BACKEND = -4
integer, parameter, public :: FORGE_ERROR_NOT_IMPLEMENTED = -5
integer, parameter, public :: FORGE_ERROR_OUT_OF_MEMORY = -6
integer, parameter, public :: FORGE_ERROR_IO = -7
integer, parameter, public :: FORGE_ERROR_TIMEOUT = -8
```

## forge_status

Lightweight status reporting for operations.

### Methods

#### set(code, message)
Sets the status code and message.

**Parameters:**
- `code` (integer): Error code
- `message` (character): Error description

**Example:**
```fortran
type(forge_status) :: status

call status%set(FORGE_ERROR_INVALID_ARG, "Width must be positive")
```

#### clear()
Clears the status (sets to success).

**Example:**
```fortran
call status%clear()
```

#### is_ok()
Checks if status indicates success.

**Returns:** logical - True if successful

**Example:**
```fortran
if (status%is_ok()) then
    print *, "Operation succeeded"
end if
```

#### is_error()
Checks if status indicates an error.

**Returns:** logical - True if error occurred

**Example:**
```fortran
if (status%is_error()) then
    print *, "Error occurred"
end if
```

#### get_code()
Gets the status code.

**Returns:** integer - Status code

#### get_message()
Gets the status message.

**Returns:** character - Status description

#### print()
Prints the status to standard output.

**Example:**
```fortran
call status%print()  ! Prints: "ERROR: Invalid argument"
```

## forge_error

More detailed error information with context.

### Methods

#### raise(code, message, context)
Raises an error with code, message, and context.

**Parameters:**
- `code` (integer): Error code
- `message` (character): Error description
- `context` (character, optional): Additional context

**Example:**
```fortran
type(forge_error) :: error

call error%raise(FORGE_ERROR_BACKEND, "Failed to initialize GUI", "Windows platform")
```

#### clear()
Clears the error.

**Example:**
```fortran
call error%clear()
```

#### get_code()
Gets the error code.

**Returns:** integer - Error code

#### get_message()
Gets the error message.

**Returns:** character - Error description

#### get_context()
Gets the error context.

**Returns:** character - Additional context information

## Error Handling Patterns

### Function Return Status
```fortran
function create_widget(...) result(success)
    ! ...
    type(forge_status), intent(out) :: status

    if (invalid_parameter) then
        call status%set(FORGE_ERROR_INVALID_ARG, "Parameter out of range")
        success = .false.
        return
    end if

    call status%clear()
    success = .true.
end function create_widget
```

### Error Propagation
```fortran
subroutine process_data(status)
    type(forge_status), intent(out) :: status

    call validate_input(status)
    if (status%is_error()) return

    call process_algorithm(status)
    if (status%is_error()) return

    call save_results(status)
end subroutine process_data
```

### Error Checking Utility
```fortran
subroutine forge_check_status(status, abort_on_error)
    type(forge_status), intent(in) :: status
    logical, intent(in), optional :: abort_on_error

    if (status%is_error()) then
        print *, "ForGE Error: ", status%get_message()
        if (present(abort_on_error)) then
            if (abort_on_error) then
                stop 1
            end if
        end if
    end if
end subroutine forge_check_status
```

## Common Error Scenarios

### Backend Initialization
```fortran
type(forge_status) :: status

call backend%init(status)
if (status%is_error()) then
    select case (status%get_code())
    case (FORGE_ERROR_BACKEND)
        print *, "Backend initialization failed"
    case (FORGE_ERROR_NOT_IMPLEMENTED)
        print *, "Backend not available on this platform"
    end select
end if
```

### Widget Creation
```fortran
type(forge_status) :: status

call create_button(button, status)
if (status%is_error()) then
    print *, "Button creation failed: ", status%get_message()
    ! Handle error: use default button, show message, etc.
end if
```

### File Operations
```fortran
type(forge_status) :: status

call load_config(filename, config, status)
if (status%is_error()) then
    select case (status%get_code())
    case (FORGE_ERROR_IO)
        print *, "Could not read config file: ", filename
    case (FORGE_ERROR_INVALID_ARG)
        print *, "Invalid config file format"
    end select
    ! Use default configuration
    call load_default_config(config)
end if
```

## Error Context

Providing context helps with debugging:

```fortran
subroutine load_image(filename, image, status)
    character(len=*), intent(in) :: filename
    type(image_type), intent(out) :: image
    type(forge_status), intent(out) :: status

    if (.not. file_exists(filename)) then
        call status%set(FORGE_ERROR_IO, "Image file not found: " // filename)
        return
    end if

    ! Attempt to load image...
    if (failed_to_load) then
        call status%set(FORGE_ERROR_IO, "Failed to decode image format", &
                       "File: " // filename // ", attempted format: " // get_format(filename))
        return
    end if
end subroutine load_image
```

## Error Recovery

Different strategies for error recovery:

### Fail Fast
```fortran
call app%init(BACKEND_QT, status)
call forge_check_status(status, abort_on_error=.true.)
! Application stops if backend fails
```

### Graceful Degradation
```fortran
call app%init(BACKEND_QT, status)
if (status%is_error()) then
    print *, "Qt backend failed, trying custom backend"
    call app%init(BACKEND_CUSTOM, status)
    call forge_check_status(status, abort_on_error=.true.)
end if
```

### User Notification
```fortran
call save_file(filename, data, status)
if (status%is_error()) then
    call show_error_dialog("Save failed: " // status%get_message())
    ! Allow user to choose different filename or location
end if
```

## Performance Considerations

Error handling has minimal performance impact:

- Status objects are lightweight
- Error messages are only created when needed
- No overhead for success cases
- String operations are efficient

## Thread Safety

Error objects are not thread-safe. Each thread should use its own error/status objects.

## Example Usage

```fortran
program error_handling_demo
    use forge
    implicit none

    type(forge_application) :: app
    type(forge_window_t) :: window
    type(forge_status) :: status

    ! Initialize application with error handling
    call app%init(BACKEND_CUSTOM, status)
    if (status%is_error()) then
        print *, "Application initialization failed: ", status%get_message()
        stop 1
    end if

    ! Create window with error checking
    window = app%create_window("Error Demo", 400, 300)
    ! Note: create_window uses internal status checking

    ! Show window
    call window%show()

    ! Run application
    call app%run()

    ! Cleanup
    call app%shutdown()

end program error_handling_demo
```

## Best Practices

### Consistent Error Checking
```fortran
! Good: Check all operations that can fail
call backend%init(status)
call forge_check_status(status)

call window%show()
! Window show operations typically don't fail

call file%save(status)
call forge_check_status(status)
```

### Meaningful Error Messages
```fortran
! Good: Specific and actionable
call status%set(FORGE_ERROR_INVALID_ARG, "Window width must be between 100 and 4096 pixels")

! Bad: Vague and unhelpful
call status%set(FORGE_ERROR_GENERIC, "Error")
```

### Error Context
```fortran
! Good: Include relevant information
call status%set(FORGE_ERROR_IO, "Failed to open file for writing", &
               "File: " // filename // ", permissions: " // get_permissions(filename))

! Bad: Missing context
call status%set(FORGE_ERROR_IO, "File operation failed")
```

### Error Propagation
```fortran
! Good: Propagate errors up the call stack
subroutine high_level_operation(status)
    type(forge_status), intent(out) :: status

    call low_level_operation(status)
    if (status%is_error()) return  ! Don't mask errors

    call another_operation(status)
    if (status%is_error()) return
end subroutine high_level_operation
```

## See Also

- [forge_application](forge_application.md) - Uses error handling
- [forge_backend_base](forge_backend_base.md) - Backend error reporting
- [forge_status](forge_status.md) - Status type details