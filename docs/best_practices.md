# ForGE Best Practices

This guide outlines recommended practices for developing high-quality ForGE applications, covering coding standards, architecture patterns, performance optimization, and maintenance strategies.

## Code Organization

### Module Structure

Organize code into logical modules with clear responsibilities:

```fortran
! Good: Clear module organization
module ui_widgets
    ! Widget-related functionality
end module ui_widgets

module data_models
    ! Data structures and business logic
end module data_models

module application_logic
    ! Main application coordination
end module application_logic
```

### File Naming Conventions

Use consistent naming for source files:

```
src/
├── main.f90              # Application entry point
├── app_config.f90        # Configuration management
├── ui/
│   ├── main_window.f90   # Main window
│   ├── dialogs.f90       # Dialog windows
│   └── widgets/
│       ├── buttons.f90   # Button widgets
│       └── forms.f90     # Form widgets
├── core/
│   ├── data_types.f90    # Core data structures
│   └── utilities.f90     # Utility functions
└── models/
    ├── user_model.f90    # User data model
    └── project_model.f90 # Project data model
```

### Import Organization

Keep imports organized and minimal:

```fortran
! Good: Organized imports
use iso_fortran_env, only: int32, real64, error_unit
use forge, only: forge_application, forge_window_t, forge_button
use my_app_models, only: user_data, project_data
use my_app_ui, only: main_window, dialog_manager

! Bad: Unorganized imports
use forge
use my_app_models
use iso_fortran_env
use my_app_ui
```

## Object-Oriented Design

### Class Design Principles

Follow SOLID principles adapted for Fortran:

#### Single Responsibility

Each class should have one clear purpose:

```fortran
! Good: Single responsibility
type :: user_validator
contains
    procedure :: validate_email
    procedure :: validate_password
end type user_validator

type :: user_repository
contains
    procedure :: save_user
    procedure :: find_user
end type user_repository

! Bad: Multiple responsibilities
type :: user_manager
contains
    procedure :: validate_email      ! Validation
    procedure :: save_user          ! Persistence
    procedure :: send_notification  ! Communication
end type user_manager
```

#### Open/Closed Principle

Classes should be open for extension, closed for modification:

```fortran
! Good: Extensible design
type, abstract :: shape
contains
    procedure(shape_area_interface), deferred :: area
end type shape

type, extends(shape) :: circle
contains
    procedure :: area => circle_area
end type circle

type, extends(shape) :: rectangle
contains
    procedure :: area => rectangle_area
end type rectangle
```

### Inheritance vs Composition

Prefer composition over inheritance:

```fortran
! Good: Composition
type :: button_with_icon
    type(forge_button) :: button
    type(icon_manager) :: icon
contains
    procedure :: set_icon
    procedure :: get_combined_size
end type button_with_icon

! Avoid: Deep inheritance hierarchies
type :: icon_button
    ! Inherits from button, which inherits from widget...
end type icon_button
```

## Error Handling

### Consistent Error Patterns

Use consistent error handling throughout the application:

```fortran
! Define error handling pattern
subroutine handle_operation_result(status)
    type(forge_status), intent(in) :: status

    if (status%is_error()) then
        call log_error(status%get_message())
        call show_error_dialog(status%get_message())
        return
    end if
end subroutine handle_operation_result

! Use consistently
call database%save_record(record, status)
call handle_operation_result(status)

call network%send_request(request, status)
call handle_operation_result(status)
```

### Resource Cleanup

Ensure proper cleanup in error paths:

```fortran
subroutine process_file(filename)
    character(len=*), intent(in) :: filename
    integer :: file_unit
    integer :: ios
    logical :: file_opened = .false.

    ! Open file
    open(newunit=file_unit, file=filename, status='old', iostat=ios)
    if (ios /= 0) then
        call log_error("Cannot open file: " // filename)
        return
    end if
    file_opened = .true.

    ! Process file
    call process_file_content(file_unit)

    ! Cleanup (always executed)
    if (file_opened) close(file_unit)

contains

    subroutine process_file_content(unit)
        integer, intent(in) :: unit
        ! File processing logic
        ! May throw errors, but cleanup still happens
    end subroutine process_file_content

end subroutine process_file
```

## Memory Management

### RAII Pattern

Implement Resource Acquisition Is Initialization:

```fortran
type :: file_handler
    integer :: unit = -1
    logical :: is_open = .false.
contains
    procedure :: open_file
    procedure :: close_file
    procedure :: write_data
    final :: cleanup  ! Automatic cleanup
end type file_handler

subroutine open_file(this, filename)
    class(file_handler), intent(inout) :: this
    character(len=*), intent(in) :: filename

    open(newunit=this%unit, file=filename, status='replace')
    this%is_open = .true.
end subroutine open_file

subroutine cleanup(this)
    type(file_handler), intent(inout) :: this

    if (this%is_open) then
        close(this%unit)
        this%is_open = .false.
    end if
end subroutine cleanup
```

### Avoid Memory Leaks

Be careful with allocations:

```fortran
! Good: Proper deallocation
subroutine process_data()
    real, allocatable :: data(:)

    allocate(data(1000))
    call fill_data(data)
    call process_data(data)
    deallocate(data)  ! Explicit cleanup
end subroutine process_data

! Bad: Memory leak
subroutine leaky_process()
    real, allocatable :: data(:)

    allocate(data(1000))
    call fill_data(data)
    ! Forgot to deallocate!
end subroutine leaky_process
```

## Signal/Slot Usage

### Naming Conventions

Use clear, consistent signal/slot names:

```fortran
! Good: Clear naming
type(signal_string) :: text_changed
type(signal_void) :: button_clicked
type(signal_int) :: value_selected

type(slot_string_proc) :: on_text_changed
type(slot_void_proc) :: on_button_clicked
type(slot_int_proc) :: on_value_selected

! Bad: Unclear naming
type(signal_string) :: sig1
type(slot_string_proc) :: slot1
```

### Connection Management

Manage signal/slot connections properly:

```fortran
type :: widget_manager
    type(signal_void) :: data_updated
    type(connection_manager) :: connections
contains
    procedure :: connect_signals
    procedure :: disconnect_signals
end type widget_manager

subroutine connect_signals(this)
    class(widget_manager), intent(inout) :: this

    ! Store connection handles for later disconnection
    call this%connections%add( &
        this%data_updated%connect(on_data_updated) &
    )
end subroutine connect_signals

subroutine disconnect_signals(this)
    class(widget_manager), intent(inout) :: this

    call this%connections%disconnect_all()
end subroutine disconnect_signals
```

## Performance Optimization

### Profile-Guided Optimization

Identify bottlenecks before optimizing:

```fortran
subroutine performance_critical_function()
    use iso_fortran_env, only: real64

    real(real64) :: start_time, end_time

    call cpu_time(start_time)
    ! Performance-critical code here
    call cpu_time(end_time)

    if (end_time - start_time > 0.1d0) then  ! More than 100ms
        call log_performance_warning("Slow operation detected")
    end if
end subroutine performance_critical_function
```

### Efficient Data Structures

Choose appropriate data structures:

```fortran
! Good: Efficient for frequent access
type :: user_cache
    type(hash_table) :: users_by_id    ! O(1) lookup
    type(linked_list) :: recent_users  ! O(1) append
end type user_cache

! Avoid: Inefficient for large datasets
type :: user_list
    type(user), allocatable :: users(:)  ! Linear search
end type user_list
```

### Lazy Loading

Load resources on demand:

```fortran
type :: resource_manager
    type(image), pointer :: large_image => null()
contains
    procedure :: get_image
end type resource_manager

function get_image(this) result(img)
    class(resource_manager), intent(inout) :: this
    type(image), pointer :: img

    if (.not. associated(this%large_image)) then
        ! Load image only when first requested
        allocate(this%large_image)
        call load_image_from_file("large_image.png", this%large_image)
    end if

    img => this%large_image
end function get_image
```

## Testing

### Unit Test Structure

Organize tests clearly:

```fortran
module test_user_validator
    use fruit  ! Testing framework
    use user_validator

    implicit none

contains

    subroutine test_valid_email()
        type(user_validator) :: validator
        logical :: result

        result = validator%validate_email("user@example.com")
        call assert_true(result, "Valid email should pass")
    end subroutine test_valid_email

    subroutine test_invalid_email()
        type(user_validator) :: validator
        logical :: result

        result = validator%validate_email("invalid-email")
        call assert_false(result, "Invalid email should fail")
    end subroutine test_invalid_email

end module test_user_validator
```

### Test Coverage

Aim for comprehensive coverage:

```fortran
! Test normal operation
call test_normal_case()

! Test edge cases
call test_empty_input()
call test_null_values()
call test_large_data()

! Test error conditions
call test_file_not_found()
call test_network_error()
call test_insufficient_memory()
```

## Documentation

### Code Documentation

Document all public interfaces:

```fortran
!> @brief Validates user email addresses
!!
!! This function checks if the provided email address
!! conforms to standard email format requirements.
!!
!! @param[in] email The email address to validate
!! @return True if email is valid, false otherwise
!!
!! @note This function performs basic format checking only.
!!       It does not verify if the email actually exists.
function validate_email(email) result(is_valid)
    character(len=*), intent(in) :: email
    logical :: is_valid

    ! Implementation...
end function validate_email
```

### API Documentation

Maintain up-to-date API documentation:

```fortran
! API documentation example
module my_library
    !> @defgroup user_management User Management
    !! Functions for managing user accounts and authentication

    !> @ingroup user_management
    !! @brief Creates a new user account
    subroutine create_user(username, password, status)
        ! Implementation...
    end subroutine create_user
end module my_library
```

## Build and Deployment

### Build Configuration

Use appropriate compiler flags:

```cmake
# Debug build
target_compile_options(my_app PRIVATE
    $<$<CONFIG:Debug>:-g -O0 -Wall -Wextra -fcheck=all>
)

# Release build
target_compile_options(my_app PRIVATE
    $<$<CONFIG:Release>:-O3 -march=native -flto>
)
```

### Version Management

Handle versioning properly:

```fortran
module version_info
    character(len=*), parameter :: VERSION = "1.2.3"
    character(len=*), parameter :: GIT_COMMIT = "@GIT_COMMIT@"
    character(len=*), parameter :: BUILD_DATE = "@BUILD_DATE@"
end module version_info
```

### Cross-Platform Compatibility

Write platform-independent code:

```fortran
! Good: Platform-independent paths
character(len=:), allocatable :: config_path

config_path = get_config_directory() // "/app.config"

! Bad: Platform-specific paths
character(len=256) :: config_path = "/home/user/.config/app.config"  ! Linux only
```

## Security

### Input Validation

Always validate user input:

```fortran
subroutine process_user_input(input)
    character(len=*), intent(in) :: input

    ! Validate input length
    if (len_trim(input) == 0) then
        call show_error("Input cannot be empty")
        return
    end if

    if (len_trim(input) > 1000) then
        call show_error("Input too long")
        return
    end if

    ! Validate input content
    if (.not. is_valid_input(input)) then
        call show_error("Invalid input format")
        return
    end if

    ! Process validated input
    call process_valid_input(input)
end subroutine process_user_input
```

### Secure Data Handling

Handle sensitive data carefully:

```fortran
subroutine handle_password(password)
    character(len=*), intent(in) :: password

    ! Don't log passwords
    ! call log_info("Password: " // password)  ! BAD

    ! Use secure password handling
    call hash_password(password, hashed_password)
    call store_hashed_password(hashed_password)

    ! Clear sensitive data from memory
    call secure_wipe(password)
end subroutine handle_password
```

## Maintenance

### Code Reviews

Establish code review practices:

```fortran
! Code review checklist
! [ ] Code compiles without warnings
! [ ] Unit tests pass
! [ ] Documentation updated
! [ ] No memory leaks
! [ ] Error handling implemented
! [ ] Security considerations addressed
! [ ] Performance acceptable
! [ ] Code follows style guidelines
```

### Refactoring

Regularly refactor code:

```fortran
! Before: Long function
subroutine process_data_and_save()
    ! 100+ lines of mixed concerns
    ! Data processing, validation, saving, UI updates
end subroutine process_data_and_save

! After: Separated concerns
subroutine process_data_and_save()
    type(processed_data) :: data

    data = validate_and_process_input()
    call save_to_database(data)
    call update_ui_status("Data saved")
end subroutine process_data_and_save
```

### Dependency Management

Keep dependencies up to date and minimal:

```toml
# fpm.toml - Minimal dependencies
[dependencies]
ForGE = "1.0.0"          # Core dependency
json-fortran = "8.3.0"   # JSON handling
fruit = "3.4.3"          # Testing framework
```

## Debugging

### Logging Strategy

Implement comprehensive logging:

```fortran
module logging
    integer, parameter :: LOG_ERROR = 1
    integer, parameter :: LOG_WARNING = 2
    integer, parameter :: LOG_INFO = 3
    integer, parameter :: LOG_DEBUG = 4
contains
    subroutine log_message(level, message, source)
        integer, intent(in) :: level
        character(len=*), intent(in) :: message, source

        if (level <= current_log_level) then
            write(log_unit, '(A, A, " [", A, "] ", A)') &
                get_timestamp(), get_level_name(level), source, message
        end if
    end subroutine log_message
end module logging
```

### Debug Builds

Use debug-specific code:

```fortran
subroutine debug_assertion(condition, message)
    logical, intent(in) :: condition
    character(len=*), intent(in) :: message

#ifdef DEBUG
    if (.not. condition) then
        call log_error("Assertion failed: " // message)
        call abort_program()
    end if
#endif
end subroutine debug_assertion
```

## Performance Monitoring

### Application Metrics

Track application performance:

```fortran
module performance_monitor
    real :: startup_time
    integer :: ui_response_count
    real :: average_response_time
contains
    subroutine record_ui_response(start_time)
        real, intent(in) :: start_time
        real :: response_time

        response_time = get_current_time() - start_time
        ui_response_count = ui_response_count + 1
        average_response_time = &
            (average_response_time * (ui_response_count - 1) + response_time) / ui_response_count

        if (response_time > 0.1) then  ! 100ms threshold
            call log_performance_warning("Slow UI response: " // real_to_string(response_time))
        end if
    end subroutine record_ui_response
end module performance_monitor
```

## Conclusion

Following these best practices will result in:

- **Maintainable code**: Clear structure and documentation
- **Reliable applications**: Proper error handling and testing
- **Performant software**: Optimized algorithms and data structures
- **Secure systems**: Input validation and secure coding practices
- **Scalable architecture**: Modular design and separation of concerns

Regularly review and update these practices as the project evolves and new requirements emerge.