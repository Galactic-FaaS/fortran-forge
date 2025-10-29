!> @brief App lifecycle management for mobile platforms
!> @details Background/foreground transitions and state management
!> @author ForGE Contributors
!> @date 2025
!> @license GPL-3.0-or-later

module forge_app_lifecycle
    use iso_c_binding
    use forge_types
    use forge_platform
    implicit none
    private

    public :: forge_app_lifecycle_manager
    public :: app_lifecycle_create, app_lifecycle_register_callback
    public :: app_lifecycle_handle_event, app_lifecycle_get_current_state

    !> @brief Lifecycle event types
    integer, parameter, public :: LIFECYCLE_EVENT_START = 1
    integer, parameter, public :: LIFECYCLE_EVENT_RESUME = 2
    integer, parameter, public :: LIFECYCLE_EVENT_PAUSE = 3
    integer, parameter, public :: LIFECYCLE_EVENT_STOP = 4
    integer, parameter, public :: LIFECYCLE_EVENT_DESTROY = 5
    integer, parameter, public :: LIFECYCLE_EVENT_BACKGROUND = 6
    integer, parameter, public :: LIFECYCLE_EVENT_FOREGROUND = 7
    integer, parameter, public :: LIFECYCLE_EVENT_LOW_MEMORY = 8
    integer, parameter, public :: LIFECYCLE_EVENT_CONFIG_CHANGED = 9

    !> @brief Lifecycle callback interface
    abstract interface
        subroutine lifecycle_callback_interface(event_type, user_data)
            import :: c_int, c_ptr
            integer(c_int), intent(in) :: event_type
            type(c_ptr), intent(in) :: user_data
        end subroutine lifecycle_callback_interface
    end interface

    !> @brief Lifecycle callback entry
    type :: lifecycle_callback_entry
        procedure(lifecycle_callback_interface), pointer, nopass :: callback => null()
        type(c_ptr) :: user_data = c_null_ptr
        integer :: event_mask = 0  ! Bitmask of events this callback handles
    end type lifecycle_callback_entry

    !> @brief App lifecycle manager
    type :: forge_app_lifecycle_manager
        private
        type(platform_lifecycle_state) :: current_state
        type(lifecycle_callback_entry), allocatable :: callbacks(:)
        integer :: callback_count = 0
        logical :: initialized = .false.
    contains
        procedure :: register_callback => lifecycle_register_callback
        procedure :: unregister_callback => lifecycle_unregister_callback
        procedure :: handle_event => lifecycle_handle_event
        procedure :: get_current_state => lifecycle_get_current_state
        procedure :: transition_to_state => lifecycle_transition_to_state
    end type forge_app_lifecycle_manager

contains

    !> @brief Create app lifecycle manager
    function app_lifecycle_create() result(manager)
        type(forge_app_lifecycle_manager) :: manager

        manager%current_state%current_state = LIFECYCLE_CREATED
        manager%current_state%is_background = .false.
        manager%current_state%is_foreground = .true.
        manager%callback_count = 0
        manager%initialized = .true.

        allocate(manager%callbacks(5))  ! Initial capacity
    end function app_lifecycle_create

    !> @brief Register lifecycle callback
    subroutine app_lifecycle_register_callback(manager, callback, event_mask, user_data)
        type(forge_app_lifecycle_manager), intent(inout) :: manager
        procedure(lifecycle_callback_interface) :: callback
        integer, intent(in) :: event_mask
        type(c_ptr), intent(in), optional :: user_data

        if (manager%callback_count >= size(manager%callbacks)) then
            call resize_callback_array(manager)
        end if

        manager%callback_count = manager%callback_count + 1
        manager%callbacks(manager%callback_count)%callback => callback
        manager%callbacks(manager%callback_count)%event_mask = event_mask

        if (present(user_data)) then
            manager%callbacks(manager%callback_count)%user_data = user_data
        end if
    end subroutine app_lifecycle_register_callback

    !> @brief Handle lifecycle event
    subroutine app_lifecycle_handle_event(manager, event_type)
        type(forge_app_lifecycle_manager), intent(inout) :: manager
        integer, intent(in) :: event_type
        integer :: i

        ! Update internal state based on event
        select case (event_type)
        case (LIFECYCLE_EVENT_START)
            manager%current_state%current_state = LIFECYCLE_STARTED
        case (LIFECYCLE_EVENT_RESUME)
            manager%current_state%current_state = LIFECYCLE_RESUMED
            manager%current_state%is_background = .false.
            manager%current_state%is_foreground = .true.
        case (LIFECYCLE_EVENT_PAUSE)
            manager%current_state%current_state = LIFECYCLE_PAUSED
            manager%current_state%is_background = .true.
            manager%current_state%is_foreground = .false.
        case (LIFECYCLE_EVENT_STOP)
            manager%current_state%current_state = LIFECYCLE_STOPPED
        case (LIFECYCLE_EVENT_DESTROY)
            manager%current_state%current_state = LIFECYCLE_DESTROYED
        end select

        ! Notify registered callbacks
        do i = 1, manager%callback_count
            if (iand(manager%callbacks(i)%event_mask, event_type) /= 0) then
                if (associated(manager%callbacks(i)%callback)) then
                    call manager%callbacks(i)%callback(event_type, manager%callbacks(i)%user_data)
                end if
            end if
        end do
    end subroutine app_lifecycle_handle_event

    !> @brief Get current lifecycle state
    function app_lifecycle_get_current_state(manager) result(state)
        type(forge_app_lifecycle_manager), intent(in) :: manager
        type(platform_lifecycle_state) :: state

        state = manager%current_state
    end function app_lifecycle_get_current_state

    !> @brief Register callback (method version)
    subroutine lifecycle_register_callback(this, callback, event_mask, user_data)
        class(forge_app_lifecycle_manager), intent(inout) :: this
        procedure(lifecycle_callback_interface) :: callback
        integer, intent(in) :: event_mask
        type(c_ptr), intent(in), optional :: user_data

        call app_lifecycle_register_callback(this, callback, event_mask, user_data)
    end subroutine lifecycle_register_callback

    !> @brief Unregister callback
    subroutine lifecycle_unregister_callback(this, callback)
        class(forge_app_lifecycle_manager), intent(inout) :: this
        procedure(lifecycle_callback_interface) :: callback
        integer :: i, found_index

        found_index = -1
        do i = 1, this%callback_count
            if (associated(this%callbacks(i)%callback, callback)) then
                found_index = i
                exit
            end if
        end do

        if (found_index > 0) then
            ! Remove callback by shifting array
            do i = found_index, this%callback_count - 1
                this%callbacks(i) = this%callbacks(i + 1)
            end do
            this%callback_count = this%callback_count - 1
        end if
    end subroutine lifecycle_unregister_callback

    !> @brief Handle event (method version)
    subroutine lifecycle_handle_event(this, event_type)
        class(forge_app_lifecycle_manager), intent(inout) :: this
        integer, intent(in) :: event_type

        call app_lifecycle_handle_event(this, event_type)
    end subroutine lifecycle_handle_event

    !> @brief Get current state (method version)
    function lifecycle_get_current_state(this) result(state)
        class(forge_app_lifecycle_manager), intent(in) :: this
        type(platform_lifecycle_state) :: state

        state = app_lifecycle_get_current_state(this)
    end function lifecycle_get_current_state

    !> @brief Transition to new state
    subroutine lifecycle_transition_to_state(this, new_state)
        class(forge_app_lifecycle_manager), intent(inout) :: this
        integer, intent(in) :: new_state

        ! Validate state transition
        if (is_valid_state_transition(this%current_state%current_state, new_state)) then
            this%current_state%current_state = new_state

            ! Update background/foreground flags
            select case (new_state)
            case (LIFECYCLE_RESUMED)
                this%current_state%is_background = .false.
                this%current_state%is_foreground = .true.
            case (LIFECYCLE_PAUSED)
                this%current_state%is_background = .true.
                this%current_state%is_foreground = .false.
            end select

            ! Trigger appropriate event
            select case (new_state)
            case (LIFECYCLE_STARTED)
                call this%handle_event(LIFECYCLE_EVENT_START)
            case (LIFECYCLE_RESUMED)
                call this%handle_event(LIFECYCLE_EVENT_RESUME)
            case (LIFECYCLE_PAUSED)
                call this%handle_event(LIFECYCLE_EVENT_PAUSE)
            case (LIFECYCLE_STOPPED)
                call this%handle_event(LIFECYCLE_EVENT_STOP)
            case (LIFECYCLE_DESTROYED)
                call this%handle_event(LIFECYCLE_EVENT_DESTROY)
            end select
        end if
    end subroutine lifecycle_transition_to_state

    !> @brief Check if state transition is valid
    function is_valid_state_transition(from_state, to_state) result(valid)
        integer, intent(in) :: from_state, to_state
        logical :: valid

        valid = .true.

        ! Basic validation - in a real implementation this would be more sophisticated
        select case (from_state)
        case (LIFECYCLE_CREATED)
            valid = (to_state == LIFECYCLE_STARTED)
        case (LIFECYCLE_STARTED)
            valid = (to_state == LIFECYCLE_RESUMED .or. to_state == LIFECYCLE_STOPPED)
        case (LIFECYCLE_RESUMED)
            valid = (to_state == LIFECYCLE_PAUSED)
        case (LIFECYCLE_PAUSED)
            valid = (to_state == LIFECYCLE_RESUMED .or. to_state == LIFECYCLE_STOPPED)
        case (LIFECYCLE_STOPPED)
            valid = (to_state == LIFECYCLE_DESTROYED)
        case (LIFECYCLE_DESTROYED)
            valid = .false.  ! Can't transition from destroyed
        end select
    end function is_valid_state_transition

    !> @brief Resize callback array
    subroutine resize_callback_array(this)
        class(forge_app_lifecycle_manager), intent(inout) :: this
        type(lifecycle_callback_entry), allocatable :: new_callbacks(:)
        integer :: new_size

        new_size = size(this%callbacks) * 2
        allocate(new_callbacks(new_size))
        new_callbacks(1:size(this%callbacks)) = this%callbacks

        deallocate(this%callbacks)
        this%callbacks = new_callbacks
    end subroutine resize_callback_array

end module forge_app_lifecycle