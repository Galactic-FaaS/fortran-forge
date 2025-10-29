!> @brief Platform-specific accessibility implementations
!> @details Implements Windows UIA, Linux AT-SPI, and macOS NSAccessibility APIs
!> @author ForGE Contributors
!> @date 2025
!> @license GPL-3.0-or-later

module forge_accessibility_platform
    use iso_c_binding
    use forge_types
    use forge_errors
    use forge_accessibility
    use iso_fortran_env, only: output_unit
    implicit none
    private

    public :: QAccessible_Platform
    public :: QAccessible_Windows_UIA
    public :: QAccessible_Linux_AT_SPI
    public :: QAccessible_Mac_NSAccessibility

    !> @brief Platform accessibility base class
    type :: QAccessible_Platform
        private
        logical :: initialized = .false.
        integer :: platform_type = 0  ! 1=Windows, 2=Linux, 3=macOS
    contains
        procedure :: initialize_platform => platform_initialize
        procedure :: shutdown_platform => platform_shutdown
        procedure :: notify_accessible_event => platform_notify_event
        procedure :: get_platform_type => platform_get_type
        procedure :: is_supported => platform_is_supported
    end type QAccessible_Platform

    !> @brief Windows UIA (UI Automation) implementation
    type, extends(QAccessible_Platform) :: QAccessible_Windows_UIA
        private
        type(c_ptr) :: uia_client = c_null_ptr
        type(c_ptr) :: automation_element = c_null_ptr
        logical :: uia_available = .false.
    contains
        procedure :: initialize_platform => uia_initialize
        procedure :: shutdown_platform => uia_shutdown
        procedure :: notify_accessible_event => uia_notify_event
        procedure :: create_uia_element => uia_create_element
        procedure :: update_uia_properties => uia_update_properties
        procedure :: raise_uia_event => uia_raise_event
        procedure :: get_uia_pattern => uia_get_pattern
    end type QAccessible_Windows_UIA

    !> @brief Linux AT-SPI implementation
    type, extends(QAccessible_Platform) :: QAccessible_Linux_AT_SPI
        private
        type(c_ptr) :: atspi_registry = c_null_ptr
        type(c_ptr) :: accessible_object = c_null_ptr
        logical :: atspi_available = .false.
    contains
        procedure :: initialize_platform => atspi_initialize
        procedure :: shutdown_platform => atspi_shutdown
        procedure :: notify_accessible_event => atspi_notify_event
        procedure :: register_atspi_object => atspi_register_object
        procedure :: emit_atspi_signal => atspi_emit_signal
        procedure :: get_atspi_interfaces => atspi_get_interfaces
    end type QAccessible_Linux_AT_SPI

    !> @brief macOS NSAccessibility implementation
    type, extends(QAccessible_Platform) :: QAccessible_Mac_NSAccessibility
        private
        type(c_ptr) :: nsaccessibility_object = c_null_ptr
        logical :: nsaccessibility_available = .false.
    contains
        procedure :: initialize_platform => nsaccessibility_initialize
        procedure :: shutdown_platform => nsaccessibility_shutdown
        procedure :: notify_accessible_event => nsaccessibility_notify_event
        procedure :: create_nsaccessibility_element => nsaccessibility_create_element
        procedure :: update_nsaccessibility_attributes => nsaccessibility_update_attributes
        procedure :: post_nsaccessibility_notification => nsaccessibility_post_notification
    end type QAccessible_Mac_NSAccessibility

contains

    ! ========== QAccessible_Platform Implementation ==========

    subroutine platform_initialize(this)
        class(QAccessible_Platform), intent(inout) :: this

        if (this%initialized) return

        ! Detect platform
#ifdef _WIN32
        this%platform_type = 1  ! Windows
#elif defined(__linux__)
        this%platform_type = 2  ! Linux
#elif defined(__APPLE__)
        this%platform_type = 3  ! macOS
#else
        this%platform_type = 0  ! Unknown
#endif

        this%initialized = .true.
        write(output_unit, '(A,I0)') "[ACCESSIBILITY] Platform initialized, type: ", this%platform_type
    end subroutine platform_initialize

    subroutine platform_shutdown(this)
        class(QAccessible_Platform), intent(inout) :: this
        if (.not. this%initialized) return
        this%initialized = .false.
        write(output_unit, '(A)') "[ACCESSIBILITY] Platform shutdown"
    end subroutine platform_shutdown

    subroutine platform_notify_event(this, event)
        class(QAccessible_Platform), intent(inout) :: this
        type(QAccessibleEvent), intent(in) :: event
        ! Base implementation - override in subclasses
    end subroutine platform_notify_event

    function platform_get_type(this) result(platform_type)
        class(QAccessible_Platform), intent(in) :: this
        integer :: platform_type
        platform_type = this%platform_type
    end function platform_get_type

    function platform_is_supported(this) result(supported)
        class(QAccessible_Platform), intent(in) :: this
        logical :: supported
        supported = this%platform_type > 0
    end function platform_is_supported

    ! ========== QAccessible_Windows_UIA Implementation ==========

    subroutine uia_initialize(this)
        class(QAccessible_Windows_UIA), intent(inout) :: this

        call this%QAccessible_Platform%initialize_platform()

        if (this%platform_type /= 1) return  ! Not Windows

        ! Initialize UIA client
        this%uia_client = uia_initialize_client()
        if (c_associated(this%uia_client)) then
            this%uia_available = .true.
            write(output_unit, '(A)') "[ACCESSIBILITY] Windows UIA initialized"
        else
            write(output_unit, '(A)') "[ACCESSIBILITY] Windows UIA initialization failed"
        end if
    end subroutine uia_initialize

    subroutine uia_shutdown(this)
        class(QAccessible_Windows_UIA), intent(inout) :: this

        if (c_associated(this%uia_client)) then
            call uia_shutdown_client(this%uia_client)
            this%uia_client = c_null_ptr
        end if

        if (c_associated(this%automation_element)) then
            call uia_release_element(this%automation_element)
            this%automation_element = c_null_ptr
        end if

        this%uia_available = .false.
        call this%QAccessible_Platform%shutdown_platform()
        write(output_unit, '(A)') "[ACCESSIBILITY] Windows UIA shutdown"
    end subroutine uia_shutdown

    subroutine uia_notify_event(this, event)
        class(QAccessible_Windows_UIA), intent(inout) :: this
        type(QAccessibleEvent), intent(in) :: event
        class(QAccessibleInterface), pointer :: accessible_obj
        integer :: event_type

        if (.not. this%uia_available) return

        accessible_obj => event%get_accessible_object()
        if (.not. associated(accessible_obj)) return

        event_type = event%get_accessible_event_type()

        select case (event_type)
        case (QAccessible_Event_NameChanged)
            call this%update_uia_properties(accessible_obj, UIA_NamePropertyId)
        case (QAccessible_Event_StateChanged)
            call this%update_uia_properties(accessible_obj, UIA_IsEnabledPropertyId)
        case (QAccessible_Event_ValueChanged)
            call this%update_uia_properties(accessible_obj, UIA_ValueValuePropertyId)
        case (QAccessible_Event_Focus)
            call this%raise_uia_event(UIA_AutomationFocusChangedEventId)
        end select
    end subroutine uia_notify_event

    subroutine uia_create_element(this, accessible_obj)
        class(QAccessible_Windows_UIA), intent(inout) :: this
        class(QAccessibleInterface), intent(in) :: accessible_obj

        if (.not. this%uia_available) return

        ! Create UIA automation element
        this%automation_element = uia_create_automation_element( &
            this%uia_client, &
            accessible_obj%get_name(), &
            accessible_obj%get_role(), &
            accessible_obj%get_state()%get_flags() &
        )
    end subroutine uia_create_element

    subroutine uia_update_properties(this, accessible_obj, property_id)
        class(QAccessible_Windows_UIA), intent(inout) :: this
        class(QAccessibleInterface), intent(in) :: accessible_obj
        integer, intent(in) :: property_id

        if (.not. this%uia_available .or. .not. c_associated(this%automation_element)) return

        select case (property_id)
        case (UIA_NamePropertyId)
            call uia_set_property_string(this%automation_element, property_id, accessible_obj%get_name())
        case (UIA_IsEnabledPropertyId)
            call uia_set_property_bool(this%automation_element, property_id, &
                .not. accessible_obj%get_state()%has_flag(QAccessible_Unavailable))
        case (UIA_ValueValuePropertyId)
            ! For value interface objects
            if (accessible_obj%value()%get_current_value() >= 0.0) then
                call uia_set_property_string(this%automation_element, property_id, &
                    forge_string_from_real(accessible_obj%value()%get_current_value()))
            end if
        end select
    end subroutine uia_update_properties

    subroutine uia_raise_event(this, event_id)
        class(QAccessible_Windows_UIA), intent(inout) :: this
        integer, intent(in) :: event_id

        if (.not. this%uia_available .or. .not. c_associated(this%automation_element)) return

        call uia_raise_automation_event(this%automation_element, event_id)
    end subroutine uia_raise_event

    function uia_get_pattern(this, pattern_id) result(pattern)
        class(QAccessible_Windows_UIA), intent(in) :: this
        integer, intent(in) :: pattern_id
        type(c_ptr) :: pattern

        if (.not. this%uia_available .or. .not. c_associated(this%automation_element)) then
            pattern = c_null_ptr
            return
        end if

        pattern = uia_get_pattern_interface(this%automation_element, pattern_id)
    end function uia_get_pattern

    ! ========== QAccessible_Linux_AT_SPI Implementation ==========

    subroutine atspi_initialize(this)
        class(QAccessible_Linux_AT_SPI), intent(inout) :: this

        call this%QAccessible_Platform%initialize_platform()

        if (this%platform_type /= 2) return  ! Not Linux

        ! Initialize AT-SPI registry
        this%atspi_registry = atspi_initialize_registry()
        if (c_associated(this%atspi_registry)) then
            this%atspi_available = .true.
            write(output_unit, '(A)') "[ACCESSIBILITY] Linux AT-SPI initialized"
        else
            write(output_unit, '(A)') "[ACCESSIBILITY] Linux AT-SPI initialization failed"
        end if
    end subroutine atspi_initialize

    subroutine atspi_shutdown(this)
        class(QAccessible_Linux_AT_SPI), intent(inout) :: this

        if (c_associated(this%atspi_registry)) then
            call atspi_shutdown_registry(this%atspi_registry)
            this%atspi_registry = c_null_ptr
        end if

        if (c_associated(this%accessible_object)) then
            call atspi_release_object(this%accessible_object)
            this%accessible_object = c_null_ptr
        end if

        this%atspi_available = .false.
        call this%QAccessible_Platform%shutdown_platform()
        write(output_unit, '(A)') "[ACCESSIBILITY] Linux AT-SPI shutdown"
    end subroutine atspi_shutdown

    subroutine atspi_notify_event(this, event)
        class(QAccessible_Linux_AT_SPI), intent(inout) :: this
        type(QAccessibleEvent), intent(in) :: event
        class(QAccessibleInterface), pointer :: accessible_obj
        integer :: event_type

        if (.not. this%atspi_available) return

        accessible_obj => event%get_accessible_object()
        if (.not. associated(accessible_obj)) return

        event_type = event%get_accessible_event_type()

        select case (event_type)
        case (QAccessible_Event_NameChanged)
            call this%emit_atspi_signal("accessible-name")
        case (QAccessible_Event_StateChanged)
            call this%emit_atspi_signal("accessible-state-changed")
        case (QAccessible_Event_ValueChanged)
            call this%emit_atspi_signal("accessible-value-changed")
        case (QAccessible_Event_Focus)
            call this%emit_atspi_signal("accessible-focus")
        end select
    end subroutine atspi_notify_event

    subroutine atspi_register_object(this, accessible_obj)
        class(QAccessible_Linux_AT_SPI), intent(inout) :: this
        class(QAccessibleInterface), intent(in) :: accessible_obj

        if (.not. this%atspi_available) return

        ! Register accessible object with AT-SPI
        this%accessible_object = atspi_register_accessible_object( &
            this%atspi_registry, &
            accessible_obj%get_name(), &
            accessible_obj%get_role(), &
            accessible_obj%get_description() &
        )
    end subroutine atspi_register_object

    subroutine atspi_emit_signal(this, signal_name)
        class(QAccessible_Linux_AT_SPI), intent(inout) :: this
        character(len=*), intent(in) :: signal_name

        if (.not. this%atspi_available .or. .not. c_associated(this%accessible_object)) return

        call atspi_emit_dbus_signal(this%accessible_object, signal_name)
    end subroutine atspi_emit_signal

    function atspi_get_interfaces(this) result(interfaces)
        class(QAccessible_Linux_AT_SPI), intent(in) :: this
        type(c_ptr), dimension(:), allocatable :: interfaces

        if (.not. this%atspi_available) then
            allocate(interfaces(0))
            return
        end if

        ! Get supported AT-SPI interfaces
        interfaces = atspi_get_supported_interfaces(this%accessible_object)
    end function atspi_get_interfaces

    ! ========== QAccessible_Mac_NSAccessibility Implementation ==========

    subroutine nsaccessibility_initialize(this)
        class(QAccessible_Mac_NSAccessibility), intent(inout) :: this

        call this%QAccessible_Platform%initialize_platform()

        if (this%platform_type /= 3) return  ! Not macOS

        ! Initialize NSAccessibility
        this%nsaccessibility_available = nsaccessibility_initialize()
        if (this%nsaccessibility_available) then
            write(output_unit, '(A)') "[ACCESSIBILITY] macOS NSAccessibility initialized"
        else
            write(output_unit, '(A)') "[ACCESSIBILITY] macOS NSAccessibility initialization failed"
        end if
    end subroutine nsaccessibility_initialize

    subroutine nsaccessibility_shutdown(this)
        class(QAccessible_Mac_NSAccessibility), intent(inout) :: this

        if (c_associated(this%nsaccessibility_object)) then
            call nsaccessibility_release_object(this%nsaccessibility_object)
            this%nsaccessibility_object = c_null_ptr
        end if

        this%nsaccessibility_available = .false.
        call this%QAccessible_Platform%shutdown_platform()
        write(output_unit, '(A)') "[ACCESSIBILITY] macOS NSAccessibility shutdown"
    end subroutine nsaccessibility_shutdown

    subroutine nsaccessibility_notify_event(this, event)
        class(QAccessible_Mac_NSAccessibility), intent(inout) :: this
        type(QAccessibleEvent), intent(in) :: event
        class(QAccessibleInterface), pointer :: accessible_obj
        integer :: event_type

        if (.not. this%nsaccessibility_available) return

        accessible_obj => event%get_accessible_object()
        if (.not. associated(accessible_obj)) return

        event_type = event%get_accessible_event_type()

        select case (event_type)
        case (QAccessible_Event_NameChanged)
            call this%post_nsaccessibility_notification(NSAccessibilityTitleChangedNotification)
        case (QAccessible_Event_StateChanged)
            call this%post_nsaccessibility_notification(NSAccessibilityValueChangedNotification)
        case (QAccessible_Event_ValueChanged)
            call this%post_nsaccessibility_notification(NSAccessibilityValueChangedNotification)
        case (QAccessible_Event_Focus)
            call this%post_nsaccessibility_notification(NSAccessibilityFocusedUIElementChangedNotification)
        end select
    end subroutine nsaccessibility_notify_event

    subroutine nsaccessibility_create_element(this, accessible_obj)
        class(QAccessible_Mac_NSAccessibility), intent(inout) :: this
        class(QAccessibleInterface), intent(in) :: accessible_obj

        if (.not. this%nsaccessibility_available) return

        ! Create NSAccessibility element
        this%nsaccessibility_object = nsaccessibility_create_element( &
            accessible_obj%get_name(), &
            accessible_obj%get_role(), &
            accessible_obj%get_description() &
        )
    end subroutine nsaccessibility_create_element

    subroutine nsaccessibility_update_attributes(this, accessible_obj)
        class(QAccessible_Mac_NSAccessibility), intent(inout) :: this
        class(QAccessibleInterface), intent(in) :: accessible_obj

        if (.not. this%nsaccessibility_available .or. .not. c_associated(this%nsaccessibility_object)) return

        ! Update NSAccessibility attributes
        call nsaccessibility_set_attribute(this%nsaccessibility_object, "AXTitle", accessible_obj%get_name())
        call nsaccessibility_set_attribute(this%nsaccessibility_object, "AXRole", accessible_obj%role_string())
        call nsaccessibility_set_attribute(this%nsaccessibility_object, "AXDescription", accessible_obj%get_description())
        call nsaccessibility_set_attribute(this%nsaccessibility_object, "AXEnabled", &
            merge("YES", "NO", .not. accessible_obj%get_state()%has_flag(QAccessible_Unavailable)))
    end subroutine nsaccessibility_update_attributes

    subroutine nsaccessibility_post_notification(this, notification)
        class(QAccessible_Mac_NSAccessibility), intent(inout) :: this
        integer, intent(in) :: notification

        if (.not. this%nsaccessibility_available .or. .not. c_associated(this%nsaccessibility_object)) return

        call nsaccessibility_post_notification(this%nsaccessibility_object, notification)
    end subroutine nsaccessibility_post_notification

    ! ========== Platform-specific C bindings (stubs) ==========

    ! Windows UIA bindings
    function uia_initialize_client() bind(c, name="uia_initialize_client")
        import :: c_ptr
        type(c_ptr) :: uia_initialize_client
    end function uia_initialize_client

    subroutine uia_shutdown_client(client) bind(c, name="uia_shutdown_client")
        import :: c_ptr
        type(c_ptr), value :: client
    end subroutine uia_shutdown_client

    function uia_create_automation_element(client, name, role, state_flags) bind(c, name="uia_create_automation_element")
        import :: c_ptr, c_int, c_int64_t
        type(c_ptr), value :: client
        character(kind=c_char), dimension(*) :: name
        integer(c_int), value :: role
        integer(c_int64_t), value :: state_flags
        type(c_ptr) :: uia_create_automation_element
    end function uia_create_automation_element

    subroutine uia_release_element(element) bind(c, name="uia_release_element")
        import :: c_ptr
        type(c_ptr), value :: element
    end subroutine uia_release_element

    subroutine uia_set_property_string(element, property_id, value) bind(c, name="uia_set_property_string")
        import :: c_ptr, c_int, c_char
        type(c_ptr), value :: element
        integer(c_int), value :: property_id
        character(kind=c_char), dimension(*) :: value
    end subroutine uia_set_property_string

    subroutine uia_set_property_bool(element, property_id, value) bind(c, name="uia_set_property_bool")
        import :: c_ptr, c_int, c_bool
        type(c_ptr), value :: element
        integer(c_int), value :: property_id
        logical(c_bool), value :: value
    end subroutine uia_set_property_bool

    subroutine uia_raise_automation_event(element, event_id) bind(c, name="uia_raise_automation_event")
        import :: c_ptr, c_int
        type(c_ptr), value :: element
        integer(c_int), value :: event_id
    end subroutine uia_raise_automation_event

    function uia_get_pattern_interface(element, pattern_id) bind(c, name="uia_get_pattern_interface")
        import :: c_ptr, c_int
        type(c_ptr), value :: element
        integer(c_int), value :: pattern_id
        type(c_ptr) :: uia_get_pattern_interface
    end function uia_get_pattern_interface

    ! Linux AT-SPI bindings
    function atspi_initialize_registry() bind(c, name="atspi_initialize_registry")
        import :: c_ptr
        type(c_ptr) :: atspi_initialize_registry
    end function atspi_initialize_registry

    subroutine atspi_shutdown_registry(registry) bind(c, name="atspi_shutdown_registry")
        import :: c_ptr
        type(c_ptr), value :: registry
    end subroutine atspi_shutdown_registry

    function atspi_register_accessible_object(registry, name, role, description) bind(c, name="atspi_register_accessible_object")
        import :: c_ptr, c_int, c_char
        type(c_ptr), value :: registry
        character(kind=c_char), dimension(*) :: name
        integer(c_int), value :: role
        character(kind=c_char), dimension(*) :: description
        type(c_ptr) :: atspi_register_accessible_object
    end function atspi_register_accessible_object

    subroutine atspi_release_object(obj) bind(c, name="atspi_release_object")
        import :: c_ptr
        type(c_ptr), value :: obj
    end subroutine atspi_release_object

    subroutine atspi_emit_dbus_signal(obj, signal_name) bind(c, name="atspi_emit_dbus_signal")
        import :: c_ptr, c_char
        type(c_ptr), value :: obj
        character(kind=c_char), dimension(*) :: signal_name
    end subroutine atspi_emit_dbus_signal

    function atspi_get_supported_interfaces(obj) bind(c, name="atspi_get_supported_interfaces")
        import :: c_ptr
        type(c_ptr), value :: obj
        type(c_ptr), dimension(:), allocatable :: atspi_get_supported_interfaces
    end function atspi_get_supported_interfaces

    ! macOS NSAccessibility bindings
    function nsaccessibility_initialize() bind(c, name="nsaccessibility_initialize")
        import :: c_bool
        logical(c_bool) :: nsaccessibility_initialize
    end function nsaccessibility_initialize

    function nsaccessibility_create_element(name, role, description) bind(c, name="nsaccessibility_create_element")
        import :: c_ptr, c_int, c_char
        character(kind=c_char), dimension(*) :: name
        integer(c_int), value :: role
        character(kind=c_char), dimension(*) :: description
        type(c_ptr) :: nsaccessibility_create_element
    end function nsaccessibility_create_element

    subroutine nsaccessibility_release_object(obj) bind(c, name="nsaccessibility_release_object")
        import :: c_ptr
        type(c_ptr), value :: obj
    end subroutine nsaccessibility_release_object

    subroutine nsaccessibility_set_attribute(obj, attribute, value) bind(c, name="nsaccessibility_set_attribute")
        import :: c_ptr, c_char
        type(c_ptr), value :: obj
        character(kind=c_char), dimension(*) :: attribute
        character(kind=c_char), dimension(*) :: value
    end subroutine nsaccessibility_set_attribute

    subroutine nsaccessibility_post_notification(obj, notification) bind(c, name="nsaccessibility_post_notification")
        import :: c_ptr, c_int
        type(c_ptr), value :: obj
        integer(c_int), value :: notification
    end subroutine nsaccessibility_post_notification

    ! UIA Constants (Windows)
    integer(c_int), parameter :: UIA_NamePropertyId = 30005
    integer(c_int), parameter :: UIA_IsEnabledPropertyId = 30010
    integer(c_int), parameter :: UIA_ValueValuePropertyId = 30045
    integer(c_int), parameter :: UIA_AutomationFocusChangedEventId = 20005

    ! NSAccessibility Constants (macOS)
    integer(c_int), parameter :: NSAccessibilityTitleChangedNotification = 1
    integer(c_int), parameter :: NSAccessibilityValueChangedNotification = 2
    integer(c_int), parameter :: NSAccessibilityFocusedUIElementChangedNotification = 3

    ! Utility functions
    function forge_string_from_real(value) result(str)
        real, intent(in) :: value
        character(len=:), allocatable :: str
        character(len=32) :: temp

        write(temp, '(F0.2)') value
        str = trim(adjustl(temp))
    end function forge_string_from_real

end module forge_accessibility_platform