!> @brief C bindings for accessibility APIs
!> @details Provides Fortran interfaces to platform accessibility C APIs
!> @author ForGE Contributors
!> @date 2025
!> @license GPL-3.0-or-later

module forge_accessibility_bindings
    use iso_c_binding
    implicit none
    private

    public :: qaccessible_create_interface
    public :: qaccessible_destroy_interface
    public :: qaccessible_update_properties
    public :: qaccessible_notify_event
    public :: qaccessible_get_child_count
    public :: qaccessible_get_child
    public :: qaccessible_navigate
    public :: qaccessible_do_action
    public :: qaccessible_get_text
    public :: qaccessible_set_text
    public :: qaccessible_get_value
    public :: qaccessible_set_value
    public :: qaccessible_get_state
    public :: qaccessible_set_state

    !> Qt accessibility C API bindings
    interface
        !> Create accessible interface
        function qaccessible_create_interface(qobject, role) bind(c, name="qaccessible_create_interface")
            import :: c_ptr, c_int
            type(c_ptr), value :: qobject
            integer(c_int), value :: role
            type(c_ptr) :: qaccessible_create_interface
        end function qaccessible_create_interface

        !> Destroy accessible interface
        subroutine qaccessible_destroy_interface(interface_ptr) bind(c, name="qaccessible_destroy_interface")
            import :: c_ptr
            type(c_ptr), value :: interface_ptr
        end subroutine qaccessible_destroy_interface

        !> Update accessible properties
        subroutine qaccessible_update_properties(interface_ptr, name, description, state, value) bind(c, name="qaccessible_update_properties")
            import :: c_ptr, c_char, c_int64_t, c_double
            type(c_ptr), value :: interface_ptr
            character(kind=c_char), dimension(*) :: name
            character(kind=c_char), dimension(*) :: description
            integer(c_int64_t), value :: state
            real(c_double), value :: value
        end subroutine qaccessible_update_properties

        !> Notify accessible event
        subroutine qaccessible_notify_event(interface_ptr, event_type) bind(c, name="qaccessible_notify_event")
            import :: c_ptr, c_int
            type(c_ptr), value :: interface_ptr
            integer(c_int), value :: event_type
        end subroutine qaccessible_notify_event

        !> Get child count
        function qaccessible_get_child_count(interface_ptr) bind(c, name="qaccessible_get_child_count")
            import :: c_ptr, c_int
            type(c_ptr), value :: interface_ptr
            integer(c_int) :: qaccessible_get_child_count
        end function qaccessible_get_child_count

        !> Get child at index
        function qaccessible_get_child(interface_ptr, index) bind(c, name="qaccessible_get_child")
            import :: c_ptr, c_int
            type(c_ptr), value :: interface_ptr
            integer(c_int), value :: index
            type(c_ptr) :: qaccessible_get_child
        end function qaccessible_get_child

        !> Navigate to related accessible object
        function qaccessible_navigate(interface_ptr, relation) bind(c, name="qaccessible_navigate")
            import :: c_ptr, c_int
            type(c_ptr), value :: interface_ptr
            integer(c_int), value :: relation
            type(c_ptr) :: qaccessible_navigate
        end function qaccessible_navigate

        !> Perform accessible action
        subroutine qaccessible_do_action(interface_ptr, action_name) bind(c, name="qaccessible_do_action")
            import :: c_ptr, c_char
            type(c_ptr), value :: interface_ptr
            character(kind=c_char), dimension(*) :: action_name
        end subroutine qaccessible_do_action

        !> Get accessible text
        subroutine qaccessible_get_text(interface_ptr, text_type, text, text_len) bind(c, name="qaccessible_get_text")
            import :: c_ptr, c_int, c_char
            type(c_ptr), value :: interface_ptr
            integer(c_int), value :: text_type
            character(kind=c_char), dimension(*) :: text
            integer(c_int) :: text_len
        end subroutine qaccessible_get_text

        !> Set accessible text
        subroutine qaccessible_set_text(interface_ptr, text_type, text) bind(c, name="qaccessible_set_text")
            import :: c_ptr, c_int, c_char
            type(c_ptr), value :: interface_ptr
            integer(c_int), value :: text_type
            character(kind=c_char), dimension(*) :: text
        end subroutine qaccessible_set_text

        !> Get accessible value
        function qaccessible_get_value(interface_ptr) bind(c, name="qaccessible_get_value")
            import :: c_ptr, c_double
            type(c_ptr), value :: interface_ptr
            real(c_double) :: qaccessible_get_value
        end function qaccessible_get_value

        !> Set accessible value
        subroutine qaccessible_set_value(interface_ptr, value) bind(c, name="qaccessible_set_value")
            import :: c_ptr, c_double
            type(c_ptr), value :: interface_ptr
            real(c_double), value :: value
        end subroutine qaccessible_set_value

        !> Get accessible state
        function qaccessible_get_state(interface_ptr) bind(c, name="qaccessible_get_state")
            import :: c_ptr, c_int64_t
            type(c_ptr), value :: interface_ptr
            integer(c_int64_t) :: qaccessible_get_state
        end function qaccessible_get_state

        !> Set accessible state
        subroutine qaccessible_set_state(interface_ptr, state) bind(c, name="qaccessible_set_state")
            import :: c_ptr, c_int64_t
            type(c_ptr), value :: interface_ptr
            integer(c_int64_t), value :: state
        end subroutine qaccessible_set_state
    end interface

end module forge_accessibility_bindings