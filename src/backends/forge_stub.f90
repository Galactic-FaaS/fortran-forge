!> @brief Stub backend for testing/compilation
!> @details A minimal backend implementation for testing the API without GUI functionality
!> @author ForGE Contributors
!> @date 2025
!> @license GPL-3.0-or-later

module forge_stub_backend
    use iso_c_binding
    use forge_backend
    use forge_types
    use forge_errors
    use iso_fortran_env, only: output_unit
    implicit none
    private

    public :: forge_stub_backend_t

    !> @brief Stub backend implementation
    type, extends(forge_backend_base) :: forge_stub_backend_t
        private
        integer :: window_count = 0
        integer :: widget_count = 0
    contains
        procedure :: init => stub_init
        procedure :: shutdown => stub_shutdown
        procedure :: run => stub_run
        procedure :: process_events => stub_process_events
        procedure :: create_window => stub_create_window
        procedure :: destroy_window => stub_destroy_window
        procedure :: show_window => stub_show_window
        procedure :: hide_window => stub_hide_window
        procedure :: create_widget => stub_create_widget
        procedure :: destroy_widget => stub_destroy_widget
        procedure :: get_name => stub_get_name
    end type forge_stub_backend_t

contains

    subroutine stub_init(this, status)
        class(forge_stub_backend_t), intent(inout) :: this
        type(forge_status), intent(out) :: status
        
        write(output_unit, '(A)') "[STUB] Initializing stub backend"
        this%initialized = .true.
        this%backend_type%id = BACKEND_CUSTOM
        call status%clear()
    end subroutine stub_init

    subroutine stub_shutdown(this)
        class(forge_stub_backend_t), intent(inout) :: this
        
        write(output_unit, '(A)') "[STUB] Shutting down stub backend"
        this%initialized = .false.
    end subroutine stub_shutdown

    subroutine stub_run(this)
        class(forge_stub_backend_t), intent(inout) :: this
        
        write(output_unit, '(A)') "[STUB] Running main loop (would block in real backend)"
        write(output_unit, '(A)') "[STUB] Exiting immediately in stub mode"
    end subroutine stub_run

    subroutine stub_process_events(this)
        class(forge_stub_backend_t), intent(inout) :: this
        
        ! No-op in stub
    end subroutine stub_process_events

    subroutine stub_create_window(this, handle, title, width, height, status)
        class(forge_stub_backend_t), intent(inout) :: this
        type(forge_window_handle), intent(out) :: handle
        character(len=*), intent(in) :: title
        integer(c_int), intent(in) :: width, height
        type(forge_status), intent(out) :: status
        
        this%window_count = this%window_count + 1
        handle%window_id = this%window_count
        handle%ptr = c_null_ptr
        
        write(output_unit, '(A,I0,A,A,A,I0,A,I0,A)') &
            "[STUB] Created window #", handle%window_id, &
            ' "', trim(title), '" (', width, 'x', height, ')'
        
        call status%clear()
    end subroutine stub_create_window

    subroutine stub_destroy_window(this, handle)
        class(forge_stub_backend_t), intent(inout) :: this
        type(forge_window_handle), intent(in) :: handle
        
        write(output_unit, '(A,I0)') "[STUB] Destroyed window #", handle%window_id
    end subroutine stub_destroy_window

    subroutine stub_show_window(this, handle)
        class(forge_stub_backend_t), intent(inout) :: this
        type(forge_window_handle), intent(in) :: handle
        
        write(output_unit, '(A,I0)') "[STUB] Showing window #", handle%window_id
    end subroutine stub_show_window

    subroutine stub_hide_window(this, handle)
        class(forge_stub_backend_t), intent(inout) :: this
        type(forge_window_handle), intent(in) :: handle
        
        write(output_unit, '(A,I0)') "[STUB] Hiding window #", handle%window_id
    end subroutine stub_hide_window

    subroutine stub_create_widget(this, handle, widget_type, parent, status)
        class(forge_stub_backend_t), intent(inout) :: this
        type(forge_widget_handle), intent(out) :: handle
        character(len=*), intent(in) :: widget_type
        type(forge_window_handle), intent(in) :: parent
        type(forge_status), intent(out) :: status
        
        this%widget_count = this%widget_count + 1
        handle%widget_id = this%widget_count
        handle%ptr = c_null_ptr
        
        write(output_unit, '(A,I0,A,A,A,I0)') &
            "[STUB] Created widget #", handle%widget_id, &
            ' type="', trim(widget_type), '" in window #', parent%window_id
        
        call status%clear()
    end subroutine stub_create_widget

    subroutine stub_destroy_widget(this, handle)
        class(forge_stub_backend_t), intent(inout) :: this
        type(forge_widget_handle), intent(in) :: handle
        
        write(output_unit, '(A,I0)') "[STUB] Destroyed widget #", handle%widget_id
    end subroutine stub_destroy_widget

    function stub_get_name(this) result(name)
        class(forge_stub_backend_t), intent(in) :: this
        character(len=:), allocatable :: name
        
        name = "Stub Backend (Testing Only)"
    end function stub_get_name

end module forge_stub_backend

