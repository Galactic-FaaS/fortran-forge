!> @brief Tcl/Tk backend implementation
!> @details Implements ForGE backend using Tcl/Tk GUI library
!> @author ForGE Contributors
!> @date 2025
!> @license GPL-3.0-or-later

module forge_tcltk_backend
    use iso_c_binding
    use forge_backend
    use forge_types
    use forge_errors
    use forge_tcltk_bindings
    use iso_fortran_env, only: output_unit
    implicit none
    private

    public :: forge_tcltk_backend_t

    !> @brief Tcl/Tk backend implementation
    type, extends(forge_backend_base) :: forge_tcltk_backend_t
        private
        type(tcl_interp) :: interp
        type(tk_window) :: main_window
        integer :: window_count = 0
        integer :: widget_count = 0
    contains
        procedure :: init => tcltk_init
        procedure :: shutdown => tcltk_shutdown
        procedure :: run => tcltk_run
        procedure :: process_events => tcltk_process_events
        procedure :: create_window => tcltk_create_window
        procedure :: destroy_window => tcltk_destroy_window
        procedure :: show_window => tcltk_show_window
        procedure :: hide_window => tcltk_hide_window
        procedure :: create_widget => tcltk_create_widget
        procedure :: destroy_widget => tcltk_destroy_widget
        procedure :: get_name => tcltk_get_name
    end type forge_tcltk_backend_t

contains

    !> @brief Initialize Tcl/Tk backend
    subroutine tcltk_init(this, status)
        class(forge_tcltk_backend_t), intent(inout) :: this
        type(forge_status), intent(out) :: status
        integer(c_int) :: result

        ! Create Tcl interpreter
        this%interp%ptr = tcl_createinterp()
        if (.not. c_associated(this%interp%ptr)) then
            call status%set(FORGE_ERROR_BACKEND, "Failed to create Tcl interpreter")
            return
        end if

        ! Initialize Tcl
        result = tcl_init(this%interp%ptr)
        if (result /= 0) then
            call status%set(FORGE_ERROR_BACKEND, "Failed to initialize Tcl")
            return
        end if

        ! Initialize Tk
        result = tk_init(this%interp%ptr)
        if (result /= 0) then
            call status%set(FORGE_ERROR_BACKEND, "Failed to initialize Tk")
            return
        end if

        ! Get main window
        this%main_window%ptr = tk_mainwindow(this%interp%ptr)
        if (.not. c_associated(this%main_window%ptr)) then
            call status%set(FORGE_ERROR_BACKEND, "Failed to get Tk main window")
            return
        end if

        this%initialized = .true.
        this%backend_type%id = BACKEND_TCL_TK
        call status%clear()

        write(output_unit, '(A)') "[TCLTK] Initialized Tcl/Tk backend"
    end subroutine tcltk_init

    !> @brief Shutdown Tcl/Tk backend
    subroutine tcltk_shutdown(this)
        class(forge_tcltk_backend_t), intent(inout) :: this

        if (c_associated(this%interp%ptr)) then
            call tcl_deleteinterp(this%interp%ptr)
            this%interp%ptr = c_null_ptr
        end if

        this%initialized = .false.
        write(output_unit, '(A)') "[TCLTK] Shutdown Tcl/Tk backend"
    end subroutine tcltk_shutdown

    !> @brief Run main event loop
    subroutine tcltk_run(this)
        class(forge_tcltk_backend_t), intent(inout) :: this

        if (.not. this%initialized) return

        write(output_unit, '(A)') "[TCLTK] Starting Tk main loop"
        call tk_mainloop()
        write(output_unit, '(A)') "[TCLTK] Exited Tk main loop"
    end subroutine tcltk_run

    !> @brief Process pending events (non-blocking)
    subroutine tcltk_process_events(this)
        class(forge_tcltk_backend_t), intent(inout) :: this

        if (.not. this%initialized) return

        ! Process one event if available
        call tk_dooneevent(TCL_DONT_WAIT)
    end subroutine tcltk_process_events

    !> @brief Create a window
    subroutine tcltk_create_window(this, handle, title, width, height, status)
        class(forge_tcltk_backend_t), intent(inout) :: this
        type(forge_window_handle), intent(out) :: handle
        character(len=*), intent(in) :: title
        integer(c_int), intent(in) :: width, height
        type(forge_status), intent(out) :: status
        type(tk_window) :: tk_win
        character(kind=c_char, len=:), allocatable :: name, script
        integer(c_int) :: result

        if (.not. this%initialized) then
            call status%set(FORGE_ERROR_BACKEND, "Backend not initialized")
            return
        end if

        ! Create unique window name
        this%window_count = this%window_count + 1
        name = ".win" // trim(adjustl(int_to_string(this%window_count))) // c_null_char

        ! Create Tk window
        tk_win%ptr = tk_createwindow(this%interp%ptr, this%main_window%ptr, &
                                   name // c_null_char, "toplevel" // c_null_char)
        if (.not. c_associated(tk_win%ptr)) then
            call status%set(FORGE_ERROR_BACKEND, "Failed to create Tk window")
            return
        end if

        ! Set window properties via Tcl script
        script = "wm title " // name // " {" // trim(title) // "}; " // &
                "wm geometry " // name // " " // trim(adjustl(int_to_string(width))) // &
                "x" // trim(adjustl(int_to_string(height))) // c_null_char

        result = tcl_eval(this%interp%ptr, script)
        if (result /= 0) then
            call status%set(FORGE_ERROR_BACKEND, "Failed to set window properties")
            return
        end if

        ! Store handle
        handle%window_id = this%window_count
        handle%ptr = tk_win%ptr

        call status%clear()
        write(output_unit, '(A,I0,A,A,A,I0,A,I0)') &
            "[TCLTK] Created window #", handle%window_id, &
            ' "', trim(title), '" (', width, 'x', height, ')'
    end subroutine tcltk_create_window

    !> @brief Destroy a window
    subroutine tcltk_destroy_window(this, handle)
        class(forge_tcltk_backend_t), intent(inout) :: this
        type(forge_window_handle), intent(in) :: handle
        type(tk_window) :: tk_win

        tk_win%ptr = handle%ptr
        if (c_associated(tk_win%ptr)) then
            call tk_destroywindow(tk_win%ptr)
        end if

        write(output_unit, '(A,I0)') "[TCLTK] Destroyed window #", handle%window_id
    end subroutine tcltk_destroy_window

    !> @brief Show a window
    subroutine tcltk_show_window(this, handle)
        class(forge_tcltk_backend_t), intent(inout) :: this
        type(forge_window_handle), intent(in) :: handle
        type(tk_window) :: tk_win

        tk_win%ptr = handle%ptr
        if (c_associated(tk_win%ptr)) then
            call tk_mapwindow(tk_win%ptr)
        end if

        write(output_unit, '(A,I0)') "[TCLTK] Showing window #", handle%window_id
    end subroutine tcltk_show_window

    !> @brief Hide a window
    subroutine tcltk_hide_window(this, handle)
        class(forge_tcltk_backend_t), intent(inout) :: this
        type(forge_window_handle), intent(in) :: handle
        type(tk_window) :: tk_win

        tk_win%ptr = handle%ptr
        if (c_associated(tk_win%ptr)) then
            call tk_unmapwindow(tk_win%ptr)
        end if

        write(output_unit, '(A,I0)') "[TCLTK] Hiding window #", handle%window_id
    end subroutine tcltk_hide_window

    !> @brief Create a widget
    subroutine tcltk_create_widget(this, handle, widget_type, parent, status)
        class(forge_tcltk_backend_t), intent(inout) :: this
        type(forge_widget_handle), intent(out) :: handle
        character(len=*), intent(in) :: widget_type
        type(forge_window_handle), intent(in) :: parent
        type(forge_status), intent(out) :: status
        type(tk_window) :: parent_win, widget_win
        character(kind=c_char, len=:), allocatable :: name, tk_class

        if (.not. this%initialized) then
            call status%set(FORGE_ERROR_BACKEND, "Backend not initialized")
            return
        end if

        ! Map widget type to Tk class
        select case (trim(widget_type))
        case ("button")
            tk_class = "button"
        case ("label")
            tk_class = "label"
        case ("entry")
            tk_class = "entry"
        case ("text")
            tk_class = "text"
        case ("canvas")
            tk_class = "canvas"
        case ("frame")
            tk_class = "frame"
        case default
            tk_class = "label"  ! Default fallback
        end select

        ! Create unique widget name
        this%widget_count = this%widget_count + 1
        name = ".wid" // trim(adjustl(int_to_string(this%widget_count))) // c_null_char

        parent_win%ptr = parent%ptr

        ! Create Tk widget
        widget_win%ptr = tk_createwidget(this%interp%ptr, parent_win%ptr, &
                                       tk_class // c_null_char, name // c_null_char, &
                                       "" // c_null_char, TK_WIDGET_OPTION_NONE)
        if (.not. c_associated(widget_win%ptr)) then
            call status%set(FORGE_ERROR_BACKEND, "Failed to create Tk widget")
            return
        end if

        ! Store handle
        handle%widget_id = this%widget_count
        handle%ptr = widget_win%ptr

        call status%clear()
        write(output_unit, '(A,I0,A,A,A,I0)') &
            "[TCLTK] Created widget #", handle%widget_id, &
            ' type="', trim(widget_type), '" in window #', parent%window_id
    end subroutine tcltk_create_widget

    !> @brief Destroy a widget
    subroutine tcltk_destroy_widget(this, handle)
        class(forge_tcltk_backend_t), intent(inout) :: this
        type(forge_widget_handle), intent(in) :: handle
        type(tk_window) :: widget_win

        widget_win%ptr = handle%ptr
        if (c_associated(widget_win%ptr)) then
            call tk_destroywidget(widget_win%ptr)
        end if

        write(output_unit, '(A,I0)') "[TCLTK] Destroyed widget #", handle%widget_id
    end subroutine tcltk_destroy_widget

    !> @brief Get backend name
    function tcltk_get_name(this) result(name)
        class(forge_tcltk_backend_t), intent(in) :: this
        character(len=:), allocatable :: name

        name = "Tcl/Tk Backend"
    end function tcltk_get_name

    !> @brief Convert integer to string (utility function)
    function int_to_string(i) result(str)
        integer, intent(in) :: i
        character(len=20) :: str
        write(str, '(I0)') i
        str = trim(adjustl(str))
    end function int_to_string

end module forge_tcltk_backend