!> @brief Qt backend implementation
!> @details Implements ForGE backend using Qt6 GUI library
!> @author ForGE Contributors
!> @date 2025
!> @license GPL-3.0-or-later

module forge_qt_backend
    use iso_c_binding
    use forge_backend
    use forge_types
    use forge_errors
    use forge_qt_bindings
    use iso_fortran_env, only: output_unit
    implicit none
    private

    public :: forge_qt_backend_t

    !> @brief Qt backend implementation
    type, extends(forge_backend_base) :: forge_qt_backend_t
        private
        type(qapplication) :: app
        integer :: window_count = 0
        integer :: widget_count = 0
    contains
        procedure :: init => qt_init
        procedure :: shutdown => qt_shutdown
        procedure :: run => qt_run
        procedure :: process_events => qt_process_events
        procedure :: create_window => qt_create_window
        procedure :: destroy_window => qt_destroy_window
        procedure :: show_window => qt_show_window
        procedure :: hide_window => qt_hide_window
        procedure :: create_widget => qt_create_widget
        procedure :: destroy_widget => qt_destroy_widget
        procedure :: get_name => qt_get_name
    end type forge_qt_backend_t

contains

    !> @brief Initialize Qt backend
    subroutine qt_init(this, status)
        class(forge_qt_backend_t), intent(inout) :: this
        type(forge_status), intent(out) :: status

        ! Create QApplication
        this%app%ptr = qapplication_new(0, c_null_ptr)
        if (.not. c_associated(this%app%ptr)) then
            call status%set(FORGE_ERROR_BACKEND, "Failed to create Qt application")
            return
        end if

        this%initialized = .true.
        this%backend_type%id = BACKEND_QT
        call status%clear()

        write(output_unit, '(A)') "[QT] Initialized Qt backend"
    end subroutine qt_init

    !> @brief Shutdown Qt backend
    subroutine qt_shutdown(this)
        class(forge_qt_backend_t), intent(inout) :: this

        if (c_associated(this%app%ptr)) then
            call qapplication_quit()
            this%app%ptr = c_null_ptr
        end if

        this%initialized = .false.
        write(output_unit, '(A)') "[QT] Shutdown Qt backend"
    end subroutine qt_shutdown

    !> @brief Run main event loop
    subroutine qt_run(this)
        class(forge_qt_backend_t), intent(inout) :: this
        integer(c_int) :: exit_code

        if (.not. this%initialized) return

        write(output_unit, '(A)') "[QT] Starting Qt main loop"
        exit_code = qapplication_exec()
        write(output_unit, '(A,I0)') "[QT] Exited Qt main loop with code ", exit_code
    end subroutine qt_run

    !> @brief Process pending events (non-blocking)
    subroutine qt_process_events(this)
        class(forge_qt_backend_t), intent(inout) :: this

        if (.not. this%initialized) return

        call qcoreapplication_process_events()
    end subroutine qt_process_events

    !> @brief Create a window
    subroutine qt_create_window(this, handle, title, width, height, status)
        class(forge_qt_backend_t), intent(inout) :: this
        type(forge_window_handle), intent(out) :: handle
        character(len=*), intent(in) :: title
        integer(c_int), intent(in) :: width, height
        type(forge_status), intent(out) :: status
        type(qmainwindow) :: qt_win

        if (.not. this%initialized) then
            call status%set(FORGE_ERROR_BACKEND, "Backend not initialized")
            return
        end if

        ! Create QMainWindow
        qt_win%ptr = qmainwindow_new(c_null_ptr)
        if (.not. c_associated(qt_win%ptr)) then
            call status%set(FORGE_ERROR_BACKEND, "Failed to create Qt main window")
            return
        end if

        ! Set window properties
        call qwidget_set_window_title(qt_win%ptr, trim(title) // c_null_char)
        call qwidget_resize(qt_win%ptr, width, height)

        ! Store handle
        this%window_count = this%window_count + 1
        handle%window_id = this%window_count
        handle%ptr = qt_win%ptr

        call status%clear()
        write(output_unit, '(A,I0,A,A,A,I0,A,I0)') &
            "[QT] Created window #", handle%window_id, &
            ' "', trim(title), '" (', width, 'x', height, ')'
    end subroutine qt_create_window

    !> @brief Destroy a window
    subroutine qt_destroy_window(this, handle)
        class(forge_qt_backend_t), intent(inout) :: this
        type(forge_window_handle), intent(in) :: handle
        type(qwidget) :: qt_win

        qt_win%ptr = handle%ptr
        if (c_associated(qt_win%ptr)) then
            call qobject_delete(qt_win%ptr)
        end if

        write(output_unit, '(A,I0)') "[QT] Destroyed window #", handle%window_id
    end subroutine qt_destroy_window

    !> @brief Show a window
    subroutine qt_show_window(this, handle)
        class(forge_qt_backend_t), intent(inout) :: this
        type(forge_window_handle), intent(in) :: handle
        type(qwidget) :: qt_win

        qt_win%ptr = handle%ptr
        if (c_associated(qt_win%ptr)) then
            call qwidget_show(qt_win%ptr)
        end if

        write(output_unit, '(A,I0)') "[QT] Showing window #", handle%window_id
    end subroutine qt_show_window

    !> @brief Hide a window
    subroutine qt_hide_window(this, handle)
        class(forge_qt_backend_t), intent(inout) :: this
        type(forge_window_handle), intent(in) :: handle
        type(qwidget) :: qt_win

        qt_win%ptr = handle%ptr
        if (c_associated(qt_win%ptr)) then
            call qwidget_hide(qt_win%ptr)
        end if

        write(output_unit, '(A,I0)') "[QT] Hiding window #", handle%window_id
    end subroutine qt_hide_window

    !> @brief Create a widget
    subroutine qt_create_widget(this, handle, widget_type, parent, status)
        class(forge_qt_backend_t), intent(inout) :: this
        type(forge_widget_handle), intent(out) :: handle
        character(len=*), intent(in) :: widget_type
        type(forge_window_handle), intent(in) :: parent
        type(forge_status), intent(out) :: status
        type(qwidget) :: qt_wid, parent_win

        if (.not. this%initialized) then
            call status%set(FORGE_ERROR_BACKEND, "Backend not initialized")
            return
        end if

        parent_win%ptr = parent%ptr

        ! Create widget based on type
        select case (trim(widget_type))
        case ("button")
            qt_wid%ptr = qpushbutton_new("Button" // c_null_char, parent_win%ptr)
        case ("label")
            qt_wid%ptr = qlabel_new("Label" // c_null_char, parent_win%ptr)
        case ("entry")
            qt_wid%ptr = qlineedit_new(parent_win%ptr)
        case ("text")
            qt_wid%ptr = qtextedit_new(parent_win%ptr)
        case ("frame")
            qt_wid%ptr = qwidget_new(parent_win%ptr)  ! Use QWidget as frame
        case default
            qt_wid%ptr = qlabel_new("Widget" // c_null_char, parent_win%ptr)  ! Default fallback
        end select

        if (.not. c_associated(qt_wid%ptr)) then
            call status%set(FORGE_ERROR_BACKEND, "Failed to create Qt widget")
            return
        end if

        ! Store handle
        this%widget_count = this%widget_count + 1
        handle%widget_id = this%widget_count
        handle%ptr = qt_wid%ptr

        call status%clear()
        write(output_unit, '(A,I0,A,A,A,I0)') &
            "[QT] Created widget #", handle%widget_id, &
            ' type="', trim(widget_type), '" in window #', parent%window_id
    end subroutine qt_create_widget

    !> @brief Destroy a widget
    subroutine qt_destroy_widget(this, handle)
        class(forge_qt_backend_t), intent(inout) :: this
        type(forge_widget_handle), intent(in) :: handle
        type(qwidget) :: qt_wid

        qt_wid%ptr = handle%ptr
        if (c_associated(qt_wid%ptr)) then
            call qobject_delete(qt_wid%ptr)
        end if

        write(output_unit, '(A,I0)') "[QT] Destroyed widget #", handle%widget_id
    end subroutine qt_destroy_widget

    !> @brief Get backend name
    function qt_get_name(this) result(name)
        class(forge_qt_backend_t), intent(in) :: this
        character(len=:), allocatable :: name

        name = "Qt Backend"
    end function qt_get_name

end module forge_qt_backend