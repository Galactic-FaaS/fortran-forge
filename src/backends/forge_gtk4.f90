!> @brief GTK4 backend implementation
!> @details Implements ForGE backend using GTK4 GUI library
!> @author ForGE Contributors
!> @date 2025
!> @license GPL-3.0-or-later

module forge_gtk4_backend
    use iso_c_binding
    use forge_backend
    use forge_types
    use forge_errors
    use forge_gtk4_bindings
    use iso_fortran_env, only: output_unit
    implicit none
    private

    public :: forge_gtk4_backend_t

    !> @brief GTK4 backend implementation
    type, extends(forge_backend_base) :: forge_gtk4_backend_t
        private
        type(gtk_application) :: app
        integer :: window_count = 0
        integer :: widget_count = 0
    contains
        procedure :: init => gtk4_init
        procedure :: shutdown => gtk4_shutdown
        procedure :: run => gtk4_run
        procedure :: process_events => gtk4_process_events
        procedure :: create_window => gtk4_create_window
        procedure :: destroy_window => gtk4_destroy_window
        procedure :: show_window => gtk4_show_window
        procedure :: hide_window => gtk4_hide_window
        procedure :: create_widget => gtk4_create_widget
        procedure :: destroy_widget => gtk4_destroy_widget
        procedure :: get_name => gtk4_get_name
    end type forge_gtk4_backend_t

contains

    !> @brief Initialize GTK4 backend
    subroutine gtk4_init(this, status)
        class(forge_gtk4_backend_t), intent(inout) :: this
        type(forge_status), intent(out) :: status

        ! Create GTK application
        this%app%ptr = gtk_application_new("com.forge.gui" // c_null_char, G_APPLICATION_FLAGS_NONE)
        if (.not. c_associated(this%app%ptr)) then
            call status%set(FORGE_ERROR_BACKEND, "Failed to create GTK application")
            return
        end if

        this%initialized = .true.
        this%backend_type%id = BACKEND_GTK4
        call status%clear()

        write(output_unit, '(A)') "[GTK4] Initialized GTK4 backend"
    end subroutine gtk4_init

    !> @brief Shutdown GTK4 backend
    subroutine gtk4_shutdown(this)
        class(forge_gtk4_backend_t), intent(inout) :: this

        if (c_associated(this%app%ptr)) then
            call g_application_quit(this%app%ptr)
            this%app%ptr = c_null_ptr
        end if

        this%initialized = .false.
        write(output_unit, '(A)') "[GTK4] Shutdown GTK4 backend"
    end subroutine gtk4_shutdown

    !> @brief Run main event loop
    subroutine gtk4_run(this)
        class(forge_gtk4_backend_t), intent(inout) :: this
        integer(c_int) :: exit_code

        if (.not. this%initialized) return

        write(output_unit, '(A)') "[GTK4] Starting GTK main loop"
        exit_code = g_application_run(this%app%ptr, 0, c_null_ptr)
        write(output_unit, '(A,I0)') "[GTK4] Exited GTK main loop with code ", exit_code
    end subroutine gtk4_run

    !> @brief Process pending events (non-blocking)
    subroutine gtk4_process_events(this)
        class(forge_gtk4_backend_t), intent(inout) :: this
        type(c_ptr) :: context
        logical(c_bool) :: events_processed

        if (.not. this%initialized) return

        ! Get default main context and process one iteration
        context = g_main_context_default()
        events_processed = g_main_context_iteration(context, .false.)
    end subroutine gtk4_process_events

    !> @brief Create a window
    subroutine gtk4_create_window(this, handle, title, width, height, status)
        class(forge_gtk4_backend_t), intent(inout) :: this
        type(forge_window_handle), intent(out) :: handle
        character(len=*), intent(in) :: title
        integer(c_int), intent(in) :: width, height
        type(forge_status), intent(out) :: status
        type(gtk_window) :: gtk_win

        if (.not. this%initialized) then
            call status%set(FORGE_ERROR_BACKEND, "Backend not initialized")
            return
        end if

        ! Create GTK window
        gtk_win%ptr = gtk_window_new(GTK_WINDOW_TOPLEVEL)
        if (.not. c_associated(gtk_win%ptr)) then
            call status%set(FORGE_ERROR_BACKEND, "Failed to create GTK window")
            return
        end if

        ! Set window properties
        call gtk_window_set_title(gtk_win%ptr, trim(title) // c_null_char)
        call gtk_window_set_default_size(gtk_win%ptr, width, height)

        ! Store handle
        this%window_count = this%window_count + 1
        handle%window_id = this%window_count
        handle%ptr = gtk_win%ptr

        call status%clear()
        write(output_unit, '(A,I0,A,A,A,I0,A,I0)') &
            "[GTK4] Created window #", handle%window_id, &
            ' "', trim(title), '" (', width, 'x', height, ')'
    end subroutine gtk4_create_window

    !> @brief Destroy a window
    subroutine gtk4_destroy_window(this, handle)
        class(forge_gtk4_backend_t), intent(inout) :: this
        type(forge_window_handle), intent(in) :: handle
        type(gtk_window) :: gtk_win

        gtk_win%ptr = handle%ptr
        if (c_associated(gtk_win%ptr)) then
            call gtk_window_destroy(gtk_win%ptr)
        end if

        write(output_unit, '(A,I0)') "[GTK4] Destroyed window #", handle%window_id
    end subroutine gtk4_destroy_window

    !> @brief Show a window
    subroutine gtk4_show_window(this, handle)
        class(forge_gtk4_backend_t), intent(inout) :: this
        type(forge_window_handle), intent(in) :: handle
        type(gtk_window) :: gtk_win

        gtk_win%ptr = handle%ptr
        if (c_associated(gtk_win%ptr)) then
            call gtk_window_present(gtk_win%ptr)
        end if

        write(output_unit, '(A,I0)') "[GTK4] Showing window #", handle%window_id
    end subroutine gtk4_show_window

    !> @brief Hide a window
    subroutine gtk4_hide_window(this, handle)
        class(forge_gtk4_backend_t), intent(inout) :: this
        type(forge_window_handle), intent(in) :: handle
        type(gtk_window) :: gtk_win

        gtk_win%ptr = handle%ptr
        if (c_associated(gtk_win%ptr)) then
            call gtk_window_hide(gtk_win%ptr)
        end if

        write(output_unit, '(A,I0)') "[GTK4] Hiding window #", handle%window_id
    end subroutine gtk4_hide_window

    !> @brief Create a widget
    subroutine gtk4_create_widget(this, handle, widget_type, parent, status)
        class(forge_gtk4_backend_t), intent(inout) :: this
        type(forge_widget_handle), intent(out) :: handle
        character(len=*), intent(in) :: widget_type
        type(forge_window_handle), intent(in) :: parent
        type(forge_status), intent(out) :: status
        type(gtk_widget) :: gtk_wid

        if (.not. this%initialized) then
            call status%set(FORGE_ERROR_BACKEND, "Backend not initialized")
            return
        end if

        ! Create widget based on type
        select case (trim(widget_type))
        case ("button")
            gtk_wid%ptr = gtk_button_new_with_label("Button" // c_null_char)
        case ("label")
            gtk_wid%ptr = gtk_label_new("Label" // c_null_char)
        case ("entry")
            gtk_wid%ptr = gtk_entry_new()
        case ("text")
            gtk_wid%ptr = gtk_text_view_new()
        case ("canvas")
            gtk_wid%ptr = gtk_drawing_area_new()
        case ("frame")
            gtk_wid%ptr = gtk_frame_new(c_null_char)
        case default
            gtk_wid%ptr = gtk_label_new("Widget" // c_null_char)  ! Default fallback
        end select

        if (.not. c_associated(gtk_wid%ptr)) then
            call status%set(FORGE_ERROR_BACKEND, "Failed to create GTK widget")
            return
        end if

        ! Store handle
        this%widget_count = this%widget_count + 1
        handle%widget_id = this%widget_count
        handle%ptr = gtk_wid%ptr

        call status%clear()
        write(output_unit, '(A,I0,A,A,A,I0)') &
            "[GTK4] Created widget #", handle%widget_id, &
            ' type="', trim(widget_type), '" in window #', parent%window_id
    end subroutine gtk4_create_widget

    !> @brief Destroy a widget
    subroutine gtk4_destroy_widget(this, handle)
        class(forge_gtk4_backend_t), intent(inout) :: this
        type(forge_widget_handle), intent(in) :: handle
        type(gtk_widget) :: gtk_wid

        gtk_wid%ptr = handle%ptr
        if (c_associated(gtk_wid%ptr)) then
            call gtk_widget_destroy(gtk_wid%ptr)
        end if

        write(output_unit, '(A,I0)') "[GTK4] Destroyed widget #", handle%widget_id
    end subroutine gtk4_destroy_widget

    !> @brief Get backend name
    function gtk4_get_name(this) result(name)
        class(forge_gtk4_backend_t), intent(in) :: this
        character(len=:), allocatable :: name

        name = "GTK4 Backend"
    end function gtk4_get_name

end module forge_gtk4_backend