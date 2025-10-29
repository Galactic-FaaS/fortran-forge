!> @brief C bindings for GTK4 library
!> @details Provides Fortran interfaces to GTK4 C API
!> @author ForGE Contributors
!> @date 2025
!> @license GPL-3.0-or-later

module forge_gtk4_bindings
    use iso_c_binding
    implicit none
    private

    public :: gtk_application, gtk_window, gtk_widget
    public :: gtk_application_new, gtk_application_run, gtk_application_quit
    public :: gtk_window_new, gtk_window_set_title, gtk_window_set_default_size
    public :: gtk_window_present, gtk_window_hide, gtk_window_destroy
    public :: gtk_widget_show, gtk_widget_hide, gtk_widget_destroy
    public :: gtk_button_new_with_label, gtk_label_new, gtk_entry_new
    public :: gtk_text_view_new, gtk_drawing_area_new, gtk_frame_new
    public :: g_signal_connect_data, g_main_context_iteration
    public :: g_application_run, g_application_quit

    !> Opaque GTK handles
    type :: gtk_application
        type(c_ptr) :: ptr = c_null_ptr
    end type gtk_application

    type :: gtk_window
        type(c_ptr) :: ptr = c_null_ptr
    end type gtk_window

    type :: gtk_widget
        type(c_ptr) :: ptr = c_null_ptr
    end type gtk_widget

    !> GTK4 C API bindings
    interface
        !> Create new GTK application
        function gtk_application_new(app_id, flags) bind(c, name="gtk_application_new")
            import :: c_ptr, c_char, c_int
            character(kind=c_char), dimension(*) :: app_id
            integer(c_int), value :: flags
            type(c_ptr) :: gtk_application_new
        end function gtk_application_new

        !> Run GTK application
        function g_application_run(app, argc, argv) bind(c, name="g_application_run")
            import :: c_ptr, c_int
            type(c_ptr), value :: app
            integer(c_int), value :: argc
            type(c_ptr), value :: argv
            integer(c_int) :: g_application_run
        end function g_application_run

        !> Quit GTK application
        subroutine g_application_quit(app) bind(c, name="g_application_quit")
            import :: c_ptr
            type(c_ptr), value :: app
        end subroutine g_application_quit

        !> Create new window
        function gtk_window_new(window_type) bind(c, name="gtk_window_new")
            import :: c_ptr, c_int
            integer(c_int), value :: window_type
            type(c_ptr) :: gtk_window_new
        end function gtk_window_new

        !> Set window title
        subroutine gtk_window_set_title(window, title) bind(c, name="gtk_window_set_title")
            import :: c_ptr, c_char
            type(c_ptr), value :: window
            character(kind=c_char), dimension(*) :: title
        end subroutine gtk_window_set_title

        !> Set window default size
        subroutine gtk_window_set_default_size(window, width, height) bind(c, name="gtk_window_set_default_size")
            import :: c_ptr, c_int
            type(c_ptr), value :: window
            integer(c_int), value :: width
            integer(c_int), value :: height
        end subroutine gtk_window_set_default_size

        !> Present (show) window
        subroutine gtk_window_present(window) bind(c, name="gtk_window_present")
            import :: c_ptr
            type(c_ptr), value :: window
        end subroutine gtk_window_present

        !> Hide window
        subroutine gtk_window_hide(window) bind(c, name="gtk_window_hide")
            import :: c_ptr
            type(c_ptr), value :: window
        end subroutine gtk_window_hide

        !> Destroy window
        subroutine gtk_window_destroy(window) bind(c, name="gtk_window_destroy")
            import :: c_ptr
            type(c_ptr), value :: window
        end subroutine gtk_window_destroy

        !> Show widget
        subroutine gtk_widget_show(widget) bind(c, name="gtk_widget_show")
            import :: c_ptr
            type(c_ptr), value :: widget
        end subroutine gtk_widget_show

        !> Hide widget
        subroutine gtk_widget_hide(widget) bind(c, name="gtk_widget_hide")
            import :: c_ptr
            type(c_ptr), value :: widget
        end subroutine gtk_widget_hide

        !> Destroy widget
        subroutine gtk_widget_destroy(widget) bind(c, name="gtk_widget_destroy")
            import :: c_ptr
            type(c_ptr), value :: widget
        end subroutine gtk_widget_destroy

        !> Create button with label
        function gtk_button_new_with_label(label) bind(c, name="gtk_button_new_with_label")
            import :: c_ptr, c_char
            character(kind=c_char), dimension(*) :: label
            type(c_ptr) :: gtk_button_new_with_label
        end function gtk_button_new_with_label

        !> Create label
        function gtk_label_new(text) bind(c, name="gtk_label_new")
            import :: c_ptr, c_char
            character(kind=c_char), dimension(*) :: text
            type(c_ptr) :: gtk_label_new
        end function gtk_label_new

        !> Create entry (text input)
        function gtk_entry_new() bind(c, name="gtk_entry_new")
            import :: c_ptr
            type(c_ptr) :: gtk_entry_new
        end function gtk_entry_new

        !> Create text view
        function gtk_text_view_new() bind(c, name="gtk_text_view_new")
            import :: c_ptr
            type(c_ptr) :: gtk_text_view_new
        end function gtk_text_view_new

        !> Create drawing area
        function gtk_drawing_area_new() bind(c, name="gtk_drawing_area_new")
            import :: c_ptr
            type(c_ptr) :: gtk_drawing_area_new
        end function gtk_drawing_area_new

        !> Create frame
        function gtk_frame_new(label) bind(c, name="gtk_frame_new")
            import :: c_ptr, c_char
            character(kind=c_char), dimension(*) :: label
            type(c_ptr) :: gtk_frame_new
        end function gtk_frame_new

        !> Connect signal handler
        function g_signal_connect_data(instance, signal, handler, data, destroy_data, flags) &
                bind(c, name="g_signal_connect_data")
            import :: c_ptr, c_char, c_funptr, c_int
            type(c_ptr), value :: instance
            character(kind=c_char), dimension(*) :: signal
            type(c_funptr), value :: handler
            type(c_ptr), value :: data
            type(c_funptr), value :: destroy_data
            integer(c_int), value :: flags
            integer(c_long) :: g_signal_connect_data
        end function g_signal_connect_data

        !> Run main context iteration (non-blocking)
        function g_main_context_iteration(context, may_block) bind(c, name="g_main_context_iteration")
            import :: c_ptr, c_bool
            type(c_ptr), value :: context
            logical(c_bool), value :: may_block
            logical(c_bool) :: g_main_context_iteration
        end function g_main_context_iteration

        !> Get default main context
        function g_main_context_default() bind(c, name="g_main_context_default")
            import :: c_ptr
            type(c_ptr) :: g_main_context_default
        end function g_main_context_default
    end interface

    !> Constants
    integer(c_int), parameter :: G_APPLICATION_FLAGS_NONE = 0
    integer(c_int), parameter :: GTK_WINDOW_TOPLEVEL = 0
    integer(c_int), parameter :: G_CONNECT_DEFAULT = 0

end module forge_gtk4_bindings