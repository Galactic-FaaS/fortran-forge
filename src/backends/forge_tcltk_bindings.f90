!> @brief C bindings for Tcl/Tk library
!> @details Provides Fortran interfaces to Tcl/Tk C API
!> @author ForGE Contributors
!> @date 2025
!> @license GPL-3.0-or-later

module forge_tcltk_bindings
    use iso_c_binding
    implicit none
    private

    public :: tcl_interp, tk_window
    public :: tcl_createinterp, tcl_deleteinterp, tcl_init, tcl_eval
    public :: tk_init, tk_mainwindow, tk_createwindow, tk_destroywindow
    public :: tk_mapwindow, tk_unmapwindow, tk_createwidget, tk_destroywidget

    !> Opaque Tcl interpreter handle
    type :: tcl_interp
        type(c_ptr) :: ptr = c_null_ptr
    end type tcl_interp

    !> Opaque Tk window handle
    type :: tk_window
        type(c_ptr) :: ptr = c_null_ptr
    end type tk_window

    !> Tcl/Tk C API bindings
    interface
        !> Create a new Tcl interpreter
        function tcl_createinterp() bind(c, name="Tcl_CreateInterp")
            import :: c_ptr
            type(c_ptr) :: tcl_createinterp
        end function tcl_createinterp

        !> Delete a Tcl interpreter
        subroutine tcl_deleteinterp(interp) bind(c, name="Tcl_DeleteInterp")
            import :: c_ptr
            type(c_ptr), value :: interp
        end subroutine tcl_deleteinterp

        !> Initialize Tcl
        function tcl_init(interp) bind(c, name="Tcl_Init")
            import :: c_ptr, c_int
            type(c_ptr), value :: interp
            integer(c_int) :: tcl_init
        end function tcl_init

        !> Evaluate a Tcl script
        function tcl_eval(interp, script) bind(c, name="Tcl_Eval")
            import :: c_ptr, c_char, c_int
            type(c_ptr), value :: interp
            character(kind=c_char), dimension(*) :: script
            integer(c_int) :: tcl_eval
        end function tcl_eval

        !> Initialize Tk
        function tk_init(interp) bind(c, name="Tk_Init")
            import :: c_ptr, c_int
            type(c_ptr), value :: interp
            integer(c_int) :: tk_init
        end function tk_init

        !> Get main window
        function tk_mainwindow(interp) bind(c, name="Tk_MainWindow")
            import :: c_ptr
            type(c_ptr), value :: interp
            type(c_ptr) :: tk_mainwindow
        end function tk_mainwindow

        !> Create a new window
        function tk_createwindow(interp, parent, name, class) bind(c, name="Tk_CreateWindow")
            import :: c_ptr, c_char
            type(c_ptr), value :: interp
            type(c_ptr), value :: parent
            character(kind=c_char), dimension(*) :: name
            character(kind=c_char), dimension(*) :: class
            type(c_ptr) :: tk_createwindow
        end function tk_createwindow

        !> Destroy a window
        subroutine tk_destroywindow(window) bind(c, name="Tk_DestroyWindow")
            import :: c_ptr
            type(c_ptr), value :: window
        end subroutine tk_destroywindow

        !> Map (show) a window
        subroutine tk_mapwindow(window) bind(c, name="Tk_MapWindow")
            import :: c_ptr
            type(c_ptr), value :: window
        end subroutine tk_mapwindow

        !> Unmap (hide) a window
        subroutine tk_unmapwindow(window) bind(c, name="Tk_UnmapWindow")
            import :: c_ptr
            type(c_ptr), value :: window
        end subroutine tk_unmapwindow

        !> Create a widget
        function tk_createwidget(interp, window, class, name, args, options) bind(c, name="Tk_CreateWidget")
            import :: c_ptr, c_char, c_int
            type(c_ptr), value :: interp
            type(c_ptr), value :: window
            character(kind=c_char), dimension(*) :: class
            character(kind=c_char), dimension(*) :: name
            character(kind=c_char), dimension(*) :: args
            integer(c_int), value :: options
            type(c_ptr) :: tk_createwidget
        end function tk_createwidget

        !> Destroy a widget
        subroutine tk_destroywidget(widget) bind(c, name="Tk_DestroyWidget")
            import :: c_ptr
            type(c_ptr), value :: widget
        end subroutine tk_destroywidget

        !> Run Tk main loop
        subroutine tk_mainloop() bind(c, name="Tk_MainLoop")
        end subroutine tk_mainloop

        !> Process Tk events (non-blocking)
        subroutine tk_dooneevent(flags) bind(c, name="Tcl_DoOneEvent")
            import :: c_int
            integer(c_int), value :: flags
        end subroutine tk_dooneevent
    end interface

    !> Constants
    integer(c_int), parameter :: TCL_DONT_WAIT = 1
    integer(c_int), parameter :: TK_WIDGET_OPTION_NONE = 0

end module forge_tcltk_bindings