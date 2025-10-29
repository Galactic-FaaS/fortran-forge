!> @brief C bindings for Qt6 library
!> @details Provides Fortran interfaces to Qt6 C API
!> @author ForGE Contributors
!> @date 2025
!> @license GPL-3.0-or-later

module forge_qt_bindings
    use iso_c_binding
    implicit none
    private

    public :: qapplication, qwidget, qpushbutton, qlabel, qlineedit, qtextedit
    public :: qmainwindow, qapplication_new, qapplication_exec, qapplication_quit
    public :: qwidget_new, qwidget_show, qwidget_hide, qwidget_close
    public :: qwidget_set_window_title, qwidget_resize, qwidget_set_parent
    public :: qpushbutton_new, qlabel_new, qlineedit_new, qtextedit_new
    public :: qmainwindow_new, qmainwindow_set_central_widget
    public :: qobject_connect, qcoreapplication_process_events

    !> Opaque Qt handles
    type :: qapplication
        type(c_ptr) :: ptr = c_null_ptr
    end type qapplication

    type :: qwidget
        type(c_ptr) :: ptr = c_null_ptr
    end type qwidget

    type :: qpushbutton
        type(c_ptr) :: ptr = c_null_ptr
    end type qpushbutton

    type :: qlabel
        type(c_ptr) :: ptr = c_null_ptr
    end type qlabel

    type :: qlineedit
        type(c_ptr) :: ptr = c_null_ptr
    end type qlineedit

    type :: qtextedit
        type(c_ptr) :: ptr = c_null_ptr
    end type qtextedit

    type :: qmainwindow
        type(c_ptr) :: ptr = c_null_ptr
    end type qmainwindow

    !> Qt6 C API bindings (simplified wrapper functions)
    interface
        !> Create QApplication
        function qapplication_new(argc, argv) bind(c, name="qapplication_new")
            import :: c_ptr, c_int
            integer(c_int), value :: argc
            type(c_ptr), value :: argv
            type(c_ptr) :: qapplication_new
        end function qapplication_new

        !> Execute QApplication event loop
        function qapplication_exec() bind(c, name="qapplication_exec")
            import :: c_int
            integer(c_int) :: qapplication_exec
        end function qapplication_exec

        !> Quit QApplication
        subroutine qapplication_quit() bind(c, name="qapplication_quit")
        end subroutine qapplication_quit

        !> Create QWidget
        function qwidget_new(parent) bind(c, name="qwidget_new")
            import :: c_ptr
            type(c_ptr), value :: parent
            type(c_ptr) :: qwidget_new
        end function qwidget_new

        !> Show QWidget
        subroutine qwidget_show(widget) bind(c, name="qwidget_show")
            import :: c_ptr
            type(c_ptr), value :: widget
        end subroutine qwidget_show

        !> Hide QWidget
        subroutine qwidget_hide(widget) bind(c, name="qwidget_hide")
            import :: c_ptr
            type(c_ptr), value :: widget
        end subroutine qwidget_hide

        !> Close QWidget
        subroutine qwidget_close(widget) bind(c, name="qwidget_close")
            import :: c_ptr
            type(c_ptr), value :: widget
        end subroutine qwidget_close

        !> Set QWidget window title
        subroutine qwidget_set_window_title(widget, title) bind(c, name="qwidget_set_window_title")
            import :: c_ptr, c_char
            type(c_ptr), value :: widget
            character(kind=c_char), dimension(*) :: title
        end subroutine qwidget_set_window_title

        !> Resize QWidget
        subroutine qwidget_resize(widget, width, height) bind(c, name="qwidget_resize")
            import :: c_ptr, c_int
            type(c_ptr), value :: widget
            integer(c_int), value :: width
            integer(c_int), value :: height
        end subroutine qwidget_resize

        !> Set QWidget parent
        subroutine qwidget_set_parent(widget, parent) bind(c, name="qwidget_set_parent")
            import :: c_ptr
            type(c_ptr), value :: widget
            type(c_ptr), value :: parent
        end subroutine qwidget_set_parent

        !> Create QPushButton
        function qpushbutton_new(text, parent) bind(c, name="qpushbutton_new")
            import :: c_ptr, c_char
            character(kind=c_char), dimension(*) :: text
            type(c_ptr), value :: parent
            type(c_ptr) :: qpushbutton_new
        end function qpushbutton_new

        !> Create QLabel
        function qlabel_new(text, parent) bind(c, name="qlabel_new")
            import :: c_ptr, c_char
            character(kind=c_char), dimension(*) :: text
            type(c_ptr), value :: parent
            type(c_ptr) :: qlabel_new
        end function qlabel_new

        !> Create QLineEdit
        function qlineedit_new(parent) bind(c, name="qlineedit_new")
            import :: c_ptr
            type(c_ptr), value :: parent
            type(c_ptr) :: qlineedit_new
        end function qlineedit_new

        !> Create QTextEdit
        function qtextedit_new(parent) bind(c, name="qtextedit_new")
            import :: c_ptr
            type(c_ptr), value :: parent
            type(c_ptr) :: qtextedit_new
        end function qtextedit_new

        !> Create QMainWindow
        function qmainwindow_new(parent) bind(c, name="qmainwindow_new")
            import :: c_ptr
            type(c_ptr), value :: parent
            type(c_ptr) :: qmainwindow_new
        end function qmainwindow_new

        !> Set QMainWindow central widget
        subroutine qmainwindow_set_central_widget(window, widget) bind(c, name="qmainwindow_set_central_widget")
            import :: c_ptr
            type(c_ptr), value :: window
            type(c_ptr), value :: widget
        end subroutine qmainwindow_set_central_widget

        !> Connect QObject signal to slot
        subroutine qobject_connect(sender, signal, receiver, slot) bind(c, name="qobject_connect")
            import :: c_ptr, c_char
            type(c_ptr), value :: sender
            character(kind=c_char), dimension(*) :: signal
            type(c_ptr), value :: receiver
            character(kind=c_char), dimension(*) :: slot
        end subroutine qobject_connect

        !> Process pending events (non-blocking)
        subroutine qcoreapplication_process_events() bind(c, name="qcoreapplication_process_events")
        end subroutine qcoreapplication_process_events

        !> Delete QObject
        subroutine qobject_delete(obj) bind(c, name="qobject_delete")
            import :: c_ptr
            type(c_ptr), value :: obj
        end subroutine qobject_delete
    end interface

end module forge_qt_bindings