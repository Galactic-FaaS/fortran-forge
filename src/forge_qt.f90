!> @brief ForGE Qt - Complete Application Framework
!> @details Main module that re-exports all Qt-equivalent functionality
!> @author ForGE Contributors
!> @date 2025
!> @license GPL-3.0-or-later
!>
!> ForGE Qt is a comprehensive application framework for Fortran,
!> providing Qt-equivalent functionality including:
!> - Rich widget library (50+ widgets planned)
!> - Signals & Slots event system
!> - QString and container types
!> - Networking (HTTP, Sockets)
!> - Threading and concurrency
!> - JSON/XML parsing
!> - Multi-platform support
!>
!> @par Quick Start:
!> @code{.f90}
!> use forge_qt
!> type(QApplication) :: app
!> type(QPushButton) :: button
!> 
!> call app%init()
!> button = QPushButton("Click Me")
!> conn = button%clicked%connect(on_clicked)
!> call app%run()
!> @endcode

module forge_qt
    ! Base framework
    use forge
    
    ! Signals & Slots
    use forge_signals
    
    ! Core utilities
    use forge_string_utils
    use forge_containers
    use forge_json
    
    ! Networking
    use forge_socket
    use forge_http
    
    ! Threading
    use forge_thread
    
    ! Extended widgets
    use forge_checkbox
    use forge_radiobutton
    use forge_combobox
    use forge_spinbox
    use forge_slider
    use forge_menubar
    use forge_groupbox
    use forge_tabwidget
    use forge_listview
    use forge_statusbar
    use forge_messagebox
    
    implicit none
    private

    ! ========== Re-export All Public Interfaces ==========

    ! Core framework
    public :: forge_application, forge_window_t, forge_widget
    public :: forge_error, forge_status
    public :: forge_event, forge_event_handler
    public :: forge_backend_base
    
    ! Signals & Slots
    public :: signal_void, signal_int, signal_string, signal_bool
    public :: slot_void, slot_int, slot_string, slot_bool
    public :: forge_connection
    
    ! String utilities
    public :: QString
    public :: string_split, string_join, string_replace
    public :: string_to_upper, string_to_lower
    public :: int_to_string, real_to_string
    
    ! Containers
    public :: QList_int, QList_real, QList_string
    public :: QMap_string_int
    public :: QStack_int, QQueue_int
    
    ! JSON
    public :: QJsonValue, QJsonObject, QJsonArray
    public :: parse_json, json_to_string
    public :: JSON_NULL, JSON_BOOL, JSON_NUMBER, JSON_STRING, JSON_ARRAY, JSON_OBJECT
    
    ! Networking
    public :: QTcpSocket, QUdpSocket, QHostAddress
    public :: QHttpClient, QHttpRequest, QHttpResponse
    public :: HTTP_GET, HTTP_POST, HTTP_PUT, HTTP_DELETE
    
    ! Threading
    public :: QThread, QMutex, QSemaphore, QWaitCondition
    
    ! Basic widgets
    public :: forge_button, forge_label, forge_entry
    public :: forge_text_view, forge_progress_bar, forge_separator
    
    ! Input widgets
    public :: QCheckBox, QRadioButton, QButtonGroup
    public :: QComboBox
    public :: QSpinBox, QDoubleSpinBox
    public :: QSlider
    
    ! Menu widgets
    public :: QMenuBar, QMenu, QAction
    
    ! Container widgets
    public :: QGroupBox, QTabWidget
    
    ! Item views
    public :: QListView, QListWidget
    
    ! Dialogs
    public :: QMessageBox
    public :: show_information, show_warning, show_error, show_question
    
    ! Other widgets
    public :: QStatusBar
    
    ! Layouts
    public :: forge_layout_base, forge_grid_layout, forge_box_layout
    public :: LAYOUT_HORIZONTAL, LAYOUT_VERTICAL
    
    ! Types
    public :: forge_color, forge_size, forge_position, forge_rect
    
    ! Qt-style type aliases for convenience
    public :: QApplication, QPushButton, QLabel, QLineEdit
    public :: QTextEdit, QProgressBar

    !> Qt-style application alias
    type :: QApplication
        type(forge_application) :: app
    contains
        procedure :: init => qapp_init
        procedure :: exec => qapp_exec
        procedure :: run => qapp_run
        procedure :: quit => qapp_quit
    end type QApplication

    ! Widget aliases for Qt naming convention
    type, extends(forge_button) :: QPushButton
    end type QPushButton

    type, extends(forge_label) :: QLabel
    end type QLabel

    type, extends(forge_entry) :: QLineEdit
    end type QLineEdit

    type, extends(forge_text_view) :: QTextEdit
    end type QTextEdit

    type, extends(forge_progress_bar) :: QProgressBar
    end type QProgressBar

    !> ForGE Qt version
    character(len=*), parameter, public :: FORGE_QT_VERSION = "1.0.0-qt-alpha"

contains

    !> @brief Initialize Qt application
    subroutine qapp_init(this, backend_id, args)
        class(QApplication), intent(inout) :: this
        integer, intent(in), optional :: backend_id
        character(len=*), intent(in), optional :: args(:)
        type(forge_status) :: status
        integer :: backend
        
        backend = BACKEND_CUSTOM
        if (present(backend_id)) backend = backend_id
        
        call this%app%init(backend, status)
        if (status%is_error()) then
            call status%print()
        end if
    end subroutine qapp_init

    !> @brief Execute application (alias for run)
    subroutine qapp_exec(this)
        class(QApplication), intent(inout) :: this
        call this%app%run()
    end subroutine qapp_exec

    !> @brief Run application event loop
    subroutine qapp_run(this)
        class(QApplication), intent(inout) :: this
        call this%app%run()
    end subroutine qapp_run

    !> @brief Quit application
    subroutine qapp_quit(this)
        class(QApplication), intent(inout) :: this
        call this%app%shutdown()
    end subroutine qapp_quit

end module forge_qt

