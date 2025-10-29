!> @brief Qt-style accessibility framework for ForGE
!> @details Implements QAccessibleInterface, QAccessibleObject, QAccessibleWidget,
!> and platform-specific accessibility APIs for screen reader support
!> @author ForGE Contributors
!> @date 2025
!> @license GPL-3.0-or-later

module forge_accessibility
    use iso_c_binding
    use forge_types
    use forge_errors
    use forge_qobject
    use forge_signals
    use iso_fortran_env, only: output_unit
    implicit none
    private

    type(QAccessible) :: global_accessible_manager

    public :: QAccessibleInterface
    public :: QAccessibleObject
    public :: QAccessibleWidget
    public :: QAccessible
    public :: QAccessibleEvent
    public :: QAccessibleState
    public :: QAccessibleRole
    public :: QAccessibleRelation
    public :: QAccessibleText
    public :: QAccessibleValue
    public :: QAccessibleAction
    public :: QAccessibleTable
    public :: QAccessibleTableCell

    !> @brief Accessible roles enumeration (Qt-style)
    enum, bind(c)
        enumerator :: QAccessible_NoRole = 0
        enumerator :: QAccessible_TitleBar = 1
        enumerator :: QAccessible_MenuBar = 2
        enumerator :: QAccessible_ScrollBar = 3
        enumerator :: QAccessible_Grip = 4
        enumerator :: QAccessible_Sound = 5
        enumerator :: QAccessible_Cursor = 6
        enumerator :: QAccessible_Caret = 7
        enumerator :: QAccessible_AlertMessage = 8
        enumerator :: QAccessible_Window = 9
        enumerator :: QAccessible_Client = 10
        enumerator :: QAccessible_PopupMenu = 11
        enumerator :: QAccessible_MenuItem = 12
        enumerator :: QAccessible_ToolTip = 13
        enumerator :: QAccessible_Application = 14
        enumerator :: QAccessible_Document = 15
        enumerator :: QAccessible_Pane = 16
        enumerator :: QAccessible_Chart = 17
        enumerator :: QAccessible_Dialog = 18
        enumerator :: QAccessible_Border = 19
        enumerator :: QAccessible_Grouping = 20
        enumerator :: QAccessible_Separator = 21
        enumerator :: QAccessible_ToolBar = 22
        enumerator :: QAccessible_StatusBar = 23
        enumerator :: QAccessible_Table = 24
        enumerator :: QAccessible_ColumnHeader = 25
        enumerator :: QAccessible_RowHeader = 26
        enumerator :: QAccessible_Column = 27
        enumerator :: QAccessible_Row = 28
        enumerator :: QAccessible_Cell = 29
        enumerator :: QAccessible_Link = 30
        enumerator :: QAccessible_HelpBalloon = 31
        enumerator :: QAccessible_Assistant = 32
        enumerator :: QAccessible_List = 33
        enumerator :: QAccessible_ListItem = 34
        enumerator :: QAccessible_Tree = 35
        enumerator :: QAccessible_TreeItem = 36
        enumerator :: QAccessible_PageTab = 37
        enumerator :: QAccessible_PropertyPage = 38
        enumerator :: QAccessible_Indicator = 39
        enumerator :: QAccessible_Graphic = 40
        enumerator :: QAccessible_StaticText = 41
        enumerator :: QAccessible_EditableText = 42
        enumerator :: QAccessible_Button = 43
        enumerator :: QAccessible_CheckBox = 44
        enumerator :: QAccessible_RadioButton = 45
        enumerator :: QAccessible_ComboBox = 46
        enumerator :: QAccessible_DropList = 47
        enumerator :: QAccessible_ProgressBar = 48
        enumerator :: QAccessible_Dial = 49
        enumerator :: QAccessible_HotkeyField = 50
        enumerator :: QAccessible_Slider = 51
        enumerator :: QAccessible_SpinBox = 52
        enumerator :: QAccessible_Canvas = 53
        enumerator :: QAccessible_Animation = 54
        enumerator :: QAccessible_Equation = 55
        enumerator :: QAccessible_ButtonDropDown = 56
        enumerator :: QAccessible_ButtonMenu = 57
        enumerator :: QAccessible_ButtonDropGrid = 58
        enumerator :: QAccessible_Whitespace = 59
        enumerator :: QAccessible_PageTabList = 60
        enumerator :: QAccessible_Clock = 61
        enumerator :: QAccessible_Splitter = 62
        enumerator :: QAccessible_LayeredPane = 63
        enumerator :: QAccessible_WebDocument = 64
        enumerator :: QAccessible_Paragraph = 65
        enumerator :: QAccessible_Section = 66
        enumerator :: QAccessible_ColorChooser = 67
        enumerator :: QAccessible_Footer = 68
        enumerator :: QAccessible_Form = 69
        enumerator :: QAccessible_Heading = 70
        enumerator :: QAccessible_Header = 71
        enumerator :: QAccessible_Heading1 = 72
        enumerator :: QAccessible_Heading2 = 73
        enumerator :: QAccessible_Heading3 = 74
        enumerator :: QAccessible_Heading4 = 75
        enumerator :: QAccessible_Heading5 = 76
        enumerator :: QAccessible_Heading6 = 77
        enumerator :: QAccessible_ComplementaryContent = 78
        enumerator :: QAccessible_UserRole = 0x00010000
    end enum

    !> @brief Accessible states enumeration (Qt-style)
    enum, bind(c)
        enumerator :: QAccessible_NoState = 0
        enumerator :: QAccessible_Unavailable = 0x1
        enumerator :: QAccessible_Selected = 0x2
        enumerator :: QAccessible_Focused = 0x4
        enumerator :: QAccessible_Pressed = 0x8
        enumerator :: QAccessible_Checked = 0x10
        enumerator :: QAccessible_Mixed = 0x20
        enumerator :: QAccessible_ReadOnly = 0x40
        enumerator :: QAccessible_HotTracked = 0x80
        enumerator :: QAccessible_DefaultButton = 0x100
        enumerator :: QAccessible_Expanded = 0x200
        enumerator :: QAccessible_Collapsed = 0x400
        enumerator :: QAccessible_Busy = 0x800
        enumerator :: QAccessible_Marqueed = 0x1000
        enumerator :: QAccessible_Animated = 0x2000
        enumerator :: QAccessible_Invisible = 0x4000
        enumerator :: QAccessible_Offscreen = 0x8000
        enumerator :: QAccessible_Sizeable = 0x10000
        enumerator :: QAccessible_Movable = 0x20000
        enumerator :: QAccessible_SelfVoicing = 0x40000
        enumerator :: QAccessible_Focusable = 0x80000
        enumerator :: QAccessible_Selectable = 0x100000
        enumerator :: QAccessible_Linked = 0x200000
        enumerator :: QAccessible_Traversed = 0x400000
        enumerator :: QAccessible_MultiSelectable = 0x800000
        enumerator :: QAccessible_ExtSelectable = 0x1000000
        enumerator :: QAccessible_Protected = 0x2000000
        enumerator :: QAccessible_HasPopup = 0x4000000
        enumerator :: QAccessible_Modal = 0x8000000
    end enum

    !> @brief Accessible event types
    enum, bind(c)
        enumerator :: QAccessible_Event_None = 0
        enumerator :: QAccessible_Event_NameChanged = 1
        enumerator :: QAccessible_Event_DescriptionChanged = 2
        enumerator :: QAccessible_Event_StateChanged = 3
        enumerator :: QAccessible_Event_ValueChanged = 4
        enumerator :: QAccessible_Event_BoundsChanged = 5
        enumerator :: QAccessible_Event_SelectionChanged = 6
        enumerator :: QAccessible_Event_LinkedChanged = 7
        enumerator :: QAccessible_Event_VisibleDataChanged = 8
        enumerator :: QAccessible_Event_Invalidated = 9
        enumerator :: QAccessible_Event_TableModelChanged = 10
        enumerator :: QAccessible_Event_Focus = 11
        enumerator :: QAccessible_Event_Selection = 12
        enumerator :: QAccessible_Event_SelectionAdd = 13
        enumerator :: QAccessible_Event_SelectionRemove = 14
        enumerator :: QAccessible_Event_SelectionWithin = 15
        enumerator :: QAccessible_Event_ActionChanged = 16
        enumerator :: QAccessible_Event_PageChanged = 17
        enumerator :: QAccessible_Event_Notification = 18
        enumerator :: QAccessible_Event_User = 0x1000
    end enum

    !> @brief Accessible text properties
    enum, bind(c)
        enumerator :: QAccessible_Text_Name = 0
        enumerator :: QAccessible_Text_Description = 1
        enumerator :: QAccessible_Text_Value = 2
        enumerator :: QAccessible_Text_Help = 3
        enumerator :: QAccessible_Text_Accelerator = 4
        enumerator :: QAccessible_Text_DebugDescription = 5
        enumerator :: QAccessible_Text_UserText = 0x1000
    end enum

    !> @brief Accessible relation types
    enum, bind(c)
        enumerator :: QAccessible_NoRelation = 0
        enumerator :: QAccessible_Self = 1
        enumerator :: QAccessible_Ancestor = 2
        enumerator :: QAccessible_Child = 3
        enumerator :: QAccessible_Descendent = 4
        enumerator :: QAccessible_Sibling = 5
        enumerator :: QAccessible_Up = 6
        enumerator :: QAccessible_Down = 7
        enumerator :: QAccessible_Left = 8
        enumerator :: QAccessible_Right = 9
        enumerator :: QAccessible_Covers = 10
        enumerator :: QAccessible_Covered = 11
        enumerator :: QAccessible_FlowsTo = 12
        enumerator :: QAccessible_FlowsFrom = 13
        enumerator :: QAccessible_Controller = 14
        enumerator :: QAccessible_Controlled = 15
        enumerator :: QAccessible_Label = 16
        enumerator :: QAccessible_Labeled = 17
        enumerator :: QAccessible_Description = 18
        enumerator :: QAccessible_Described = 19
    end enum

    !> @brief Accessible action names
    character(len=*), parameter :: QAccessible_Action_Press = "Press"
    character(len=*), parameter :: QAccessible_Action_Increase = "Increase"
    character(len=*), parameter :: QAccessible_Action_Decrease = "Decrease"
    character(len=*), parameter :: QAccessible_Action_Accept = "Accept"
    character(len=*), parameter :: QAccessible_Action_Cancel = "Cancel"
    character(len=*), parameter :: QAccessible_Action_Select = "Select"
    character(len=*), parameter :: QAccessible_Action_Unselect = "Unselect"
    character(len=*), parameter :: QAccessible_Action_Check = "Check"
    character(len=*), parameter :: QAccessible_Action_Uncheck = "Uncheck"
    character(len=*), parameter :: QAccessible_Action_Open = "Open"
    character(len=*), parameter :: QAccessible_Action_Close = "Close"
    character(len=*), parameter :: QAccessible_Action_Show = "Show"
    character(len=*), parameter :: QAccessible_Action_Hide = "Hide"
    character(len=*), parameter :: QAccessible_Action_ScrollUp = "ScrollUp"
    character(len=*), parameter :: QAccessible_Action_ScrollDown = "ScrollDown"
    character(len=*), parameter :: QAccessible_Action_ScrollLeft = "ScrollLeft"
    character(len=*), parameter :: QAccessible_Action_ScrollRight = "ScrollRight"
    character(len=*), parameter :: QAccessible_Action_ScrollUpPage = "ScrollUpPage"
    character(len=*), parameter :: QAccessible_Action_ScrollDownPage = "ScrollDownPage"
    character(len=*), parameter :: QAccessible_Action_ScrollLeftPage = "ScrollLeftPage"
    character(len=*), parameter :: QAccessible_Action_ScrollRightPage = "ScrollRightPage"
    character(len=*), parameter :: QAccessible_Action_SetFocus = "SetFocus"
    character(len=*), parameter :: QAccessible_Action_Menu = "Menu"

    !> @brief Accessible state structure
    type :: QAccessibleState
        private
        integer(c_int64_t) :: state_flags = 0
    contains
        procedure :: set_flag => accessible_state_set_flag
        procedure :: clear_flag => accessible_state_clear_flag
        procedure :: has_flag => accessible_state_has_flag
        procedure :: get_flags => accessible_state_get_flags
        procedure :: set_flags => accessible_state_set_flags
    end type QAccessibleState

    !> @brief Accessible text interface
    type :: QAccessibleText
        private
        character(len=:), allocatable :: text
        integer :: cursor_position = 0
        integer :: selection_start = -1
        integer :: selection_end = -1
    contains
        procedure :: get_text => accessible_text_get_text
        procedure :: set_text => accessible_text_set_text
        procedure :: get_cursor_position => accessible_text_get_cursor_position
        procedure :: set_cursor_position => accessible_text_set_cursor_position
        procedure :: get_selection => accessible_text_get_selection
        procedure :: set_selection => accessible_text_set_selection
        procedure :: get_character_count => accessible_text_get_character_count
        procedure :: get_character_at => accessible_text_get_character_at
        procedure :: get_word_at => accessible_text_get_word_at
        procedure :: get_line_at => accessible_text_get_line_at
        procedure :: get_attributes => accessible_text_get_attributes
    end type QAccessibleText

    !> @brief Accessible value interface
    type :: QAccessibleValue
        private
        real :: minimum_value = 0.0
        real :: maximum_value = 100.0
        real :: current_value = 0.0
        real :: step_size = 1.0
    contains
        procedure :: get_minimum_value => accessible_value_get_minimum
        procedure :: set_minimum_value => accessible_value_set_minimum
        procedure :: get_maximum_value => accessible_value_get_maximum
        procedure :: set_maximum_value => accessible_value_set_maximum
        procedure :: get_current_value => accessible_value_get_current
        procedure :: set_current_value => accessible_value_set_current
        procedure :: get_step_size => accessible_value_get_step
        procedure :: set_step_size => accessible_value_set_step
    end type QAccessibleValue

    !> @brief Accessible action interface
    type :: QAccessibleAction
        private
        type(forge_string), dimension(:), allocatable :: action_names
        integer :: action_count = 0
    contains
        procedure :: get_action_count => accessible_action_get_count
        procedure :: get_action_name => accessible_action_get_name
        procedure :: add_action => accessible_action_add_action
        procedure :: remove_action => accessible_action_remove_action
        procedure :: do_action => accessible_action_do_action
        procedure :: get_action_description => accessible_action_get_description
        procedure :: get_action_key_binding => accessible_action_get_key_binding
    end type QAccessibleAction

    !> @brief Accessible table interface
    type :: QAccessibleTable
        private
        integer :: row_count = 0
        integer :: column_count = 0
        integer :: selected_row_count = 0
        integer :: selected_column_count = 0
        integer, dimension(:), allocatable :: selected_rows
        integer, dimension(:), allocatable :: selected_columns
    contains
        procedure :: get_row_count => accessible_table_get_row_count
        procedure :: get_column_count => accessible_table_get_column_count
        procedure :: get_selected_row_count => accessible_table_get_selected_row_count
        procedure :: get_selected_column_count => accessible_table_get_selected_column_count
        procedure :: get_selected_rows => accessible_table_get_selected_rows
        procedure :: get_selected_columns => accessible_table_get_selected_columns
        procedure :: select_row => accessible_table_select_row
        procedure :: select_column => accessible_table_select_column
        procedure :: unselect_row => accessible_table_unselect_row
        procedure :: unselect_column => accessible_table_unselect_column
        procedure :: get_row_description => accessible_table_get_row_description
        procedure :: get_column_description => accessible_table_get_column_description
        procedure :: get_row_header => accessible_table_get_row_header
        procedure :: get_column_header => accessible_table_get_column_header
        procedure :: get_cell_at => accessible_table_get_cell_at
        procedure :: get_cell_row => accessible_table_get_cell_row
        procedure :: get_cell_column => accessible_table_get_cell_column
        procedure :: is_row_selected => accessible_table_is_row_selected
        procedure :: is_column_selected => accessible_table_is_column_selected
    end type QAccessibleTable

    !> @brief Accessible table cell interface
    type :: QAccessibleTableCell
        private
        integer :: row = -1
        integer :: column = -1
        integer :: row_span = 1
        integer :: column_span = 1
        logical :: is_selected = .false
    contains
        procedure :: get_row => accessible_table_cell_get_row
        procedure :: get_column => accessible_table_cell_get_column
        procedure :: get_row_span => accessible_table_cell_get_row_span
        procedure :: get_column_span => accessible_table_cell_get_column_span
        procedure :: is_selected => accessible_table_cell_is_selected
        procedure :: set_selected => accessible_table_cell_set_selected
    end type QAccessibleTableCell

    !> @brief Accessible event
    type, extends(forge_event) :: QAccessibleEvent
        private
        integer :: accessible_event_type = QAccessible_Event_None
        class(QAccessibleInterface), pointer :: accessible_object => null()
    contains
        procedure :: get_accessible_event_type => accessible_event_get_type
        procedure :: set_accessible_event_type => accessible_event_set_type
        procedure :: get_accessible_object => accessible_event_get_object
        procedure :: set_accessible_object => accessible_event_set_object
    end type QAccessibleEvent

    !> @brief Base accessible interface (abstract)
    type, abstract :: QAccessibleInterface
        private
        integer :: role = QAccessible_NoRole
        type(QAccessibleState) :: state
        character(len=:), allocatable :: name
        character(len=:), allocatable :: description
        character(len=:), allocatable :: help_text
        character(len=:), allocatable :: accelerator
        type(forge_rect) :: bounding_rect
        class(QAccessibleInterface), pointer :: parent => null()
        type(QAccessibleText) :: text_interface
        type(QAccessibleValue) :: value_interface
        type(QAccessibleAction) :: action_interface
        type(QAccessibleTable) :: table_interface
        logical :: is_valid = .true.
    contains
        ! Core accessible properties
        procedure :: get_role => accessible_get_role
        procedure :: set_role => accessible_set_role
        procedure :: get_state => accessible_get_state
        procedure :: set_state => accessible_set_state
        procedure :: get_name => accessible_get_name
        procedure :: set_name => accessible_set_name
        procedure :: get_description => accessible_get_description
        procedure :: set_description => accessible_set_description
        procedure :: get_help_text => accessible_get_help_text
        procedure :: set_help_text => accessible_set_help_text
        procedure :: get_accelerator => accessible_get_accelerator
        procedure :: set_accelerator => accessible_set_accelerator
        procedure :: get_bounding_rect => accessible_get_bounding_rect
        procedure :: set_bounding_rect => accessible_set_bounding_rect
        procedure :: get_parent => accessible_get_parent
        procedure :: set_parent => accessible_set_parent

        ! Navigation
        procedure :: get_child_count => accessible_get_child_count
        procedure :: get_child => accessible_get_child
        procedure :: get_child_at => accessible_get_child_at
        procedure :: get_index_in_parent => accessible_get_index_in_parent
        procedure :: navigate => accessible_navigate

        ! Text interface
        procedure :: text => accessible_text
        procedure :: set_text => accessible_set_text

        ! Value interface
        procedure :: value => accessible_value
        procedure :: set_value => accessible_set_value

        ! Action interface
        procedure :: action => accessible_action
        procedure :: set_action => accessible_set_action

        ! Table interface
        procedure :: table => accessible_table
        procedure :: set_table => accessible_set_table

        ! Table cell interface
        procedure :: table_cell => accessible_table_cell
        procedure :: set_table_cell => accessible_set_table_cell

        ! Utility methods
        procedure :: is_valid_interface => accessible_is_valid
        procedure :: set_valid => accessible_set_valid
        procedure :: role_string => accessible_role_string
        procedure :: state_string => accessible_state_string

        ! Virtual methods to be overridden
        procedure(accessible_child_count), deferred :: child_count
        procedure(accessible_child), deferred :: child
        procedure(accessible_child_at), deferred :: child_at
        procedure(accessible_index_in_parent), deferred :: index_in_parent
        procedure(accessible_navigate_impl), deferred :: navigate_impl
        procedure(accessible_do_action), deferred :: do_action
        procedure(accessible_key_bindings_for_action), deferred :: key_bindings_for_action
    end type QAccessibleInterface

    !> @brief QObject-based accessible implementation
    type, extends(QAccessibleInterface) :: QAccessibleObject
        private
        class(forge_qobject), pointer :: object => null()
    contains
        procedure :: init => accessible_object_init
        procedure :: get_qobject => accessible_object_get_qobject
        procedure :: set_qobject => accessible_object_set_qobject
        procedure :: child_count => accessible_object_child_count
        procedure :: child => accessible_object_child
        procedure :: child_at => accessible_object_child_at
        procedure :: index_in_parent => accessible_object_index_in_parent
        procedure :: navigate_impl => accessible_object_navigate_impl
        procedure :: do_action => accessible_object_do_action
        procedure :: key_bindings_for_action => accessible_object_key_bindings_for_action
        procedure :: get_previous_sibling => accessible_object_get_previous_sibling
        procedure :: get_next_sibling => accessible_object_get_next_sibling
    end type QAccessibleObject

    !> @brief Widget-specific accessible implementation
    type, extends(QAccessibleObject) :: QAccessibleWidget
        private
        logical :: focusable = .false.
        logical :: has_focus = .false.
        integer :: widget_role = QAccessible_NoRole
    contains
        procedure :: init_widget => accessible_widget_init
        procedure :: set_focusable => accessible_widget_set_focusable
        procedure :: is_focusable => accessible_widget_is_focusable
        procedure :: set_focus => accessible_widget_set_focus
        procedure :: has_focus => accessible_widget_has_focus
        procedure :: grab_focus => accessible_widget_grab_focus
        procedure :: child_count => accessible_widget_child_count
        procedure :: child => accessible_widget_child
        procedure :: child_at => accessible_widget_child_at
        procedure :: navigate_impl => accessible_widget_navigate_impl
    end type QAccessibleWidget

    !> @brief Main accessibility manager
    type :: QAccessible
        private
        type(QAccessibleInterface), dimension(:), allocatable :: accessible_objects
        integer :: object_count = 0
        logical :: initialized = .false.
        type(QAccessible_Windows_UIA) :: windows_uia
        type(QAccessible_Linux_AT_SPI) :: linux_atspi
        type(QAccessible_Mac_NSAccessibility) :: mac_nsaccessibility
    contains
        procedure :: initialize => accessible_initialize
        procedure :: shutdown => accessible_shutdown
        procedure :: register_object => accessible_register_object
        procedure :: unregister_object => accessible_unregister_object
        procedure :: query_accessible_interface => accessible_query_interface
        procedure :: update_accessibility => accessible_update_accessibility
        procedure :: notify_event => accessible_notify_event
        procedure :: set_root_object => accessible_set_root_object
        procedure :: get_root_object => accessible_get_root_object
        procedure :: cleanup_invalid_objects => accessible_cleanup_invalid_objects
    end type QAccessible

    ! Abstract interface procedures
    abstract interface
        function accessible_child_count(this) result(count)
            import :: QAccessibleInterface
            class(QAccessibleInterface), intent(in) :: this
            integer :: count
        end function accessible_child_count

        function accessible_child(this, index) result(child)
            import :: QAccessibleInterface
            class(QAccessibleInterface), intent(in) :: this
            integer, intent(in) :: index
            class(QAccessibleInterface), pointer :: child
        end function accessible_child

        function accessible_child_at(this, x, y) result(child)
            import :: QAccessibleInterface
            class(QAccessibleInterface), intent(in) :: this
            integer, intent(in) :: x, y
            class(QAccessibleInterface), pointer :: child
        end function accessible_child_at

        function accessible_index_in_parent(this) result(index)
            import :: QAccessibleInterface
            class(QAccessibleInterface), intent(in) :: this
            integer :: index
        end function accessible_index_in_parent

        function accessible_navigate_impl(this, relation) result(target)
            import :: QAccessibleInterface
            class(QAccessibleInterface), intent(in) :: this
            integer, intent(in) :: relation
            class(QAccessibleInterface), pointer :: target
        end function accessible_navigate_impl

        subroutine accessible_do_action(this, action_name)
            import :: QAccessibleInterface
            class(QAccessibleInterface), intent(inout) :: this
            character(len=*), intent(in) :: action_name
        end subroutine accessible_do_action

        function accessible_key_bindings_for_action(this, action_name) result(key_bindings)
            import :: QAccessibleInterface
            class(QAccessibleInterface), intent(in) :: this
            character(len=*), intent(in) :: action_name
            type(forge_string), dimension(:), allocatable :: key_bindings
        end function accessible_key_bindings_for_action
    end interface

contains

    ! ========== QAccessibleState Implementation ==========

    subroutine accessible_state_set_flag(this, flag)
        class(QAccessibleState), intent(inout) :: this
        integer, intent(in) :: flag
        this%state_flags = ior(this%state_flags, int(flag, c_int64_t))
    end subroutine accessible_state_set_flag

    subroutine accessible_state_clear_flag(this, flag)
        class(QAccessibleState), intent(inout) :: this
        integer, intent(in) :: flag
        this%state_flags = iand(this%state_flags, not(int(flag, c_int64_t)))
    end subroutine accessible_state_clear_flag

    function accessible_state_has_flag(this, flag) result(has)
        class(QAccessibleState), intent(in) :: this
        integer, intent(in) :: flag
        logical :: has
        has = iand(this%state_flags, int(flag, c_int64_t)) /= 0
    end function accessible_state_has_flag

    function accessible_state_get_flags(this) result(flags)
        class(QAccessibleState), intent(in) :: this
        integer(c_int64_t) :: flags
        flags = this%state_flags
    end function accessible_state_get_flags

    subroutine accessible_state_set_flags(this, flags)
        class(QAccessibleState), intent(inout) :: this
        integer(c_int64_t), intent(in) :: flags
        this%state_flags = flags
    end subroutine accessible_state_set_flags

    ! ========== QAccessibleText Implementation ==========

    function accessible_text_get_text(this, start_offset, end_offset) result(text)
        class(QAccessibleText), intent(in) :: this
        integer, intent(in), optional :: start_offset, end_offset
        character(len=:), allocatable :: text

        if (.not. allocated(this%text)) then
            text = ""
            return
        end if

        if (.not. present(start_offset)) then
            text = this%text
        else if (.not. present(end_offset)) then
            if (start_offset >= 1 .and. start_offset <= len(this%text)) then
                text = this%text(start_offset:)
            else
                text = ""
            end if
        else
            if (start_offset >= 1 .and. end_offset >= start_offset .and. end_offset <= len(this%text)) then
                text = this%text(start_offset:end_offset)
            else
                text = ""
            end if
        end if
    end function accessible_text_get_text

    subroutine accessible_text_set_text(this, text)
        class(QAccessibleText), intent(inout) :: this
        character(len=*), intent(in) :: text
        this%text = text
    end subroutine accessible_text_set_text

    function accessible_text_get_cursor_position(this) result(position)
        class(QAccessibleText), intent(in) :: this
        integer :: position
        position = this%cursor_position
    end function accessible_text_get_cursor_position

    subroutine accessible_text_set_cursor_position(this, position)
        class(QAccessibleText), intent(inout) :: this
        integer, intent(in) :: position
        this%cursor_position = max(0, min(position, len(this%text)))
    end subroutine accessible_text_set_cursor_position

    subroutine accessible_text_get_selection(this, start_offset, end_offset)
        class(QAccessibleText), intent(out) :: this
        integer, intent(out) :: start_offset, end_offset
        start_offset = this%selection_start
        end_offset = this%selection_end
    end subroutine accessible_text_get_selection

    subroutine accessible_text_set_selection(this, start_offset, end_offset)
        class(QAccessibleText), intent(inout) :: this
        integer, intent(in) :: start_offset, end_offset
        this%selection_start = max(0, min(start_offset, len(this%text)))
        this%selection_end = max(0, min(end_offset, len(this%text)))
    end subroutine accessible_text_set_selection

    function accessible_text_get_character_count(this) result(count)
        class(QAccessibleText), intent(in) :: this
        integer :: count
        if (allocated(this%text)) then
            count = len(this%text)
        else
            count = 0
        end if
    end function accessible_text_get_character_count

    function accessible_text_get_character_at(this, offset) result(character)
        class(QAccessibleText), intent(in) :: this
        integer, intent(in) :: offset
        character(len=1) :: character

        if (allocated(this%text) .and. offset >= 1 .and. offset <= len(this%text)) then
            character = this%text(offset:offset)
        else
            character = char(0)
        end if
    end function accessible_text_get_character_at

    function accessible_text_get_word_at(this, offset) result(word)
        class(QAccessibleText), intent(in) :: this
        integer, intent(in) :: offset
        character(len=:), allocatable :: word
        integer :: start_pos, end_pos, i

        if (.not. allocated(this%text) .or. offset < 1 .or. offset > len(this%text)) then
            word = ""
            return
        end if

        ! Find word boundaries (simplified - considers spaces as word separators)
        start_pos = offset
        do i = offset, 1, -1
            if (this%text(i:i) == ' ') then
                start_pos = i + 1
                exit
            end if
        end do

        end_pos = offset
        do i = offset, len(this%text)
            if (this%text(i:i) == ' ') then
                end_pos = i - 1
                exit
            end if
        end do

        if (start_pos <= end_pos) then
            word = this%text(start_pos:end_pos)
        else
            word = ""
        end if
    end function accessible_text_get_word_at

    function accessible_text_get_line_at(this, offset) result(line)
        class(QAccessibleText), intent(in) :: this
        integer, intent(in) :: offset
        character(len=:), allocatable :: line
        integer :: start_pos, end_pos, i

        if (.not. allocated(this%text) .or. offset < 1 .or. offset > len(this%text)) then
            line = ""
            return
        end if

        ! Find line boundaries (simplified - considers newlines as line separators)
        start_pos = offset
        do i = offset, 1, -1
            if (this%text(i:i) == new_line('a')) then
                start_pos = i + 1
                exit
            end if
        end do

        end_pos = offset
        do i = offset, len(this%text)
            if (this%text(i:i) == new_line('a')) then
                end_pos = i - 1
                exit
            end if
        end do

        if (start_pos <= end_pos) then
            line = this%text(start_pos:end_pos)
        else
            line = ""
        end if
    end function accessible_text_get_line_at

    function accessible_text_get_attributes(this, offset, start_offset, end_offset) result(attributes)
        class(QAccessibleText), intent(in) :: this
        integer, intent(in) :: offset
        integer, intent(out) :: start_offset, end_offset
        character(len=:), allocatable :: attributes

        ! Simplified implementation - no text attributes for now
        start_offset = offset
        end_offset = offset
        attributes = ""
    end function accessible_text_get_attributes

    ! ========== QAccessibleValue Implementation ==========

    function accessible_value_get_minimum(this) result(value)
        class(QAccessibleValue), intent(in) :: this
        real :: value
        value = this%minimum_value
    end function accessible_value_get_minimum

    subroutine accessible_value_set_minimum(this, value)
        class(QAccessibleValue), intent(inout) :: this
        real, intent(in) :: value
        this%minimum_value = value
    end subroutine accessible_value_set_minimum

    function accessible_value_get_maximum(this) result(value)
        class(QAccessibleValue), intent(in) :: this
        real :: value
        value = this%maximum_value
    end function accessible_value_get_maximum

    subroutine accessible_value_set_maximum(this, value)
        class(QAccessibleValue), intent(inout) :: this
        real, intent(in) :: value
        this%maximum_value = value
    end subroutine accessible_value_set_maximum

    function accessible_value_get_current(this) result(value)
        class(QAccessibleValue), intent(in) :: this
        real :: value
        value = this%current_value
    end function accessible_value_get_current

    subroutine accessible_value_set_current(this, value)
        class(QAccessibleValue), intent(inout) :: this
        real, intent(in) :: value
        this%current_value = max(this%minimum_value, min(value, this%maximum_value))
    end subroutine accessible_value_set_current

    function accessible_value_get_step(this) result(value)
        class(QAccessibleValue), intent(in) :: this
        real :: value
        value = this%step_size
    end function accessible_value_get_step

    subroutine accessible_value_set_step(this, value)
        class(QAccessibleValue), intent(inout) :: this
        real, intent(in) :: value
        this%step_size = max(0.0, value)
    end subroutine accessible_value_set_step

    ! ========== QAccessibleAction Implementation ==========

    function accessible_action_get_count(this) result(count)
        class(QAccessibleAction), intent(in) :: this
        integer :: count
        count = this%action_count
    end function accessible_action_get_count

    function accessible_action_get_name(this, index) result(name)
        class(QAccessibleAction), intent(in) :: this
        integer, intent(in) :: index
        character(len=:), allocatable :: name

        if (index >= 1 .and. index <= this%action_count) then
            name = this%action_names(index)%get()
        else
            name = ""
        end if
    end function accessible_action_get_name

    subroutine accessible_action_add_action(this, name)
        class(QAccessibleAction), intent(inout) :: this
        character(len=*), intent(in) :: name
        type(forge_string), dimension(:), allocatable :: temp

        if (.not. allocated(this%action_names)) then
            allocate(this%action_names(1))
        else
            allocate(temp(size(this%action_names) + 1))
            temp(1:size(this%action_names)) = this%action_names
            call move_alloc(temp, this%action_names)
        end if

        call this%action_names(this%action_count + 1)%set(name)
        this%action_count = this%action_count + 1
    end subroutine accessible_action_add_action

    subroutine accessible_action_remove_action(this, name)
        class(QAccessibleAction), intent(inout) :: this
        character(len=*), intent(in) :: name
        integer :: i, j
        type(forge_string), dimension(:), allocatable :: temp

        do i = 1, this%action_count
            if (this%action_names(i)%get() == name) then
                ! Remove this action
                if (this%action_count > 1) then
                    allocate(temp(this%action_count - 1))
                    temp(1:i-1) = this%action_names(1:i-1)
                    temp(i:) = this%action_names(i+1:)
                    call move_alloc(temp, this%action_names)
                else
                    deallocate(this%action_names)
                end if
                this%action_count = this%action_count - 1
                exit
            end if
        end do
    end subroutine accessible_action_remove_action

    subroutine accessible_action_do_action(this, action_name)
        class(QAccessibleAction), intent(inout) :: this
        character(len=*), intent(in) :: action_name
        ! Default implementation - override in subclasses
    end subroutine accessible_action_do_action

    function accessible_action_get_description(this, action_name) result(description)
        class(QAccessibleAction), intent(in) :: this
        character(len=*), intent(in) :: action_name
        character(len=:), allocatable :: description

        ! Default descriptions for standard actions
        select case (action_name)
        case (QAccessible_Action_Press)
            description = "Press the button"
        case (QAccessible_Action_Increase)
            description = "Increase the value"
        case (QAccessible_Action_Decrease)
            description = "Decrease the value"
        case (QAccessible_Action_Accept)
            description = "Accept the current selection"
        case (QAccessible_Action_Cancel)
            description = "Cancel the current operation"
        case (QAccessible_Action_Select)
            description = "Select the item"
        case (QAccessible_Action_Unselect)
            description = "Unselect the item"
        case (QAccessible_Action_Check)
            description = "Check the item"
        case (QAccessible_Action_Uncheck)
            description = "Uncheck the item"
        case (QAccessible_Action_Open)
            description = "Open the item"
        case (QAccessible_Action_Close)
            description = "Close the item"
        case (QAccessible_Action_Show)
            description = "Show the item"
        case (QAccessible_Action_Hide)
            description = "Hide the item"
        case (QAccessible_Action_SetFocus)
            description = "Set focus to the item"
        case (QAccessible_Action_Menu)
            description = "Show the context menu"
        case default
            description = "Perform action: " // action_name
        end select
    end function accessible_action_get_description

    function accessible_action_get_key_binding(this, action_name) result(key_bindings)
        class(QAccessibleAction), intent(in) :: this
        character(len=*), intent(in) :: action_name
        type(forge_string), dimension(:), allocatable :: key_bindings

        ! Default key bindings for standard actions
        select case (action_name)
        case (QAccessible_Action_Press)
            allocate(key_bindings(1))
            call key_bindings(1)%set("Space")
        case (QAccessible_Action_Accept)
            allocate(key_bindings(2))
            call key_bindings(1)%set("Enter")
            call key_bindings(2)%set("Return")
        case (QAccessible_Action_Cancel)
            allocate(key_bindings(1))
            call key_bindings(1)%set("Escape")
        case (QAccessible_Action_SetFocus)
            allocate(key_bindings(1))
            call key_bindings(1)%set("Tab")
        case default
            allocate(key_bindings(0))  ! No default key bindings
        end select
    end function accessible_action_get_key_binding

    ! ========== QAccessibleTable Implementation ==========

    function accessible_table_get_row_count(this) result(count)
        class(QAccessibleTable), intent(in) :: this
        integer :: count
        count = this%row_count
    end function accessible_table_get_row_count

    function accessible_table_get_column_count(this) result(count)
        class(QAccessibleTable), intent(in) :: this
        integer :: count
        count = this%column_count
    end function accessible_table_get_column_count

    function accessible_table_get_selected_row_count(this) result(count)
        class(QAccessibleTable), intent(in) :: this
        integer :: count
        count = this%selected_row_count
    end function accessible_table_get_selected_row_count

    function accessible_table_get_selected_column_count(this) result(count)
        class(QAccessibleTable), intent(in) :: this
        integer :: count
        count = this%selected_column_count
    end function accessible_table_get_selected_column_count

    function accessible_table_get_selected_rows(this) result(rows)
        class(QAccessibleTable), intent(in) :: this
        integer, dimension(:), allocatable :: rows
        if (allocated(this%selected_rows)) then
            rows = this%selected_rows
        else
            allocate(rows(0))
        end if
    end function accessible_table_get_selected_rows

    function accessible_table_get_selected_columns(this) result(columns)
        class(QAccessibleTable), intent(in) :: this
        integer, dimension(:), allocatable :: columns
        if (allocated(this%selected_columns)) then
            columns = this%selected_columns
        else
            allocate(columns(0))
        end if
    end function accessible_table_get_selected_columns

    subroutine accessible_table_select_row(this, row)
        class(QAccessibleTable), intent(inout) :: this
        integer, intent(in) :: row
        integer, dimension(:), allocatable :: temp

        if (row < 0 .or. row >= this%row_count) return

        if (.not. allocated(this%selected_rows)) then
            allocate(this%selected_rows(1))
            this%selected_rows(1) = row
        else
            ! Check if already selected
            if (any(this%selected_rows == row)) return

            allocate(temp(size(this%selected_rows) + 1))
            temp(1:size(this%selected_rows)) = this%selected_rows
            temp(size(temp)) = row
            call move_alloc(temp, this%selected_rows)
        end if
        this%selected_row_count = this%selected_row_count + 1
    end subroutine accessible_table_select_row

    subroutine accessible_table_select_column(this, column)
        class(QAccessibleTable), intent(inout) :: this
        integer, intent(in) :: column
        integer, dimension(:), allocatable :: temp

        if (column < 0 .or. column >= this%column_count) return

        if (.not. allocated(this%selected_columns)) then
            allocate(this%selected_columns(1))
            this%selected_columns(1) = column
        else
            ! Check if already selected
            if (any(this%selected_columns == column)) return

            allocate(temp(size(this%selected_columns) + 1))
            temp(1:size(this%selected_columns)) = this%selected_columns
            temp(size(temp)) = column
            call move_alloc(temp, this%selected_columns)
        end if
        this%selected_column_count = this%selected_column_count + 1
    end subroutine accessible_table_select_column

    subroutine accessible_table_unselect_row(this, row)
        class(QAccessibleTable), intent(inout) :: this
        integer, intent(in) :: row
        integer, dimension(:), allocatable :: temp
        integer :: i, j

        if (.not. allocated(this%selected_rows)) return

        do i = 1, size(this%selected_rows)
            if (this%selected_rows(i) == row) then
                if (size(this%selected_rows) > 1) then
                    allocate(temp(size(this%selected_rows) - 1))
                    temp(1:i-1) = this%selected_rows(1:i-1)
                    temp(i:) = this%selected_rows(i+1:)
                    call move_alloc(temp, this%selected_rows)
                else
                    deallocate(this%selected_rows)
                end if
                this%selected_row_count = this%selected_row_count - 1
                exit
            end if
        end do
    end subroutine accessible_table_unselect_row

    subroutine accessible_table_unselect_column(this, column)
        class(QAccessibleTable), intent(inout) :: this
        integer, intent(in) :: column
        integer, dimension(:), allocatable :: temp
        integer :: i

        if (.not. allocated(this%selected_columns)) return

        do i = 1, size(this%selected_columns)
            if (this%selected_columns(i) == column) then
                if (size(this%selected_columns) > 1) then
                    allocate(temp(size(this%selected_columns) - 1))
                    temp(1:i-1) = this%selected_columns(1:i-1)
                    temp(i:) = this%selected_columns(i+1:)
                    call move_alloc(temp, this%selected_columns)
                else
                    deallocate(this%selected_columns)
                end if
                this%selected_column_count = this%selected_column_count - 1
                exit
            end if
        end do
    end subroutine accessible_table_unselect_column

    function accessible_table_get_row_description(this, row) result(description)
        class(QAccessibleTable), intent(in) :: this
        integer, intent(in) :: row
        character(len=:), allocatable :: description
        description = "Row " // trim(adjustl(forge_string_from_int(row + 1)))
    end function accessible_table_get_row_description

    function accessible_table_get_column_description(this, column) result(description)
        class(QAccessibleTable), intent(in) :: this
        integer, intent(in) :: column
        character(len=:), allocatable :: description
        description = "Column " // trim(adjustl(forge_string_from_int(column + 1)))
    end function accessible_table_get_column_description

    function accessible_table_get_row_header(this) result(header)
        class(QAccessibleTable), intent(in) :: this
        class(QAccessibleInterface), pointer :: header
        allocate(QAccessibleObject :: header)
        call header%set_role(QAccessible_RowHeader)
        call header%set_name("Row Header")
    end function accessible_table_get_row_header

    function accessible_table_get_column_header(this) result(header)
        class(QAccessibleTable), intent(in) :: this
        class(QAccessibleInterface), pointer :: header
        allocate(QAccessibleObject :: header)
        call header%set_role(QAccessible_ColumnHeader)
        call header%set_name("Column Header")
    end function accessible_table_get_column_header

    function accessible_table_get_cell_at(this, row, column) result(cell)
        class(QAccessibleTable), intent(in) :: this
        integer, intent(in) :: row, column
        class(QAccessibleInterface), pointer :: cell
        if (row < 0 .or. row >= this%row_count .or. column < 0 .or. column >= this%column_count) then
            cell => null()
            return
        end if
        allocate(QAccessibleObject :: cell)
        call cell%set_role(QAccessible_Cell)
        call cell%set_name("Cell (" // forge_string_from_int(row) // "," // forge_string_from_int(column) // ")")
        type(QAccessibleTableCell) :: cell_data
        cell_data%row = row
        cell_data%column = column
        cell_data%row_span = 1
        cell_data%column_span = 1
        cell_data%is_selected = .false.
        call cell%set_table_cell(cell_data)
    end function accessible_table_get_cell_at

    function accessible_table_get_cell_row(this, cell) result(row)
        class(QAccessibleInterface), intent(in) :: cell
        integer :: row
        if (associated(cell%table_cell)) then
            row = cell%table_cell%get_row()
        else
            row = -1
        end if
    end function accessible_table_get_cell_row

    function accessible_table_get_cell_column(this, cell) result(column)
        class(QAccessibleTable), intent(in) :: this
        class(QAccessibleInterface), intent(in) :: cell
        integer :: column
        if (associated(cell%table_cell)) then
            column = cell%table_cell%get_column()
        else
            column = -1
        end if
    end function accessible_table_get_cell_column

    function accessible_table_is_row_selected(this, row) result(selected)
        class(QAccessibleTable), intent(in) :: this
        integer, intent(in) :: row
        logical :: selected
        if (allocated(this%selected_rows)) then
            selected = any(this%selected_rows == row)
        else
            selected = .false.
        end if
    end function accessible_table_is_row_selected

    function accessible_table_is_column_selected(this, column) result(selected)
        class(QAccessibleTable), intent(in) :: this
        integer, intent(in) :: column
        logical :: selected
        if (allocated(this%selected_columns)) then
            selected = any(this%selected_columns == column)
        else
            selected = .false.
        end if
    end function accessible_table_is_column_selected

    ! ========== QAccessibleTableCell Implementation ==========

    function accessible_table_cell_get_row(this) result(row)
        class(QAccessibleTableCell), intent(in) :: this
        integer :: row
        row = this%row
    end function accessible_table_cell_get_row

    function accessible_table_cell_get_column(this) result(column)
        class(QAccessibleTableCell), intent(in) :: this
        integer :: column
        column = this%column
    end function accessible_table_cell_get_column

    function accessible_table_cell_get_row_span(this) result(span)
        class(QAccessibleTableCell), intent(in) :: this
        integer :: span
        span = this%row_span
    end function accessible_table_cell_get_row_span

    function accessible_table_cell_get_column_span(this) result(span)
        class(QAccessibleTableCell), intent(in) :: this
        integer :: span
        span = this%column_span
    end function accessible_table_cell_get_column_span

    function accessible_table_cell_is_selected(this) result(selected)
        class(QAccessibleTableCell), intent(in) :: this
        logical :: selected
        selected = this%is_selected
    end function accessible_table_cell_is_selected

    subroutine accessible_table_cell_set_selected(this, selected)
        class(QAccessibleTableCell), intent(inout) :: this
        logical, intent(in) :: selected
        this%is_selected = selected
    end subroutine accessible_table_cell_set_selected

    ! ========== QAccessibleEvent Implementation ==========

    function accessible_event_get_type(this) result(event_type)
        class(QAccessibleEvent), intent(in) :: this
        integer :: event_type
        event_type = this%accessible_event_type
    end function accessible_event_get_type

    subroutine accessible_event_set_type(this, event_type)
        class(QAccessibleEvent), intent(inout) :: this
        integer, intent(in) :: event_type
        this%accessible_event_type = event_type
    end subroutine accessible_event_set_type

    function accessible_event_get_object(this) result(obj)
        class(QAccessibleEvent), intent(in) :: this
        class(QAccessibleInterface), pointer :: obj
        obj => this%accessible_object
    end function accessible_event_get_object

    subroutine accessible_event_set_object(this, obj)
        class(QAccessibleEvent), intent(inout) :: this
        class(QAccessibleInterface), target :: obj
        this%accessible_object => obj
    end subroutine accessible_event_set_object

    ! ========== QAccessibleInterface Implementation ==========

    function accessible_get_role(this) result(role)
        class(QAccessibleInterface), intent(in) :: this
        integer :: role
        role = this%role
    end function accessible_get_role

    subroutine accessible_set_role(this, role)
        class(QAccessibleInterface), intent(inout) :: this
        integer, intent(in) :: role
        this%role = role
    end subroutine accessible_set_role

    function accessible_get_state(this) result(state)
        class(QAccessibleInterface), intent(in) :: this
        type(QAccessibleState) :: state
        state = this%state
    end function accessible_get_state

    subroutine accessible_set_state(this, state)
        class(QAccessibleInterface), intent(inout) :: this
        type(QAccessibleState), intent(in) :: state
        this%state = state
    end subroutine accessible_set_state

    function accessible_get_name(this) result(name)
        class(QAccessibleInterface), intent(in) :: this
        character(len=:), allocatable :: name
        if (allocated(this%name)) then
            name = this%name
        else
            name = ""
        end if
    end function accessible_get_name

    subroutine accessible_set_name(this, name)
        class(QAccessibleInterface), intent(inout) :: this
        character(len=*), intent(in) :: name
        this%name = name
    end subroutine accessible_set_name

    function accessible_get_description(this) result(description)
        class(QAccessibleInterface), intent(in) :: this
        character(len=:), allocatable :: description
        if (allocated(this%description)) then
            description = this%description
        else
            description = ""
        end if
    end function accessible_get_description

    subroutine accessible_set_description(this, description)
        class(QAccessibleInterface), intent(inout) :: this
        character(len=*), intent(in) :: description
        this%description = description
    end subroutine accessible_set_description

    function accessible_get_help_text(this) result(help_text)
        class(QAccessibleInterface), intent(in) :: this
        character(len=:), allocatable :: help_text
        if (allocated(this%help_text)) then
            help_text = this%help_text
        else
            help_text = ""
        end if
    end function accessible_get_help_text

    subroutine accessible_set_help_text(this, help_text)
        class(QAccessibleInterface), intent(inout) :: this
        character(len=*), intent(in) :: help_text
        this%help_text = help_text
    end subroutine accessible_set_help_text

    function accessible_get_accelerator(this) result(accelerator)
        class(QAccessibleInterface), intent(in) :: this
        character(len=:), allocatable :: accelerator
        if (allocated(this%accelerator)) then
            accelerator = this%accelerator
        else
            accelerator = ""
        end if
    end function accessible_get_accelerator

    subroutine accessible_set_accelerator(this, accelerator)
        class(QAccessibleInterface), intent(inout) :: this
        character(len=*), intent(in) :: accelerator
        this%accelerator = accelerator
    end subroutine accessible_set_accelerator

    function accessible_get_bounding_rect(this) result(rect)
        class(QAccessibleInterface), intent(in) :: this
        type(forge_rect) :: rect
        rect = this%bounding_rect
    end function accessible_get_bounding_rect

    subroutine accessible_set_bounding_rect(this, rect)
        class(QAccessibleInterface), intent(inout) :: this
        type(forge_rect), intent(in) :: rect
        this%bounding_rect = rect
    end subroutine accessible_set_bounding_rect

    function accessible_get_parent(this) result(parent)
        class(QAccessibleInterface), intent(in) :: this
        class(QAccessibleInterface), pointer :: parent
        parent => this%parent
    end function accessible_get_parent

    subroutine accessible_set_parent(this, parent)
        class(QAccessibleInterface), intent(inout) :: this
        class(QAccessibleInterface), target, optional :: parent
        if (present(parent)) then
            this%parent => parent
        else
            this%parent => null()
        end if
    end subroutine accessible_set_parent

    function accessible_get_child_count(this) result(count)
        class(QAccessibleInterface), intent(in) :: this
        integer :: count
        count = this%child_count()
    end function accessible_get_child_count

    function accessible_get_child(this, index) result(child)
        class(QAccessibleInterface), intent(in) :: this
        integer, intent(in) :: index
        class(QAccessibleInterface), pointer :: child
        child => this%child(index)
    end function accessible_get_child

    function accessible_get_child_at(this, x, y) result(child)
        class(QAccessibleInterface), intent(in) :: this
        integer, intent(in) :: x, y
        class(QAccessibleInterface), pointer :: child
        child => this%child_at(x, y)
    end function accessible_get_child_at

    function accessible_get_index_in_parent(this) result(index)
        class(QAccessibleInterface), intent(in) :: this
        integer :: index
        index = this%index_in_parent()
    end function accessible_get_index_in_parent

    function accessible_navigate(this, relation) result(target)
        class(QAccessibleInterface), intent(in) :: this
        integer, intent(in) :: relation
        class(QAccessibleInterface), pointer :: target
        target => this%navigate_impl(relation)
    end function accessible_navigate

    function accessible_text(this) result(text_interface)
        class(QAccessibleInterface), intent(in) :: this
        type(QAccessibleText) :: text_interface
        text_interface = this%text_interface
    end function accessible_text

    subroutine accessible_set_text(this, text_interface)
        class(QAccessibleInterface), intent(inout) :: this
        type(QAccessibleText), intent(in) :: text_interface
        this%text_interface = text_interface
    end subroutine accessible_set_text

    function accessible_value(this) result(value_interface)
        class(QAccessibleInterface), intent(in) :: this
        type(QAccessibleValue) :: value_interface
        value_interface = this%value_interface
    end function accessible_value

    subroutine accessible_set_value(this, value_interface)
        class(QAccessibleInterface), intent(inout) :: this
        type(QAccessibleValue), intent(in) :: value_interface
        this%value_interface = value_interface
    end subroutine accessible_set_value

    function accessible_action(this) result(action_interface)
        class(QAccessibleInterface), intent(in) :: this
        type(QAccessibleAction) :: action_interface
        action_interface = this%action_interface
    end function accessible_action

    subroutine accessible_set_action(this, action_interface)
        class(QAccessibleInterface), intent(inout) :: this
        type(QAccessibleAction), intent(in) :: action_interface
        this%action_interface = action_interface
    end subroutine accessible_set_action

    function accessible_table(this) result(table_interface)
        class(QAccessibleInterface), intent(in) :: this
        type(QAccessibleTable) :: table_interface
        table_interface = this%table_interface
    end function accessible_table

    subroutine accessible_set_table(this, table_interface)
        class(QAccessibleInterface), intent(inout) :: this
        type(QAccessibleTable), intent(in) :: table_interface
        this%table_interface = table_interface
    end subroutine accessible_set_table

    function accessible_table_cell(this) result(cell_interface)
        class(QAccessibleInterface), intent(in) :: this
        type(QAccessibleTableCell) :: cell_interface
        cell_interface = this%table_cell_interface
    end function accessible_table_cell

    subroutine accessible_set_table_cell(this, cell_interface)
        class(QAccessibleInterface), intent(inout) :: this
        type(QAccessibleTableCell), intent(in) :: cell_interface
        this%table_cell_interface = cell_interface
    end subroutine accessible_set_table_cell

    function accessible_is_valid(this) result(valid)
        class(QAccessibleInterface), intent(in) :: this
        logical :: valid
        valid = this%is_valid
    end function accessible_is_valid

    subroutine accessible_set_valid(this, valid)
        class(QAccessibleInterface), intent(inout) :: this
        logical, intent(in) :: valid
        this%is_valid = valid
    end subroutine accessible_set_valid

    function accessible_role_string(this) result(role_str)
        class(QAccessibleInterface), intent(in) :: this
        character(len=:), allocatable :: role_str

        select case (this%role)
        case (QAccessible_NoRole)
            role_str = "No Role"
        case (QAccessible_TitleBar)
            role_str = "Title Bar"
        case (QAccessible_MenuBar)
            role_str = "Menu Bar"
        case (QAccessible_ScrollBar)
            role_str = "Scroll Bar"
        case (QAccessible_Grip)
            role_str = "Grip"
        case (QAccessible_Sound)
            role_str = "Sound"
        case (QAccessible_Cursor)
            role_str = "Cursor"
        case (QAccessible_Caret)
            role_str = "Caret"
        case (QAccessible_AlertMessage)
            role_str = "Alert Message"
        case (QAccessible_Window)
            role_str = "Window"
        case (QAccessible_Client)
            role_str = "Client"
        case (QAccessible_PopupMenu)
            role_str = "Popup Menu"
        case (QAccessible_MenuItem)
            role_str = "Menu Item"
        case (QAccessible_ToolTip)
            role_str = "Tool Tip"
        case (QAccessible_Application)
            role_str = "Application"
        case (QAccessible_Document)
            role_str = "Document"
        case (QAccessible_Pane)
            role_str = "Pane"
        case (QAccessible_Chart)
            role_str = "Chart"
        case (QAccessible_Dialog)
            role_str = "Dialog"
        case (QAccessible_Border)
            role_str = "Border"
        case (QAccessible_Grouping)
            role_str = "Grouping"
        case (QAccessible_Separator)
            role_str = "Separator"
        case (QAccessible_ToolBar)
            role_str = "Tool Bar"
        case (QAccessible_StatusBar)
            role_str = "Status Bar"
        case (QAccessible_Table)
            role_str = "Table"
        case (QAccessible_ColumnHeader)
            role_str = "Column Header"
        case (QAccessible_RowHeader)
            role_str = "Row Header"
        case (QAccessible_Column)
            role_str = "Column"
        case (QAccessible_Row)
            role_str = "Row"
        case (QAccessible_Cell)
            role_str = "Cell"
        case (QAccessible_Link)
            role_str = "Link"
        case (QAccessible_HelpBalloon)
            role_str = "Help Balloon"
        case (QAccessible_Assistant)
            role_str = "Assistant"
        case (QAccessible_List)
            role_str = "List"
        case (QAccessible_ListItem)
            role_str = "List Item"
        case (QAccessible_Tree)
            role_str = "Tree"
        case (QAccessible_TreeItem)
            role_str = "Tree Item"
        case (QAccessible_PageTab)
            role_str = "Page Tab"
        case (QAccessible_PropertyPage)
            role_str = "Property Page"
        case (QAccessible_Indicator)
            role_str = "Indicator"
        case (QAccessible_Graphic)
            role_str = "Graphic"
        case (QAccessible_StaticText)
            role_str = "Static Text"
        case (QAccessible_EditableText)
            role_str = "Editable Text"
        case (QAccessible_Button)
            role_str = "Button"
        case (QAccessible_CheckBox)
            role_str = "Check Box"
        case (QAccessible_RadioButton)
            role_str = "Radio Button"
        case (QAccessible_ComboBox)
            role_str = "Combo Box"
        case (QAccessible_DropList)
            role_str = "Drop List"
        case (QAccessible_ProgressBar)
            role_str = "Progress Bar"
        case (QAccessible_Dial)
            role_str = "Dial"
        case (QAccessible_HotkeyField)
            role_str = "Hotkey Field"
        case (QAccessible_Slider)
            role_str = "Slider"
        case (QAccessible_SpinBox)
            role_str = "Spin Box"
        case (QAccessible_Canvas)
            role_str = "Canvas"
        case (QAccessible_Animation)
            role_str = "Animation"
        case (QAccessible_Equation)
            role_str = "Equation"
        case (QAccessible_ButtonDropDown)
            role_str = "Button Drop Down"
        case (QAccessible_ButtonMenu)
            role_str = "Button Menu"
        case (QAccessible_ButtonDropGrid)
            role_str = "Button Drop Grid"
        case (QAccessible_Whitespace)
            role_str = "Whitespace"
        case (QAccessible_PageTabList)
            role_str = "Page Tab List"
        case (QAccessible_Clock)
            role_str = "Clock"
        case (QAccessible_Splitter)
            role_str = "Splitter"
        case (QAccessible_LayeredPane)
            role_str = "Layered Pane"
        case (QAccessible_WebDocument)
            role_str = "Web Document"
        case (QAccessible_Paragraph)
            role_str = "Paragraph"
        case (QAccessible_Section)
            role_str = "Section"
        case (QAccessible_ColorChooser)
            role_str = "Color Chooser"
        case (QAccessible_Footer)
            role_str = "Footer"
        case (QAccessible_Form)
            role_str = "Form"
        case (QAccessible_Heading)
            role_str = "Heading"
        case (QAccessible_Header)
            role_str = "Header"
        case (QAccessible_Heading1)
            role_str = "Heading 1"
        case (QAccessible_Heading2)
            role_str = "Heading 2"
        case (QAccessible_Heading3)
            role_str = "Heading 3"
        case (QAccessible_Heading4)
            role_str = "Heading 4"
        case (QAccessible_Heading5)
            role_str = "Heading 5"
        case (QAccessible_Heading6)
            role_str = "Heading 6"
        case (QAccessible_ComplementaryContent)
            role_str = "Complementary Content"
        case default
            role_str = "Unknown Role"
        end select
    end function accessible_role_string

    function accessible_state_string(this) result(state_str)
        class(QAccessibleInterface), intent(in) :: this
        character(len=:), allocatable :: state_str
        integer(c_int64_t) :: flags

        flags = this%state%get_flags()
        state_str = ""

        if (iand(flags, int(QAccessible_Unavailable, c_int64_t)) /= 0) then
            state_str = state_str // "Unavailable "
        end if
        if (iand(flags, int(QAccessible_Selected, c_int64_t)) /= 0) then
            state_str = state_str // "Selected "
        end if
        if (iand(flags, int(QAccessible_Focused, c_int64_t)) /= 0) then
            state_str = state_str // "Focused "
        end if
        if (iand(flags, int(QAccessible_Pressed, c_int64_t)) /= 0) then
            state_str = state_str // "Pressed "
        end if
        if (iand(flags, int(QAccessible_Checked, c_int64_t)) /= 0) then
            state_str = state_str // "Checked "
        end if
        if (iand(flags, int(QAccessible_ReadOnly, c_int64_t)) /= 0) then
            state_str = state_str // "ReadOnly "
        end if
        if (iand(flags, int(QAccessible_Focusable, c_int64_t)) /= 0) then
            state_str = state_str // "Focusable "
        end if

        if (len(state_str) == 0) then
            state_str = "Normal"
        else
            state_str = trim(state_str)
        end if
    end function accessible_state_string

    ! ========== QAccessibleObject Implementation ==========

    subroutine accessible_object_init(this, qobject)
        class(QAccessibleObject), intent(inout) :: this
        class(forge_qobject), target :: qobject

        this%object => qobject
        call this%set_name(qobject%get_object_name())
        call this%set_description("")
        call this%set_help_text("")
        call this%set_role(QAccessible_NoRole)
    end subroutine accessible_object_init

    function accessible_object_get_qobject(this) result(qobject)
        class(QAccessibleObject), intent(in) :: this
        class(forge_qobject), pointer :: qobject
        qobject => this%object
    end function accessible_object_get_qobject

    subroutine accessible_object_set_qobject(this, qobject)
        class(QAccessibleObject), intent(inout) :: this
        class(forge_qobject), target :: qobject
        this%object => qobject
    end subroutine accessible_object_set_qobject

    function accessible_object_child_count(this) result(count)
        class(QAccessibleObject), intent(in) :: this
        integer :: count

        if (.not. associated(this%object)) then
            count = 0
            return
        end if

        count = this%object%child_count()
    end function accessible_object_child_count

    function accessible_object_child(this, index) result(child)
        class(QAccessibleObject), intent(in) :: this
        integer, intent(in) :: index
        class(QAccessibleInterface), pointer :: child
        class(forge_qobject), pointer :: qobj_child
        type(QAccessibleObject), pointer :: accessible_child

        child => null()
        if (.not. associated(this%object)) return

        qobj_child => this%object%find_child_by_index(index)
        if (associated(qobj_child)) then
            ! Create accessible wrapper for child
            allocate(accessible_child)
            call accessible_child%init(qobj_child)
            child => accessible_child
        end if
    end function accessible_object_child

    function accessible_object_child_at(this, x, y) result(child)
        class(QAccessibleObject), intent(in) :: this
        integer, intent(in) :: x, y
        class(QAccessibleInterface), pointer :: child

        ! Simplified implementation - return first child for now
        child => this%child(1)
    end function accessible_object_child_at

    function accessible_object_index_in_parent(this) result(index)
        class(QAccessibleObject), intent(in) :: this
        integer :: index
        class(forge_qobject), pointer :: parent
        type(forge_qobject_ptr), dimension(:), allocatable :: siblings
        integer :: i

        index = 0
        if (.not. associated(this%object)) return

        parent => this%object%get_parent()
        if (.not. associated(parent)) return

        siblings = parent%get_children()
        do i = 1, size(siblings)
            if (associated(siblings(i)%get(), this%object)) then
                index = i - 1  ! 0-based index
                exit
            end if
        end do
    end function accessible_object_index_in_parent

    function accessible_object_navigate_impl(this, relation) result(target)
        class(QAccessibleObject), intent(in) :: this
        integer, intent(in) :: relation
        class(QAccessibleInterface), pointer :: target
        class(forge_qobject), pointer :: qobj_target
        type(QAccessibleObject), pointer :: accessible_target

        target => null()
        if (.not. associated(this%object)) return

        select case (relation)
        case (QAccessible_Parent)
            qobj_target => this%object%get_parent()
        case (QAccessible_Child)
            qobj_target => this%object%find_child_by_index(1)
        case default
            return
        end select

        if (associated(qobj_target)) then
            allocate(accessible_target)
            call accessible_target%init(qobj_target)
            target => accessible_target
        end if
    end function accessible_object_navigate_impl

    subroutine accessible_object_do_action(this, action_name)
        class(QAccessibleObject), intent(inout) :: this
        character(len=*), intent(in) :: action_name
        ! Default implementation - no action
    end subroutine accessible_object_do_action

    function accessible_object_key_bindings_for_action(this, action_name) result(key_bindings)
        class(QAccessibleObject), intent(in) :: this
        character(len=*), intent(in) :: action_name
        type(forge_string), dimension(:), allocatable :: key_bindings
        allocate(key_bindings(0))  ! No key bindings by default
    end function accessible_object_key_bindings_for_action

    function accessible_object_get_previous_sibling(this) result(sibling)
        class(QAccessibleObject), intent(in) :: this
        class(QAccessibleInterface), pointer :: sibling
        class(forge_qobject), pointer :: parent, prev
        integer :: index

        sibling => null()
        if (.not. associated(this%object)) return

        parent => this%object%get_parent()
        if (.not. associated(parent)) return

        index = this%index_in_parent()
        if (index > 0) then
            prev => parent%find_child_by_index(index - 1)
            if (associated(prev)) then
                allocate(QAccessibleObject :: sibling)
                call sibling%init(prev)
            end if
        end if
    end function accessible_object_get_previous_sibling

    function accessible_object_get_next_sibling(this) result(sibling)
        class(QAccessibleObject), intent(in) :: this
        class(QAccessibleInterface), pointer :: sibling
        class(forge_qobject), pointer :: parent, next
        integer :: index

        sibling => null()
        if (.not. associated(this%object)) return

        parent => this%object%get_parent()
        if (.not. associated(parent)) return

        index = this%index_in_parent()
        next => parent%find_child_by_index(index + 1)
        if (associated(next)) then
            allocate(QAccessibleObject :: sibling)
            call sibling%init(next)
        end if
    end function accessible_object_get_next_sibling

    ! ========== QAccessibleWidget Implementation ==========

    subroutine accessible_widget_init(this, qobject, role)
        class(QAccessibleWidget), intent(inout) :: this
        class(forge_qobject), target :: qobject
        integer, intent(in), optional :: role

        call this%QAccessibleObject%init(qobject)

        if (present(role)) then
            this%widget_role = role
        else
            this%widget_role = QAccessible_NoRole
        end if

        call this%set_role(this%widget_role)
        this%focusable = .false.
        this%has_focus = .false.
    end subroutine accessible_widget_init

    subroutine accessible_widget_set_focusable(this, focusable)
        class(QAccessibleWidget), intent(inout) :: this
        logical, intent(in) :: focusable
        this%focusable = focusable
        if (focusable) then
            call this%state%set_flag(QAccessible_Focusable)
        else
            call this%state%clear_flag(QAccessible_Focusable)
        end if
    end subroutine accessible_widget_set_focusable

    function accessible_widget_is_focusable(this) result(focusable)
        class(QAccessibleWidget), intent(in) :: this
        logical :: focusable
        focusable = this%focusable
    end function accessible_widget_is_focusable

    subroutine accessible_widget_set_focus(this, has_focus)
        class(QAccessibleWidget), intent(inout) :: this
        logical, intent(in) :: has_focus
        this%has_focus = has_focus
        if (has_focus) then
            call this%state%set_flag(QAccessible_Focused)
            ! Notify accessibility system
            type(QAccessibleEvent) :: focus_event
            call focus_event%set_accessible_event_type(QAccessible_Event_Focus)
            call focus_event%set_accessible_object(this)
            call global_accessible_manager%notify_event(focus_event)
        else
            call this%state%clear_flag(QAccessible_Focused)
        end if
    end subroutine accessible_widget_set_focus

    function accessible_widget_has_focus(this) result(has_focus)
        class(QAccessibleWidget), intent(in) :: this
        logical :: has_focus
        has_focus = this%has_focus
    end function accessible_widget_has_focus

    subroutine accessible_widget_grab_focus(this)
        class(QAccessibleWidget), intent(inout) :: this
        if (this%focusable) then
            call this%set_focus(.true.)
        end if
    end subroutine accessible_widget_grab_focus

    function accessible_widget_child_count(this) result(count)
        class(QAccessibleWidget), intent(in) :: this
        count = this%QAccessibleObject%child_count()
    end function accessible_widget_child_count

    function accessible_widget_child(this, index) result(child)
        class(QAccessibleWidget), intent(in) :: this
        integer, intent(in) :: index
        class(QAccessibleInterface), pointer :: child
        child => this%QAccessibleObject%child(index)
    end function accessible_widget_child

    function accessible_widget_child_at(this, x, y) result(child)
        class(QAccessibleWidget), intent(in) :: this
        integer, intent(in) :: x, y
        class(QAccessibleInterface), pointer :: child
        child => this%QAccessibleObject%child_at(x, y)
    end function accessible_widget_child_at

    function accessible_widget_navigate_impl(this, relation) result(target)
        class(QAccessibleWidget), intent(in) :: this
        integer, intent(in) :: relation
        class(QAccessibleInterface), pointer :: target

        select case (relation)
        case (QAccessible_Up, QAccessible_Down, QAccessible_Left, QAccessible_Right)
            ! Handle keyboard navigation for widgets
            target => this%navigate_keyboard(relation)
        case default
            target => this%QAccessibleObject%navigate_impl(relation)
        end select
    end function accessible_widget_navigate_impl

    function accessible_widget_navigate_keyboard(this, direction) result(target)
        class(QAccessibleWidget), intent(in) :: this
        integer, intent(in) :: direction
        class(QAccessibleInterface), pointer :: target

        target => null()
        select case (direction)
        case (QAccessible_Up, QAccessible_Left)
            target => this%get_previous_sibling()
        case (QAccessible_Down, QAccessible_Right)
            target => this%get_next_sibling()
        end select
    end function accessible_widget_navigate_keyboard

    ! ========== QAccessible Implementation ==========

    subroutine accessible_initialize(this)
        class(QAccessible), intent(inout) :: this
        if (this%initialized) return
        this%initialized = .true.
        allocate(this%accessible_objects(10))  ! Initial capacity
        call this%windows_uia%initialize_platform()
        call this%linux_atspi%initialize_platform()
        call this%mac_nsaccessibility%initialize_platform()
        write(output_unit, '(A)') "[ACCESSIBILITY] Initialized accessibility framework"
    end subroutine accessible_initialize

    subroutine accessible_shutdown(this)
        class(QAccessible), intent(inout) :: this
        if (.not. this%initialized) return
        if (allocated(this%accessible_objects)) deallocate(this%accessible_objects)
        this%initialized = .false.
        this%object_count = 0
        write(output_unit, '(A)') "[ACCESSIBILITY] Shutdown accessibility framework"
    end subroutine accessible_shutdown

    subroutine accessible_register_object(this, accessible_object)
        class(QAccessible), intent(inout) :: this
        class(QAccessibleInterface), intent(in) :: accessible_object
        type(QAccessibleInterface), dimension(:), allocatable :: temp

        if (.not. this%initialized) call this%initialize()

        if (this%object_count >= size(this%accessible_objects)) then
            ! Resize array
            allocate(temp(size(this%accessible_objects) * 2))
            temp(1:size(this%accessible_objects)) = this%accessible_objects
            call move_alloc(temp, this%accessible_objects)
        end if

        this%object_count = this%object_count + 1
        ! Note: In Fortran, we can't directly assign polymorphic objects to arrays
        ! This would need a different approach in a real implementation
    end subroutine accessible_register_object

    subroutine accessible_unregister_object(this, accessible_object)
        class(QAccessible), intent(inout) :: this
        class(QAccessibleInterface), intent(in) :: accessible_object
        ! Implementation would remove object from array
    end subroutine accessible_unregister_object

    function accessible_query_interface(this, qobject) result(accessible_interface)
        class(QAccessible), intent(in) :: this
        class(forge_qobject), intent(in) :: qobject
        class(QAccessibleInterface), pointer :: accessible_interface

        accessible_interface => null()
        ! Implementation would look up accessible interface for QObject
    end function accessible_query_interface

    subroutine accessible_update_accessibility(this)
        class(QAccessible), intent(inout) :: this
        ! Implementation would update all registered accessible objects
    end subroutine accessible_update_accessibility

    subroutine accessible_notify_event(this, event)
        class(QAccessible), intent(inout) :: this
        type(QAccessibleEvent), intent(in) :: event
        class(QAccessibleInterface), pointer :: obj

        obj => event%get_accessible_object()
        if (.not. associated(obj)) return

        ! Notify platform accessibility APIs about the event
        select case (this%windows_uia%get_platform_type())
        case (1)
            call this%windows_uia%notify_accessible_event(event)
        case (2)
            call this%linux_atspi%notify_accessible_event(event)
        case (3)
            call this%mac_nsaccessibility%notify_accessible_event(event)
        end select

        select case (event%get_accessible_event_type())
        case (QAccessible_Event_NameChanged)
            call this%notify_name_changed(event)
        case (QAccessible_Event_StateChanged)
            call this%notify_state_changed(event)
        case (QAccessible_Event_ValueChanged)
            call this%notify_value_changed(event)
        case (QAccessible_Event_Focus)
            call this%notify_focus_changed(event)
        end select
    end subroutine accessible_notify_event

    subroutine accessible_set_root_object(this, root)
        class(QAccessible), intent(inout) :: this
        class(QAccessibleInterface), target :: root
        ! Set root accessible object
    end subroutine accessible_set_root_object

    function accessible_get_root_object(this) result(root)
        class(QAccessible), intent(in) :: this
        class(QAccessibleInterface), pointer :: root
        root => null()
    end function accessible_get_root_object

    subroutine accessible_cleanup_invalid_objects(this)
        class(QAccessible), intent(inout) :: this
        ! Remove invalid accessible objects
    end subroutine accessible_cleanup_invalid_objects

    ! Platform-specific notification methods
    subroutine accessible_notify_name_changed(this, event)
        class(QAccessible), intent(in) :: this
        type(QAccessibleEvent), intent(in) :: event
        ! Notify platform accessibility API about name change
    end subroutine accessible_notify_name_changed

    subroutine accessible_notify_state_changed(this, event)
        class(QAccessible), intent(in) :: this
        type(QAccessibleEvent), intent(in) :: event
        ! Notify platform accessibility API about state change
    end subroutine accessible_notify_state_changed

    subroutine accessible_notify_value_changed(this, event)
        class(QAccessible), intent(in) :: this
        type(QAccessibleEvent), intent(in) :: event
        ! Notify platform accessibility API about value change
    end subroutine accessible_notify_value_changed

    subroutine accessible_notify_focus_changed(this, event)
        class(QAccessible), intent(in) :: this
        type(QAccessibleEvent), intent(in) :: event
        ! Notify platform accessibility API about focus change
    end subroutine accessible_notify_focus_changed

    ! Utility functions
    function forge_string_from_int(value) result(str)
        integer, intent(in) :: value
        character(len=:), allocatable :: str
        character(len=20) :: temp

        write(temp, '(I0)') value
        str = trim(adjustl(temp))
    end function forge_string_from_int

end module forge_accessibility
        class(QAccessibleTable), intent(in)