!> @brief MVC Framework Demonstration
!> @details Example showing Qt MVC architecture with list, table, and tree models
!> @author ForGE Contributors
!> @date 2025
!> @license GPL-3.0-or-later

program mvc_demo
    use iso_c_binding
    use forge_types
    use forge_backend
    use forge_qt_backend
    use forge_mvc
    use forge_signals
    implicit none

    ! Backend and window
    type(forge_qt_backend_t), target :: backend
    type(forge_window_handle) :: window_handle
    type(forge_status) :: status

    ! MVC components
    type(StringListModel) :: list_model
    type(PersonTableModel) :: table_model
    type(FileSystemTreeModel) :: tree_model
    type(QListView) :: list_view
    type(QTableView) :: table_view
    type(QTreeView) :: tree_view
    type(QItemSelectionModel) :: selection_model

    ! Initialize backend
    call backend%init(status)
    if (.not. status%is_ok()) then
        write(*,*) "Failed to initialize backend: ", status%get_message()
        stop
    end if

    ! Create main window
    call backend%create_window(window_handle, "Qt MVC Demo", 800, 600, status)
    if (.not. status%is_ok()) then
        write(*,*) "Failed to create window: ", status%get_message()
        stop
    end if

    ! Initialize models
    call list_model%init()
    call table_model%init()
    call tree_model%init()

    ! Initialize views
    call list_view%init()
    call table_view%init()
    call tree_view%init()

    ! Connect models to views
    call list_view%set_model(list_model)
    call table_view%set_model(table_model)
    call tree_view%set_model(tree_model)

    ! Setup selection models
    call selection_model%init(list_model)
    call list_view%set_selection_model(selection_model)

    ! Populate models with sample data
    call populate_list_model(list_model)
    call populate_table_model(table_model)
    call populate_tree_model(tree_model)

    ! Show window
    call backend%show_window(window_handle)

    ! Run event loop
    call backend%run()

    ! Cleanup
    call backend%shutdown()

contains

    !> @brief Custom string list model
    type, extends(QAbstractListModel) :: StringListModel
        private
            character(len=:), dimension(:), allocatable :: strings
    contains
        procedure :: data => stringlistmodel_data
        procedure :: set_data => stringlistmodel_set_data
        procedure :: flags => stringlistmodel_flags
        procedure :: row_count => stringlistmodel_row_count
        procedure :: insert_rows => stringlistmodel_insert_rows
        procedure :: remove_rows => stringlistmodel_remove_rows
    end type StringListModel

    !> @brief Custom person table model
    type, extends(QAbstractTableModel) :: PersonTableModel
        private
            type :: Person
                character(len=50) :: name
                integer :: age
                character(len=50) :: city
            end type Person
            type(Person), dimension(:), allocatable :: people
    contains
        procedure :: data => persontablemodel_data
        procedure :: set_data => persontablemodel_set_data
        procedure :: flags => persontablemodel_flags
        procedure :: row_count => persontablemodel_row_count
        procedure :: column_count => persontablemodel_column_count
        procedure :: header_data => persontablemodel_header_data
        procedure :: insert_rows => persontablemodel_insert_rows
        procedure :: remove_rows => persontablemodel_remove_rows
    end type PersonTableModel

    !> @brief Custom file system tree model
    type, extends(QAbstractTreeModel) :: FileSystemTreeModel
        private
            type :: TreeNode
                character(len=:), allocatable :: name
                logical :: is_directory
                type(TreeNode), pointer :: children(:) => null()
                integer :: child_count = 0
            end type TreeNode
            type(TreeNode), pointer :: root => null()
    contains
        procedure :: data => filesystemtreemodel_data
        procedure :: flags => filesystemtreemodel_flags
        procedure :: row_count => filesystemtreemodel_row_count
        procedure :: column_count => filesystemtreemodel_column_count
        procedure :: header_data => filesystemtreemodel_header_data
        procedure :: index => filesystemtreemodel_index
        procedure :: parent => filesystemtreemodel_parent
    end type FileSystemTreeModel

    ! ========== StringListModel Implementation ==========

    function stringlistmodel_data(this, index, role) result(data)
        class(StringListModel), intent(in) :: this
        type(QModelIndex), intent(in) :: index
        integer, intent(in) :: role
        character(len=:), allocatable :: data

        if (.not. index%is_valid()) then
            allocate(character(len=0) :: data)
            return
        end if

        select case (role)
        case (Qt_DisplayRole)
            if (allocated(this%strings) .and. index%row() >= 0 .and. index%row() < size(this%strings)) then
                data = trim(this%strings(index%row() + 1))
            else
                allocate(character(len=0) :: data)
            end if
        case default
            allocate(character(len=0) :: data)
        end select
    end function stringlistmodel_data

    function stringlistmodel_set_data(this, index, value, role) result(success)
        class(StringListModel), intent(inout) :: this
        type(QModelIndex), intent(in) :: index
        character(len=*), intent(in) :: value
        integer, intent(in) :: role
        logical :: success

        success = .false.
        if (.not. index%is_valid() .or. role /= Qt_EditRole) return

        if (allocated(this%strings) .and. index%row() >= 0 .and. index%row() < size(this%strings)) then
            this%strings(index%row() + 1) = value
            success = .true.
            call this%emit_data_changed(index, index, [Qt_DisplayRole])
        end if
    end function stringlistmodel_set_data

    function stringlistmodel_flags(this, index) result(flags)
        class(StringListModel), intent(in) :: this
        type(QModelIndex), intent(in) :: index
        type(Qt_ItemFlags) :: flags

        call flags%set_flag(Qt_ItemIsEnabled)
        call flags%set_flag(Qt_ItemIsSelectable)
        call flags%set_flag(Qt_ItemIsEditable)
    end function stringlistmodel_flags

    function stringlistmodel_row_count(this, parent) result(count)
        class(StringListModel), intent(in) :: this
        type(QModelIndex), intent(in) :: parent
        integer :: count

        if (parent%is_valid()) then
            count = 0
        else if (allocated(this%strings)) then
            count = size(this%strings)
        else
            count = 0
        end if
    end function stringlistmodel_row_count

    function stringlistmodel_insert_rows(this, row, count, parent) result(success)
        class(StringListModel), intent(inout) :: this
        integer, intent(in) :: row, count
        type(QModelIndex), intent(in) :: parent
        logical :: success

        success = .false.
        if (parent%is_valid()) return

        call this%begin_insert_rows(parent, row, row + count - 1)

        if (.not. allocated(this%strings)) then
            allocate(this%strings(count))
        else
            ! Expand array (simplified)
            this%strings = [this%strings(:row), &
                           [(character(len=10) :: "New Item", i=1,count)], &
                           this%strings(row+1:)]
        end if

        call this%end_insert_rows()
        success = .true.
    end function stringlistmodel_insert_rows

    function stringlistmodel_remove_rows(this, row, count, parent) result(success)
        class(StringListModel), intent(inout) :: this
        integer, intent(in) :: row, count
        type(QModelIndex), intent(in) :: parent
        logical :: success

        success = .false.
        if (parent%is_valid() .or. .not. allocated(this%strings)) return
        if (row < 0 .or. row + count > size(this%strings)) return

        call this%begin_remove_rows(parent, row, row + count - 1)

        ! Remove elements (simplified)
        if (row == 0) then
            this%strings = this%strings(count+1:)
        else if (row + count >= size(this%strings)) then
            this%strings = this%strings(:row)
        else
            this%strings = [this%strings(:row), this%strings(row+count+1:)]
        end if

        call this%end_remove_rows()
        success = .true.
    end function stringlistmodel_remove_rows

    ! ========== PersonTableModel Implementation ==========

    function persontablemodel_data(this, index, role) result(data)
        class(PersonTableModel), intent(in) :: this
        type(QModelIndex), intent(in) :: index
        integer, intent(in) :: role
        character(len=:), allocatable :: data

        if (.not. index%is_valid() .or. .not. allocated(this%people)) then
            allocate(character(len=0) :: data)
            return
        end if

        select case (role)
        case (Qt_DisplayRole)
            select case (index%column())
            case (0)
                data = trim(this%people(index%row() + 1)%name)
            case (1)
                write(data, '(I0)') this%people(index%row() + 1)%age
            case (2)
                data = trim(this%people(index%row() + 1)%city)
            case default
                allocate(character(len=0) :: data)
            end select
        case default
            allocate(character(len=0) :: data)
        end select
    end function persontablemodel_data

    function persontablemodel_set_data(this, index, value, role) result(success)
        class(PersonTableModel), intent(inout) :: this
        type(QModelIndex), intent(in) :: index
        character(len=*), intent(in) :: value
        integer, intent(in) :: role
        logical :: success

        success = .false.
        if (.not. index%is_valid() .or. role /= Qt_EditRole .or. .not. allocated(this%people)) return

        select case (index%column())
        case (0)
            this%people(index%row() + 1)%name = value
            success = .true.
        case (1)
            read(value, *, iostat=ios) this%people(index%row() + 1)%age
            success = (ios == 0)
        case (2)
            this%people(index%row() + 1)%city = value
            success = .true.
        end select

        if (success) then
            call this%emit_data_changed(index, index, [Qt_DisplayRole])
        end if
    end function persontablemodel_set_data

    function persontablemodel_flags(this, index) result(flags)
        class(PersonTableModel), intent(in) :: this
        type(QModelIndex), intent(in) :: index
        type(Qt_ItemFlags) :: flags

        call flags%set_flag(Qt_ItemIsEnabled)
        call flags%set_flag(Qt_ItemIsSelectable)
        if (index%column() /= 1) then  ! Age column is not editable
            call flags%set_flag(Qt_ItemIsEditable)
        end if
    end function persontablemodel_flags

    function persontablemodel_row_count(this, parent) result(count)
        class(PersonTableModel), intent(in) :: this
        type(QModelIndex), intent(in) :: parent
        integer :: count

        if (parent%is_valid()) then
            count = 0
        else if (allocated(this%people)) then
            count = size(this%people)
        else
            count = 0
        end if
    end function persontablemodel_row_count

    function persontablemodel_column_count(this, parent) result(count)
        class(PersonTableModel), intent(in) :: this
        type(QModelIndex), intent(in) :: parent
        integer :: count

        if (parent%is_valid()) then
            count = 0
        else
            count = 3  ! Name, Age, City
        end if
    end function persontablemodel_column_count

    function persontablemodel_header_data(this, section, orientation, role) result(data)
        class(PersonTableModel), intent(in) :: this
        integer, intent(in) :: section, orientation, role
        character(len=:), allocatable :: data

        if (role /= Qt_DisplayRole) then
            allocate(character(len=0) :: data)
            return
        end if

        if (orientation == Qt_Horizontal) then
            select case (section)
            case (0)
                data = "Name"
            case (1)
                data = "Age"
            case (2)
                data = "City"
            case default
                allocate(character(len=0) :: data)
            end select
        else
            allocate(character(len=0) :: data)
        end if
    end function persontablemodel_header_data

    function persontablemodel_insert_rows(this, row, count, parent) result(success)
        class(PersonTableModel), intent(inout) :: this
        integer, intent(in) :: row, count
        type(QModelIndex), intent(in) :: parent
        logical :: success

        success = .false.
        if (parent%is_valid()) return

        call this%begin_insert_rows(parent, row, row + count - 1)

        if (.not. allocated(this%people)) then
            allocate(this%people(count))
        else
            ! Expand array (simplified)
            this%people = [this%people(:row), &
                          [(Person("New Person", 0, "Unknown"), i=1,count)], &
                          this%people(row+1:)]
        end if

        call this%end_insert_rows()
        success = .true.
    end function persontablemodel_insert_rows

    function persontablemodel_remove_rows(this, row, count, parent) result(success)
        class(PersonTableModel), intent(inout) :: this
        integer, intent(in) :: row, count
        type(QModelIndex), intent(in) :: parent
        logical :: success

        success = .false.
        if (parent%is_valid() .or. .not. allocated(this%people)) return
        if (row < 0 .or. row + count > size(this%people)) return

        call this%begin_remove_rows(parent, row, row + count - 1)

        ! Remove elements (simplified)
        if (row == 0) then
            this%people = this%people(count+1:)
        else if (row + count >= size(this%people)) then
            this%people = this%people(:row)
        else
            this%people = [this%people(:row), this%people(row+count+1:)]
        end if

        call this%end_remove_rows()
        success = .true.
    end function persontablemodel_remove_rows

    ! ========== FileSystemTreeModel Implementation ==========

    function filesystemtreemodel_data(this, index, role) result(data)
        class(FileSystemTreeModel), intent(in) :: this
        type(QModelIndex), intent(in) :: index
        integer, intent(in) :: role
        character(len=:), allocatable :: data

        ! Simplified implementation
        allocate(character(len=0) :: data)
    end function filesystemtreemodel_data

    function filesystemtreemodel_flags(this, index) result(flags)
        class(FileSystemTreeModel), intent(in) :: this
        type(QModelIndex), intent(in) :: index
        type(Qt_ItemFlags) :: flags

        call flags%set_flag(Qt_ItemIsEnabled)
        call flags%set_flag(Qt_ItemIsSelectable)
    end function filesystemtreemodel_flags

    function filesystemtreemodel_row_count(this, parent) result(count)
        class(FileSystemTreeModel), intent(in) :: this
        type(QModelIndex), intent(in) :: parent
        integer :: count

        ! Simplified implementation
        count = 0
    end function filesystemtreemodel_row_count

    function filesystemtreemodel_column_count(this, parent) result(count)
        class(FileSystemTreeModel), intent(in) :: this
        type(QModelIndex), intent(in) :: parent
        integer :: count

        count = 1
    end function filesystemtreemodel_column_count

    function filesystemtreemodel_header_data(this, section, orientation, role) result(data)
        class(FileSystemTreeModel), intent(in) :: this
        integer, intent(in) :: section, orientation, role
        character(len=:), allocatable :: data

        if (role == Qt_DisplayRole .and. orientation == Qt_Horizontal .and. section == 0) then
            data = "Name"
        else
            allocate(character(len=0) :: data)
        end if
    end function filesystemtreemodel_header_data

    function filesystemtreemodel_index(this, row, column, parent) result(index)
        class(FileSystemTreeModel), intent(in) :: this
        integer(c_int), intent(in) :: row, column
        type(QModelIndex), intent(in) :: parent
        type(QModelIndex) :: index

        ! Simplified implementation
        index%qt_index%ptr = c_null_ptr
    end function filesystemtreemodel_index

    function filesystemtreemodel_parent(this, child) result(parent)
        class(FileSystemTreeModel), intent(in) :: this
        type(QModelIndex), intent(in) :: child
        type(QModelIndex) :: parent

        ! Simplified implementation
        parent%qt_index%ptr = c_null_ptr
    end function filesystemtreemodel_parent

    ! ========== Helper Subroutines ==========

    subroutine populate_list_model(model)
        type(StringListModel), intent(inout) :: model

        allocate(model%strings(5))
        model%strings(1) = "Apple"
        model%strings(2) = "Banana"
        model%strings(3) = "Cherry"
        model%strings(4) = "Date"
        model%strings(5) = "Elderberry"
    end subroutine populate_list_model

    subroutine populate_table_model(model)
        type(PersonTableModel), intent(inout) :: model

        allocate(model%people(4))
        model%people(1) = Person("Alice", 25, "New York")
        model%people(2) = Person("Bob", 30, "London")
        model%people(3) = Person("Charlie", 35, "Paris")
        model%people(4) = Person("Diana", 28, "Tokyo")
    end subroutine populate_table_model

    subroutine populate_tree_model(model)
        type(FileSystemTreeModel), intent(inout) :: model

        ! Simplified - would populate with actual file system structure
        allocate(model%root)
        model%root%name = "/"
        model%root%is_directory = .true.
    end subroutine populate_tree_model

end program mvc_demo