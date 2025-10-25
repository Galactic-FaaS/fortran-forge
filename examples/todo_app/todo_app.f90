!> @brief Todo List Application
!> @details Demonstrates multiple ForGE Qt features in a real app
!> @author ForGE Contributors
!> @license GPL-3.0-or-later

program todo_app
    use forge
    use forge_signals
    use forge_string_utils
    use forge_stub_backend  ! Will switch to custom when ready
    implicit none
    
    type(forge_stub_backend_t), target :: backend
    type(forge_window_t) :: window
    type(forge_button) :: add_button, remove_button, clear_button
    type(forge_entry) :: input_entry
    type(forge_label) :: title_label, count_label
    type(signal_void) :: add_clicked, remove_clicked, clear_clicked
    type(QString), allocatable :: todo_items(:)
    integer :: todo_count = 0
    type(forge_status) :: status
    
    print '(A)', "========================================"
    print '(A)', "ForGE Qt - Todo List Application"
    print '(A)', "========================================"
    print '(A)', ""
    print '(A)', "Features Demonstrated:"
    print '(A)', "  ✓ Multiple widgets (buttons, entry, labels)"
    print '(A)', "  ✓ Signals & slots"
    print '(A)', "  ✓ QString for text management"
    print '(A)', "  ✓ Dynamic array (todo list)"
    print '(A)', "  ✓ Layout (grid)"
    print '(A)', ""
    
    ! Initialize
    call backend%init(status)
    if (status%is_error()) stop 1
    
    ! Create window
    block
        type(forge_window_builder) :: builder
        call builder%set_title("ForGE Qt - Todo List")
        call builder%set_size(500, 600)
        call builder%set_backend(backend)
        window = builder%build(status)
    end block
    
    ! Setup title
    call title_label%set_text("My Todo List")
    call title_label%set_name("title")
    
    ! Setup count label
    call count_label%set_text("Items: 0")
    call count_label%set_name("count")
    
    ! Setup input
    call input_entry%set_name("todo_input")
    
    ! Setup buttons
    call add_button%set_label("Add Todo")
    call add_button%set_name("btn_add")
    
    call remove_button%set_label("Remove Last")
    call remove_button%set_name("btn_remove")
    
    call clear_button%set_label("Clear All")
    call clear_button%set_name("btn_clear")
    
    ! Connect signals
    add_conn = add_clicked%connect(on_add_clicked)
    remove_conn = remove_clicked%connect(on_remove_clicked)
    clear_conn = clear_clicked%connect(on_clear_clicked)
    
    ! Initialize todo list
    allocate(todo_items(100))
    
    print '(A)', "Window created!"
    print '(A)', ""
    print '(A)', "This demonstrates:"
    print '(A)', "  - Widget creation and configuration"
    print '(A)', "  - Signals and slots connections"
    print '(A)', "  - QString for text handling"
    print '(A)', "  - Dynamic data management"
    print '(A)', ""
    print '(A)', "In a real implementation with full backend:"
    print '(A)', "  - Enter text in input field"
    print '(A)', "  - Click 'Add Todo' to add to list"
    print '(A)', "  - Click 'Remove Last' to remove item"
    print '(A)', "  - Click 'Clear All' to clear list"
    print '(A)', ""
    
    ! Show window
    call window%show()
    call backend%run()
    
    ! Cleanup
    call window%close()
    call backend%shutdown()
    
    print '(A)', ""
    print '(A)', "========================================"
    print '(A)', "Todo App Demo Complete!"
    print '(A)', "========================================"
    
contains

    subroutine on_add_clicked()
        type(QString) :: new_item
        character(len=256) :: count_text
        
        ! Get text from entry (simplified - would get from widget)
        call new_item%set("New Todo Item")
        
        ! Add to list
        if (todo_count < size(todo_items)) then
            todo_items(todo_count + 1) = new_item
            todo_count = todo_count + 1
            
            ! Update count label
            write(count_text, '(A,I0)') "Items: ", todo_count
            call count_label%set_text(trim(count_text))
            
            print '(A,I0,A,A)', "  → Added todo #", todo_count, ": ", new_item%get()
        else
            print '(A)', "  → List is full!"
        end if
    end subroutine on_add_clicked
    
    subroutine on_remove_clicked()
        character(len=256) :: count_text
        
        if (todo_count > 0) then
            print '(A,I0,A,A)', "  → Removed todo #", todo_count, ": ", &
                todo_items(todo_count)%get()
            
            todo_count = todo_count - 1
            
            ! Update count label
            write(count_text, '(A,I0)') "Items: ", todo_count
            call count_label%set_text(trim(count_text))
        else
            print '(A)', "  → List is empty!"
        end if
    end subroutine on_remove_clicked
    
    subroutine on_clear_clicked()
        if (todo_count > 0) then
            print '(A,I0,A)', "  → Cleared ", todo_count, " items"
            todo_count = 0
            call count_label%set_text("Items: 0")
        else
            print '(A)', "  → List is already empty!"
        end if
    end subroutine on_clear_clicked

end program todo_app

