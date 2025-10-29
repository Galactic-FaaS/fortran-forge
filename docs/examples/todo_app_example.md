# Todo App Example

A complete todo list application demonstrating data management, dynamic arrays, and user interface interactions in ForGE.

## Overview

This example shows how to build a functional todo list application with:
- Text input for new todo items
- Add, remove, and clear functionality
- Dynamic data storage with QString arrays
- Real-time UI updates
- Signal/slot event system
- Data persistence concepts

## Code

```fortran
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
```

## Explanation

### Data Model

```fortran
type(QString), allocatable :: todo_items(:)
integer :: todo_count = 0
```

The todo list uses:
- `QString` array for storing todo text
- Dynamic allocation for variable-sized list
- Counter to track number of items

### Widget Setup

```fortran
! Setup input
call input_entry%set_name("todo_input")

! Setup buttons
call add_button%set_label("Add Todo")
call add_button%set_name("btn_add")

call remove_button%set_label("Remove Last")
call remove_button%set_name("btn_remove")

call clear_button%set_label("Clear All")
call clear_button%set_name("btn_clear")
```

Creates input field and three action buttons with descriptive names.

### Signal Connections

```fortran
! Connect signals
add_conn = add_clicked%connect(on_add_clicked)
remove_conn = remove_clicked%connect(on_remove_clicked)
clear_conn = clear_clicked%connect(on_clear_clicked)
```

Connects button signals to handler subroutines using the signal/slot system.

### Adding Todos

```fortran
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
```

Handles adding new todo items:
1. Creates new QString for the item
2. Adds to array if space available
3. Updates counter and display
4. Provides feedback

### Removing Todos

```fortran
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
```

Removes the last todo item with bounds checking and UI updates.

### Clearing All Todos

```fortran
subroutine on_clear_clicked()
    if (todo_count > 0) then
        print '(A,I0,A)', "  → Cleared ", todo_count, " items"
        todo_count = 0
        call count_label%set_text("Items: 0")
    else
        print '(A)', "  → List is already empty!"
    end if
end subroutine on_clear_clicked
```

Clears all todos and resets the counter.

## Running the Example

### With fpm

```bash
fpm run todo_app
```

### With CMake

```bash
cd build
cmake --build . --target todo_app_example
./examples/todo_app/todo_app_example
```

### Manual Build

```bash
gfortran -I/path/to/forge/include examples/todo_app/todo_app.f90 -lforge -o todo_app
./todo_app
```

## Expected Output

```
========================================
ForGE Qt - Todo List Application
========================================

Features Demonstrated:
  ✓ Multiple widgets (buttons, entry, labels)
  ✓ Signals & slots
  ✓ QString for text management
  ✓ Dynamic array (todo list)
  ✓ Layout (grid)

Window created!

This demonstrates:
  - Widget creation and configuration
  - Signals and slots connections
  - QString for text handling
  - Dynamic data management

In a real implementation with full backend:
  - Enter text in input field
  - Click 'Add Todo' to add to list
  - Click 'Remove Last' to remove item
  - Click 'Clear All' to clear list

========================================
Todo App Demo Complete!
========================================
```

## Key Concepts Demonstrated

1. **Data Management**: Dynamic arrays and QString for text storage
2. **Signal/Slot System**: Qt-style event handling
3. **State Synchronization**: Keeping UI and data model in sync
4. **Error Handling**: Bounds checking and user feedback
5. **Resource Management**: Proper cleanup and memory handling

## Application Architecture

The todo app demonstrates a simple MVC-like pattern:

- **Model**: QString array storing todo items
- **View**: Widgets displaying the UI
- **Controller**: Event handlers managing interactions

## Advanced Features

### Data Persistence

```fortran
! Conceptual file I/O for persistence
subroutine save_todos(filename)
    character(len=*), intent(in) :: filename
    integer :: i, unit
    
    open(newunit=unit, file=filename, status='replace')
    do i = 1, todo_count
        write(unit, '(A)') todo_items(i)%get()
    end do
    close(unit)
end subroutine save_todos

subroutine load_todos(filename)
    character(len=*), intent(in) :: filename
    character(len=256) :: line
    integer :: unit, ios
    
    open(newunit=unit, file=filename, status='old', iostat=ios)
    if (ios /= 0) return  ! File doesn't exist
    
    todo_count = 0
    do
        read(unit, '(A)', iostat=ios) line
        if (ios /= 0) exit
        
        todo_count = todo_count + 1
        call todo_items(todo_count)%set(trim(line))
    end do
    close(unit)
end subroutine load_todos
```

### Undo/Redo System

```fortran
type :: undo_action
    integer :: action_type  ! 1=add, 2=remove, 3=clear
    type(QString) :: item
    integer :: index
end type undo_action

type(undo_action), allocatable :: undo_stack(:)
integer :: undo_count = 0

subroutine push_undo(action)
    type(undo_action), intent(in) :: action
    
    ! Add to undo stack
    undo_count = undo_count + 1
    if (.not. allocated(undo_stack)) allocate(undo_stack(100))
    undo_stack(undo_count) = action
end subroutine push_undo

subroutine undo_last_action()
    type(undo_action) :: action
    
    if (undo_count > 0) then
        action = undo_stack(undo_count)
        undo_count = undo_count - 1
        
        ! Reverse the action
        select case (action%action_type)
        case (1)  ! Undo add
            call remove_todo(action%index)
        case (2)  ! Undo remove
            call insert_todo(action%index, action%item)
        case (3)  ! Undo clear
            ! Restore all items
        end select
    end if
end subroutine undo_last_action
```

### Search and Filtering

```fortran
function find_todos(search_term) result(indices)
    character(len=*), intent(in) :: search_term
    integer, allocatable :: indices(:)
    
    integer :: i, count
    character(len=:), allocatable :: item_text
    
    count = 0
    do i = 1, todo_count
        item_text = todo_items(i)%get()
        if (index(item_text, search_term) > 0) then
            count = count + 1
            if (.not. allocated(indices)) allocate(indices(count))
            indices(count) = i
        end if
    end do
end function find_todos

subroutine filter_todos(search_term)
    integer, allocatable :: matching_indices
    integer :: i
    
    matching_indices = find_todos(search_term)
    
    ! Hide non-matching items in UI
    ! (Implementation depends on actual UI widgets)
end subroutine filter_todos
```

## Next Steps

- Try the [calculator](../examples/calculator.md) for complex state management
- Learn about [data binding](../tutorials/data_binding.md) for automatic UI updates
- Explore [custom widgets](../tutorials/advanced_widgets.md) for specialized controls
- See the [Qt demo](../examples/qt_demo.md) for full GUI integration