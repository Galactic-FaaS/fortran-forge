# forge_stack_layout

Layout manager that stacks widgets on top of each other, showing only one at a time.

## Synopsis

```fortran
type, extends(forge_layout_base) :: forge_stack_layout
    private
    type(QList_widget) :: widgets
    integer :: current_index = 1
contains
    procedure :: set_current => forge_stack_layout_set_current
    procedure :: get_current => forge_stack_layout_get_current
    procedure :: add_widget => forge_stack_layout_add_widget
    procedure :: remove_widget => forge_stack_layout_remove_widget
    procedure :: compute => forge_stack_layout_compute
end type forge_stack_layout
```

## Description

The `forge_stack_layout` arranges widgets in a stack where only one widget is visible at a time. It provides:

- Layered widget arrangement
- Tab-like interface without tabs
- Wizard-style navigation
- Modal dialog stacking
- Screen/page transitions

Stack layouts are perfect for:
- Multi-page wizards
- Tabbed interfaces (content only)
- Modal dialogs
- Screen flows
- Settings panels

## Properties

- `widgets`: List of stacked widgets
- `current_index`: Index of currently visible widget (1-based)

## Methods

### set_current(index)

Sets which widget is currently visible.

**Parameters:**
- `index` (integer): Widget index (1-based)

**Example:**
```fortran
type(forge_stack_layout) :: stack

call stack%set_current(2)  ! Show second widget
```

### get_current()

Gets the index of the currently visible widget.

**Returns:** integer - Current widget index (1-based)

**Example:**
```fortran
integer :: current

current = stack%get_current()
print *, "Current page:", current
```

### add_widget(widget, row, col, row_span, col_span)

Adds a widget to the stack.

**Parameters:**
- `widget` (forge_widget): Widget to add

**Example:**
```fortran
call stack%add_widget(page1)
call stack%add_widget(page2)
call stack%add_widget(page3)
```

### remove_widget(widget)

Removes a widget from the stack.

**Parameters:**
- `widget` (forge_widget): Widget to remove

**Example:**
```fortran
call stack%remove_widget(page2)
```

### compute()

Calculates and applies widget positions and sizes.

**Example:**
```fortran
call stack%compute()
```

## Layout Behavior

Stack layouts position all widgets at the same location:

- All widgets have identical size and position
- Only the current widget is visible
- Other widgets are hidden
- Size matches the layout's available space

## Common Patterns

### Wizard Interface
```fortran
type(forge_stack_layout) :: wizard
type(forge_widget) :: welcome_page, config_page, confirm_page
type(forge_button) :: next_button, back_button

! Add pages
call wizard%add_widget(welcome_page)
call wizard%add_widget(config_page)
call wizard%add_widget(confirm_page)

! Navigation
call next_button%on_click(go_next)
call back_button%on_click(go_back)

subroutine go_next(event)
    integer :: current
    current = wizard%get_current()
    if (current < wizard%widgets%size()) then
        call wizard%set_current(current + 1)
    end if
end subroutine go_next
```

### Tab Content
```fortran
type(forge_stack_layout) :: content_area
type(forge_widget) :: tab1_content, tab2_content, tab3_content

! Add tab contents
call content_area%add_widget(tab1_content)
call content_area%add_widget(tab2_content)
call content_area%add_widget(tab3_content)

! Tab selection
subroutine on_tab_selected(tab_index)
    call content_area%set_current(tab_index)
end subroutine on_tab_selected
```

### Modal Dialogs
```fortran
type(forge_stack_layout) :: dialog_stack
type(forge_widget) :: main_content, error_dialog, confirm_dialog

! Add content layers
call dialog_stack%add_widget(main_content)
call dialog_stack%add_widget(error_dialog)
call dialog_stack%add_widget(confirm_dialog)

! Show dialogs
call dialog_stack%set_current(2)  ! Show error dialog
call dialog_stack%set_current(1)  ! Return to main content
```

### Screen Flow
```fortran
type(forge_stack_layout) :: app_screens
type(forge_widget) :: login_screen, main_screen, settings_screen

call app_screens%add_widget(login_screen)
call app_screens%add_widget(main_screen)
call app_screens%add_widget(settings_screen)

! Navigation
call navigate_to_screen(SCREEN_MAIN)
call navigate_to_screen(SCREEN_SETTINGS)

subroutine navigate_to_screen(screen_id)
    integer, intent(in) :: screen_id
    call app_screens%set_current(screen_id)
end subroutine navigate_to_screen
```

## Qt Compatibility

ForGE provides Qt-style stacked layout:

```fortran
type(QStackedLayout) :: stack

! Same interface as forge_stack_layout
call stack%add_widget(widget)
call stack%set_current(2)
```

## Widget Management

Stack layouts handle widget visibility automatically:

- **Current widget**: Visible and positioned
- **Other widgets**: Hidden (not destroyed)
- **Size**: All widgets sized to fill layout area
- **Position**: All widgets positioned at (0,0) relative to layout

## Transitions

Stack layouts can support transitions (planned):

```fortran
! Future transition support
call stack%set_transition(TRANSITION_FADE)
call stack%set_transition_duration(300)  ! milliseconds
call stack%set_current(2)  ! Animated transition
```

## Performance

Stack layouts are efficient:

- O(1) widget switching
- Minimal computation overhead
- Memory efficient (widgets stay allocated)
- Fast visibility changes

## Thread Safety

Stack layout operations are not thread-safe. Modifications should occur on the main GUI thread.

## Example Usage

```fortran
program stack_layout_demo
    use forge
    implicit none

    type(forge_application) :: app
    type(forge_window_t) :: window
    type(forge_stack_layout) :: stack
    type(forge_label) :: page1, page2, page3
    type(forge_button) :: next_button, prev_button
    type(forge_box_layout) :: button_layout
    type(forge_status) :: status

    ! Initialize
    call app%init(BACKEND_CUSTOM, status)
    window = app%create_window("Stack Layout Demo", 400, 300)

    ! Create pages
    call page1%set_text("Page 1: Welcome")
    call page2%set_text("Page 2: Configuration")
    call page3%set_text("Page 3: Complete")

    ! Create navigation buttons
    call prev_button%set_label("Previous")
    call next_button%set_label("Next")

    call prev_button%on_click(go_previous)
    call next_button%on_click(go_next)

    ! Create button layout
    call button_layout%set_orientation(LAYOUT_HORIZONTAL)
    call button_layout%add_widget(prev_button)
    call button_layout%add_widget(next_button)

    ! Create stack layout
    call stack%add_widget(page1)
    call stack%add_widget(page2)
    call stack%add_widget(page3)

    ! Combine layouts
    type(forge_box_layout) :: main_layout
    call main_layout%set_orientation(LAYOUT_VERTICAL)
    call main_layout%add_widget(stack)
    call main_layout%add_widget(button_layout)

    ! Set layout size and compute
    call main_layout%set_parent_size(400, 300)
    call main_layout%compute()

    ! Show window
    call window%show()
    call app%run()

contains

    subroutine go_next(event)
        type(forge_event), intent(in) :: event
        integer :: current

        current = stack%get_current()
        if (current < 3) then
            call stack%set_current(current + 1)
            call stack%compute()
        end if
    end subroutine go_next

    subroutine go_previous(event)
        type(forge_event), intent(in) :: event
        integer :: current

        current = stack%get_current()
        if (current > 1) then
            call stack%set_current(current - 1)
            call stack%compute()
        end if
    end subroutine go_previous

end program stack_layout_demo
```

## Advanced Usage

### Dynamic Stacks
```fortran
subroutine add_dynamic_page(stack, page_content)
    type(forge_stack_layout), intent(inout) :: stack
    character(len=*), intent(in) :: page_content

    type(forge_label) :: new_page

    call new_page%set_text(page_content)
    call stack%add_widget(new_page)
end subroutine add_dynamic_page
```

### Stack History
```fortran
type :: stack_history
    integer, allocatable :: history(:)
    integer :: current_pos = 0
contains
    procedure :: push => history_push
    procedure :: pop => history_pop
    procedure :: can_go_back => history_can_go_back
end type stack_history

subroutine navigate_with_history(stack, history, target_index)
    type(forge_stack_layout), intent(inout) :: stack
    type(stack_history), intent(inout) :: history
    integer, intent(in) :: target_index

    call history%push(stack%get_current())
    call stack%set_current(target_index)
end subroutine navigate_with_history
```

### Conditional Navigation
```fortran
function can_navigate_to(stack, target_index) result(allowed)
    type(forge_stack_layout), intent(in) :: stack
    integer, intent(in) :: target_index
    logical :: allowed

    select case (target_index)
    case (PAGE_CONFIG)
        allowed = is_form_valid()
    case (PAGE_CONFIRM)
        allowed = is_config_complete()
    case default
        allowed = .true.
    end select
end function can_navigate_to
```

### Stack Persistence
```fortran
subroutine save_stack_state(stack, filename)
    type(forge_stack_layout), intent(in) :: stack
    character(len=*), intent(in) :: filename

    integer :: unit, current

    current = stack%get_current()
    open(newunit=unit, file=filename)
    write(unit, *) current
    close(unit)
end subroutine save_stack_state

subroutine restore_stack_state(stack, filename)
    type(forge_stack_layout), intent(inout) :: stack
    character(len=*), intent(in) :: filename

    integer :: unit, current

    open(newunit=unit, file=filename, status='old')
    read(unit, *) current
    close(unit)

    call stack%set_current(current)
end subroutine restore_stack_state
```

## See Also

- [forge_layout_base](forge_layout_base.md) - Base layout class
- [forge_box_layout](forge_box_layout.md) - Linear layouts
- [forge_grid_layout](forge_grid_layout.md) - Grid layouts
- [QStackedLayout](qstackedlayout.md) - Qt-style stacked layout