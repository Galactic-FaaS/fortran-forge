# Layout Managers Tutorial

This tutorial covers ForGE's layout management system, which automatically positions and sizes widgets within windows and containers.

## Overview

Layout managers in ForGE handle the complex task of widget positioning and sizing. Instead of manually calculating coordinates, you describe how widgets should be arranged, and the layout manager handles the rest.

## Available Layout Managers

ForGE provides several layout managers:

- **Box Layout**: Horizontal or vertical linear arrangement
- **Grid Layout**: Table-like grid arrangement
- **Form Layout**: Two-column label-field arrangement
- **Stack Layout**: Layered widgets (one visible at a time)

## Box Layout

The simplest layout manager - arranges widgets in a line.

### Horizontal Box Layout

```fortran
program horizontal_box_demo
    use forge
    implicit none

    type(forge_application) :: app
    type(forge_window_t) :: window
    type(forge_button) :: button1, button2, button3
    type(forge_box_layout) :: layout
    type(forge_status) :: status

    ! Initialize application
    call app%init(BACKEND_CUSTOM, status)
    window = app%create_window("Horizontal Box Layout", 400, 100)

    ! Create buttons
    call button1%set_label("OK")
    call button2%set_label("Cancel")
    call button3%set_label("Help")

    ! Create horizontal layout
    call layout%set_orientation(LAYOUT_HORIZONTAL)
    call layout%set_spacing(10)  ! 10 pixels between widgets
    call layout%set_padding(20)  ! 20 pixels from window edges

    ! Add buttons to layout
    call layout%add_widget(button1)
    call layout%add_widget(button2)
    call layout%add_widget(button3)

    ! Set layout size and compute positions
    call layout%set_parent_size(400, 100)
    call layout%compute()

    ! Show window
    call window%show()
    call app%run()
    call app%shutdown()

end program horizontal_box_demo
```

### Vertical Box Layout

```fortran
! Create vertical layout
call layout%set_orientation(LAYOUT_VERTICAL)
call layout%set_spacing(15)

! Add widgets vertically
call layout%add_widget(title_label)
call layout%add_widget(content_area)
call layout%add_widget(button_bar)
```

## Grid Layout

Arranges widgets in a table-like grid with rows and columns.

```fortran
program grid_layout_demo
    use forge
    implicit none

    type(forge_application) :: app
    type(forge_window_t) :: window
    type(forge_label) :: name_label, email_label
    type(forge_entry) :: name_entry, email_entry
    type(forge_button) :: submit_button
    type(forge_grid_layout) :: grid
    type(forge_status) :: status

    ! Initialize
    call app%init(BACKEND_CUSTOM, status)
    window = app%create_window("Grid Layout Form", 400, 150)

    ! Create form elements
    call name_label%set_text("Name:")
    call name_entry%set_placeholder("Enter your name")

    call email_label%set_text("Email:")
    call email_entry%set_placeholder("Enter your email")

    call submit_button%set_label("Submit")

    ! Create 3x3 grid
    call grid%set_dimensions(3, 3)
    call grid%set_spacing(10)
    call grid%set_padding(15)

    ! Add widgets to grid positions
    call grid%add_widget(name_label, 1, 1)    ! Row 1, Column 1
    call grid%add_widget(name_entry, 1, 2, 1, 2)   ! Row 1, Columns 2-3

    call grid%add_widget(email_label, 2, 1)   ! Row 2, Column 1
    call grid%add_widget(email_entry, 2, 2, 1, 2)  ! Row 2, Columns 2-3

    call grid%add_widget(submit_button, 3, 3) ! Row 3, Column 3

    ! Compute layout
    call grid%set_parent_size(400, 150)
    call grid%compute()

    ! Show window
    call window%show()
    call app%run()
    call app%shutdown()

end program grid_layout_demo
```

### Grid Layout with Spanning

```fortran
! Widget spanning multiple cells
call grid%add_widget(title_label, 1, 1, 1, 3)  ! Full width title

! Normal widgets
call grid%add_widget(field1, 2, 1)
call grid%add_widget(field2, 2, 2)
call grid%add_widget(field3, 2, 3)
```

## Form Layout

Specialized layout for forms with labels and fields.

```fortran
program form_layout_demo
    use forge
    implicit none

    type(forge_application) :: app
    type(forge_window_t) :: window
    type(forge_label) :: name_label, age_label, email_label
    type(forge_entry) :: name_entry, email_entry
    type(forge_spin_button) :: age_spin
    type(forge_form_layout) :: form
    type(forge_status) :: status

    ! Initialize
    call app%init(BACKEND_CUSTOM, status)
    window = app%create_window("Form Layout", 350, 200)

    ! Create form elements
    call name_label%set_text("Name:")
    call name_entry%set_placeholder("Enter your name")

    call age_label%set_text("Age:")
    call age_spin%set_range(0.0_c_double, 120.0_c_double)

    call email_label%set_text("Email:")
    call email_entry%set_placeholder("Enter your email")

    ! Create form layout
    call form%set_spacing(10)
    call form%set_padding(20)

    ! Add label-field pairs
    call form%add_row(name_label, name_entry)
    call form%add_row(age_label, age_spin)
    call form%add_row(email_label, email_entry)

    ! Compute layout
    call form%set_parent_size(350, 200)
    call form%compute()

    ! Show window
    call window%show()
    call app%run()
    call app%shutdown()

end program form_layout_demo
```

## Stack Layout

Shows only one widget at a time, like tabbed content without tabs.

```fortran
program stack_layout_demo
    use forge
    implicit none

    type(forge_application) :: app
    type(forge_window_t) :: window
    type(forge_label) :: page1, page2, page3
    type(forge_button) :: next_button, prev_button
    type(forge_box_layout) :: button_layout, main_layout
    type(forge_stack_layout) :: stack
    type(forge_status) :: status

    ! Initialize
    call app%init(BACKEND_CUSTOM, status)
    window = app%create_window("Stack Layout", 300, 200)

    ! Create pages
    call page1%set_text("Welcome to page 1")
    call page2%set_text("This is page 2")
    call page3%set_text("Final page 3")

    ! Create navigation buttons
    call prev_button%set_label("Previous")
    call next_button%set_label("Next")

    ! Create stack layout for pages
    call stack%add_widget(page1)
    call stack%add_widget(page2)
    call stack%add_widget(page3)
    call stack%set_current(1)  ! Show first page

    ! Create button layout
    call button_layout%set_orientation(LAYOUT_HORIZONTAL)
    call button_layout%add_widget(prev_button)
    call button_layout%add_widget(next_button)

    ! Combine layouts
    call main_layout%set_orientation(LAYOUT_VERTICAL)
    call main_layout%add_widget(stack)
    call main_layout%add_widget(button_layout)

    ! Set up event handlers
    call next_button%on_click(go_next)
    call prev_button%on_click(go_previous)

    ! Compute layout
    call main_layout%set_parent_size(300, 200)
    call main_layout%compute()

    ! Show window
    call window%show()
    call app%run()
    call app%shutdown()

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

## Size Policies

Control how widgets behave in layouts using size policies.

### Expanding Widgets

```fortran
! Widget that grows to fill available space
call text_area%set_size_policy(QSizePolicy_Expanding, QSizePolicy_Expanding)

! Button that stays fixed size
call ok_button%set_size_policy(QSizePolicy_Preferred, QSizePolicy_Fixed)
```

### Common Size Policies

```fortran
! Fixed size - never changes
call widget%set_size_policy(QSizePolicy_Fixed, QSizePolicy_Fixed)

! Can grow from minimum size
call widget%set_size_policy(QSizePolicy_Minimum, QSizePolicy_Minimum)

! Uses preferred size when possible
call widget%set_size_policy(QSizePolicy_Preferred, QSizePolicy_Preferred)

! Wants all available space
call widget%set_size_policy(QSizePolicy_Expanding, QSizePolicy_Expanding)
```

## Nested Layouts

Combine layouts for complex interfaces.

```fortran
program nested_layouts_demo
    use forge
    implicit none

    type(forge_application) :: app
    type(forge_window_t) :: window
    type(forge_box_layout) :: main_layout, button_layout
    type(forge_form_layout) :: form_layout
    type(forge_label) :: title_label
    type(forge_button) :: ok_button, cancel_button
    type(forge_status) :: status

    ! Initialize
    call app%init(BACKEND_CUSTOM, status)
    window = app%create_window("Nested Layouts", 400, 300)

    ! Create title
    call title_label%set_text("User Registration")

    ! Create form layout
    ! ... add form fields to form_layout ...

    ! Create button layout
    call button_layout%set_orientation(LAYOUT_HORIZONTAL)
    call button_layout%add_widget(ok_button)
    call button_layout%add_widget(cancel_button)

    ! Create main vertical layout
    call main_layout%set_orientation(LAYOUT_VERTICAL)
    call main_layout%set_spacing(15)

    ! Nest layouts
    call main_layout%add_widget(title_label)
    call main_layout%add_widget(form_layout)      ! Nested form layout
    call main_layout%add_widget(button_layout)    ! Nested button layout

    ! Compute all layouts
    call main_layout%set_parent_size(400, 300)
    call main_layout%compute()

    ! Show window
    call window%show()
    call app%run()
    call app%shutdown()

end program nested_layouts_demo
```

## Qt-Style Convenience Classes

ForGE provides Qt-compatible layout classes.

```fortran
! Qt-style layouts
type(QHBoxLayout) :: hbox     ! Horizontal box
type(QVBoxLayout) :: vbox     ! Vertical box
type(QGridLayout) :: grid     ! Grid layout
type(QFormLayout) :: form     ! Form layout
type(QStackedLayout) :: stack ! Stack layout

! These work exactly like their forge_* counterparts
call hbox%add_widget(button1)
call hbox%add_widget(button2)
```

## Layout Computation

Layouts need to know the available space and be told to compute positions.

```fortran
! Set available space
call layout%set_parent_size(width, height)

! Compute positions and sizes
call layout%compute()

! Layout is now applied to widgets
```

## Dynamic Layouts

Update layouts when content changes.

```fortran
! Add widget dynamically
call layout%add_widget(new_button)

! Invalidate and recompute
call layout%invalidate()
call layout%set_parent_size(new_width, new_height)
call layout%compute()
```

## Best Practices

### Choose the Right Layout

- **Box Layout**: Simple linear arrangements
- **Grid Layout**: Structured forms and tables
- **Form Layout**: Label-field pairs
- **Stack Layout**: Wizards and tab content

### Use Size Policies

```fortran
! Good: Let layouts control sizing
call text_area%set_size_policy(QSizePolicy_Expanding, QSizePolicy_Expanding)
call button%set_size_policy(QSizePolicy_Preferred, QSizePolicy_Fixed)

! Avoid: Manual positioning
! call widget%set_position(100, 50)  ! Let layouts handle this
```

### Set Spacing and Padding

```fortran
! Consistent spacing
call layout%set_spacing(10)   ! Between widgets
call layout%set_padding(15)   ! From container edges
```

### Handle Resizing

```fortran
! Update layout when window resizes
subroutine on_window_resize(new_width, new_height)
    call main_layout%set_parent_size(new_width, new_height)
    call main_layout%compute()
end subroutine on_window_resize
```

## Common Mistakes

### Forgetting to Compute

```fortran
! Wrong: Layout not applied
call layout%add_widget(button)
call window%show()  ! Widgets not positioned!

! Right: Compute layout first
call layout%add_widget(button)
call layout%set_parent_size(400, 300)
call layout%compute()
call window%show()
```

### Manual Positioning

```fortran
! Wrong: Manual positioning defeats layout purpose
call button%set_position(10, 10)
call layout%add_widget(button)  ! Position ignored!

! Right: Let layout control positioning
call layout%add_widget(button)
call layout%compute()  ! Layout sets position
```

### Ignoring Size Policies

```fortran
! Wrong: All widgets same policy
call button1%set_size_policy(QSizePolicy_Fixed, QSizePolicy_Fixed)
call text_area%set_size_policy(QSizePolicy_Fixed, QSizePolicy_Fixed)

! Right: Appropriate policies
call button1%set_size_policy(QSizePolicy_Preferred, QSizePolicy_Fixed)
call text_area%set_size_policy(QSizePolicy_Expanding, QSizePolicy_Expanding)
```

## Performance Tips

- Call `compute()` only when needed
- Use `invalidate()` to mark layouts as dirty
- Cache layout computations when possible
- Avoid deep nesting of layouts

## Next Steps

- Read the [API documentation](../api/) for detailed layout method references
- Explore the [examples](../../examples/) directory for more layout examples
- Learn about [event handling](event_handling.md) for interactive layouts
- Study [widget gallery](widget_gallery.md) to see all available widgets