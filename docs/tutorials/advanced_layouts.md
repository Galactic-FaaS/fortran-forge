# Advanced Layout Techniques

This tutorial covers advanced layout techniques, nested layouts, responsive design, and complex UI organization in ForGE.

## Nested Layouts

Combine multiple layout managers for complex interfaces.

### Sidebar with Content Area

```fortran
program sidebar_layout_demo
    use forge
    implicit none

    type(forge_application) :: app
    type(forge_window_t) :: window

    ! Widgets
    type(forge_label) :: sidebar_title, content_title
    type(forge_button) :: button1, button2, button3
    type(forge_text_view) :: content_area

    ! Layouts
    type(forge_box_layout) :: main_layout, sidebar_layout
    type(forge_status) :: status

    ! Initialize
    call app%init(BACKEND_CUSTOM, status)
    window = app%create_window("Sidebar Layout", 600, 400)

    ! Create widgets
    call sidebar_title%set_text("Navigation")
    call content_title%set_text("Main Content")
    call content_area%set_text("Content area with lots of text...")
    call content_area%set_editable(.false.)

    call button1%set_label("Section 1")
    call button2%set_label("Section 2")
    call button3%set_label("Section 3")

    ! Sidebar layout (vertical)
    call sidebar_layout%set_orientation(LAYOUT_VERTICAL)
    call sidebar_layout%add_widget(sidebar_title)
    call sidebar_layout%add_widget(button1)
    call sidebar_layout%add_widget(button2)
    call sidebar_layout%add_widget(button3)

    ! Main layout (horizontal)
    call main_layout%set_orientation(LAYOUT_HORIZONTAL)
    call main_layout%set_spacing(10)

    ! Add nested layouts
    call main_layout%add_widget(sidebar_layout)  ! Sidebar takes preferred width
    call main_layout%add_widget(content_area)    ! Content expands

    ! Set size policies for proper sizing
    call sidebar_layout%set_size_policy(QSizePolicy_Preferred, QSizePolicy_Expanding)
    call content_area%set_size_policy(QSizePolicy_Expanding, QSizePolicy_Expanding)

    ! Compute layout
    call main_layout%set_parent_size(600, 400)
    call main_layout%compute()

    ! Show window
    call window%show()
    call app%run()
    call app%shutdown()

end program sidebar_layout_demo
```

### Form with Button Row

```fortran
program form_with_buttons_demo
    use forge
    implicit none

    type(forge_application) :: app
    type(forge_window_t) :: window

    ! Form widgets
    type(forge_label) :: name_label, email_label
    type(forge_entry) :: name_entry, email_entry

    ! Button widgets
    type(forge_button) :: ok_button, cancel_button, help_button

    ! Layouts
    type(forge_form_layout) :: form_layout
    type(forge_box_layout) :: button_layout, main_layout
    type(forge_status) :: status

    ! Initialize
    call app%init(BACKEND_CUSTOM, status)
    window = app%create_window("Form with Buttons", 400, 200)

    ! Create form fields
    call name_label%set_text("Name:")
    call name_entry%set_placeholder("Enter name")

    call email_label%set_text("Email:")
    call email_entry%set_placeholder("Enter email")

    ! Create buttons
    call ok_button%set_label("OK")
    call cancel_button%set_label("Cancel")
    call help_button%set_label("Help")

    ! Form layout
    call form_layout%add_row(name_label, name_entry)
    call form_layout%add_row(email_label, email_entry)

    ! Button layout (horizontal)
    call button_layout%set_orientation(LAYOUT_HORIZONTAL)
    call button_layout%set_spacing(5)
    call button_layout%add_widget(ok_button)
    call button_layout%add_widget(cancel_button)
    call button_layout%add_widget(help_button)

    ! Main layout (vertical)
    call main_layout%set_orientation(LAYOUT_VERTICAL)
    call main_layout%set_spacing(15)
    call main_layout%add_widget(form_layout)
    call main_layout%add_widget(button_layout)

    ! Set size policies
    call button_layout%set_size_policy(QSizePolicy_Expanding, QSizePolicy_Fixed)

    ! Compute layout
    call main_layout%set_parent_size(400, 200)
    call main_layout%compute()

    ! Show window
    call window%show()
    call app%run()
    call app%shutdown()

end program form_with_buttons_demo
```

## Grid Layout Techniques

Advanced grid layout patterns.

### Responsive Grid

```fortran
subroutine create_responsive_grid(grid, width, height)
    type(forge_grid_layout), intent(inout) :: grid
    integer, intent(in) :: width, height

    ! Clear existing widgets
    call grid%clear_items()

    if (width > height) then
        ! Wide layout: 2 columns
        call grid%set_dimensions(2, 2)
        call grid%add_widget(widget1, 1, 1)
        call grid%add_widget(widget2, 1, 2)
        call grid%add_widget(widget3, 2, 1)
        call grid%add_widget(widget4, 2, 2)
    else
        ! Tall layout: 4 rows, 1 column
        call grid%set_dimensions(4, 1)
        call grid%add_widget(widget1, 1, 1)
        call grid%add_widget(widget2, 2, 1)
        call grid%add_widget(widget3, 3, 1)
        call grid%add_widget(widget4, 4, 1)
    end if

    call grid%set_parent_size(width, height)
    call grid%compute()
end subroutine create_responsive_grid
```

### Complex Grid with Spanning

```fortran
program complex_grid_demo
    use forge
    implicit none

    type(forge_application) :: app
    type(forge_window_t) :: window
    type(forge_grid_layout) :: grid
    type(forge_label) :: title, label1, label2, label3, label4
    type(forge_entry) :: entry1, entry2, entry3, entry4
    type(forge_button) :: submit_button
    type(forge_status) :: status

    ! Initialize
    call app%init(BACKEND_CUSTOM, status)
    window = app%create_window("Complex Grid", 500, 300)

    ! Create widgets
    call title%set_text("Advanced Form")
    call label1%set_text("Field 1:")
    call label2%set_text("Field 2:")
    call label3%set_text("Field 3:")
    call label4%set_text("Field 4:")
    call submit_button%set_label("Submit")

    ! 4x4 grid layout
    call grid%set_dimensions(4, 4)
    call grid%set_spacing(5)

    ! Title spans full width
    call grid%add_widget(title, 1, 1, 1, 4)

    ! Form fields
    call grid%add_widget(label1, 2, 1)
    call grid%add_widget(entry1, 2, 2, 1, 2)
    call grid%add_widget(label2, 2, 4)

    call grid%add_widget(label3, 3, 1)
    call grid%add_widget(entry3, 3, 2, 1, 3)

    call grid%add_widget(label4, 4, 1)
    call grid%add_widget(entry4, 4, 2)
    call grid%add_widget(submit_button, 4, 4)

    ! Compute layout
    call grid%set_parent_size(500, 300)
    call grid%compute()

    ! Show window
    call window%show()
    call app%run()
    call app%shutdown()

end program complex_grid_demo
```

## Size Policy Strategies

Advanced size policy usage for responsive layouts.

### Equal Width Columns

```fortran
subroutine make_equal_width(widgets)
    type(forge_widget), intent(inout) :: widgets(:)

    integer :: i

    ! All widgets expand equally
    do i = 1, size(widgets)
        call widgets(i)%set_size_policy(QSizePolicy_Expanding, QSizePolicy_Fixed, 1, 0)
    end do
end subroutine make_equal_width
```

### Proportional Sizing

```fortran
subroutine set_proportional_widths(widgets, ratios)
    type(forge_widget), intent(inout) :: widgets(:)
    integer, intent(in) :: ratios(:)

    integer :: i

    do i = 1, size(widgets)
        call widgets(i)%set_size_policy(QSizePolicy_Expanding, QSizePolicy_Fixed, ratios(i), 0)
    end do
end subroutine set_proportional_widths

! Usage
call set_proportional_widths([sidebar, content], [1, 3])  ! 1:3 ratio
```

### Minimum Size Enforcement

```fortran
subroutine enforce_minimum_sizes(widgets, min_widths, min_heights)
    type(forge_widget), intent(inout) :: widgets(:)
    integer, intent(in) :: min_widths(:), min_heights(:)

    integer :: i

    do i = 1, size(widgets)
        call widgets(i)%set_minimum_size(min_widths(i), min_heights(i))
        call widgets(i)%set_size_policy(QSizePolicy_Preferred, QSizePolicy_Preferred)
    end do
end subroutine enforce_minimum_sizes
```

## Dynamic Layout Updates

Modify layouts at runtime.

### Adding Widgets Dynamically

```fortran
subroutine add_field_dynamically(form, label_text)
    type(forge_form_layout), intent(inout) :: form
    character(len=*), intent(in) :: label_text

    type(forge_label) :: new_label
    type(forge_entry) :: new_entry

    call new_label%set_text(label_text)
    call new_entry%set_placeholder("Enter value")

    call form%add_row(new_label, new_entry)

    ! Recompute layout
    call form%invalidate()
    call form%compute()
end subroutine add_field_dynamically
```

### Conditional Layouts

```fortran
subroutine update_layout_for_mode(layout, advanced_mode)
    type(forge_box_layout), intent(inout) :: layout
    logical, intent(in) :: advanced_mode

    ! Clear existing layout
    ! Note: This is conceptual - actual clearing depends on layout type

    if (advanced_mode) then
        ! Add advanced controls
        call layout%add_widget(advanced_panel)
        call layout%add_widget(expert_settings)
    else
        ! Add basic controls
        call layout%add_widget(basic_panel)
    end if

    call layout%compute()
end subroutine update_layout_for_mode
```

## Custom Layout Managers

Create specialized layout managers by extending the base class.

```fortran
type, extends(forge_layout_base) :: circular_layout
    private
    type(QList_widget) :: widgets
    real :: radius = 100.0
contains
    procedure :: add_widget => circular_add_widget
    procedure :: remove_widget => circular_remove_widget
    procedure :: compute => circular_compute
end type circular_layout

subroutine circular_compute(this)
    class(circular_layout), intent(inout) :: this

    integer :: i, num_widgets
    real :: angle_step, angle
    integer :: center_x, center_y

    num_widgets = this%widgets%size()
    if (num_widgets == 0) return

    angle_step = 2.0 * 3.14159 / num_widgets
    center_x = this%get_parent_width() / 2
    center_y = this%get_parent_height() / 2

    do i = 1, num_widgets
        angle = (i - 1) * angle_step
        widget_x = center_x + this%radius * cos(angle)
        widget_y = center_y + this%radius * sin(angle)

        ! Position widget at calculated location
        ! (Actual implementation would access widget through list)
    end do
end subroutine circular_compute
```

## Layout Debugging

Techniques for debugging layout issues.

### Visual Layout Debugging

```fortran
subroutine enable_layout_debugging(layout, enable)
    type(forge_layout_base), intent(inout) :: layout
    logical, intent(in) :: enable

    ! Enable visual debugging (backend-specific)
    call layout%set_debug_mode(enable)
end subroutine enable_layout_debugging
```

### Layout Information Logging

```fortran
subroutine log_layout_info(layout, label)
    type(forge_layout_base), intent(in) :: layout
    character(len=*), intent(in) :: label

    print *, label, " layout:"
    print *, "  Spacing:", layout%get_spacing()
    print *, "  Padding:", layout%get_padding()
    print *, "  Parent size:", layout%get_parent_width(), "x", layout%get_parent_height()
end subroutine log_layout_info
```

### Widget Position Verification

```fortran
subroutine verify_widget_positions(widgets)
    type(forge_widget), intent(in) :: widgets(:)

    integer :: i
    type(forge_position) :: pos
    type(forge_size) :: size

    do i = 1, size(widgets)
        pos = widgets(i)%get_position()
        size = widgets(i)%get_size()

        print *, "Widget", i, "at (", pos%get_x(), ",", pos%get_y(), &
                 ") size", size%get_width(), "x", size%get_height()
    end do
end subroutine verify_widget_positions
```

## Performance Optimization

Layout performance tips for complex UIs.

### Layout Caching

```fortran
type :: layout_cache
    logical :: valid = .false.
    integer :: cached_width = 0
    integer :: cached_height = 0
    ! Cached widget positions...
end type layout_cache

subroutine compute_cached_layout(layout, cache, width, height)
    type(forge_layout_base), intent(inout) :: layout
    type(layout_cache), intent(inout) :: cache
    integer, intent(in) :: width, height

    if (.not. cache%valid .or. &
        cache%cached_width /= width .or. &
        cache%cached_height /= height) then

        call layout%set_parent_size(width, height)
        call layout%compute()

        cache%valid = .true.
        cache%cached_width = width
        cache%cached_height = height
    end if
end subroutine compute_cached_layout
```

### Lazy Layout Updates

```fortran
type :: lazy_layout
    type(forge_layout_base), pointer :: layout
    logical :: needs_update = .true.
    integer :: pending_width = 0
    integer :: pending_height = 0
contains
    procedure :: request_update => lazy_request_update
    procedure :: apply_pending_update => lazy_apply_update
end type lazy_layout

subroutine lazy_request_update(this, width, height)
    class(lazy_layout), intent(inout) :: this
    integer, intent(in) :: width, height

    this%needs_update = .true.
    this%pending_width = width
    this%pending_height = height
end subroutine lazy_request_update

subroutine lazy_apply_update(this)
    class(lazy_layout), intent(inout) :: this

    if (this%needs_update) then
        call this%layout%set_parent_size(this%pending_width, this%pending_height)
        call this%layout%compute()
        this%needs_update = .false.
    end if
end subroutine lazy_apply_update
```

## Responsive Design

Create layouts that adapt to different screen sizes.

### Breakpoint-Based Layouts

```fortran
type :: responsive_layout
    type(forge_layout_base), allocatable :: small_layout, medium_layout, large_layout
    integer :: current_breakpoint = 0
contains
    procedure :: update_for_size => responsive_update_size
end type responsive_layout

subroutine responsive_update_size(this, width, height)
    class(responsive_layout), intent(inout) :: this
    integer, intent(in) :: width, height

    integer :: breakpoint

    ! Determine breakpoint
    if (width < 600) then
        breakpoint = 1  ! Small
    else if (width < 1200) then
        breakpoint = 2  ! Medium
    else
        breakpoint = 3  ! Large
    end if

    ! Switch layout if breakpoint changed
    if (breakpoint /= this%current_breakpoint) then
        this%current_breakpoint = breakpoint

        select case (breakpoint)
        case (1)
            call switch_to_layout(this%small_layout)
        case (2)
            call switch_to_layout(this%medium_layout)
        case (3)
            call switch_to_layout(this%large_layout)
        end select
    end if

    ! Update current layout
    call this%current_layout%set_parent_size(width, height)
    call this%current_layout%compute()
end subroutine responsive_update_size
```

### Fluid Layouts

```fortran
subroutine create_fluid_layout(layout, widgets, min_widths, max_widths)
    type(forge_box_layout), intent(inout) :: layout
    type(forge_widget), intent(inout) :: widgets(:)
    integer, intent(in) :: min_widths(:), max_widths(:)

    integer :: i, available_width, widget_width
    real :: total_weight, weight

    available_width = layout%get_parent_width() - layout%get_padding() * 2

    ! Calculate total weight
    total_weight = 0.0
    do i = 1, size(widgets)
        total_weight = total_weight + widgets(i)%get_size_policy()%get_horizontal_stretch()
    end do

    ! Distribute space proportionally
    do i = 1, size(widgets)
        weight = widgets(i)%get_size_policy()%get_horizontal_stretch()
        widget_width = (weight / total_weight) * available_width
        widget_width = max(min_widths(i), min(max_widths(i), widget_width))

        call widgets(i)%set_size(widget_width, widgets(i)%get_size()%get_height())
    end do
end subroutine create_fluid_layout
```

## Best Practices

### Layout Organization

1. **Plan your layout hierarchy** before implementing
2. **Use nested layouts** for complex UIs
3. **Choose appropriate layout managers** for each section
4. **Set size policies** thoughtfully
5. **Test with different window sizes**

### Performance Guidelines

1. **Minimize layout depth** - avoid deeply nested layouts
2. **Cache layout computations** when possible
3. **Use lazy updates** for dynamic content
4. **Batch layout changes** together
5. **Profile layout performance** in complex UIs

### Maintainability

1. **Name your layouts** descriptively
2. **Document layout purposes** in comments
3. **Separate layout logic** from business logic
4. **Use consistent spacing** and padding
5. **Test layout changes** thoroughly

## Common Patterns

### Master-Detail Layout

```fortran
! Sidebar (master) + Content area (detail)
call master%set_size_policy(QSizePolicy_Preferred, QSizePolicy_Expanding)
call detail%set_size_policy(QSizePolicy_Expanding, QSizePolicy_Expanding)
```

### Header-Body-Footer

```fortran
! Fixed header, expanding body, fixed footer
call header%set_size_policy(QSizePolicy_Expanding, QSizePolicy_Fixed)
call body%set_size_policy(QSizePolicy_Expanding, QSizePolicy_Expanding)
call footer%set_size_policy(QSizePolicy_Expanding, QSizePolicy_Fixed)
```

### Card-Based Layout

```fortran
! Grid of equally-sized cards
call grid%set_homogeneous(.true.)
do i = 1, num_cards
    call grid%add_widget(cards(i), row, col)
    ! Cards expand equally
end do
```

## Next Steps

- Read the [API documentation](../api/) for detailed layout method references
- Explore the [examples](../../examples/) directory for more advanced layout examples
- Learn about [event handling](event_handling.md) for dynamic layout updates
- Study [widget gallery](widget_gallery.md) for widget-specific layout considerations