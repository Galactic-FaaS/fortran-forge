# Advanced Widgets Tutorial

This tutorial covers advanced widget usage, custom widget creation, and complex widget interactions in ForGE applications.

## Custom Widget Creation

### Extending Base Widgets

```fortran
! Custom button with additional features
type, extends(forge_button) :: toggle_button
    private
        logical :: is_toggled = .false.
        type(forge_string) :: on_text = forge_string("ON")
        type(forge_string) :: off_text = forge_string("OFF")
    contains
        procedure :: set_toggled => toggle_button_set_toggled
        procedure :: get_toggled => toggle_button_get_toggled
        procedure :: toggle => toggle_button_toggle
        procedure :: calculate_size_hint => toggle_button_calculate_size_hint
end type toggle_button

subroutine toggle_button_set_toggled(this, toggled)
    class(toggle_button), intent(inout) :: this
    logical, intent(in) :: toggled

    this%is_toggled = toggled
    if (toggled) then
        call this%set_label(this%on_text%get())
    else
        call this%set_label(this%off_text%get())
    end if
end subroutine toggle_button_set_toggled

function toggle_button_get_toggled(this) result(toggled)
    class(toggle_button), intent(in) :: this
    logical :: toggled
    toggled = this%is_toggled
end function toggle_button_get_toggled

subroutine toggle_button_toggle(this)
    class(toggle_button), intent(inout) :: this
    call this%set_toggled(.not. this%is_toggled)
end subroutine toggle_button_toggle

subroutine toggle_button_calculate_size_hint(this)
    class(toggle_button), intent(inout) :: this

    ! Calculate size based on both ON and OFF text
    integer :: on_width, off_width, max_width, height

    on_width = len(this%on_text%get()) * 8 + 20
    off_width = len(this%off_text%get()) * 8 + 20
    max_width = max(on_width, off_width)
    height = 16 + 10

    call this%set_size_hint(max_width, height)
end subroutine toggle_button_calculate_size_hint
```

### Completely Custom Widgets

```fortran
! Custom progress indicator
type, extends(forge_widget) :: circular_progress
    private
        real(c_double) :: progress = 0.0_c_double
        integer :: radius = 50
        type(forge_color) :: progress_color = forge_color(0, 255, 0, 255)
        type(forge_color) :: background_color = forge_color(200, 200, 200, 255)
    contains
        procedure :: set_progress => circular_progress_set_progress
        procedure :: get_progress => circular_progress_get_progress
        procedure :: set_radius => circular_progress_set_radius
        procedure :: calculate_size_hint => circular_progress_calculate_size_hint
end type circular_progress

subroutine circular_progress_set_progress(this, progress)
    class(circular_progress), intent(inout) :: this
    real(c_double), intent(in) :: progress

    this%progress = max(0.0_c_double, min(1.0_c_double, progress))
    call this%invalidate_size_hint()  ! Size might change
end subroutine circular_progress_set_progress

function circular_progress_get_progress(this) result(progress)
    class(circular_progress), intent(in) :: this
    real(c_double) :: progress
    progress = this%progress
end function circular_progress_get_progress

subroutine circular_progress_set_radius(this, radius)
    class(circular_progress), intent(inout) :: this
    integer, intent(in) :: radius

    this%radius = max(10, radius)
    call this%invalidate_size_hint()
end subroutine circular_progress_set_radius

subroutine circular_progress_calculate_size_hint(this)
    class(circular_progress), intent(inout) :: this

    integer :: diameter
    diameter = this%radius * 2 + 10  ! +10 for padding
    call this%set_size_hint(diameter, diameter)
end subroutine circular_progress_calculate_size_hint
```

## Composite Widgets

### Widget Groups

```fortran
! Group of related controls
type :: color_picker
    type(forge_label) :: label
    type(forge_spin_button) :: red_spin, green_spin, blue_spin
    type(forge_label) :: red_label, green_label, blue_label
    type(forge_box_layout) :: layout
contains
    procedure :: init => color_picker_init
    procedure :: set_color => color_picker_set_color
    procedure :: get_color => color_picker_get_color
end type color_picker

subroutine color_picker_init(this, parent_layout)
    class(color_picker), intent(inout) :: this
    class(forge_layout_base), intent(inout) :: parent_layout

    ! Initialize components
    call this%label%set_text("Color:")
    call this%red_label%set_text("R:")
    call this%green_label%set_text("G:")
    call this%blue_label%set_text("B:")

    ! Configure spin buttons
    call this%red_spin%set_range(0.0_c_double, 255.0_c_double)
    call this%green_spin%set_range(0.0_c_double, 255.0_c_double)
    call this%blue_spin%set_range(0.0_c_double, 255.0_c_double)

    call this%red_spin%set_value(255.0_c_double)    ! Red default
    call this%green_spin%set_value(255.0_c_double)  ! White
    call this%blue_spin%set_value(255.0_c_double)

    ! Set up layout
    call this%layout%set_orientation(LAYOUT_VERTICAL)
    call this%layout%add_widget(this%label)

    ! Red row
    type(forge_box_layout) :: red_row
    call red_row%set_orientation(LAYOUT_HORIZONTAL)
    call red_row%add_widget(this%red_label)
    call red_row%add_widget(this%red_spin)
    call this%layout%add_widget(red_row)

    ! Green row
    type(forge_box_layout) :: green_row
    call green_row%set_orientation(LAYOUT_HORIZONTAL)
    call green_row%add_widget(this%green_label)
    call green_row%add_widget(this%green_spin)
    call this%layout%add_widget(green_row)

    ! Blue row
    type(forge_box_layout) :: blue_row
    call blue_row%set_orientation(LAYOUT_HORIZONTAL)
    call blue_row%add_widget(this%blue_label)
    call blue_row%add_widget(this%blue_spin)
    call this%layout%add_widget(blue_row)

    ! Add to parent
    call parent_layout%add_widget(this%layout)
end subroutine color_picker_init
```

### Form Builders

```fortran
! Fluent form builder
type :: form_builder
    type(forge_form_layout) :: layout
    integer :: current_row = 1
contains
    procedure :: add_text_field => form_builder_add_text_field
    procedure :: add_number_field => form_builder_add_number_field
    procedure :: add_checkbox => form_builder_add_checkbox
    procedure :: build => form_builder_build
end type form_builder

function form_builder_add_text_field(this, label_text, placeholder) result(field)
    class(form_builder), intent(inout) :: this
    character(len=*), intent(in) :: label_text, placeholder
    type(forge_entry) :: field

    type(forge_label) :: label

    call label%set_text(label_text)
    call field%set_placeholder(placeholder)

    call this%layout%add_row(label, field)
    this%current_row = this%current_row + 1
end function form_builder_add_text_field

function form_builder_add_number_field(this, label_text, min_val, max_val) result(field)
    class(form_builder), intent(inout) :: this
    character(len=*), intent(in) :: label_text
    real(c_double), intent(in) :: min_val, max_val
    type(forge_spin_button) :: field

    type(forge_label) :: label

    call label%set_text(label_text)
    call field%set_range(min_val, max_val)

    call this%layout%add_row(label, field)
    this%current_row = this%current_row + 1
end function form_builder_add_number_field

function form_builder_build(this) result(layout)
    class(form_builder), intent(in) :: this
    type(forge_form_layout) :: layout

    layout = this%layout
end function form_builder_build
```

## Advanced Event Handling

### Event Filtering

```fortran
! Custom event filter
type :: numeric_filter
    procedure(validate_number), pointer, nopass :: validator => null()
contains
    procedure :: filter_event => numeric_filter_event
end type numeric_filter

subroutine numeric_filter_event(this, event, accept)
    class(numeric_filter), intent(in) :: this
    type(forge_event), intent(inout) :: event
    logical, intent(out) :: accept

    accept = .true.  ! Default to accept

    select case (event%type)
    case (EVENT_TEXT_CHANGED)
        ! Validate numeric input
        if (.not. is_numeric(event%data%text)) then
            accept = .false.
            ! Could show error message or beep
        end if
    end select
end subroutine numeric_filter_event

function is_numeric(text) result(valid)
    character(len=*), intent(in) :: text
    logical :: valid

    integer :: i, char_code

    valid = .true.
    do i = 1, len(text)
        char_code = ichar(text(i:i))
        if (.not. ((char_code >= 48 .and. char_code <= 57) .or. &  ! 0-9
                   char_code == 46 .or. char_code == 45)) then     ! . -
            valid = .false.
            exit
        end if
    end do
end function is_numeric
```

### Custom Events

```fortran
! Define custom event types
integer, parameter :: EVENT_COLOR_CHANGED = 1001
integer, parameter :: EVENT_VALIDATION_FAILED = 1002

type :: color_changed_event
    integer :: event_type = EVENT_COLOR_CHANGED
    integer :: red, green, blue
end type color_changed_event

subroutine fire_color_changed(r, g, b)
    integer, intent(in) :: r, g, b

    type(color_changed_event) :: event
    event%red = r
    event%green = g
    event%blue = b

    ! Dispatch to registered listeners
    call dispatch_custom_event(event)
end subroutine fire_color_changed
```

### Event Bubbling

```fortran
! Event bubbling system
type :: event_bubble
    class(forge_widget), pointer :: target => null()
    type(forge_event) :: event
    logical :: handled = .false.
contains
    procedure :: bubble_up => event_bubble_up
end type event_bubble

recursive subroutine event_bubble_up(this)
    class(event_bubble), intent(inout) :: this

    ! Handle at current level
    if (associated(this%target)) then
        call handle_event_at_level(this%target, this%event, this%handled)
    end if

    ! Bubble up to parent if not handled
    if (.not. this%handled) then
        ! Find parent and continue bubbling
        ! Implementation depends on widget hierarchy
    end if
end subroutine event_bubble_up
```

## Dynamic Widget Management

### Widget Factories

```fortran
! Widget factory for creating widgets by type
type :: widget_factory
contains
    procedure :: create_widget => widget_factory_create
end type widget_factory

function widget_factory_create(this, widget_type, config) result(widget)
    class(widget_factory), intent(in) :: this
    character(len=*), intent(in) :: widget_type
    type(widget_config), intent(in) :: config
    class(forge_widget), allocatable :: widget

    select case (widget_type)
    case ("button")
        allocate(forge_button :: widget)
        select type (w => widget)
        type is (forge_button)
            call w%set_label(config%text)
        end select

    case ("entry")
        allocate(forge_entry :: widget)
        select type (w => widget)
        type is (forge_entry)
            call w%set_placeholder(config%placeholder)
        end select

    case ("label")
        allocate(forge_label :: widget)
        select type (w => widget)
        type is (forge_label)
            call w%set_text(config%text)
        end select

    case default
        ! Unknown widget type
        allocate(forge_label :: widget)  ! Fallback
    end select
end function widget_factory_create
```

### Dynamic Form Generation

```fortran
subroutine create_form_from_schema(schema, layout)
    type(form_schema), intent(in) :: schema
    type(forge_form_layout), intent(inout) :: layout

    integer :: i
    type(widget_factory) :: factory

    do i = 1, size(schema%fields)
        class(forge_widget), allocatable :: label, field

        ! Create label
        label = factory%create_widget("label", &
                                    widget_config(text=schema%fields(i)%label))

        ! Create field based on type
        select case (schema%fields(i)%type)
        case ("text")
            field = factory%create_widget("entry", &
                                        widget_config(placeholder=schema%fields(i)%placeholder))
        case ("number")
            ! Create spin button
        case ("boolean")
            ! Create checkbox
        end select

        ! Add to layout
        call layout%add_row(label, field)
    end do
end subroutine create_form_from_schema
```

## Advanced Layout Techniques

### Constraint-Based Layout

```fortran
! Constraint-based layout system
type :: constraint_layout
    type(widget_constraint), allocatable :: constraints(:)
    class(forge_layout_base), pointer :: base_layout => null()
contains
    procedure :: add_constraint => constraint_layout_add_constraint
    procedure :: apply_constraints => constraint_layout_apply_constraints
end type constraint_layout

type :: widget_constraint
    class(forge_widget), pointer :: widget => null()
    integer :: constraint_type
    real :: value
    class(forge_widget), pointer :: relative_to => null()
end type widget_constraint

subroutine constraint_layout_add_constraint(this, widget, constraint_type, value, relative_to)
    class(constraint_layout), intent(inout) :: this
    class(forge_widget), intent(in), target :: widget
    integer, intent(in) :: constraint_type
    real, intent(in) :: value
    class(forge_widget), intent(in), optional, target :: relative_to

    type(widget_constraint) :: constraint

    constraint%widget => widget
    constraint%constraint_type = constraint_type
    constraint%value = value
    if (present(relative_to)) constraint%relative_to => relative_to

    ! Add to constraints array
    ! ... implementation ...
end subroutine constraint_layout_add_constraint

subroutine constraint_layout_apply_constraints(this)
    class(constraint_layout), intent(inout) :: this

    integer :: i
    type(forge_position) :: pos
    type(forge_size) :: size

    ! Apply each constraint
    do i = 1, size(this%constraints)
        associate (constraint => this%constraints(i))
            select case (constraint%constraint_type)
            case (CONSTRAINT_WIDTH_EQUALS_HEIGHT)
                size = constraint%widget%get_size()
                call constraint%widget%set_size(size%width, size%width)

            case (CONSTRAINT_BELOW_WIDGET)
                if (associated(constraint%relative_to)) then
                    pos = constraint%relative_to%get_position()
                    size = constraint%relative_to%get_size()
                    call constraint%widget%set_position(pos%x, pos%y + size%height + 10)
                end if

            case (CONSTRAINT_MIN_WIDTH)
                size = constraint%widget%get_size()
                if (size%width < constraint%value) then
                    call constraint%widget%set_size(int(constraint%value), size%height)
                end if
            end select
        end associate
    end do
end subroutine constraint_layout_apply_constraints
```

### Responsive Layouts

```fortran
type :: responsive_layout
    type(breakpoint_config), allocatable :: breakpoints(:)
    integer :: current_breakpoint = 1
contains
    procedure :: add_breakpoint => responsive_layout_add_breakpoint
    procedure :: update_for_size => responsive_layout_update_for_size
end type responsive_layout

type :: breakpoint_config
    integer :: min_width, max_width
    procedure(layout_configurator), pointer, nopass :: configure => null()
end type breakpoint_config

subroutine responsive_layout_update_for_size(this, width, height)
    class(responsive_layout), intent(inout) :: this
    integer, intent(in) :: width, height

    integer :: i, new_breakpoint = 0

    ! Find appropriate breakpoint
    do i = 1, size(this%breakpoints)
        if (width >= this%breakpoints(i)%min_width .and. &
            (this%breakpoints(i)%max_width == 0 .or. &
             width <= this%breakpoints(i)%max_width)) then
            new_breakpoint = i
            exit
        end if
    end do

    ! Apply configuration if breakpoint changed
    if (new_breakpoint /= this%current_breakpoint .and. new_breakpoint > 0) then
        call this%breakpoints(new_breakpoint)%configure()
        this%current_breakpoint = new_breakpoint
    end if
end subroutine responsive_layout_update_for_size
```

## Widget Styling

### Custom Styling

```fortran
! Widget styling system
type :: widget_style
    type(forge_color) :: background_color
    type(forge_color) :: foreground_color
    integer :: font_size = 12
    character(len=:), allocatable :: font_family
    integer :: border_width = 1
    type(forge_color) :: border_color
contains
    procedure :: apply => widget_style_apply
end type widget_style

subroutine widget_style_apply(this, widget)
    class(widget_style), intent(in) :: this
    class(forge_widget), intent(inout) :: widget

    ! Apply styling (backend-specific implementation)
    call widget%set_background_color(this%background_color)
    call widget%set_foreground_color(this%foreground_color)
    ! ... other styling properties ...
end subroutine widget_style_apply

! Style themes
type(widget_style) :: dark_theme_button
dark_theme_button%background_color = forge_color(64, 64, 64, 255)
dark_theme_button%foreground_color = forge_color(255, 255, 255, 255)
dark_theme_button%border_color = forge_color(128, 128, 128, 255)
```

### Style Sheets

```fortran
! Simple CSS-like styling
type :: style_sheet
    type(style_rule), allocatable :: rules(:)
contains
    procedure :: add_rule => style_sheet_add_rule
    procedure :: apply_to_widget => style_sheet_apply_to_widget
end type style_sheet

type :: style_rule
    character(len=:), allocatable :: selector  ! e.g., "button", ".primary"
    type(widget_style) :: style
end type style_rule

subroutine style_sheet_apply_to_widget(this, widget, class_name)
    class(style_sheet), intent(in) :: this
    class(forge_widget), intent(inout) :: widget
    character(len=*), intent(in) :: class_name

    integer :: i

    ! Apply matching rules
    do i = 1, size(this%rules)
        if (matches_selector(this%rules(i)%selector, widget, class_name)) then
            call this%rules(i)%style%apply(widget)
        end if
    end do
end subroutine style_sheet_apply_to_widget

function matches_selector(selector, widget, class_name) result(matches)
    character(len=*), intent(in) :: selector
    class(forge_widget), intent(in) :: widget
    character(len=*), intent(in) :: class_name
    logical :: matches

    ! Simple selector matching
    select case (selector)
    case ("button")
        matches = same_type_as(widget, forge_button())
    case ("entry")
        matches = same_type_as(widget, forge_entry())
    case default
        if (selector(1:1) == ".") then
            matches = trim(selector(2:)) == trim(class_name)
        else
            matches = .false.
        end if
    end select
end function matches_selector
```

## Performance Optimization

### Widget Pooling

```fortran
! Object pool for widgets
type :: widget_pool
    class(forge_widget), allocatable :: pool(:)
    logical, allocatable :: available(:)
    integer :: pool_size = 50
contains
    procedure :: get_widget => widget_pool_get
    procedure :: return_widget => widget_pool_return
end type widget_pool

function widget_pool_get(this, widget_type) result(widget)
    class(widget_pool), intent(inout) :: this
    character(len=*), intent(in) :: widget_type
    class(forge_widget), pointer :: widget

    integer :: i

    ! Find available widget
    do i = 1, this%pool_size
        if (this%available(i)) then
            this%available(i) = .false.
            widget => this%pool(i)
            return
        end if
    end do

    widget => null()  ! Pool exhausted
end function widget_pool_get
```

### Lazy Widget Loading

```fortran
type :: lazy_widget_container
    character(len=:), allocatable :: widget_type
    type(widget_config) :: config
    class(forge_widget), pointer :: widget => null()
    logical :: loaded = .false.
contains
    procedure :: get_widget => lazy_widget_container_get
end type lazy_widget_container

function lazy_widget_container_get(this) result(widget)
    class(lazy_widget_container), intent(inout) :: this
    class(forge_widget), pointer :: widget

    if (.not. this%loaded) then
        ! Create widget on first access
        this%widget => create_widget_from_config(this%widget_type, this%config)
        this%loaded = .true.
    end if

    widget => this%widget
end function lazy_widget_container_get
```

## Testing Custom Widgets

### Widget Testing Framework

```fortran
module widget_test_framework
    implicit none

contains

    subroutine test_widget_lifecycle(widget)
        class(forge_widget), intent(inout) :: widget

        ! Test creation
        call assert(widget%is_visible(), "Widget should be visible by default")
        call assert(widget%is_enabled(), "Widget should be enabled by default")

        ! Test visibility
        call widget%hide()
        call assert(.not. widget%is_visible(), "Widget should be hidden")

        call widget%show()
        call assert(widget%is_visible(), "Widget should be visible")

        ! Test sizing
        call widget%set_size(100, 50)
        size = widget%get_size()
        call assert(size%width == 100 .and. size%height == 50, "Size should be set correctly")
    end subroutine test_widget_lifecycle

    subroutine test_widget_events(widget)
        class(forge_widget), intent(inout) :: widget

        logical :: event_received = .false.

        ! Test event handling
        select type (widget)
        type is (forge_button)
            call widget%on_click(test_event_handler)
        end select

        ! Simulate event (implementation-specific)
        call simulate_button_click(widget)
        call assert(event_received, "Event should be received")

    contains

        subroutine test_event_handler(event)
            type(forge_event), intent(in) :: event
            event_received = .true.
        end subroutine test_event_handler
    end subroutine test_widget_events

end module widget_test_framework
```

## Next Steps

- Read the [API documentation](../api/) for detailed widget method references
- Explore the [examples](../../examples/) directory for advanced widget usage
- Learn about [layout managers](layout_managers.md) for widget arrangement
- Study [event handling](event_handling.md) for widget interactions