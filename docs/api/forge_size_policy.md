# forge_size_policy

Size policy system for controlling widget sizing behavior in layout managers.

## Synopsis

```fortran
type :: forge_size_policy
    private
    integer :: horizontal_policy = QSizePolicy_Preferred
    integer :: vertical_policy = QSizePolicy_Preferred
    integer :: horizontal_stretch = 0
    integer :: vertical_stretch = 0
contains
    procedure :: set_horizontal_policy => forge_size_policy_set_horizontal_policy
    procedure :: set_vertical_policy => forge_size_policy_set_vertical_policy
    procedure :: set_horizontal_stretch => forge_size_policy_set_horizontal_stretch
    procedure :: set_vertical_stretch => forge_size_policy_set_vertical_stretch
    procedure :: get_horizontal_policy => forge_size_policy_get_horizontal_policy
    procedure :: get_vertical_policy => forge_size_policy_get_vertical_policy
    procedure :: get_horizontal_stretch => forge_size_policy_get_horizontal_stretch
    procedure :: get_vertical_stretch => forge_size_policy_get_vertical_stretch
    procedure :: calculate_horizontal_size => forge_size_policy_calculate_horizontal_size
    procedure :: calculate_vertical_size => forge_size_policy_calculate_vertical_size
end type forge_size_policy
```

## Description

The `forge_size_policy` controls how widgets behave when placed in layout managers. It determines:

- How widgets resize in response to available space
- Whether widgets have preferred, minimum, or maximum sizes
- How extra space is distributed among widgets
- Widget flexibility in different directions

Size policies work with layout managers to create responsive, well-behaved user interfaces.

## Size Policy Constants

ForGE defines Qt-style size policy constants:

```fortran
integer, parameter :: QSizePolicy_Fixed = 0        ! Widget cannot grow or shrink
integer, parameter :: QSizePolicy_Minimum = 1      ! Widget can grow, has minimum size
integer, parameter :: QSizePolicy_Maximum = 2      ! Widget can shrink, has maximum size
integer, parameter :: QSizePolicy_Preferred = 3    ! Widget has preferred size
integer, parameter :: QSizePolicy_Expanding = 4    ! Widget wants to expand
integer, parameter :: QSizePolicy_MinimumExpanding = 5  ! Widget has minimum, wants to expand
integer, parameter :: QSizePolicy_Ignored = 6      ! Widget ignores size constraints
```

## Properties

- `horizontal_policy`: Sizing behavior in horizontal direction
- `vertical_policy`: Sizing behavior in vertical direction
- `horizontal_stretch`: Stretch factor for horizontal expansion
- `vertical_stretch`: Stretch factor for vertical expansion

## Methods

### Policy Setting

#### set_horizontal_policy(policy)
Sets the horizontal size policy.

**Parameters:**
- `policy` (integer): QSizePolicy_* constant

**Example:**
```fortran
call policy%set_horizontal_policy(QSizePolicy_Expanding)
```

#### set_vertical_policy(policy)
Sets the vertical size policy.

**Parameters:**
- `policy` (integer): QSizePolicy_* constant

**Example:**
```fortran
call policy%set_vertical_policy(QSizePolicy_Fixed)
```

#### set_horizontal_stretch(stretch)
Sets the horizontal stretch factor.

**Parameters:**
- `stretch` (integer): Stretch factor (higher = more expansion)

**Example:**
```fortran
call policy%set_horizontal_stretch(2)  ! Twice the expansion of stretch=1
```

#### set_vertical_stretch(stretch)
Sets the vertical stretch factor.

**Parameters:**
- `stretch` (integer): Stretch factor

**Example:**
```fortran
call policy%set_vertical_stretch(1)
```

### Policy Access

#### get_horizontal_policy()
Gets the horizontal size policy.

**Returns:** integer - QSizePolicy_* constant

#### get_vertical_policy()
Gets the vertical size policy.

**Returns:** integer - QSizePolicy_* constant

#### get_horizontal_stretch()
Gets the horizontal stretch factor.

**Returns:** integer - Stretch factor

#### get_vertical_stretch()
Gets the vertical stretch factor.

**Returns:** integer - Stretch factor

### Size Calculation

#### calculate_horizontal_size(hint, min_size, max_size, available)
Calculates the actual horizontal size based on policy.

**Parameters:**
- `hint` (integer): Preferred size
- `min_size` (integer): Minimum size
- `max_size` (integer): Maximum size
- `available` (integer): Available space

**Returns:** integer - Calculated size

#### calculate_vertical_size(hint, min_size, max_size, available)
Calculates the actual vertical size based on policy.

**Parameters:** Same as horizontal version

**Returns:** integer - Calculated size

## Size Policy Types

### Fixed Policy
```fortran
! Widget maintains exact size, never changes
call widget%set_size_policy(QSizePolicy_Fixed, QSizePolicy_Fixed)
call widget%set_size_hint(100, 30)
```

### Minimum Policy
```fortran
! Widget can grow from minimum size
call widget%set_size_policy(QSizePolicy_Minimum, QSizePolicy_Minimum)
call widget%set_minimum_size(50, 20)
```

### Maximum Policy
```fortran
! Widget can shrink from maximum size
call widget%set_size_policy(QSizePolicy_Maximum, QSizePolicy_Maximum)
call widget%set_maximum_size(200, 100)
```

### Preferred Policy
```fortran
! Widget uses preferred size when possible
call widget%set_size_policy(QSizePolicy_Preferred, QSizePolicy_Preferred)
call widget%set_size_hint(120, 40)
```

### Expanding Policy
```fortran
! Widget wants all available space
call widget%set_size_policy(QSizePolicy_Expanding, QSizePolicy_Expanding)
```

### Minimum Expanding Policy
```fortran
! Widget has minimum size but wants to expand
call widget%set_size_policy(QSizePolicy_MinimumExpanding, QSizePolicy_MinimumExpanding)
call widget%set_minimum_size(100, 50)
```

### Ignored Policy
```fortran
! Widget ignores layout constraints (use with caution)
call widget%set_size_policy(QSizePolicy_Ignored, QSizePolicy_Ignored)
```

## Stretch Factors

Stretch factors control space distribution:

```fortran
! Three widgets with different stretch factors
call widget1%set_size_policy(QSizePolicy_Expanding, QSizePolicy_Fixed, 1, 0)
call widget2%set_size_policy(QSizePolicy_Expanding, QSizePolicy_Fixed, 2, 0)
call widget3%set_size_policy(QSizePolicy_Expanding, QSizePolicy_Fixed, 1, 0)

! widget2 gets twice as much horizontal space as widget1 and widget3
```

## Common Patterns

### Button Layout
```fortran
! OK button expands, Cancel stays fixed
call ok_button%set_size_policy(QSizePolicy_Expanding, QSizePolicy_Fixed)
call cancel_button%set_size_policy(QSizePolicy_Preferred, QSizePolicy_Fixed)
```

### Form Fields
```fortran
! Labels stay at preferred size, fields expand
call label%set_size_policy(QSizePolicy_Preferred, QSizePolicy_Preferred)
call entry%set_size_policy(QSizePolicy_Expanding, QSizePolicy_Fixed)
```

### Content Areas
```fortran
! Text area expands to fill space
call text_view%set_size_policy(QSizePolicy_Expanding, QSizePolicy_Expanding)
```

### Sidebar Layout
```fortran
! Sidebar has minimum width, content expands
call sidebar%set_size_policy(QSizePolicy_Minimum, QSizePolicy_Expanding, 0, 1)
call content%set_size_policy(QSizePolicy_Expanding, QSizePolicy_Expanding, 1, 1)
```

## Layout Integration

Size policies work with all layout managers:

```fortran
type(forge_box_layout) :: layout

! Add widgets with different policies
call layout%add_widget(fixed_button)    ! QSizePolicy_Fixed
call layout%add_widget(expand_button)   ! QSizePolicy_Expanding

! Layout respects policies when distributing space
call layout%compute()
```

## Size Hints and Constraints

Size policies interact with widget size hints:

```fortran
! Set preferred size
call widget%set_size_hint(150, 40)

! Set constraints
call widget%set_minimum_size(100, 30)
call widget%set_maximum_size(300, 60)

! Set policy
call widget%set_size_policy(QSizePolicy_Preferred, QSizePolicy_Preferred)
```

## Performance

Size policy calculations are efficient:

- O(1) policy evaluation
- Minimal memory usage
- Fast layout computation
- Cached size hint calculations

## Thread Safety

Size policy objects are thread-safe for reading. Modifications should be synchronized.

## Example Usage

```fortran
program size_policy_demo
    use forge
    implicit none

    type(forge_application) :: app
    type(forge_window_t) :: window
    type(forge_button) :: button1, button2, button3
    type(forge_box_layout) :: layout
    type(forge_status) :: status

    ! Initialize
    call app%init(BACKEND_CUSTOM, status)
    window = app%create_window("Size Policy Demo", 400, 100)

    ! Configure buttons with different policies
    call button1%set_label("Fixed")
    call button1%set_size_policy(QSizePolicy_Fixed, QSizePolicy_Fixed)
    call button1%set_size_hint(80, 30)

    call button2%set_label("Preferred")
    call button2%set_size_policy(QSizePolicy_Preferred, QSizePolicy_Fixed)

    call button3%set_label("Expanding")
    call button3%set_size_policy(QSizePolicy_Expanding, QSizePolicy_Fixed)

    ! Create horizontal layout
    call layout%set_orientation(LAYOUT_HORIZONTAL)
    call layout%set_spacing(5)

    ! Add buttons to layout
    call layout%add_widget(button1)
    call layout%add_widget(button2)
    call layout%add_widget(button3)

    ! Layout will distribute space: Fixed(80px) + spacing + Preferred(auto) + spacing + Expanding(rest)
    call layout%set_parent_size(400, 100)
    call layout%compute()

    ! Show window
    call window%show()
    call app%run()

end program size_policy_demo
```

## Advanced Usage

### Custom Size Policies
```fortran
subroutine set_custom_policy(widget, policy_type)
    type(forge_widget), intent(inout) :: widget
    integer, intent(in) :: policy_type

    select case (policy_type)
    case (POLICY_TOOLBAR_BUTTON)
        call widget%set_size_policy(QSizePolicy_Preferred, QSizePolicy_Fixed)
        call widget%set_size_hint(60, 25)
    case (POLICY_DIALOG_BUTTON)
        call widget%set_size_policy(QSizePolicy_Preferred, QSizePolicy_Fixed)
        call widget%set_size_hint(80, 30)
    case (POLICY_CONTENT_AREA)
        call widget%set_size_policy(QSizePolicy_Expanding, QSizePolicy_Expanding)
    end select
end subroutine set_custom_policy
```

### Responsive Policies
```fortran
subroutine adjust_policies_for_size(widgets, available_width)
    type(forge_widget), intent(inout) :: widgets(:)
    integer, intent(in) :: available_width

    if (available_width < 600) then
        ! Small screen: stack vertically
        call widgets(1)%set_size_policy(QSizePolicy_Expanding, QSizePolicy_Preferred)
        call widgets(2)%set_size_policy(QSizePolicy_Expanding, QSizePolicy_Expanding)
    else
        ! Large screen: side by side
        call widgets(1)%set_size_policy(QSizePolicy_Preferred, QSizePolicy_Expanding)
        call widgets(2)%set_size_policy(QSizePolicy_Expanding, QSizePolicy_Expanding)
    end if
end subroutine adjust_policies_for_size
```

### Policy Validation
```fortran
function is_valid_policy(policy) result(valid)
    type(forge_size_policy), intent(in) :: policy
    logical :: valid

    integer :: h_policy, v_policy

    h_policy = policy%get_horizontal_policy()
    v_policy = policy%get_vertical_policy()

    valid = (h_policy >= QSizePolicy_Fixed .and. h_policy <= QSizePolicy_Ignored) .and. &
            (v_policy >= QSizePolicy_Fixed .and. v_policy <= QSizePolicy_Ignored)
end function is_valid_policy
```

## See Also

- [forge_widget](forge_widget.md) - Widget size management
- [forge_layout_base](forge_layout_base.md) - Layout managers
- [forge_box_layout](forge_box_layout.md) - Box layout sizing
- [forge_grid_layout](forge_grid_layout.md) - Grid layout sizing