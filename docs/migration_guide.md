# Migration Guide

This guide helps developers migrate existing Fortran GUI applications to ForGE, covering common migration patterns, compatibility considerations, and modernization strategies.

## Assessing Your Current Application

### Code Inventory

First, analyze your existing codebase:

```bash
# Count lines of code by file type
find . -name "*.f90" -o -name "*.f" -o -name "*.for" | xargs wc -l

# Find GUI-related code
grep -r "window\|widget\|button\|dialog" --include="*.f90" .

# Identify external dependencies
grep -r "use \|include" --include="*.f90" . | sort | uniq
```

### GUI Framework Analysis

Identify current GUI implementation:

```fortran
! Common patterns to look for:

! Direct API calls (Windows)
! call CreateWindow(...)
! call ShowWindow(...)

! GTK+ bindings
! use gtk
! call gtk_init(...)

! Qt bindings
! use qt_bindings
! type(QWidget) :: window

! Custom drawing
! call draw_line(...)
! call fill_rect(...)
```

## Migration Strategies

### Incremental Migration

Migrate components gradually rather than all at once:

```fortran
! Phase 1: Core application structure
module my_app
    use forge  ! New ForGE imports
    ! use old_gui  ! Keep old imports temporarily
    implicit none
contains
    subroutine init_app()
        ! Old initialization
        ! call old_gui_init()

        ! New ForGE initialization
        type(forge_application) :: app
        call app%init(BACKEND_CUSTOM)
    end subroutine init_app
end module my_app
```

### Wrapper Approach

Create compatibility wrappers:

```fortran
module gui_wrapper
    use forge
    ! use old_gui_library
    implicit none

    ! Wrapper types
    type :: window_wrapper
        type(forge_window_t) :: forge_window
        ! integer :: old_window_handle  ! Keep old handle for compatibility
    contains
        procedure :: create => wrapper_create_window
        procedure :: show => wrapper_show_window
    end type window_wrapper

contains

    subroutine wrapper_create_window(this, title, width, height)
        class(window_wrapper), intent(inout) :: this
        character(len=*), intent(in) :: title
        integer, intent(in) :: width, height

        ! Create ForGE window
        this%forge_window = app%create_window(title, width, height)

        ! Optionally create old window for compatibility
        ! this%old_window_handle = old_create_window(title, width, height)
    end subroutine wrapper_create_window

end module gui_wrapper
```

## Converting Window Management

### From Windows API

```fortran
! Old Windows API code
integer :: hwnd
hwnd = CreateWindow("MyClass", "Title", WS_OVERLAPPEDWINDOW, &
                   100, 100, 400, 300, NULL, NULL, hInstance, NULL)
call ShowWindow(hwnd, SW_SHOW)

! New ForGE code
type(forge_window_t) :: window
window = app%create_window("Title", 400, 300)
call window%set_position(100, 100)
call window%show()
```

### From GTK

```fortran
! Old GTK code
type(c_ptr) :: window
window = gtk_window_new(GTK_WINDOW_TOPLEVEL)
call gtk_window_set_title(window, "Title"//c_null_char)
call gtk_widget_show(window)

! New ForGE code
type(forge_window_t) :: window
window = app%create_window("Title", 400, 300)
call window%show()
```

### From Qt

```fortran
! Old Qt code
type(QWidget) :: window
call window%setWindowTitle("Title")
call window%resize(400, 300)
call window%show()

! New ForGE code
type(forge_window_t) :: window
window = app%create_window("Title", 400, 300)
call window%show()
```

## Converting Widgets

### Button Migration

```fortran
! Old custom button
type :: old_button
    integer :: x, y, width, height
    character(len=100) :: text
contains
    procedure :: draw
    procedure :: handle_click
end type old_button

! New ForGE button
type(forge_button) :: button
call button%set_label("Click Me")
call button%on_click(handle_click)

subroutine handle_click(event)
    type(forge_event), intent(in) :: event
    ! Handle click
end subroutine handle_click
```

### Text Input Migration

```fortran
! Old text input
character(len=256) :: input_buffer
logical :: input_active

! New ForGE entry
type(forge_entry) :: text_entry
call text_entry%set_placeholder("Enter text...")
call text_entry%on_change(handle_text_change)

subroutine handle_text_change(event)
    type(forge_event), intent(in) :: event
    character(len=:), allocatable :: text
    text = text_entry%get_text()
    ! Process text
end subroutine handle_text_change
```

### List/Tree Migration

```fortran
! Old list implementation
type :: old_list
    character(len=100), allocatable :: items(:)
    integer :: selected_index
end type old_list

! New ForGE combo box
type(forge_combo_box) :: combo
call combo%add_item("Item 1")
call combo%add_item("Item 2")
call combo%on_selection_changed(handle_selection)

subroutine handle_selection(event)
    type(forge_event), intent(in) :: event
    character(len=:), allocatable :: selected
    selected = combo%get_selected_text()
    ! Process selection
end subroutine handle_selection
```

## Converting Event Handling

### From Callback-Based Systems

```fortran
! Old callback system
subroutine old_button_callback(button_id)
    integer, intent(in) :: button_id
    select case (button_id)
    case (BUTTON_OK)
        call handle_ok()
    case (BUTTON_CANCEL)
        call handle_cancel()
    end select
end subroutine old_button_callback

! New ForGE event system
type(forge_button) :: ok_button, cancel_button

call ok_button%on_click(handle_ok_click)
call cancel_button%on_click(handle_cancel_click)

subroutine handle_ok_click(event)
    type(forge_event), intent(in) :: event
    call handle_ok()
end subroutine handle_ok_click
```

### From Message Loops

```fortran
! Old Windows message loop
do while (GetMessage(msg, NULL, 0, 0) /= 0)
    call TranslateMessage(msg)
    call DispatchMessage(msg)
end do

! New ForGE event loop
call app%run()  ! Handles event loop internally
```

## Converting Drawing Operations

### From Direct Drawing

```fortran
! Old direct drawing
call draw_rectangle(x, y, width, height)
call draw_text(x, y, "Hello")

! New ForGE drawing (if needed)
type(forge_painter) :: painter
call painter%begin(window)
call painter%draw_rect(x, y, width, height)
call painter%draw_text(x, y, "Hello")
call painter%end()
```

### From Custom Widgets

```fortran
! Old custom widget
type :: old_custom_widget
contains
    procedure :: paint
end type old_custom_widget

subroutine paint(this, dc)
    class(old_custom_widget), intent(in) :: this
    type(device_context), intent(in) :: dc
    ! Custom drawing code
end subroutine paint

! New ForGE custom widget
type, extends(forge_widget) :: custom_widget
contains
    procedure :: calculate_size_hint
end type custom_widget

subroutine calculate_size_hint(this)
    class(custom_widget), intent(inout) :: this
    ! Size calculation
end subroutine calculate_size_hint
```

## Converting Layout Management

### From Manual Positioning

```fortran
! Old manual positioning
button1%x = 10
button1%y = 10
button1%width = 100
button1%height = 30

button2%x = 120
button2%y = 10
button2%width = 100
button2%height = 30

! New ForGE layout
type(forge_box_layout) :: layout
call layout%set_orientation(LAYOUT_HORIZONTAL)
call layout%add_widget(button1)
call layout%add_widget(button2)
call layout%compute()
```

### From Grid-Based Layouts

```fortran
! Old grid positioning
widgets(1,1)%x = 10
widgets(1,1)%y = 10
widgets(1,2)%x = 120
widgets(1,2)%y = 10

! New ForGE grid layout
type(forge_grid_layout) :: grid
call grid%set_dimensions(2, 2)
call grid%add_widget(widget1, 1, 1)
call grid%add_widget(widget2, 1, 2)
call grid%compute()
```

## Converting Resource Management

### From External Files

```fortran
! Old external file loading
call load_image("logo.png", logo_image)
call load_text("help.txt", help_text)

! New ForGE embedded resources
! Use rcc tool to embed resources
! logo_data => qresource_get('images/logo.png')
! help_text = load_embedded_text('help.txt')
```

### From Dynamic Libraries

```fortran
! Old plugin loading
call load_library("plugin.dll", plugin_handle)
call get_proc_address(plugin_handle, "init_plugin", init_proc)

! New ForGE plugin system (future)
! call plugin_manager%load_plugin("plugin.dll")
! call plugin_manager%get_procedure("init_plugin", init_proc)
```

## Converting Build Systems

### From Make to CMake

```makefile
# Old Makefile
FC = gfortran
FFLAGS = -O2
LIBS = -lgtk-3

my_app: main.f90 gui.f90
	$(FC) $(FFLAGS) $^ $(LIBS) -o $@

# New CMakeLists.txt
cmake_minimum_required(VERSION 3.16)
project(my_app Fortran)

find_package(ForGE REQUIRED)

add_executable(my_app main.f90 gui.f90)
target_link_libraries(my_app ForGE::forge)
```

### From Custom Build to FPM

```toml
# New fpm.toml
name = "my_app"
version = "1.0.0"

[dependencies]
ForGE = { git = "https://github.com/your-org/fortran-forge.git" }

[[executable]]
name = "my_app"
source-dir = "src"
main = "main.f90"
```

## Converting Data Models

### From Custom Structures

```fortran
! Old data structures
type :: old_user
    integer :: id
    character(len=50) :: name
    character(len=100) :: email
end type old_user

! New ForGE-compatible structures
type, extends(forge_qobject) :: user_model
    integer :: id
    type(forge_string) :: name
    type(forge_string) :: email
contains
    procedure :: to_json => user_to_json
    procedure :: from_json => user_from_json
end type user_model
```

### From File-Based Storage

```fortran
! Old file I/O
subroutine save_users(filename, users)
    character(len=*), intent(in) :: filename
    type(user), intent(in) :: users(:)
    integer :: unit, i

    open(newunit=unit, file=filename)
    do i = 1, size(users)
        write(unit, *) users(i)%id, users(i)%name, users(i)%email
    end do
    close(unit)
end subroutine save_users

! New ForGE data persistence
subroutine save_users_json(filename, users)
    character(len=*), intent(in) :: filename
    type(user_model), intent(in) :: users(:)
    type(json_object) :: json_array
    integer :: i

    json_array = json_array()
    do i = 1, size(users)
        call json_array%add(users(i)%to_json())
    end do

    call json_array%write(filename)
end subroutine save_users_json
```

## Testing Migration

### Unit Test Migration

```fortran
! Old test framework
subroutine test_old_button()
    type(old_button) :: button
    call button%init()
    call assert(button%is_visible(), "Button should be visible")
end subroutine test_old_button

! New ForGE test
subroutine test_forge_button()
    use fruit  ! Testing framework
    type(forge_button) :: button

    ! Test button functionality
    call button%set_label("Test")
    call assert_equals("Test", button%get_label(), "Label should be set")

    ! Test event handling
    call button%on_click(test_click_handler)
    ! Simulate click and verify handler called
end subroutine test_forge_button
```

### Integration Test Migration

```fortran
! Old integration test
subroutine test_full_application()
    call init_old_gui()
    call create_main_window()
    call simulate_user_interaction()
    call verify_application_state()
end subroutine test_full_application

! New ForGE integration test
subroutine test_forge_application()
    type(forge_application) :: app
    type(forge_window_t) :: window

    call app%init(BACKEND_CUSTOM)
    window = app%create_window("Test", 400, 300)

    ! Test UI interactions
    call simulate_forge_interaction(window)
    call verify_forge_state(window)

    call app%shutdown()
end subroutine test_forge_application
```

## Performance Considerations

### Identifying Performance Regressions

```fortran
! Performance comparison
subroutine benchmark_old_vs_new()
    real :: old_time, new_time

    ! Benchmark old implementation
    call cpu_time(old_time)
    call old_render_scene()
    call cpu_time(old_time)

    ! Benchmark new implementation
    call cpu_time(new_time)
    call forge_render_scene()
    call cpu_time(new_time)

    write(*,*) "Old time:", old_time, "New time:", new_time
    write(*,*) "Performance ratio:", old_time / new_time
end subroutine benchmark_old_vs_new
```

### Memory Usage Comparison

```fortran
! Memory usage tracking
subroutine compare_memory_usage()
    use memory_profiler

    type(memory_stats) :: old_stats, new_stats

    ! Profile old implementation
    call profile_old_app(old_stats)

    ! Profile new implementation
    call profile_forge_app(new_stats)

    call old_stats%report()
    call new_stats%report()
end subroutine compare_memory_usage
```

## Rollback Strategy

### Feature Flags

Implement feature flags for gradual migration:

```fortran
module feature_flags
    logical, parameter :: USE_FORGE_GUI = .true.
    logical, parameter :: USE_OLD_RENDERING = .false.
end module feature_flags

subroutine create_window(title, width, height)
    character(len=*), intent(in) :: title
    integer, intent(in) :: width, height

    if (USE_FORGE_GUI) then
        ! Use ForGE
        window = app%create_window(title, width, height)
    else
        ! Use old system
        window_handle = old_create_window(title, width, height)
    end if
end subroutine create_window
```

### Compatibility Layer

Maintain backward compatibility:

```fortran
module compatibility_layer
    interface create_button
        module procedure create_forge_button
        module procedure create_old_button  ! For legacy code
    end interface create_button

contains

    function create_forge_button(label) result(button)
        character(len=*), intent(in) :: label
        type(forge_button) :: button
        call button%set_label(label)
    end function create_forge_button

    function create_old_button(label) result(button_handle)
        character(len=*), intent(in) :: label
        integer :: button_handle
        button_handle = old_button_create(label)
    end function create_old_button

end module compatibility_layer
```

## Best Practices for Migration

### Planning

1. **Assess scope**: Determine which components to migrate first
2. **Set milestones**: Define measurable migration goals
3. **Risk assessment**: Identify high-risk migration areas
4. **Resource allocation**: Ensure adequate testing resources

### Execution

1. **Incremental approach**: Migrate small, testable units
2. **Parallel development**: Keep old and new implementations in sync
3. **Comprehensive testing**: Test each migrated component thoroughly
4. **Performance monitoring**: Track performance throughout migration

### Quality Assurance

1. **Automated testing**: Maintain test coverage during migration
2. **User acceptance**: Validate with end users regularly
3. **Performance benchmarks**: Ensure no performance regressions
4. **Documentation updates**: Keep documentation current

### Team Coordination

1. **Training**: Train team members on ForGE concepts
2. **Code reviews**: Review migrated code for quality
3. **Knowledge sharing**: Document migration decisions and patterns
4. **Support structure**: Provide migration support resources

## Common Pitfalls

### Over-Migration

Avoid migrating everything at once:

```fortran
! Bad: Trying to migrate everything
subroutine migrate_everything()
    ! Migrate windows, widgets, events, layouts, data models...
    ! Too much change at once - high risk of errors
end subroutine migrate_everything

! Good: Incremental migration
subroutine migrate_window_system()
    ! Only migrate window creation and management
    ! Test thoroughly before next component
end subroutine migrate_window_system
```

### Ignoring Platform Differences

Account for platform-specific behavior:

```fortran
! Platform-aware migration
subroutine platform_aware_migration()
#ifdef _WIN32
    ! Windows-specific migration considerations
    call migrate_windows_specific_code()
#endif

#ifdef __linux__
    ! Linux-specific migration considerations
    call migrate_linux_specific_code()
#endif

#ifdef __APPLE__
    ! macOS-specific migration considerations
    call migrate_macos_specific_code()
#endif
end subroutine platform_aware_migration
```

### Breaking API Compatibility

Maintain backward compatibility where possible:

```fortran
! Maintain API compatibility
subroutine backward_compatible_api()
    ! Keep old function signatures
    interface old_create_window
        module procedure new_create_window  ! Forward to new implementation
    end interface old_create_window

    ! Deprecation warnings
    call log_warning("old_create_window is deprecated, use forge_window_builder")
end subroutine backward_compatible_api
```

This migration guide provides a structured approach to transitioning from legacy Fortran GUI code to modern ForGE applications, with strategies for minimizing risk and ensuring successful adoption.