# Backend Guide

This guide explains ForGE's backend system, how to choose backends, and backend-specific considerations for application development.

## Overview

ForGE uses a backend abstraction layer that allows applications to run on different GUI frameworks. Backends handle:

- Window creation and management
- Widget rendering and interaction
- Event processing
- Platform-specific integration

## Available Backends

### Custom Backend (BACKEND_CUSTOM)

The primary ForGE backend with custom rendering.

**Features:**
- Platform-specific window management
- Custom widget rendering
- Direct platform API access
- Lightweight and fast
- Full control over appearance

**Platforms:**
- Windows: Win32 API integration
- Linux: X11 support (planned)
- macOS: Cocoa support (planned)

**Use Cases:**
- High-performance applications
- Custom UI requirements
- Platform-specific features
- Embedded systems

### Qt Backend (BACKEND_QT) - Planned

Qt framework integration.

**Features:**
- Cross-platform Qt widgets
- Rich widget set
- Theming support
- Accessibility features
- Mature ecosystem

**Use Cases:**
- Complex desktop applications
- Cross-platform consistency
- Existing Qt code integration

### GTK Backend (BACKEND_GTK4) - Planned

GTK4 integration.

**Features:**
- Native Linux integration
- GNOME desktop integration
- Accessibility support
- Modern widget set

**Use Cases:**
- Linux-native applications
- GNOME integration
- Accessibility requirements

### Tcl/Tk Backend (BACKEND_TCL_TK) - Planned

Tcl/Tk integration.

**Features:**
- Lightweight and simple
- Good for simple applications
- Cross-platform
- Small footprint

**Use Cases:**
- Simple utilities
- Prototyping
- Educational applications

## Backend Selection

### Choosing a Backend

```fortran
program backend_selection
    use forge
    implicit none

    type(forge_application) :: app
    type(forge_status) :: status

    ! Try backends in order of preference
    call app%init(BACKEND_QT, status)
    if (status%is_error()) then
        print *, "Qt backend failed, trying custom backend"
        call app%init(BACKEND_CUSTOM, status)
        if (status%is_error()) then
            print *, "Custom backend failed, trying GTK"
            call app%init(BACKEND_GTK4, status)
            if (status%is_error()) then
                print *, "All backends failed"
                stop
            end if
        end if
    end if

    ! Application continues with selected backend
    print *, "Using backend:", app%get_backend_type()%id

end program backend_selection
```

### Runtime Backend Detection

```fortran
subroutine configure_for_backend(app)
    type(forge_application), intent(in) :: app

    select case (app%get_backend_type()%id)
    case (BACKEND_CUSTOM)
        call configure_custom_backend()
    case (BACKEND_QT)
        call configure_qt_backend()
    case (BACKEND_GTK4)
        call configure_gtk_backend()
    case (BACKEND_TCL_TK)
        call configure_tcl_backend()
    end select
end subroutine configure_for_backend
```

## Custom Backend Details

### Platform Support

The custom backend provides platform-specific implementations:

#### Windows (forge_platform_windows)

```fortran
! Windows-specific features
type(forge_custom_backend_t) :: backend

call backend%init(status)
if (backend%get_platform_type() == PLATFORM_WINDOWS) then
    ! Windows-specific code
    call backend%set_windows_style(window_handle, WS_OVERLAPPEDWINDOW)
end if
```

**Windows Features:**
- Win32 window management
- Windows message loop
- Direct2D rendering (planned)
- Windows theming integration

#### Linux (Planned)

**Linux Features:**
- X11 window management
- X11 event handling
- Cairo rendering
- GTK integration option

#### macOS (Planned)

**macOS Features:**
- Cocoa window management
- Cocoa event handling
- Core Graphics rendering
- Native macOS integration

### Custom Rendering

The custom backend provides direct access to rendering:

```fortran
! Planned rendering API
type(forge_rendering_context) :: context

call backend%get_rendering_context(window_handle, context)
call context%set_color(1.0, 0.0, 0.0, 1.0)  ! Red
call context%draw_rectangle(10, 10, 100, 50)
call context%draw_text(20, 30, "Hello World")
```

### Performance Characteristics

- **Startup:** Fast initialization
- **Memory:** Low memory footprint
- **Rendering:** Hardware accelerated (planned)
- **Widgets:** Custom rendering, consistent appearance

## Backend-Specific Considerations

### Widget Availability

| Widget | Custom | Qt | GTK | Tcl/Tk |
|--------|--------|----|-----|--------|
| Button | ✓ | ✓ | ✓ | ✓ |
| Label | ✓ | ✓ | ✓ | ✓ |
| Entry | ✓ | ✓ | ✓ | ✓ |
| Text View | ✓ | ✓ | ✓ | ✓ |
| Slider | ✓ | ✓ | ✓ | ✓ |
| Spin Button | ✓ | ✓ | ✓ | ✓ |
| Combo Box | ✓ | ✓ | ✓ | ✓ |
| Check Box | ✓ | ✓ | ✓ | ✓ |
| Radio Button | ✓ | ✓ | ✓ | ✓ |
| Progress Bar | ✓ | ✓ | ✓ | ✓ |
| Table | Planned | ✓ | ✓ | Limited |
| Tree | Planned | ✓ | ✓ | Limited |

### Event Handling Differences

Backends may handle events differently:

```fortran
! Custom backend: Direct platform events
! Qt backend: Qt event system
! GTK backend: GTK event system

! Write backend-agnostic event code
call button%on_click(handle_click)

subroutine handle_click(event)
    type(forge_event), intent(in) :: event
    ! Event handling works the same across backends
end subroutine handle_click
```

### Appearance and Theming

Backends provide different visual styles:

```fortran
! Custom backend: Consistent ForGE styling
! Qt backend: Qt theme or system theme
! GTK backend: GTK theme
! Tcl/Tk backend: Tk styling

! Backend-specific theming (planned)
call backend%set_theme("dark")
call backend%set_font("Arial", 12)
```

## Backend Development

### Implementing a New Backend

To add a new backend:

1. **Extend forge_backend_base**
```fortran
type, extends(forge_backend_base) :: my_backend
contains
    procedure :: init => my_init
    procedure :: create_window => my_create_window
    ! Implement all required methods
end type my_backend
```

2. **Implement Required Methods**
```fortran
subroutine my_init(this, status)
    class(my_backend), intent(inout) :: this
    type(forge_status), intent(out) :: status

    ! Initialize your GUI framework
    call my_gui_init()
    this%initialized = .true.
    call status%clear()
end subroutine my_init
```

3. **Handle Platform Differences**
```fortran
subroutine my_create_window(this, handle, title, width, height, status)
    class(my_backend), intent(inout) :: this
    type(forge_window_handle), intent(out) :: handle
    character(len=*), intent(in) :: title
    integer(c_int), intent(in) :: width, height
    type(forge_status), intent(out) :: status

    ! Create window using your GUI framework
    handle%window_id = my_gui_create_window(title, width, height)
    handle%ptr = c_null_ptr  ! Or platform-specific handle
end subroutine my_create_window
```

4. **Register Backend**
```fortran
! Add to backend selection
integer, parameter :: BACKEND_MY_GUI = 5

! Update application initialization
select case (backend_id)
case (BACKEND_MY_GUI)
    allocate(my_backend :: app%backend)
end select
```

### Backend Testing

Test backends across platforms:

```fortran
program backend_test
    use forge
    implicit none

    type(forge_application) :: app
    type(forge_window_t) :: window
    type(forge_button) :: test_button
    type(forge_status) :: status

    ! Test all available backends
    integer :: backends(4) = [BACKEND_CUSTOM, BACKEND_QT, BACKEND_GTK4, BACKEND_TCL_TK]
    integer :: i

    do i = 1, size(backends)
        print *, "Testing backend:", backends(i)

        call app%init(backends(i), status)
        if (status%is_ok()) then
            print *, "Backend", backends(i), "initialized successfully"

            ! Create test window
            window = app%create_window("Backend Test", 300, 200)
            call test_button%set_label("Test Button")
            call test_button%on_click(test_callback)

            ! Quick test
            call window%show()
            call app%process_events()  ! Process one event loop iteration

            call app%shutdown()
            print *, "Backend", backends(i), "test passed"
        else
            print *, "Backend", backends(i), "failed:", status%get_message()
        end if
    end do

contains

    subroutine test_callback(event)
        type(forge_event), intent(in) :: event
        print *, "Button click handled successfully"
    end subroutine test_callback

end program backend_test
```

## Backend Configuration

### Initialization Options

```fortran
! Backend-specific initialization options (planned)
type(forge_backend_config) :: config

call config%set_theme("dark")
call config%set_font_size(12)
call config%set_double_buffering(.true.)

call app%init(BACKEND_CUSTOM, status, config)
```

### Runtime Configuration

```fortran
! Change backend settings at runtime (planned)
call backend%set_property("theme", "light")
call backend%set_property("animation_enabled", "true")
call backend%apply_settings()
```

## Troubleshooting

### Backend Initialization Fails

```fortran
call app%init(BACKEND_QT, status)
if (status%is_error()) then
    select case (status%get_code())
    case (FORGE_ERROR_BACKEND)
        print *, "Backend library not found - check installation"
    case (FORGE_ERROR_NOT_IMPLEMENTED)
        print *, "Backend not available on this platform"
    case (FORGE_ERROR_GENERIC)
        print *, "Backend initialization error:", status%get_message()
    end select

    ! Try fallback backend
    call app%init(BACKEND_CUSTOM, status)
end if
```

### Widget Creation Fails

```fortran
call create_button(button, status)
if (status%is_error()) then
    print *, "Widget creation failed in backend:", app%get_backend_type()%id
    print *, "Error:", status%get_message()

    ! Backend-specific troubleshooting
    select case (app%get_backend_type()%id)
    case (BACKEND_CUSTOM)
        print *, "Check custom backend platform support"
    case (BACKEND_QT)
        print *, "Check Qt installation and version"
    end select
end if
```

### Performance Issues

```fortran
! Enable backend performance monitoring (planned)
call backend%enable_performance_logging(.true.)

! Monitor performance
call backend%get_performance_stats(fps, memory_usage, draw_calls)
print *, "FPS:", fps, "Memory:", memory_usage, "Draw calls:", draw_calls
```

### Visual Artifacts

```fortran
! Backend-specific fixes
select case (app%get_backend_type()%id)
case (BACKEND_CUSTOM)
    ! Custom backend fixes
    call backend%enable_double_buffering(.true.)
case (BACKEND_QT)
    ! Qt-specific fixes
    call backend%set_attribute("WA_OpaquePaintEvent")
end select
```

## Best Practices

### Backend Selection

1. **Choose based on requirements:**
   - Custom backend for performance/control
   - Qt backend for features/complexity
   - GTK for Linux integration
   - Tcl/Tk for simplicity

2. **Provide fallback options:**
   ```fortran
   ! Try preferred backend, fall back to custom
   call try_backend(BACKEND_QT)
   if (.not. success) call try_backend(BACKEND_CUSTOM)
   ```

3. **Test across platforms:**
   - Develop on multiple platforms
   - Use continuous integration
   - Test backend switching

### Backend-Agnostic Code

1. **Use ForGE APIs only:**
   ```fortran
   ! Good: Backend-agnostic
   call button%on_click(my_handler)

   ! Bad: Backend-specific
   ! call qt_button%connect("clicked", my_handler)
   ```

2. **Avoid backend-specific features:**
   ```fortran
   ! Good: Check availability
   if (backend%supports_feature("custom_rendering")) then
       call backend%custom_render()
   end if

   ! Bad: Assume backend capabilities
   ! call backend%qt_specific_method()
   ```

3. **Handle backend differences:**
   ```fortran
   ! Account for backend differences
   select case (app%get_backend_type()%id)
   case (BACKEND_CUSTOM)
       ! Custom backend adjustments
   case (BACKEND_QT)
       ! Qt backend adjustments
   end select
   ```

### Performance Optimization

1. **Choose appropriate backend:**
   - Custom backend for simple, fast UIs
   - Qt backend for complex, feature-rich UIs

2. **Optimize for backend:**
   ```fortran
   ! Custom backend: Minimize widget count
   ! Qt backend: Use native widgets when possible
   ```

3. **Monitor performance:**
   ```fortran
   ! Log performance metrics
   call backend%log_performance_stats()
   ```

## Future Backend Development

### Planned Backends

- **Web Backend:** WebAssembly + HTML5 Canvas
- **Mobile Backends:** iOS UIKit, Android Views
- **Game Engine Backends:** SDL, GLFW integration
- **Console Backends:** Terminal UI, curses

### Backend Plugins

Dynamic backend loading:

```fortran
! Planned plugin system
call backend%load_plugin("my_custom_backend.so")
call app%init(BACKEND_PLUGIN, status)
```

## Next Steps

- Read the [API documentation](../api/) for backend-specific method details
- Explore the [examples](../../examples/) directory for backend usage examples
- Learn about [layout managers](layout_managers.md) for cross-backend layouts
- Study [widget gallery](widget_gallery.md) for backend compatibility information