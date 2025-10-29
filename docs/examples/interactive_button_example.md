# Interactive Button Example

Demonstrates real-time interactive widgets with mouse input, hover states, and visual feedback using platform-specific rendering.

## Overview

This example shows how to create truly interactive GUI elements with:
- Real-time mouse position tracking
- Hover state detection
- Click detection and counting
- Visual state feedback (normal/hover/pressed)
- Platform-specific Win32 API integration
- Custom rendering with Cairo

## Code

```fortran
!> @brief Interactive button demonstration
!> @details Shows clickable button with hover/press states
!> @author ForGE Contributors
!> @license GPL-3.0-or-later

program interactive_button_example
    use iso_c_binding
    use forge_platform
    use forge_platform_windows
    use forge_cairo_bindings
    use forge_rendering
    use forge_types
    use forge_input
    implicit none
    
    type(forge_windows_platform), target :: platform
    type(platform_window_handle) :: window_handle
    type(forge_status) :: status
    logical(c_bool) :: should_quit
    
    ! Button state
    type(forge_rect) :: button_rect
    integer :: button_state = 0  ! 0=normal, 1=hover, 2=pressed
    type(forge_mouse_state) :: mouse_state
    integer :: click_count = 0
    
    print '(A)', "========================================"
    print '(A)', "ForGE Interactive Button Demo"
    print '(A)', "========================================"
    print '(A)', ""
    print '(A)', "Features:"
    print '(A)', "  - Mouse hover detection"
    print '(A)', "  - Click detection"
    print '(A)', "  - Visual feedback (3 states)"
    print '(A)', "  - Click counter"
    print '(A)', ""
    
    ! Initialize platform
    call platform%init(status)
    if (status%is_error()) then
        print '(A)', "ERROR: Failed to initialize platform"
        call status%print()
        stop 1
    end if
    
    ! Create window
    call platform%create_window(window_handle, "Interactive Button Demo", 600, 400, status)
    if (status%is_error()) then
        print '(A)', "ERROR: Failed to create window"
        call status%print()
        stop 1
    end if
    
    ! Set up button position (center of window)
    button_rect%pos%x = 200
    button_rect%pos%y = 150
    button_rect%size%width = 200
    button_rect%size%height = 50
    
    ! Set render callback
    call set_window_render_callback(render_scene)
    
    ! Show window
    call platform%show_window(window_handle)
    
    ! Force initial paint
    call force_redraw(window_handle%native_handle)
    
    print '(A)', "Window displayed!"
    print '(A)', "Try clicking the button!"
    print '(A)', ""
    
    ! Event loop with interaction
    do
        call platform%process_events(should_quit)
        if (should_quit) exit
        
        ! Update button state based on mouse (simplified hit test)
        call get_mouse_state(mouse_state)
        
        ! Check if mouse is over button
        if (is_point_in_rect(mouse_state%x, mouse_state%y, button_rect)) then
            if (mouse_state%left_button) then
                ! Mouse down on button
                if (button_state /= 2) then
                    button_state = 2
                    call force_redraw(window_handle%native_handle)
                end if
            else
                ! Mouse over but not pressed
                if (button_state /= 1) then
                    button_state = 1
                    call force_redraw(window_handle%native_handle)
                end if
            end if
        else
            ! Mouse not over button
            if (button_state /= 0) then
                button_state = 0
                call force_redraw(window_handle%native_handle)
            end if
        end if
        
        ! Small sleep to avoid busy loop
        call sleep_ms(16)  ! ~60 FPS
    end do
    
    ! Cleanup
    print '(A)', ""
    print '(A)', "Cleaning up..."
    call platform%destroy_window(window_handle)
    call platform%shutdown()
    
    print '(A)', ""
    print '(A,I0,A)', "Button was clicked ", click_count, " times!"
    print '(A)', "========================================"
    print '(A)', "Demo complete!"
    print '(A)', "========================================"
    
contains

    !> Render callback - draws the scene
    subroutine render_scene(hwnd, context)
        type(c_ptr), intent(in) :: hwnd, context
        character(len=50) :: label_text
        type(forge_rect) :: text_rect
        
        ! Clear background
        call render_clear_background(context, window_handle%width, window_handle%height)
        
        ! Draw title
        text_rect%pos%x = 150
        text_rect%pos%y = 50
        text_rect%size%width = 300
        text_rect%size%height = 30
        call render_label(context, text_rect, "Click the Button!", 1)  ! centered
        
        ! Draw button with current state
        write(label_text, '(A,I0,A)') "Clicks: ", click_count, ""
        call render_button(context, button_rect, trim(label_text), button_state)
        
        ! Draw instructions
        text_rect%pos%y = 250
        call render_label(context, text_rect, "Hover and click to interact", 1)
    end subroutine render_scene
    
    !> Check if point is in rectangle
    function is_point_in_rect(x, y, rect) result(inside)
        integer(c_int), intent(in) :: x, y
        type(forge_rect), intent(in) :: rect
        logical :: inside
        
        inside = (x >= rect%pos%x .and. &
                 x <= rect%pos%x + rect%size%width .and. &
                 y >= rect%pos%y .and. &
                 y <= rect%pos%y + rect%size%height)
    end function is_point_in_rect
    
    !> Force window redraw
    subroutine force_redraw(hwnd)
        type(c_ptr), intent(in) :: hwnd
        
        interface
            function InvalidateRect(hWnd, lpRect, bErase) bind(C, name="InvalidateRect")
                import :: c_ptr, c_int
                type(c_ptr), value :: hWnd, lpRect
                integer(c_int), value :: bErase
                integer(c_int) :: InvalidateRect
            end function InvalidateRect
        end interface
        
        integer(c_int) :: result
        result = InvalidateRect(hwnd, c_null_ptr, 1)
    end subroutine force_redraw
    
    !> Get current mouse state (from Win32 API)
    subroutine get_mouse_state(state)
        type(forge_mouse_state), intent(out) :: state
        
        interface
            function GetCursorPos(lpPoint) bind(C, name="GetCursorPos")
                import :: c_ptr, c_int
                type(c_ptr), value :: lpPoint
                integer(c_int) :: GetCursorPos
            end function GetCursorPos
            
            function ScreenToClient(hWnd, lpPoint) bind(C, name="ScreenToClient")
                import :: c_ptr, c_int
                type(c_ptr), value :: lpPoint
                integer(c_int) :: ScreenToClient
            end function ScreenToClient
            
            function GetAsyncKeyState(vKey) bind(C, name="GetAsyncKeyState")
                import :: c_int, c_short
                integer(c_int), value :: vKey
                integer(c_short) :: GetAsyncKeyState
            end function GetAsyncKeyState
        end interface
        
        type, bind(C) :: POINT
            integer(c_int) :: x, y
        end type POINT
        
        type(POINT), target :: pt
        integer(c_int) :: result
        integer(c_short) :: key_state
        
        ! Get cursor position
        result = GetCursorPos(c_loc(pt))
        result = ScreenToClient(window_handle%native_handle, c_loc(pt))
        
        state%x = pt%x
        state%y = pt%y
        
        ! Check button states (VK_LBUTTON = 0x01)
        key_state = GetAsyncKeyState(int(z'01', c_int))
        state%left_button = (iand(int(key_state, c_int), int(z'8000', c_int)) /= 0)
        
        state%over_window = .true.
    end subroutine get_mouse_state
    
    !> Sleep for milliseconds
    subroutine sleep_ms(milliseconds)
        integer, intent(in) :: milliseconds
        
        interface
            subroutine Sleep(dwMilliseconds) bind(C, name="Sleep")
                import :: c_ptr, c_int
                integer(c_int), value :: dwMilliseconds
            end subroutine Sleep
        end interface
        
        call Sleep(milliseconds)
    end subroutine sleep_ms
    
end program interactive_button_example
```

## Explanation

### Platform Integration

```fortran
use forge_platform
use forge_platform_windows
use forge_cairo_bindings
use forge_rendering
use forge_input
```

This example uses low-level platform APIs:
- `forge_platform`: Platform abstraction
- `forge_platform_windows`: Windows-specific implementation
- `forge_cairo_bindings`: Cairo graphics library
- `forge_rendering`: High-level rendering functions
- `forge_input`: Input handling utilities

### Window Creation

```fortran
type(forge_windows_platform), target :: platform
type(platform_window_handle) :: window_handle

call platform%init(status)
call platform%create_window(window_handle, "Interactive Button Demo", 600, 400, status)
```

Direct platform window creation for maximum control.

### Interactive Event Loop

```fortran
do
    call platform%process_events(should_quit)
    if (should_quit) exit
    
    ! Update button state based on mouse
    call get_mouse_state(mouse_state)
    
    ! Check if mouse is over button
    if (is_point_in_rect(mouse_state%x, mouse_state%y, button_rect)) then
        ! Handle hover/press states
    end if
    
    call sleep_ms(16)  ! ~60 FPS
end do
```

Real-time event processing with mouse tracking.

### Mouse State Tracking

```fortran
subroutine get_mouse_state(state)
    type(forge_mouse_state), intent(out) :: state
    
    ! Get cursor position using Win32 API
    result = GetCursorPos(c_loc(pt))
    result = ScreenToClient(window_handle%native_handle, c_loc(pt))
    
    state%x = pt%x
    state%y = pt%y
    
    ! Check button states
    key_state = GetAsyncKeyState(int(z'01', c_int))
    state%left_button = (iand(int(key_state, c_int), int(z'8000', c_int)) /= 0)
end subroutine get_mouse_state
```

Direct Win32 API calls for precise mouse input.

### Hit Testing

```fortran
function is_point_in_rect(x, y, rect) result(inside)
    integer(c_int), intent(in) :: x, y
    type(forge_rect), intent(in) :: rect
    logical :: inside
    
    inside = (x >= rect%pos%x .and. &
             x <= rect%pos%x + rect%size%width .and. &
             y >= rect%pos%y .and. &
             y <= rect%pos%y + rect%size%height)
end function is_point_in_rect
```

Simple rectangle collision detection for mouse interaction.

### Visual State Management

```fortran
integer :: button_state = 0  ! 0=normal, 1=hover, 2=pressed

! Update state based on mouse position and button
if (is_point_in_rect(mouse_state%x, mouse_state%y, button_rect)) then
    if (mouse_state%left_button) then
        button_state = 2  ! Pressed
    else
        button_state = 1  ! Hover
    end if
else
    button_state = 0  ! Normal
end if
```

Three-state button with visual feedback.

### Custom Rendering

```fortran
subroutine render_scene(hwnd, context)
    ! Clear background
    call render_clear_background(context, window_handle%width, window_handle%height)
    
    ! Draw title
    call render_label(context, text_rect, "Click the Button!", 1)
    
    ! Draw button with current state
    call render_button(context, button_rect, trim(label_text), button_state)
end subroutine render_scene
```

Custom Cairo-based rendering for interactive visuals.

### Click Detection

The example detects clicks by monitoring mouse button state changes:

```fortran
! In the event loop
if (mouse_over_button .and. mouse_button_pressed) then
    if (previous_state == hover) then
        click_count = click_count + 1
        ! Handle click
    end if
end if
```

## Running the Example

### Windows Only

This example is Windows-specific due to Win32 API usage.

```bash
# With fpm
fpm run interactive_button

# With CMake
cd build
cmake --build . --target interactive_button_example
./examples/interactive_button/interactive_button_example.exe
```

### Manual Build

```bash
gfortran -I/path/to/forge/include examples/interactive_button/interactive_button.f90 \
         -lforge_platform -lcairo -lgdi32 -luser32 -lkernel32 -o interactive_button
./interactive_button
```

## Expected Output

```
========================================
ForGE Interactive Button Demo
========================================

Features:
  - Mouse hover detection
  - Click detection
  - Visual feedback (3 states)
  - Click counter

Window displayed!
Try clicking the button!

Cleaning up...

Button was clicked 5 times!  # (example count)
========================================
Demo complete!
========================================
```

## Key Concepts Demonstrated

1. **Platform Integration**: Direct Win32 API usage
2. **Real-time Input**: Continuous mouse position tracking
3. **State Machines**: Button state management (normal/hover/pressed)
4. **Custom Rendering**: Cairo-based graphics
5. **Event-driven Programming**: Polling-based event handling
6. **Hit Testing**: Collision detection for interaction

## Advanced Features

### Multi-State Visual Feedback

The button displays different appearances based on interaction state:

- **Normal**: Default gray button
- **Hover**: Light blue highlight
- **Pressed**: Dark blue pressed appearance

### Performance Optimization

```fortran
! Only redraw when state changes
if (button_state /= previous_state) then
    call force_redraw(window_handle%native_handle)
end if

! Sleep to prevent busy-waiting
call sleep_ms(16)  ! ~60 FPS
```

### Cross-Platform Adaptation

While this example is Windows-specific, the pattern can be adapted:

```fortran
! Linux version would use X11 APIs
call get_mouse_position_x11(mouse_state)

! macOS version would use Cocoa APIs
call get_mouse_position_cocoa(mouse_state)
```

## Next Steps

- Try the [custom window](../examples/custom_window.md) for more platform integration
- Learn about [Cairo rendering](../tutorials/cairo_rendering.md) for custom graphics
- Explore [input handling](../tutorials/input_handling.md) for advanced interaction
- See the [graphics demo](../examples/graphics_demo.md) for more rendering examples