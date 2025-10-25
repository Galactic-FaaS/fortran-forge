# Custom GUI Framework Design for ForGE

## Overview

ForGE will implement a custom, lightweight GUI framework built from the ground up for Fortran. This provides:
- **Zero external GUI dependencies** (only platform APIs)
- **Complete control** over rendering and behavior
- **Lightweight** and fast
- **Cross-platform** via platform abstraction
- **Fortran-native** design

## Architecture

```
┌─────────────────────────────────────────────────────┐
│           ForGE Application Layer                   │
│     (forge.f90, forge_window.f90, etc.)            │
├─────────────────────────────────────────────────────┤
│           Widget Rendering Layer                    │
│      (Custom widget drawing routines)               │
├─────────────────────────────────────────────────────┤
│           2D Graphics Engine                        │
│         (Cairo or custom rasterizer)                │
├─────────────────────────────────────────────────────┤
│           Platform Abstraction Layer                │
│  (forge_platform_windows/linux/macos.f90)          │
├─────────────────────────────────────────────────────┤
│              Native OS APIs                         │
│      Win32 / X11/Wayland / Cocoa                   │
└─────────────────────────────────────────────────────┘
```

## Technology Stack

### Rendering Engine Options

**Option 1: Cairo (Recommended)**
- **Pros**: Mature, cross-platform, high-quality 2D rendering
- **Cons**: External dependency (but widely available)
- **Interface**: Use ISO_C_BINDING to call Cairo C API
- **Features**: Paths, gradients, text, transformations, alpha blending

**Option 2: Custom Rasterizer**
- **Pros**: No dependencies, complete control
- **Cons**: Significant development effort, complex (anti-aliasing, text rendering)
- **Features**: Basic shapes, bitmap text only initially

**Decision: Start with Cairo, plan for optional custom rasterizer later**

### Platform Windowing

#### Windows (Win32 API)
- **Window Creation**: CreateWindowEx
- **Event Loop**: GetMessage/TranslateMessage/DispatchMessage
- **Device Context**: GetDC for drawing surface
- **Input**: WM_* messages for mouse/keyboard
- **API Binding**: ISO_C_BINDING to user32.dll, gdi32.dll

#### Linux (X11)
- **Window Creation**: XCreateWindow
- **Event Loop**: XNextEvent
- **Drawing Surface**: XCreatePixmap
- **Input**: X11 events
- **API Binding**: ISO_C_BINDING to libX11

#### Linux (Wayland) - Future
- **Modern alternative to X11**
- **Better performance and security**
- **Implementation**: Post-v1.0

#### macOS (Cocoa) - Future
- **Window Creation**: NSWindow
- **Event Loop**: NSApplication
- **Drawing**: Core Graphics
- **Challenge**: Objective-C interface requires C wrapper

## Implementation Phases

### Phase 1: Windows Backend (Primary Platform)

**Milestone: Basic window with pixel buffer**

#### 1.1 Win32 Window Creation
```fortran
module forge_platform_windows
    use iso_c_binding
    implicit none
    
    ! Win32 API bindings
    interface
        function CreateWindowExA(...) bind(C, name="CreateWindowExA")
            ! Window creation
        end function
        
        function GetMessage(...) bind(C, name="GetMessageA")
            ! Event retrieval
        end function
    end interface
    
contains
    
    subroutine create_native_window(width, height, handle)
        ! Creates Win32 window
    end subroutine
    
    subroutine process_events()
        ! Processes Windows messages
    end subroutine
    
end module
```

#### 1.2 Cairo Integration
```fortran
module forge_cairo
    use iso_c_binding
    implicit none
    
    interface
        function cairo_create(...) bind(C, name="cairo_create")
        end function
        
        subroutine cairo_set_source_rgb(...) bind(C, name="cairo_set_source_rgb")
        end subroutine
    end interface
    
contains
    
    subroutine initialize_cairo_surface(window_handle)
        ! Creates Cairo surface from window DC
    end subroutine
    
end module
```

#### 1.3 Basic Widget Rendering
```fortran
module forge_rendering
    use forge_cairo
    use forge_types
    
contains
    
    subroutine render_button(ctx, rect, label, state)
        ! Draws a button using Cairo
        type(c_ptr), intent(in) :: ctx
        type(forge_rect), intent(in) :: rect
        character(len=*), intent(in) :: label
        integer, intent(in) :: state
        
        ! Draw background
        ! Draw border
        ! Draw text
    end subroutine
    
end module
```

### Phase 2: Core Widget Library

**Milestone: Functional basic widgets**

#### Widget Rendering Functions

**Button**
- Background fill (flat or gradient)
- Border (normal, hover, pressed states)
- Text centered
- Icon support (optional)

**Label**
- Text with alignment
- Word wrapping
- Font styles

**Entry**
- Border and background
- Text input with cursor
- Selection highlighting
- Scrolling for long text

**TextView**
- Multi-line text
- Scrollbars
- Line numbers (optional)
- Syntax highlighting (future)

**ProgressBar**
- Background bar
- Filled portion
- Optional text overlay
- Indeterminate mode (animated)

**Separator**
- Horizontal or vertical line
- Customizable thickness and color

### Phase 3: Event System

**Mouse Events**
- Move: Track position
- Click: Button down/up
- Double-click: Timing detection
- Wheel: Scrolling

**Keyboard Events**
- Key down/up
- Character input
- Modifiers (Shift, Ctrl, Alt)
- Focus management

**Window Events**
- Resize
- Move
- Close
- Minimize/Maximize
- Focus/Blur

**Event Dispatch**
```fortran
subroutine dispatch_mouse_event(window, x, y, button, event_type)
    ! 1. Convert to widget coordinates
    ! 2. Hit test to find target widget
    ! 3. Update hover states
    ! 4. Call widget's event handler
    ! 5. Update cursor if needed
end subroutine
```

### Phase 4: Layout System

**Layout Calculation**
```fortran
subroutine calculate_grid_layout(container, widgets)
    ! For each widget:
    !   1. Determine grid cell
    !   2. Calculate available space
    !   3. Apply size constraints
    !   4. Position widget
    !   5. Account for padding/spacing
end subroutine
```

**Layout Managers**
- **Grid**: Table-like, cells can span
- **Box**: Linear (H/V), flexible spacing
- **Stack**: Layered, only one visible

### Phase 5: Advanced Features

**Text Rendering**
- Font loading (TrueType via stb_truetype or Cairo)
- Kerning and ligatures
- Unicode support (UTF-8)
- Text shaping (complex scripts via HarfBuzz or basic)

**Double Buffering**
- Render to off-screen buffer
- Flip to screen (prevent flicker)
- Dirty rectangles (optimize redraws)

**Theming**
- Color schemes
- Widget styles
- Custom rendering callbacks

**Animations**
- Smooth transitions
- Easing functions
- Timer-based updates

## File Structure for Custom Backend

```
src/backends/custom/
├── forge_custom.f90              # Main backend implementation
├── forge_platform.f90            # Platform abstraction interface
├── forge_platform_windows.f90   # Win32 implementation
├── forge_platform_linux.f90     # X11 implementation (future)
├── forge_platform_macos.f90     # Cocoa wrapper (future)
├── forge_cairo_bindings.f90     # Cairo C API bindings
├── forge_rendering.f90          # Widget rendering routines
└── forge_text.f90               # Text layout and rendering
```

## Win32 API Functions Needed

### Window Management
```c
CreateWindowExA      - Create window
DestroyWindow        - Destroy window  
ShowWindow          - Show/hide window
SetWindowTextA      - Set window title
GetClientRect       - Get window size
```

### Message Handling
```c
GetMessageA         - Get next message
TranslateMessage    - Translate virtual-key messages
DispatchMessageA    - Dispatch message to window proc
PostQuitMessage     - Exit application
DefWindowProcA      - Default window procedure
```

### Graphics
```c
GetDC              - Get device context
ReleaseDC          - Release device context
BeginPaint         - Begin paint operation
EndPaint           - End paint operation
CreateCompatibleDC - Create memory DC
CreateDIBSection   - Create bitmap for Cairo
```

### Input
```c
SetCursor          - Set mouse cursor
SetCapture         - Capture mouse
ReleaseCapture     - Release mouse capture
GetAsyncKeyState   - Check key state
```

## Cairo Functions Needed

### Context
```c
cairo_create                - Create context
cairo_destroy               - Destroy context
cairo_save/restore         - Save/restore state
```

### Surfaces
```c
cairo_image_surface_create         - Create image surface
cairo_win32_surface_create         - Create from Win32 DC
cairo_surface_destroy              - Destroy surface
```

### Drawing
```c
cairo_set_source_rgb/rgba          - Set color
cairo_rectangle                    - Draw rectangle
cairo_arc                          - Draw arc/circle
cairo_move_to/line_to             - Path operations
cairo_stroke/fill                 - Render path
```

### Text
```c
cairo_select_font_face            - Choose font
cairo_set_font_size               - Set size
cairo_show_text                   - Draw text
cairo_text_extents                - Measure text
```

## Development Roadmap

### Week 1-2: Windows Platform Layer
- [ ] Win32 API bindings module
- [ ] Window creation and message loop
- [ ] Basic event handling
- [ ] Device context management

### Week 3-4: Cairo Integration
- [ ] Cairo API bindings
- [ ] Surface creation from Win32 DC
- [ ] Basic drawing primitives
- [ ] Text rendering

### Week 5-6: Widget Rendering
- [ ] Button rendering
- [ ] Label rendering
- [ ] Entry rendering
- [ ] Basic mouse interaction

### Week 7-8: Layout System
- [ ] Grid layout implementation
- [ ] Box layout implementation
- [ ] Widget positioning
- [ ] Resize handling

### Week 9-10: Additional Widgets
- [ ] TextView (scrolling, selection)
- [ ] ProgressBar
- [ ] Separator
- [ ] ScrollBar

### Week 11-12: Polish & Testing
- [ ] Double buffering
- [ ] Keyboard navigation
- [ ] Focus management
- [ ] Example applications

## Performance Considerations

### Rendering Optimization
- **Dirty Rectangles**: Only redraw changed areas
- **Caching**: Cache rendered widgets as pixmaps
- **Batching**: Batch multiple draws into single flush
- **Clipping**: Set clip regions to avoid overdraw

### Memory Management
- Reuse Cairo surfaces where possible
- Limit texture/pixmap allocations
- Clear resources on window close

### Event Handling
- Debounce mouse move events
- Throttle resize/paint events
- Use timers for animations

## Testing Strategy

### Unit Tests
- Platform abstraction functions
- Cairo binding functions
- Widget rendering (visual comparison)
- Layout calculations

### Integration Tests
- Window lifecycle
- Event dispatch
- Multi-window scenarios
- Resource cleanup

### Visual Tests
- Screenshot comparison
- Pixel-perfect widget rendering
- Cross-platform consistency

## Challenges & Solutions

| Challenge | Solution |
|-----------|----------|
| Text rendering complexity | Use Cairo's text API initially, custom later |
| Platform differences | Abstract platform layer, test on each OS |
| Performance | Profile and optimize hot paths, use caching |
| Memory leaks | Careful resource management, valgrind testing |
| Thread safety | Document single-threaded requirement initially |
| Font availability | Bundle fallback fonts, use system fonts |
| High DPI | Scale factor detection and handling |
| Accessibility | Plan for screen reader support later |

## Success Criteria

- ✓ Window creation on Windows
- ✓ Basic shapes rendered via Cairo
- ✓ Mouse events handled correctly
- ✓ 5+ widgets fully functional
- ✓ Layout system working
- ✓ Example application runs smoothly
- ✓ No memory leaks
- ✓ Documented API

## Future Enhancements

- **Linux X11 backend**: Port platform layer
- **macOS backend**: Create Objective-C wrapper
- **Wayland support**: Modern Linux compositor
- **Hardware acceleration**: OpenGL/Vulkan backend option
- **Custom rasterizer**: Pure Fortran rendering
- **Advanced widgets**: TreeView, TabWidget, etc.
- **Theming engine**: Customizable appearance
- **Accessibility**: Screen reader support

## Conclusion

Building a custom GUI framework is ambitious but achievable. Starting with Windows + Cairo provides a solid foundation that can be expanded to other platforms. The modular design allows incremental development and testing at each stage.

**Estimated Timeline**: 3-4 months for functional Windows backend with core widgets
**Complexity**: High, but well-defined scope
**Benefits**: Complete control, no external GUI dependencies, Fortran-native

