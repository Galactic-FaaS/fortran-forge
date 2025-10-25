# ForGE Architecture

This document describes the internal architecture of the ForGE library.

## Overview

ForGE is designed with a layered, modular architecture that separates concerns and enables flexibility through backend abstraction.

```
┌─────────────────────────────────────────┐
│         User Application                │
├─────────────────────────────────────────┤
│         ForGE API Layer                 │
│   (forge, forge_window, forge_widgets)  │
├─────────────────────────────────────────┤
│      Backend Abstraction Layer          │
│         (forge_backend)                 │
├─────────────────────────────────────────┤
│    Concrete Backend Implementations     │
│   (Tcl/Tk, GTK4, Qt, Custom, Stub)     │
├─────────────────────────────────────────┤
│        Native GUI Frameworks            │
│      (Tcl/Tk, GTK, Qt, etc.)           │
└─────────────────────────────────────────┘
```

## Module Organization

### Core Modules

#### `forge.f90` - Main Module
- Entry point for users
- Re-exports all public interfaces
- Provides `forge_application` class
- Manages application lifecycle

#### `forge_types.f90` - Type Definitions
- Fundamental types used throughout ForGE
- String handling with C interop (`forge_string`)
- Geometric types (`forge_color`, `forge_size`, `forge_position`, `forge_rect`)
- Type-bound procedures for convenience

#### `forge_errors.f90` - Error Handling
- Error codes and status types
- Error checking utilities
- Standardized error reporting

#### `forge_events.f90` - Event System
- Event type definitions
- Event handlers with procedure pointers
- Callback mechanism
- Support for mouse, keyboard, window, widget events

### GUI Components

#### `forge_window.f90` - Window Management
- `forge_window_t` class for windows
- `forge_window_builder` for fluent API
- Window lifecycle management
- Window properties and methods

#### `forge_widgets.f90` - Widget System
- Abstract `forge_widget` base class
- Concrete widget implementations:
  - `forge_button`
  - `forge_label`
  - `forge_entry`
  - `forge_text_view`
  - `forge_progress_bar`
  - `forge_separator`
- Widget properties and event handlers

#### `forge_layout.f90` - Layout Managers
- Abstract `forge_layout` interface
- Concrete layout implementations:
  - `forge_grid_layout` - table-like arrangement
  - `forge_box_layout` - linear arrangement (H/V)
  - `forge_stack_layout` - layered widgets
- Automatic widget positioning and sizing

### Backend System

#### `forge_backend.f90` - Backend Abstraction
- Abstract `forge_backend_base` class
- Defines interface all backends must implement
- Opaque handles (`forge_window_handle`, `forge_widget_handle`)
- Backend type identifiers

#### Backend Implementations
Located in `src/backends/`:

- **`forge_stub.f90`** - Testing/demonstration backend
- **`forge_tcl_tk.f90`** (planned) - Tcl/Tk backend
- **`forge_gtk4.f90`** (planned) - GTK4 backend
- **`forge_qt.f90`** (planned) - Qt backend
- **`forge_custom.f90`** (planned) - Custom rendering backend

## Design Patterns

### Abstract Interface Pattern
Used for backend abstraction, allowing multiple implementations:

```fortran
type, abstract :: forge_backend_base
contains
    procedure(backend_init_interface), deferred :: init
    ! ... more deferred procedures
end type
```

### Builder Pattern
Used for object construction with many optional parameters:

```fortran
window = builder%set_title("Title") &
               %set_size(800, 600) &
               %set_position(100, 100) &
               %build(status)
```

### Observer Pattern (Events)
Event handlers register callbacks that are invoked when events occur:

```fortran
call button%on_click(my_callback)

! my_callback is invoked when button is clicked
```

### Factory Pattern
Application class creates objects using appropriate backend:

```fortran
window = app%create_window("Title", 800, 600)
```

## Data Flow

### Window Creation
1. User calls `forge_window_builder%build()`
2. Builder validates parameters
3. Builder calls backend's `create_window()`
4. Backend creates native window
5. Backend returns opaque handle
6. ForGE wraps handle in `forge_window_t`
7. Returns window object to user

### Event Handling
1. User registers callback via `widget%on_event(callback)`
2. Event occurs in native framework
3. Backend translates native event to `forge_event`
4. Backend invokes registered callback
5. User code executes
6. Control returns to event loop

### Widget Management
1. User creates widget object
2. Widget is added to window/layout
3. Backend creates corresponding native widget
4. Backend stores mapping between ForGE widget and native widget
5. Operations on ForGE widget are forwarded to backend
6. Backend applies changes to native widget

## Memory Management

### Object Ownership
- **User owns**: `forge_window_t`, `forge_widget` objects
- **Backend owns**: Native GUI objects
- **Opaque handles**: Link ForGE objects to backend objects

### Cleanup Strategy
1. User calls `window%close()` or `app%shutdown()`
2. ForGE calls backend's `destroy_*()` methods
3. Backend cleans up native resources
4. ForGE deallocates Fortran objects

### Allocatable Components
- Strings use allocatable character arrays
- Backend can be allocatable polymorphic
- Dynamic widget lists (future enhancement)

## Thread Safety

**Current Status**: Not thread-safe

**Future Plans**:
- Event queue with mutex protection
- Thread-safe widget property access
- Async operations for long-running tasks

## Extension Points

### Adding New Widgets
1. Extend `forge_widget` base class
2. Add widget-specific properties and methods
3. Implement backend support in each backend
4. Add to `forge_widgets.f90`
5. Export from main `forge` module

### Adding New Backends
1. Create new module in `src/backends/`
2. Extend `forge_backend_base`
3. Implement all deferred procedures
4. Add backend type identifier
5. Update `forge_application%init()` to recognize new backend
6. Add build system support

### Adding New Layouts
1. Extend `forge_layout` base class
2. Implement deferred procedures
3. Add layout-specific configuration
4. Add to `forge_layout.f90`

## Performance Considerations

### Minimize Backend Calls
- Cache property values in ForGE objects
- Batch updates when possible
- Use layouts to reduce positioning calls

### Event Handling
- Keep callbacks lightweight
- Use `process_events()` for manual control
- Avoid blocking operations in event handlers

### String Handling
- Minimize string allocations
- Use `forge_string` type for efficiency
- Cache C-compatible strings when interfacing with backends

## Future Architecture Enhancements

- **Plugin System**: Dynamic backend loading
- **Theming**: Customizable widget appearance
- **Animation**: Built-in animation support
- **Accessibility**: Screen reader support
- **Internationalization**: Multi-language support
- **Resource Management**: Centralized image/icon handling

