# Changelog

All notable changes to the ForGE project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [1.0.0-alpha] - 2025-10-25

### Added (Modernization - Complete Rewrite)

#### Core Infrastructure
- Modern Fortran 2008/2018 implementation with full OOP support
- Comprehensive type system (`forge_types.f90`)
  - Dynamic string handling with C interop
  - Color, size, position, and rect types
- Robust error handling system (`forge_errors.f90`)
  - Error codes and status types
  - Error checking utilities
- Event system (`forge_events.f90`)
  - Event types for mouse, keyboard, window, widget events
  - Callback mechanism with procedure pointers
  - Event handler containers

#### Backend Architecture
- Abstract backend interface (`forge_backend.f90`)
  - Pluggable backend system
  - Support for multiple GUI frameworks
  - Runtime backend selection
- Stub backend implementation for testing
- Planned backends: Tcl/Tk, GTK4, Qt, Custom

#### GUI Components
- Window management (`forge_window.f90`)
  - Object-oriented Window class
  - Builder pattern for fluent API
  - Window properties: title, size, position, resizable
  - Window lifecycle: show, hide, close
- Widget system (`forge_widgets.f90`)
  - Abstract base Widget class
  - Concrete widgets:
    - Button with click events
    - Label for static text
    - Entry for single-line input
    - TextView for multi-line text
    - ProgressBar for visual progress
    - Separator for visual division
  - Widget properties: visible, enabled, size, position
- Layout managers (`forge_layout.f90`)
  - Abstract layout interface
  - Grid layout (table-like arrangement)
  - Box layout (horizontal/vertical)
  - Stack layout (layered widgets)

#### Build Systems
- **fpm (Fortran Package Manager)**
  - Complete fpm.toml configuration
  - Build profiles: debug, dev, release
  - Example program support
  - Test integration
- **CMake**
  - Modern CMake 3.20+ configuration
  - Cross-platform support (Linux, Windows, macOS)
  - Backend selection via options
  - pkg-config file generation
  - Both static and shared library builds

#### Documentation
- Comprehensive README.md with:
  - Quick start guide
  - Feature overview
  - Installation instructions
  - API usage examples
- GUI Framework Comparison document
  - Analysis of backend options
  - Technical recommendations
  - Proof-of-concept plan
- Project structure documentation
- Inline API documentation with Ford/Doxygen support

#### Examples
- `hello_world` - Basic window creation
- `button_demo` - Button events and handlers
- Planned examples:
  - menu_demo
  - text_editor
  - graphics_demo
  - form_builder
  - multi_window
  - theme_showcase

#### Development Infrastructure
- Directory structure reorganization
- Modern Fortran coding standards
- Prepared for CI/CD integration
- Test framework structure

### Changed from v0.4.0

#### Breaking Changes
- **Complete API rewrite** - Not compatible with v0.4.0
- Removed direct GTK2 dependency
- New object-oriented interface replaces procedural API
- Backend abstraction replaces hard-coded gtk-fortran calls

#### Improvements
- Fortran 2008/2018 features vs Fortran 90/95
- Abstract interfaces and polymorphism
- Better error handling with status types
- Cleaner event system with procedure pointers
- Builder pattern for ergonomic object construction
- Modular architecture with clear separation of concerns

### Removed from v0.4.0
- GTK2-specific code
- plplot dependency (will be optional in future)
- Global variables replaced with OOP design
- Hard-coded widget positioning (replaced with layout managers)

### Migration from v0.4.0

The API has been completely redesigned. See `docs/migration_guide.md` for:
- API mapping table (old → new)
- Code conversion examples
- Feature parity status

---

## [0.4.0] - Original Project (Inactive)

### Added
- Sub-menus, separator widgets
- Sliders and spin buttons
- Cairo drawing area integration
- Fixed duplicate widget name checking
- Revamped widget placement and sizing
- `run_on_interval` function for periodic updates

## [0.3.1] - Original Project

### Changed
- Complete rewrite to object-oriented design
- More widgets added
- Uses window type containers

## [0.2.2] - Original Project

### Changed
- Changed to static library

## [0.2.1] - Original Project

### Removed
- OpenMP support (planned for future)

### Changed
- Rearranged functions for clarity

## [0.2.0] - Original Project

### Changed
- Converted to object containers per window
- Removed global variables
- Renamed handlers module to event_handlers

### Added
- `create_button` procedure
- `create_text_entry` procedure
- Example event handlers

## [0.1.0] - Original Project

### Added
- Initial release
- GTK2 integration via gtk-fortran
- Basic widgets: windows, buttons, labels
- Grid-based layout
- Event handling

---

## Version History Summary

- **1.0.0-alpha** (2025) - Complete modernization rewrite
- **0.4.0** (2014) - Final version of original project
- **0.1.0** (2012) - Original ForGE release

[1.0.0-alpha]: https://github.com/your-org/fortran-forge/releases/tag/v1.0.0-alpha

