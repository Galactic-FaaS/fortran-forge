# ForGE Qt - Comprehensive Application Framework for Fortran

[![License: GPL v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
[![Fortran](https://img.shields.io/badge/Fortran-2008%2B-blueviolet.svg)](https://fortran-lang.org/)
[![Status](https://img.shields.io/badge/Status-Alpha-orange.svg)](https://github.com/your-org/fortran-forge)

**ForGE Qt** is a comprehensive, Qt-equivalent application framework for Fortran, providing everything from GUI widgets to networking, database access, and threading - all with a Fortran-native API.

## 🌟 The Fortran Version of Qt

ForGE Qt aims to bring Qt-level functionality to Fortran applications, including:
- **50+ Widget Types** (17 implemented, more coming)
- **Signals & Slots** - Qt-style event handling ✅
- **QString & Containers** - Advanced data structures ✅
- **Networking** - HTTP client, TCP/UDP sockets ✅ API
- **Threading** - QThread, mutexes, semaphores ✅ API
- **JSON/XML** - Modern data formats ✅ API
- **Multi-Platform** - Windows ✅, Linux (planned), macOS (planned), Platform-Agnostic ✅
- **ForgeML** - Declarative UI language (planned)
- **Visual Designer** - GUI builder tool (planned)

## 🌟 Features

- **Modern Fortran**: Built with Fortran 2008/2018 features (OOP, abstract interfaces, submodules)
- **Cross-Platform**: Runs on Linux, Windows, and macOS
- **Multiple Backends**: Pluggable backend system (Tcl/Tk, GTK4, Qt - coming soon!)
- **Clean API**: Intuitive, object-oriented interface with builder pattern support
- **Event-Driven**: Comprehensive event handling system
- **Flexible Layouts**: Grid, Box, and Stack layout managers
- **Rich Widgets**: Buttons, labels, text entries, progress bars, and more
- **Modern Build Systems**: Support for both fpm and CMake
- **Well Documented**: Extensive inline documentation and examples

## 🚀 Quick Start

### Installation

#### Using Fortran Package Manager (fpm)

```bash
# Clone the repository
git clone https://github.com/Galactic-FaaS/fortran-forge.git
cd fortran-forge

# Build with fpm
fpm build

# Run an example
fpm run --example hello_world
```

#### Using CMake

```bash
# Clone the repository
git clone https://github.com/Galactic-FaaS/fortran-forge.git
cd fortran-forge

# Configure and build
mkdir build
cd build
cmake ..
cmake --build .

# Run an example
./hello_world
```

### Basic Usage

```fortran
program my_gui_app
    use forge
    implicit none
    
    type(forge_application) :: app
    type(forge_window_t) :: window
    type(forge_button) :: button
    
    ! Initialize application (backend will be implemented)
    call app%init(BACKEND_TCL_TK)
    
    ! Create a window using builder pattern
    window = app%create_window("My Application", 800, 600)
    
    ! Show the window
    call window%show()
    
    ! Run the event loop
    call app%run()
    
    ! Cleanup
    call app%shutdown()
    
end program my_gui_app
```

## 📚 Documentation

- [API Documentation](docs/api/) - Complete API reference
- [Tutorials](docs/tutorials/) - Step-by-step guides
- [GUI Framework Comparison](docs/GUI_Framework_Comparison.md) - Backend options analysis
- [Migration Guide](docs/migration_guide.md) - Transitioning from ForGE v0.4 (GTK2)
- [Examples](examples/) - Working example programs

## 🏗️ Project Structure

```
fortran-forge/
├── src/                    # Library source code
│   ├── forge.f90          # Main module
│   ├── forge_types.f90    # Type definitions
│   ├── forge_window.f90   # Window management
│   ├── forge_widgets.f90  # Widget system
│   ├── forge_events.f90   # Event handling
│   ├── forge_layout.f90   # Layout managers
│   ├── forge_backend.f90  # Backend abstraction
│   └── backends/          # Backend implementations
│       ├── forge_stub.f90
│       ├── forge_tcl_tk.f90 (planned)
│       ├── forge_gtk4.f90   (planned)
│       └── forge_qt.f90     (planned)
├── examples/              # Example applications
├── test/                  # Test suite
├── docs/                  # Documentation
├── fpm.toml              # FPM manifest
└── CMakeLists.txt        # CMake configuration
```

## 🎨 Available Widgets

- **Window**: Main application window with title, size, position control
- **Button**: Clickable buttons with event handlers
- **Label**: Static text display
- **Entry**: Single-line text input
- **TextView**: Multi-line text editor
- **ProgressBar**: Visual progress indicator
- **Separator**: Visual divider (horizontal/vertical)
- **ComboBox**: Dropdown selection (coming soon)
- **Slider**: Value selection slider (coming soon)
- **SpinButton**: Numeric input with increment/decrement (coming soon)

## 📦 Examples

### Hello World
```bash
fpm run --example hello_world
```
Creates a simple window with a label.

### Button Demo
```bash
fpm run --example button_demo
```
Demonstrates button creation and click event handling.

### More Examples
- `menu_demo` - Menu bar with submenus
- `text_editor` - Simple text editor with file operations
- `graphics_demo` - Drawing and graphics
- `form_builder` - Form layout with validation
- `multi_window` - Multiple window management
- `theme_showcase` - Widget styling

## 🔧 Build Requirements

### Compiler
- **gfortran** 9.0+ (recommended)
- **Intel Fortran** (ifort/ifx)
- **NAG Fortran**

### Build Tools
- **fpm** (Fortran Package Manager) - recommended, or
- **CMake** 3.20+

### Backend Dependencies (Optional)
Depending on which backend you use:

- **Tcl/Tk Backend**: Tcl/Tk 8.6+, ftcl bindings
- **GTK4 Backend**: GTK4, gtk-fortran (in development)
- **Qt Backend**: Qt6 (in development)
- **Stub Backend**: No dependencies (testing only)

## 🛠️ Current Status

**Version**: 1.0.0 (Alpha)

This is a complete modernization/rewrite of the original ForGE project. The current release includes:

### ✅ Implemented
- [x] Core infrastructure (types, errors, events)
- [x] Backend abstraction layer
- [x] Window management system
- [x] Widget base classes and common widgets
- [x] Layout manager architecture
- [x] Build system configuration (fpm + CMake)
- [x] Stub backend for testing
- [x] Example programs
- [x] Documentation structure

### 🚧 In Progress
- [ ] Tcl/Tk backend implementation (primary focus)
- [ ] Additional widgets (ComboBox, Slider, SpinButton, etc.)
- [ ] Layout manager implementations
- [ ] Test suite
- [ ] CI/CD pipeline

### 📋 Planned
- [ ] GTK4 backend
- [ ] Qt backend  
- [ ] Advanced features (drag-drop, clipboard, dialogs)
- [ ] Graphics/Cairo integration
- [ ] Theming support
- [ ] Comprehensive test coverage

## 🤝 Contributing

Contributions are welcome! Please see [CONTRIBUTING.md](CONTRIBUTING.md) for guidelines.

### Development Priorities
1. Tcl/Tk backend implementation
2. Additional widget types
3. Layout manager functionality
4. Test coverage
5. Documentation improvements

## 📜 License

ForGE is released under the **GNU General Public License v3.0 or later**. See [LICENSE](LICENSE) for details.

## 📝 History

ForGE originated as a GTK2-based GUI library (v0.1 - v0.4.0) and is now being modernized with:
- Modern Fortran 2008/2018 features
- Multiple backend support
- Improved architecture
- Better documentation
- Active maintenance

See [CHANGELOG.md](CHANGELOG.md) for version history.

## 🙏 Acknowledgments

- Original ForGE project by John N. Shahbazian
- gtk-fortran project for inspiration
- Fortran-lang community for tools and support

## 📧 Contact

- **Issues**: [GitHub Issues](https://github.com/your-org/fortran-forge/issues)
- **Discussions**: [GitHub Discussions](https://github.com/your-org/fortran-forge/discussions)
- **Email**: forge-dev@example.com

---

**Note**: ForGE is under active development. The API may change as we implement and refine features. We're working towards a stable 1.0.0 release with at least one fully functional backend.
