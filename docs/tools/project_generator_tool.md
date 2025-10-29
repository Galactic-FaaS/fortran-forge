# Project Generator Tool

The ForGE Project Generator Tool creates complete Fortran Qt project templates with proper directory structure, build configuration, and starter code, accelerating project setup and ensuring best practices.

## Overview

The project generator creates ready-to-build ForGE applications with:

- Proper directory structure
- Build system configuration (CMake/FPM)
- Basic application skeleton
- Example widgets and layouts
- Documentation templates
- Tool configurations

## Usage

### Basic Usage

```bash
forge_project_generator [options] <project_name>
```

### Command Line Options

- `--type <type>`: Project type (app, lib, test) (default: app)
- `--backend <backend>`: GUI backend (custom, qt, gtk, tcl) (default: custom)
- `--build <system>`: Build system (cmake, fpm) (default: cmake)
- `--template <template>`: Project template (basic, advanced, example)
- `--output <dir>`: Output directory (default: current directory)
- `-v, --verbose`: Enable verbose output
- `-h, --help`: Display help information

### Examples

```bash
# Basic application
forge_project_generator my_app

# Library project with Qt backend
forge_project_generator --type lib --backend qt my_library

# Advanced template with FPM build
forge_project_generator --template advanced --build fpm --backend custom advanced_app

# Custom output directory
forge_project_generator --output /path/to/projects my_project
```

## Project Types

### Application Projects

Full GUI applications with windows, widgets, and event handling:

```
my_app/
├── src/
│   ├── main.f90          # Application entry point
│   ├── main_window.f90   # Main window implementation
│   └── ui/
│       └── widgets.f90   # Custom widgets
├── include/
│   └── my_app.h
├── CMakeLists.txt        # Build configuration
├── fpm.toml            # FPM configuration
├── README.md           # Documentation
└── .gitignore          # Git ignore file
```

### Library Projects

Reusable component libraries:

```
my_lib/
├── src/
│   └── my_lib.f90       # Library modules
├── include/
│   └── my_lib.h
├── test/
│   └── test_my_lib.f90  # Unit tests
├── CMakeLists.txt
├── fpm.toml
└── README.md
```

### Test Projects

Unit testing frameworks:

```
test_project/
├── src/
│   └── main.f90
├── test/
│   ├── test_main.f90
│   └── test_utils.f90
├── CMakeLists.txt
├── fpm.toml
└── README.md
```

## Templates

### Basic Template

Minimal application with essential components:

- Simple window with basic widgets
- Event handling setup
- Minimal build configuration
- Basic documentation

### Advanced Template

Full-featured application with:

- Multiple windows and dialogs
- Complex widget layouts
- Custom widget implementations
- Comprehensive event handling
- Advanced build configuration
- Complete documentation

### Example Template

Educational template with:

- Commented code examples
- Best practice demonstrations
- Multiple GUI patterns
- Tool usage examples
- Tutorial-style documentation

## Generated Files

### Source Files

#### main.f90 (Application Entry Point)

```fortran
!> @brief Main application entry point
program main
    use forge
    implicit none

    type(forge_application) :: app
    type(forge_status) :: status

    ! Initialize application
    call app%init(BACKEND_CUSTOM, status)
    call forge_check_status(status, abort_on_error=.true.)

    ! Create and show main window
    call create_main_window(app)

    ! Run event loop
    call app%run()

    ! Cleanup
    call app%shutdown()

contains

    subroutine create_main_window(app)
        type(forge_application), intent(inout) :: app
        type(main_window) :: window

        ! Create main window
        call window%init()
        call window%show()
    end subroutine create_main_window

end program main
```

#### Main Window Implementation

```fortran
!> @brief Main application window
module main_window
    use forge
    implicit none

    type :: main_window_t
        type(forge_window_t) :: window
        ! Widget declarations...
    contains
        procedure :: init => main_window_init
        procedure :: show => main_window_show
    end type main_window_t

contains

    subroutine main_window_init(this)
        class(main_window_t), intent(inout) :: this

        ! Initialize window and widgets
        this%window = app%create_window("My Application", 800, 600)

        ! Setup UI components
        call setup_ui(this)
    end subroutine main_window_init

    subroutine main_window_show(this)
        class(main_window_t), intent(inout) :: this
        call this%window%show()
    end subroutine main_window_show

end module main_window
```

### Build Configuration

#### CMakeLists.txt

```cmake
cmake_minimum_required(VERSION 3.16)
project(my_app VERSION 1.0.0 LANGUAGES Fortran)

# Find ForGE
find_package(ForGE REQUIRED)

# Source files
set(SOURCES
    src/main.f90
    src/main_window.f90
)

# Create executable
add_executable(${PROJECT_NAME} ${SOURCES})

# Link ForGE
target_link_libraries(${PROJECT_NAME} ForGE::forge)

# Include directories
target_include_directories(${PROJECT_NAME} PRIVATE include/)

# Compiler options
target_compile_options(${PROJECT_NAME} PRIVATE
    $<$<Fortran_COMPILER_ID:GNU>:-Wall -Wextra>
    $<$<Fortran_COMPILER_ID:Intel>:-warn all>
)
```

#### fpm.toml

```toml
name = "my_app"
version = "1.0.0"
license = "MIT"
author = "Your Name"
maintainer = "your.email@example.com"
copyright = "2025 Your Name"

[build]
auto-executables = true
auto-tests = true
auto-examples = true

[dependencies]
ForGE = { git = "https://github.com/your-org/fortran-forge.git" }

[[executable]]
name = "my_app"
source-dir = "src"
main = "main.f90"

[[test]]
name = "test_my_app"
source-dir = "test"
main = "test_main.f90"
```

### Documentation

#### README.md

```markdown
# My Application

A ForGE-based GUI application.

## Building

### With CMake

```bash
mkdir build
cd build
cmake ..
make
./my_app
```

### With FPM

```bash
fpm build
fpm run my_app
```

## Features

- Modern Fortran GUI
- Cross-platform compatibility
- Qt-style programming

## Dependencies

- ForGE library
- Fortran compiler (gfortran/Intel)
- CMake or FPM build system
```

### Documentation Templates

The generator creates starter documentation files:

- `API.md`: API documentation template
- `CHANGELOG.md`: Version history
- `CONTRIBUTING.md`: Contribution guidelines
- `docs/`: Additional documentation directory

## Customization Options

### Backend Selection

Choose the appropriate GUI backend:

```bash
# Custom backend (recommended for new projects)
forge_project_generator --backend custom my_app

# Qt backend (requires Qt installation)
forge_project_generator --backend qt my_qt_app

# GTK backend (Linux-focused)
forge_project_generator --backend gtk my_gtk_app
```

### Build System Selection

```bash
# CMake (cross-platform, more configuration options)
forge_project_generator --build cmake my_app

# FPM (Fortran-focused, simpler for Fortran projects)
forge_project_generator --build fpm my_app
```

### Template Selection

```bash
# Basic template (minimal, good for learning)
forge_project_generator --template basic simple_app

# Advanced template (full-featured, production-ready)
forge_project_generator --template advanced complex_app

# Example template (educational, well-commented)
forge_project_generator --template example demo_app
```

## Integration with Development Tools

### IDE Integration

Generated projects include configuration for popular IDEs:

- **VS Code**: `.vscode/` directory with Fortran extensions
- **Qt Creator**: `.pro` files for Qt projects
- **Eclipse/Photran**: Project files for Eclipse

### Version Control

Generated `.gitignore` files for:

- Build artifacts
- Generated files
- IDE-specific files
- OS-specific files

### CI/CD Integration

Starter CI configuration:

```yaml
# .github/workflows/build.yml
name: Build
on: [push, pull_request]

jobs:
  build:
    runs-on: ubuntu-latest
    steps:
    - uses: actions/checkout@v2
    - name: Setup Fortran
      uses: fortran-lang/setup-fortran@v1
    - name: Build with FPM
      run: fpm build
    - name: Run tests
      run: fpm test
```

## Advanced Features

### Custom Templates

Create custom project templates:

```bash
# Create template directory
mkdir -p ~/.forge/templates/my_template/

# Add template files
# Template files can use variables like {{PROJECT_NAME}}, {{AUTHOR}}, etc.

# Use custom template
forge_project_generator --template ~/.forge/templates/my_template my_project
```

### Project Variables

Templates support variable substitution:

- `{{PROJECT_NAME}}`: Project name
- `{{AUTHOR}}`: Author name
- `{{EMAIL}}`: Author email
- `{{YEAR}}`: Current year
- `{{VERSION}}`: Project version

### Post-Generation Hooks

Run custom commands after project generation:

```bash
# Initialize git repository
forge_project_generator my_app
cd my_app
git init
git add .
git commit -m "Initial commit"
```

## Best Practices

### Project Organization

1. **Separate concerns**: Keep UI, business logic, and data separate
2. **Use modules**: Organize code into logical modules
3. **Document interfaces**: Document public APIs
4. **Include tests**: Add unit tests from the start

### Build Configuration

1. **Choose appropriate build system**: CMake for complex projects, FPM for simpler ones
2. **Set compiler flags**: Enable warnings and optimizations
3. **Handle dependencies**: Properly declare and link dependencies
4. **Cross-platform compatibility**: Test on multiple platforms

### Code Quality

1. **Follow Fortran standards**: Use modern Fortran features
2. **Consistent naming**: Use consistent naming conventions
3. **Error handling**: Implement proper error handling
4. **Documentation**: Document code and APIs

## Examples

### Simple GUI Application

```bash
$ forge_project_generator --template basic --backend custom simple_gui
Creating project 'simple_gui'...
Template: basic
Backend: custom
Build system: cmake

Generated files:
  src/main.f90
  src/main_window.f90
  CMakeLists.txt
  README.md
  .gitignore

Project created successfully!

To build:
  cd simple_gui
  mkdir build && cd build
  cmake ..
  make
  ./simple_gui
```

### Library Project

```bash
$ forge_project_generator --type lib --build fpm my_library
Creating library project 'my_library'...
Type: library
Build system: fpm

Generated files:
  src/my_library.f90
  test/test_my_library.f90
  fpm.toml
  README.md

Library created successfully!

To build:
  cd my_library
  fpm build
  fpm test
```

### Advanced Application

```bash
$ forge_project_generator --template advanced --backend qt advanced_app
Creating project 'advanced_app'...
Template: advanced
Backend: qt
Build system: cmake

Generated files:
  src/main.f90
  src/main_window.f90
  src/dialogs.f90
  src/widgets/
    custom_widget.f90
    data_table.f90
  include/
    advanced_app.h
  CMakeLists.txt
  README.md
  docs/
    API.md
    user_guide.md

Advanced project created successfully!
```

## Troubleshooting

### Common Issues

**Template not found**
- Check template name spelling
- Verify custom template path exists

**Build system not configured**
- Ensure CMake/FPM is installed
- Check PATH environment variable

**Backend not available**
- Verify backend dependencies are installed
- Check backend-specific requirements

### Verbose Mode

Use `-v` for detailed generation information:

```bash
forge_project_generator -v my_project
Creating project 'my_project'...
Using template: basic
Setting backend: custom
Generating src/main.f90...
Generating CMakeLists.txt...
Copying .gitignore...
Project created successfully
```

## See Also

- [Getting Started Tutorial](../tutorials/getting_started.md)
- [Build System Guide](../tutorials/build_systems.md)
- [Backend Selection Guide](../tutorials/backend_selection.md)
- [Project Structure Best Practices](../tutorials/project_structure.md)