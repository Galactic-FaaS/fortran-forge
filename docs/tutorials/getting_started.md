# Getting Started with ForGE

This tutorial will guide you through creating your first GUI application with ForGE.

## Prerequisites

- Fortran compiler (gfortran 9.0+ recommended)
- fpm (Fortran Package Manager) or CMake 3.20+
- Basic Fortran knowledge

## Installation

### Option 1: Using fpm (Recommended)

```bash
# Clone the repository
git clone https://github.com/your-org/fortran-forge.git
cd fortran-forge

# Build the library
fpm build

# The library is now ready to use
```

### Option 2: Using CMake

```bash
# Clone and build
git clone https://github.com/your-org/fortran-forge.git
cd fortran-forge
mkdir build && cd build
cmake ..
cmake --build .
sudo cmake --install .
```

## Your First ForGE Application

Let's create a simple "Hello World" application.

### Step 1: Create Your Program File

Create a file named `my_first_gui.f90`:

```fortran
program my_first_gui
    use forge
    use forge_stub_backend  ! Using stub for now
    implicit none
    
    type(forge_window_t) :: window
    type(forge_label) :: label
    type(forge_stub_backend_t), target :: backend
    type(forge_status) :: status
    
    ! Initialize the backend
    call backend%init(status)
    if (status%is_error()) then
        print *, "Error initializing backend"
        stop 1
    end if
    
    ! Create a window using the builder pattern
    window = create_window_with_builder(backend)
    
    ! Show the window
    call window%show()
    
    ! Run the event loop
    call backend%run()
    
    ! Cleanup
    call window%close()
    call backend%shutdown()
    
contains

    function create_window_with_builder(backend) result(window)
        type(forge_stub_backend_t), target, intent(in) :: backend
        type(forge_window_t) :: window
        type(forge_window_builder) :: builder
        type(forge_status) :: status
        
        window = builder%set_title("My First ForGE App") &
                       %set_size(640, 480) &
                       %set_position(100, 100) &
                       %set_backend(backend) &
                       %build(status)
    end function create_window_with_builder

end program my_first_gui
```

### Step 2: Build and Run

**With fpm:**
```bash
fpm run my_first_gui
```

**With CMake:**
```bash
gfortran my_first_gui.f90 -I/usr/local/include/forge -L/usr/local/lib -lforge
./a.out
```

## Understanding the Code

### 1. Module Import
```fortran
use forge
```
Imports the main ForGE module with all public interfaces.

### 2. Backend Initialization
```fortran
call backend%init(status)
```
Initializes the GUI backend. Currently using stub backend for demonstration.

### 3. Window Creation with Builder Pattern
```fortran
window = builder%set_title("Title") &
               %set_size(width, height) &
               %set_backend(backend) &
               %build(status)
```
The builder pattern provides a fluent, readable way to configure and create objects.

### 4. Event Loop
```fortran
call backend%run()
```
Runs the main event loop. This blocks until the application closes.

## Adding Widgets

Let's enhance our application with some widgets:

```fortran
program gui_with_widgets
    use forge
    use forge_stub_backend
    implicit none
    
    type(forge_window_t) :: window
    type(forge_button) :: button
    type(forge_label) :: label
    type(forge_stub_backend_t), target :: backend
    type(forge_status) :: status
    
    ! Initialize backend
    call backend%init(status)
    call forge_check_status(status, abort_on_error=.true.)
    
    ! Create window
    window = create_main_window(backend)
    
    ! Create and configure widgets
    call label%set_text("Welcome to ForGE!")
    call label%set_name("welcome_label")
    
    call button%set_label("Click Me!")
    call button%set_name("click_button")
    call button%on_click(on_button_clicked)
    
    ! Show window and run
    call window%show()
    call backend%run()
    
    ! Cleanup
    call window%close()
    call backend%shutdown()
    
contains

    function create_main_window(backend) result(window)
        type(forge_stub_backend_t), target, intent(in) :: backend
        type(forge_window_t) :: window
        type(forge_window_builder) :: builder
        type(forge_status) :: status
        
        window = builder%set_title("ForGE Widgets Demo") &
                       %set_size(800, 600) &
                       %set_backend(backend) &
                       %build(status)
    end function create_main_window
    
    !> Button click event handler
    subroutine on_button_clicked(event)
        type(forge_event), intent(in) :: event
        print *, "Button was clicked!"
    end subroutine on_button_clicked

end program gui_with_widgets
```

## Next Steps

- Explore the [examples](../../examples/) directory for more complex applications
- Read the [API documentation](../api/) for detailed information
- Learn about [layout managers](layout_managers.md) for organizing widgets
- Understand [event handling](event_handling.md) in depth
- Check out the [widget gallery](widget_gallery.md) for all available widgets

## Troubleshooting

### Build Errors

**Problem**: Compiler can't find ForGE modules
- **Solution**: Ensure ForGE is properly installed or specify module path with `-I` flag

**Problem**: Linker can't find libforge
- **Solution**: Specify library path with `-L` flag and link with `-lforge`

### Runtime Errors

**Problem**: "Backend not initialized" error
- **Solution**: Always call `backend%init()` before creating windows

**Problem**: Window doesn't appear
- **Solution**: Ensure you're using a real backend (Tcl/Tk, GTK4, Qt) not the stub backend

## Getting Help

- Check the [FAQ](../faq.md)
- Browse [examples](../../examples/)
- Open an issue on GitHub
- Join our community discussions

Happy coding with ForGE!

