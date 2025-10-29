# Meta-Object Compiler (moc) Tool

The ForGE Meta-Object Compiler (moc) is a code generation tool that enables Qt-style signal/slot introspection and runtime type information for Fortran classes.

## Overview

The moc tool processes Fortran source files containing Qt-style class definitions and generates additional code that provides:

- Runtime type introspection
- Signal/slot connectivity information
- Meta-object data structures
- Dynamic method invocation support

## Usage

### Basic Usage

```bash
forge_moc [options] <input_file>
```

### Command Line Options

- `-o, --output <file>`: Specify output file (default: `moc_<input>.f90`)
- `-v, --verbose`: Enable verbose output
- `-h, --help`: Display help information

### Examples

```bash
# Basic usage with default output
forge_moc my_class.f90

# Specify output file
forge_moc -o generated_meta.f90 my_class.f90

# Verbose output
forge_moc -v my_class.f90
```

## Input File Format

The moc tool processes Fortran source files that contain Qt-style class definitions with the `Q_OBJECT` macro.

### Basic Class Structure

```fortran
type, extends(forge_qobject) :: my_class
    ! Q_OBJECT  ! This macro tells moc to process this class

    ! Signals
    type(signal_void) :: clicked
    type(signal_string) :: text_changed

    ! Slots
    type(slot_void_proc) :: on_clicked
    type(slot_string_proc) :: on_text_changed

contains
    procedure :: my_method
end type my_class
```

### Q_OBJECT Macro

The `Q_OBJECT` macro is a special comment that tells moc to generate meta-object code for the class:

```fortran
type, extends(forge_qobject) :: my_widget
    ! Q_OBJECT  ! Required for moc processing

    ! Class members...
end type my_widget
```

## Generated Output

For each class with `Q_OBJECT`, moc generates a module containing:

### Meta-Object Data

```fortran
type(forge_meta_object), parameter :: meta_my_class = &
    forge_meta_object(class_name='my_class', &
                     super_class_name='forge_qobject', &
                     property_count=0, &
                     method_count=1, &
                     signal_count=2, &
                     slot_count=2, &
                     property_names=[forge_string::], &
                     method_names=[forge_string::'my_method'], &
                     signal_names=[forge_string::'clicked', 'text_changed'], &
                     slot_names=[forge_string::'on_clicked', 'on_text_changed'])
```

### Initialization Subroutine

```fortran
subroutine init_meta_my_class(obj)
    class(forge_qobject), intent(inout) :: obj
    obj%meta_object = meta_my_class
end subroutine init_meta_my_class
```

## Integration with Build Systems

### CMake Integration

```cmake
# Find ForGE
find_package(ForGE REQUIRED)

# Generate moc files
forge_moc_generate(MOC_SOURCES my_widget.f90)

# Add to executable
add_executable(my_app my_app.f90 my_widget.f90 ${MOC_SOURCES})
target_link_libraries(my_app ForGE::forge)
```

### FPM Integration

```toml
# fpm.toml
[build]
# Pre-build commands
prebuild = [
    "forge_moc my_widget.f90 -o moc_my_widget.f90"
]

[[executable]]
name = "my_app"
sources = ["my_app.f90", "my_widget.f90", "moc_my_widget.f90"]
```

### Make Integration

```makefile
# Makefile
MOC = forge_moc
FORTRAN = gfortran

# Generate moc files
moc_my_widget.f90: my_widget.f90
	$(MOC) $< -o $@

# Build application
my_app: my_app.f90 my_widget.f90 moc_my_widget.f90
	$(FORTRAN) $^ -o $@ -lforge
```

## Signal/Slot System

### Signals

Signals are declared as `signal_*` types:

```fortran
type(signal_void) :: button_clicked
type(signal_int) :: value_changed
type(signal_string) :: text_updated
```

### Slots

Slots are declared as `slot_*_proc` types:

```fortran
type(slot_void_proc) :: handle_click
type(slot_int_proc) :: handle_value_change
type(slot_string_proc) :: handle_text_update
```

### Connection

```fortran
! Connect signal to slot
call button_clicked%connect(handle_click)

! Emit signal
call button_clicked%emit()
```

## Advanced Features

### Inheritance Support

Moc handles class inheritance correctly:

```fortran
type, extends(parent_class) :: child_class
    ! Q_OBJECT

    ! Additional signals/slots...
end type child_class
```

The generated meta-object will include inherited signals and slots.

### Multiple Classes per File

A single input file can contain multiple classes with `Q_OBJECT`:

```fortran
type, extends(forge_qobject) :: class1
    ! Q_OBJECT
    ! ...
end type class1

type, extends(forge_qobject) :: class2
    ! Q_OBJECT
    ! ...
end type class2
```

Moc will generate separate modules for each class.

### Custom Types

Moc supports custom signal/slot types:

```fortran
! Custom signal type
type :: custom_data
    integer :: id
    character(len=100) :: name
end type custom_data

type(signal_custom_data) :: data_changed
type(slot_custom_data_proc) :: handle_data_change
```

## Error Handling

### Common Errors

1. **Missing Q_OBJECT macro**
   ```
   Error: No Q_OBJECT found in class definition
   Solution: Add '! Q_OBJECT' comment in class definition
   ```

2. **Invalid class structure**
   ```
   Error: Unable to parse class definition
   Solution: Ensure proper Fortran class syntax
   ```

3. **Output file creation failure**
   ```
   Error: Cannot create output file
   Solution: Check write permissions in output directory
   ```

### Verbose Mode

Use `-v` flag for detailed processing information:

```bash
forge_moc -v my_class.f90
```

Output includes:
- Files being processed
- Classes found
- Signals/slots detected
- Generated code statistics

## Performance Considerations

### Compilation Time

- Moc processing is fast (typically < 1 second per file)
- Generated code is optimized for runtime performance
- Minimal impact on build times

### Runtime Overhead

- Meta-object data is stored in read-only memory
- Signal/slot connections have minimal overhead
- Introspection is fast (O(1) lookup)

### Memory Usage

- Meta-object data is compact
- No per-instance overhead for meta-objects
- Shared meta-object data across all instances

## Debugging Generated Code

### Inspecting Generated Files

The generated moc files can be examined to understand the meta-object structure:

```fortran
! Check generated meta-object
print *, "Class name:", meta_my_class%class_name
print *, "Signal count:", meta_my_class%signal_count
print *, "Signals:", meta_my_class%signal_names
```

### Runtime Introspection

Use the meta-object for runtime type checking:

```fortran
subroutine check_object_type(obj)
    class(forge_qobject), intent(in) :: obj

    if (obj%meta_object%class_name == "my_class") then
        print *, "Object is of type my_class"
    end if
end subroutine check_object_type
```

## Best Practices

### Class Organization

1. **Group related signals/slots together**
2. **Use descriptive names for signals and slots**
3. **Document signal/slot purposes with comments**

### Build System Integration

1. **Generate moc files in build directory**
2. **Include moc files in dependency tracking**
3. **Clean moc files during `make clean`**

### Error Prevention

1. **Always check moc return codes in build scripts**
2. **Use verbose mode during development**
3. **Validate generated code compiles correctly**

## Examples

### Simple Widget

```fortran
! Input: simple_widget.f90
type, extends(forge_qobject) :: simple_widget
    ! Q_OBJECT

    type(signal_void) :: clicked
    type(slot_void_proc) :: handle_click

contains
    procedure :: on_click
end type simple_widget

! Generated: moc_simple_widget.f90
! Contains meta-object data and initialization
```

### Complex Dialog

```fortran
! Input: complex_dialog.f90
type, extends(forge_qobject) :: complex_dialog
    ! Q_OBJECT

    ! Multiple signals
    type(signal_string) :: text_changed
    type(signal_int) :: value_changed
    type(signal_bool) :: accepted

    ! Multiple slots
    type(slot_string_proc) :: update_text
    type(slot_int_proc) :: update_value
    type(slot_void_proc) :: accept_dialog

contains
    procedure :: validate_input
    procedure :: save_settings
end type complex_dialog
```

## Troubleshooting

### Build Issues

**Problem**: Moc-generated files not found by compiler
**Solution**: Ensure moc is run before compilation in build system

**Problem**: Linker errors with moc-generated symbols
**Solution**: Include moc-generated modules in compilation

### Runtime Issues

**Problem**: Signals not connecting
**Solution**: Verify slot procedure signatures match signal types

**Problem**: Meta-object data incorrect
**Solution**: Regenerate moc files after class changes

## See Also

- [Signal/Slot System](../tutorials/signals_slots.md)
- [Qt Compatibility](../tutorials/qt_compatibility.md)
- [Build System Integration](../tutorials/build_integration.md)
- [RCC Tool](rcc_tool.md) - Resource compiler
- [UIC Tool](uic_tool.md) - UI compiler