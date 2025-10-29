# Code Analysis Tool

The ForGE Code Analysis Tool performs static analysis and code quality checks on Fortran Qt applications, helping developers maintain high-quality, maintainable code.

## Overview

The code analyzer examines Fortran source code for potential issues, style violations, and best practice deviations. It provides comprehensive reports on:

- Qt style guide compliance
- Memory management issues
- Signal/slot usage patterns
- Code structure and organization
- Documentation completeness

## Usage

### Basic Usage

```bash
forge_code_analyzer [options] <source_directory>
```

### Command Line Options

- `-o, --output <file>`: Specify output report file (default: `analysis_report.txt`)
- `--no-qt-style`: Skip Qt style checks
- `--no-memory`: Skip memory management checks
- `--no-signals`: Skip signal/slot checks
- `-v, --verbose`: Enable verbose output
- `-h, --help`: Display help information

### Examples

```bash
# Basic analysis
forge_code_analyzer src/

# Custom output file
forge_code_analyzer -o my_report.txt src/

# Selective checks
forge_code_analyzer --no-memory --no-signals src/

# Verbose output
forge_code_analyzer -v src/
```

## Analysis Categories

### Qt Style Checks

Ensures code follows Qt programming conventions:

- **Q_OBJECT macro usage**: Verifies proper Qt object declarations
- **Signal/slot naming**: Checks naming conventions for signals and slots
- **Error handling**: Validates error handling patterns
- **Qt API usage**: Ensures proper use of Qt classes and methods

### Memory Management Checks

Identifies potential memory issues:

- **Allocation/deallocation pairs**: Checks for unmatched allocate/deallocate
- **Pointer safety**: Validates pointer usage patterns
- **QObject lifecycle**: Ensures proper object initialization and cleanup
- **Resource leaks**: Detects potential resource leaks

### Signal/Slot Analysis

Validates signal/slot system usage:

- **Signal emissions**: Checks if signals are properly emitted
- **Connection validity**: Validates signal/slot connections
- **Orphaned signals**: Identifies signals that are never emitted
- **Slot signatures**: Verifies slot procedure signatures

### Code Structure Analysis

Examines overall code organization:

- **Module structure**: Checks module organization and dependencies
- **Documentation**: Validates documentation completeness
- **Use statements**: Optimizes module imports
- **Code organization**: Reviews file and module structure

## Generated Reports

### Report Format

The analysis tool generates detailed text reports:

```
ForGE Qt Code Analysis Report
Generated on: 2025-01-15 14:30:22

Summary:
Total issues found: 15
Errors: 2
Warnings: 8
Info: 5

Qt Style Issues (3):
warning: src/main.f90(25) - Q_OBJECT macro found but class does not extend QObject
info: src/ui.f90(12) - Signal/slot naming does not follow Qt conventions
warning: src/error.f90(45) - Potential error handling issue

Memory Issues (5):
warning: src/memory.f90(18) - Allocate statement without corresponding deallocate
error: src/pointers.f90(22) - Potential unsafe pointer usage
warning: src/objects.f90(30) - QObject initialization without proper cleanup

Signals Issues (2):
info: src/signals.f90(15) - Signal declared but never emitted
warning: src/connections.f90(28) - Signal/slot connection may be invalid

Structure Issues (3):
info: src/modules.f90(1) - Module structure may not follow best practices
info: src/docs.f90(1) - Missing or incomplete documentation
info: src/imports.f90(1) - Use statements may not be optimal
```

### Issue Severity Levels

- **Error**: Critical issues that may cause runtime failures
- **Warning**: Potential problems that should be addressed
- **Info**: Suggestions for code improvement

## Integration with Build Systems

### CMake Integration

```cmake
# Find ForGE
find_package(ForGE REQUIRED)

# Add code analysis target
add_custom_target(analyze
    COMMAND forge_code_analyzer ${CMAKE_SOURCE_DIR}/src/
    COMMENT "Running code analysis"
)

# Run analysis as part of build (optional)
# add_dependencies(all analyze)
```

### FPM Integration

```toml
# fpm.toml
[build]
# Pre-build analysis
prebuild = [
    "forge_code_analyzer src/ -o analysis.txt"
]

[[executable]]
name = "my_app"
sources = ["src/main.f90"]
```

### Make Integration

```makefile
# Makefile
ANALYZER = forge_code_analyzer
SOURCES = src/
REPORT = analysis_report.txt

# Analysis target
analyze: $(REPORT)

$(REPORT): $(wildcard $(SOURCES)*.f90)
	$(ANALYZER) -o $@ $(SOURCES)

# Clean analysis files
clean-analysis:
	rm -f $(REPORT)

.PHONY: analyze clean-analysis
```

### CI/CD Integration

```yaml
# .github/workflows/analysis.yml
name: Code Analysis
on: [push, pull_request]

jobs:
  analyze:
    runs-on: ubuntu-latest
    steps:
    - uses: actions/checkout@v2
    - name: Setup ForGE
      run: |
        # Install ForGE and dependencies
    - name: Run Code Analysis
      run: forge_code_analyzer src/ -o analysis.txt
    - name: Upload Report
      uses: actions/upload-artifact@v2
      with:
        name: analysis-report
        path: analysis.txt
```

## Detailed Check Descriptions

### Qt Style Checks

#### Q_OBJECT Macro Validation

```fortran
! Good: Proper Q_OBJECT usage
type, extends(forge_qobject) :: my_widget
    ! Q_OBJECT
    ! ... signals and slots
end type my_widget

! Bad: Q_OBJECT without extending QObject
type :: my_class
    ! Q_OBJECT  ! Warning: Should extend forge_qobject
end type my_class
```

#### Signal/Slot Naming

```fortran
! Good: Qt-style naming
type(signal_void) :: clicked
type(slot_void_proc) :: on_clicked

! Questionable: Non-standard naming
type(signal_void) :: buttonClicked  ! Should be clicked
type(slot_void_proc) :: handleClick  ! Should be on_clicked
```

### Memory Management Checks

#### Allocation Tracking

```fortran
! Good: Proper allocation/deallocation
type(my_type), allocatable :: obj
allocate(obj)
! ... use obj ...
deallocate(obj)

! Warning: Allocate without deallocate
type(my_type), allocatable :: obj
allocate(obj)  ! Warning: No corresponding deallocate found
```

#### Pointer Safety

```fortran
! Good: Safe pointer usage
type(my_type), pointer :: ptr => null()
if (associated(ptr)) then
    ! Safe to use ptr
end if

! Error: Unsafe pointer usage
type(my_type), pointer :: ptr
! ptr not initialized - error
call use_ptr(ptr)
```

### Signal/Slot Analysis

#### Connection Validation

```fortran
! Good: Proper connection
call button%on_click(handle_click)

! Warning: Potential invalid connection
call button%connect("invalid_slot")  ! Warning: Slot may not exist
```

#### Signal Emission Checks

```fortran
! Info: Signal declared but never emitted
type(signal_void) :: value_changed  ! Info: Never emitted in this file

! Good: Signal properly emitted
call value_changed%emit()
```

## Configuration and Customization

### Analysis Configuration

Create analysis configuration files for project-specific rules:

```ini
# .forge_analysis.ini
[qt_style]
allow_custom_naming = false
require_documentation = true

[memory]
strict_pointer_checks = true
track_allocations = true

[signals]
warn_unused_signals = true
validate_connections = true
```

### Custom Rules

Extend the analyzer with custom rules:

```fortran
! Custom analysis module
module custom_analysis_rules
contains
    subroutine check_custom_patterns(line, line_num, file_path, report)
        ! Implement custom checks
    end subroutine check_custom_patterns
end module custom_analysis_rules
```

## Performance Considerations

### Analysis Speed

- **Incremental analysis**: Only analyzes changed files
- **Parallel processing**: Can analyze multiple files simultaneously
- **Caching**: Remembers previous analysis results

### Large Codebases

For large projects:

```bash
# Analyze specific directories
forge_code_analyzer src/core/ src/ui/

# Exclude certain files
forge_code_analyzer src/ --exclude="test_*" --exclude="*generated*"
```

## Best Practices

### Regular Analysis

1. **Pre-commit hooks**: Run analysis before committing
2. **CI integration**: Include analysis in continuous integration
3. **Code review**: Use analysis results in code reviews

### Issue Resolution

1. **Prioritize errors**: Fix critical errors first
2. **Address warnings**: Resolve warnings systematically
3. **Review info items**: Consider improvement suggestions

### Quality Gates

Set quality thresholds:

```bash
# Fail build if too many issues
forge_code_analyzer src/ -o report.txt
if [ $(grep -c "error:" report.txt) -gt 0 ]; then
    echo "Critical errors found - build failed"
    exit 1
fi
```

## Examples

### Simple Analysis

```bash
$ forge_code_analyzer src/
Processing 12 files...
Analysis complete. Report written to analysis_report.txt

$ cat analysis_report.txt
ForGE Qt Code Analysis Report
Generated on: 2025-01-15 14:30:22

Summary:
Total issues found: 3
Errors: 0
Warnings: 2
Info: 1

Qt Style Issues (2):
warning: src/main.f90(25) - Q_OBJECT macro found but class does not extend QObject
info: src/ui.f90(12) - Signal/slot naming does not follow Qt conventions

Memory Issues (1):
warning: src/memory.f90(18) - Allocate statement without corresponding deallocate
```

### Advanced Analysis

```bash
# Detailed analysis with custom output
forge_code_analyzer -v -o detailed_report.txt \
                   --no-signals src/core/ src/ui/

# Generate XML report for CI tools
forge_code_analyzer --xml-output results.xml src/
```

## Troubleshooting

### Common Issues

**Analysis doesn't run**
- Check that source directory exists and is readable
- Verify forge_code_analyzer is in PATH

**False positives**
- Use configuration files to customize rules
- Report issues to improve the analyzer

**Performance problems**
- Analyze smaller directory subsets
- Use exclude patterns for generated files

### Verbose Mode

Use `-v` for detailed processing information:

```bash
forge_code_analyzer -v src/
Scanning for Fortran files in src/
Found 15 Fortran files
Analyzing file: src/main.f90
Analyzing file: src/ui.f90
...
Analysis complete
```

## See Also

- [Qt Style Guide](../tutorials/qt_style_guide.md)
- [Memory Management](../tutorials/memory_management.md)
- [Signal/Slot System](../tutorials/signals_slots.md)
- [Code Quality Best Practices](../tutorials/best_practices.md)