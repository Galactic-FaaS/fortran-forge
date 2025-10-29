# Translation Update Tool (lupdate)

The ForGE Translation Update Tool (lupdate) scans Fortran source code for translatable strings and generates or updates Qt-style translation source files (.ts), enabling systematic internationalization workflow.

## Overview

The lupdate tool automates the extraction of user-visible strings from Fortran applications and creates translation files that can be edited with Qt Linguist or other translation tools. This provides:

- Automated string extraction from source code
- Qt Linguist compatibility
- Context-aware translations
- Source location tracking
- Incremental translation file updates

## Usage

### Basic Usage

```bash
forge_lupdate [options] <source_directory>
```

### Command Line Options

- `-ts <file>`: Translation source file (.ts) to create/update (required)
- `-target-language <lang>`: Target language code (e.g., 'en', 'fr')
- `-no-recursive`: Don't scan subdirectories
- `-v, --verbose`: Enable verbose output
- `-h, --help`: Display help information

### Examples

```bash
# Basic usage
forge_lupdate -ts translations/app_en.ts src/

# Specify target language
forge_lupdate -ts translations/app_fr.ts -target-language fr src/

# Non-recursive scan
forge_lupdate -no-recursive -ts translations/app_en.ts src/

# Verbose output
forge_lupdate -v -ts translations/app_en.ts src/
```

## String Extraction

### Translation Function Recognition

lupdate recognizes `tr()` function calls in Fortran source code:

```fortran
! Simple translation
title = tr("Hello World")

! Translation with context
button_text = tr("menu", "Open File")

! Plural forms (future)
file_count = tr("%n file(s)", "", file_count)
```

### Supported File Types

- `.f90` - Free-form Fortran files
- `.f` - Fixed-form Fortran files
- `.for` - Fortran files

### Source Code Patterns

```fortran
program my_app
    use forge_translator

    ! UI strings
    call set_window_title(tr("My Application"))
    call add_button(tr("Save"), save_callback)
    call add_menu_item(tr("File"), tr("Exit"))

    ! Error messages
    error_msg = tr("File not found: %1", filename)

    ! Status messages
    status = tr("Processing %1 of %2", current, total)

end program my_app
```

## Generated Output

### TS File Structure

lupdate generates Qt-compatible .ts files:

```xml
<?xml version="1.0" encoding="utf-8"?>
<!DOCTYPE TS>
<TS version="2.1" language="en_US">
    <context>
        <name>ForGE Application</name>
        <message>
            <location filename="src/main.f90" line="12"/>
            <source>Hello World</source>
            <translation></translation>
        </message>
        <message>
            <location filename="src/ui.f90" line="25"/>
            <source>Save</source>
            <translation></translation>
        </message>
    </context>
</TS>
```

### Context Grouping

Strings are grouped by context when specified:

```fortran
! Different contexts for same English text
menu_open = tr("Open", "menu")
dialog_open = tr("Open", "dialog")
```

Generates separate entries with context information.

### Location Tracking

Each translatable string includes source file and line number:

```xml
<location filename="src/main.f90" line="15"/>
```

This helps translators understand string usage context.

## Integration with Build Systems

### CMake Integration

```cmake
# Generate TS files
forge_lupdate_generate(TS_FILES
    SOURCE_DIRS src/
    TS_FILES translations/app_en.ts translations/app_fr.ts
)

# Update translations when source changes
add_custom_target(update_translations
    COMMAND forge_lupdate -ts translations/app_en.ts src/
    COMMAND forge_lupdate -ts translations/app_fr.ts -target-language fr src/
    COMMENT "Updating translation files"
)
```

### FPM Integration

```toml
# fpm.toml
[build]
# Pre-build commands for translation updates
prebuild = [
    "forge_lupdate -ts translations/app_en.ts src/",
    "forge_lupdate -ts translations/app_es.ts -target-language es src/"
]

[[executable]]
name = "my_app"
sources = ["src/main.f90"]
```

### Make Integration

```makefile
# Makefile
LUPDATE = forge_lupdate
SOURCES = src/

# Translation files
translations/app_en.ts: $(wildcard $(SOURCES)*.f90)
	$(LUPDATE) -ts $@ $(SOURCES)

translations/app_es.ts: $(wildcard $(SOURCES)*.f90)
	$(LUPDATE) -target-language es -ts $@ $(SOURCES)

# Update all translations
update_translations: translations/app_en.ts translations/app_es.ts

.PHONY: update_translations
```

## Qt Linguist Workflow

### Complete Translation Workflow

1. **Mark strings in code**
   ```fortran
   title = tr("Application Title")
   ```

2. **Extract strings**
   ```bash
   forge_lupdate -ts translations/app_en.ts src/
   ```

3. **Edit translations**
   ```bash
   linguist translations/app_en.ts
   ```

4. **Generate binary files**
   ```bash
   forge_lrelease translations/app_en.ts
   ```

5. **Use in application**
   ```fortran
   call translator%load("app_en.qm")
   ```

### Qt Linguist Features

- **Graphical translation interface**
- **Translation memory**
- **Validation tools**
- **Batch translation**
- **Collaboration support**

## Advanced Features

### Context-Aware Translations

```fortran
! Same English text, different contexts
file_menu = tr("Open", "File menu")
dialog_button = tr("Open", "Open file dialog")

! Generates separate entries for translation
```

### Plural Forms

```fortran
! Plural-aware translations (future)
count_text = tr("%n file(s)", "", file_count)

! Qt Linguist handles plural forms automatically
```

### Parameter Substitution

```fortran
! Parameterized strings
message = tr("Processing file: %1", filename)
progress = tr("Step %1 of %2", current_step, total_steps)
```

### Comments for Translators

```fortran
! Comments are extracted (future feature)
! TRANSLATORS: This is the main application title
title = tr("My Application")
```

## Performance Considerations

### Scanning Performance

- **Fast file scanning**: Optimized for large codebases
- **Incremental updates**: Only processes changed files
- **Memory efficient**: Streams large files

### Build Integration

- **Minimal build impact**: Fast execution
- **Dependency tracking**: Only runs when needed
- **Parallel execution**: Can process multiple directories

## Error Handling

### Common Issues

1. **Missing tr() calls**
   ```
   Solution: Ensure all user-visible strings use tr() function
   ```

2. **Context conflicts**
   ```
   Solution: Use different contexts for same text in different situations
   ```

3. **File encoding issues**
   ```
   Solution: Ensure source files are UTF-8 encoded
   ```

### Verbose Mode

Use `-v` for detailed processing information:

```bash
forge_lupdate -v -ts translations/app_en.ts src/
```

Shows:
- Files being scanned
- Strings extracted
- Contexts identified
- Locations tracked

## Best Practices

### Code Organization

1. **Consistent tr() usage**: All user strings should use tr()
2. **Meaningful contexts**: Use descriptive context strings
3. **Avoid concatenation**: Don't concatenate translated strings
4. **Parameter placeholders**: Use %1, %2 for variable content

### Translation Management

1. **Regular updates**: Update TS files when strings change
2. **Version control**: Track TS files in version control
3. **Translation reviews**: Have translations reviewed by native speakers
4. **Fallback handling**: Always provide source language fallback

### Build System Integration

1. **Automated updates**: Integrate into build process
2. **Dependency tracking**: Rebuild when source changes
3. **Clean separation**: Keep translations separate from code
4. **Testing**: Validate translations load correctly

## Examples

### Simple Application

```fortran
! src/main.f90
program simple_app
    use forge_translator

    ! Mark strings for translation
    call set_title(tr("Simple Application"))
    call add_button(tr("Click Me"), click_handler)
    call show_message(tr("Application started"))

contains

    subroutine click_handler(event)
        type(forge_event), intent(in) :: event
        call show_message(tr("Button clicked!"))
    end subroutine click_handler

end program simple_app
```

Run lupdate:

```bash
forge_lupdate -ts translations/simple_en.ts src/
```

Generated TS file:

```xml
<?xml version="1.0" encoding="utf-8"?>
<!DOCTYPE TS>
<TS version="2.1" language="en">
    <context>
        <name>ForGE Application</name>
        <message>
            <location filename="src/main.f90" line="5"/>
            <source>Simple Application</source>
            <translation></translation>
        </message>
        <message>
            <location filename="src/main.f90" line="6"/>
            <source>Click Me</source>
            <translation></translation>
        </message>
        <message>
            <location filename="src/main.f90" line="7"/>
            <source>Application started</source>
            <translation></translation>
        </message>
        <message>
            <location filename="src/main.f90" line="12"/>
            <source>Button clicked!</source>
            <translation></translation>
        </message>
    </context>
</TS>
```

### Complex Application with Contexts

```fortran
! src/ui.f90
module ui_module
    use forge_translator

    ! Menu items with context
    file_menu = tr("File", "menu")
    open_item = tr("Open", "menu_item")
    save_item = tr("Save", "menu_item")

    ! Dialog buttons with context
    ok_button = tr("OK", "dialog_button")
    cancel_button = tr("Cancel", "dialog_button")

    ! Status messages
    status_ready = tr("Ready", "status")
    status_busy = tr("Processing...", "status")

end module ui_module
```

## Troubleshooting

### Build Issues

**Problem**: Strings not extracted
**Solution**: Check that tr() calls are properly formatted

**Problem**: Context not preserved
**Solution**: Ensure context parameter is provided correctly

**Problem**: File scanning fails
**Solution**: Check directory permissions and paths

### Translation Issues

**Problem**: Wrong context used
**Solution**: Review context parameters in tr() calls

**Problem**: Missing locations
**Solution**: Ensure source files are accessible during scanning

**Problem**: Encoding problems
**Solution**: Use UTF-8 encoding for all files

## See Also

- [Qt Linguist Manual](https://doc.qt.io/qt-5/linguist-manual.html)
- [lrelease Tool](lrelease_tool.md) - Translation compiler
- [MOC Tool](moc_tool.md) - Meta-object compiler
- [Internationalization Tutorial](../tutorials/internationalization.md)