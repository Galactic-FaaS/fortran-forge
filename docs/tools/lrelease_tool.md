# Translation Release Tool (lrelease)

The ForGE Translation Release Tool (lrelease) compiles Qt-style translation source files (.ts) into binary message files (.qm) for runtime translation loading, enabling internationalization in ForGE applications.

## Overview

The lrelease tool processes XML-based translation files and generates compact binary formats optimized for fast runtime loading. This provides:

- Efficient translation storage and access
- Runtime language switching
- Qt Linguist compatibility
- Cross-platform translation support

## Usage

### Basic Usage

```bash
forge_lrelease [options] <input_file.ts>
```

### Command Line Options

- `-qm <file>`: Specify output .qm file (default: `<input>.qm`)
- `-no-compress`: Disable compression (compression enabled by default)
- `-v, --verbose`: Enable verbose output
- `-h, --help`: Display help information

### Examples

```bash
# Basic translation compilation
forge_lrelease app_en.ts

# Custom output file
forge_lrelease -qm translations/app_en.qm app_en.ts

# Disable compression
forge_lrelease -no-compress app_en.ts

# Verbose output
forge_lrelease -v app_en.ts
```

## Input File Format

The lrelease tool processes Qt Linguist .ts (translation source) files containing XML translation data.

### Basic TS File Structure

```xml
<?xml version="1.0" encoding="utf-8"?>
<!DOCTYPE TS>
<TS version="2.1" language="en_US">
<context>
    <name>MainWindow</name>
    <message>
        <source>Hello World</source>
        <translation>Hello World</translation>
    </message>
    <message>
        <source>Click me</source>
        <translation>Click me</translation>
    </message>
</context>
</TS>
```

### Translation Elements

- **`<TS>`**: Root element with language attribute
- **`<context>`**: Grouping of related messages
- **`<message>`**: Individual translation entry
- **`<source>`**: Original (source language) text
- **`<translation>`**: Translated text
- **`<location>`**: Optional source file location information

### Advanced TS Features

```xml
<?xml version="1.0" encoding="utf-8"?>
<!DOCTYPE TS>
<TS version="2.1" language="es_ES">
<context>
    <name>MainWindow</name>
    <message>
        <location filename="mainwindow.ui" line="12"/>
        <source>&File</source>
        <translation>&Archivo</translation>
    </message>
    <message>
        <location filename="mainwindow.ui" line="15"/>
        <source>&Edit</source>
        <translation>&Editar</translation>
    </message>
    <message>
        <source>%1 of %2</source>
        <translation>%1 de %2</translation>
        <numerusform>%1 of %2</numerusform>
    </message>
</context>
</TS>
```

## Generated Output

The lrelease tool generates Qt-compatible .qm (Qt message) binary files containing:

### QM File Structure

```
Magic Number (4 bytes): 0x950412DE
Version (4 bytes): 1
Message Count (4 bytes): N
Message 1: Context + Source + Translation
Message 2: Context + Source + Translation
...
Message N: Context + Source + Translation
```

### Binary Format Details

- **Big-endian byte order** for cross-platform compatibility
- **UTF-8 encoded strings** for Unicode support
- **Compressed data** (when enabled) for smaller file sizes
- **Indexed access** for fast translation lookups

## Using Translations at Runtime

### Loading Translation Files

```fortran
program localized_app
    use forge_translator

    type(forge_translator) :: translator

    ! Load translation file
    call translator%load("translations/app_es.qm")

    ! Install translator
    call app%install_translator(translator)

    ! Use translated strings
    title = tr("Hello World")
    button_text = tr("Click me")

end program localized_app
```

### Translation Function

```fortran
! Translation function (tr)
function tr(source, context) result(translation)
    character(len=*), intent(in) :: source
    character(len=*), intent(in), optional :: context
    character(len=:), allocatable :: translation

    ! Look up translation in loaded translation files
    translation = lookup_translation(source, context)

    ! Fall back to source text if no translation found
    if (len(translation) == 0) then
        translation = source
    end if
end function tr
```

### Language Switching

```fortran
program multi_language_app
    use forge_translator

    type(forge_translator) :: english_trans, spanish_trans

    ! Load multiple translations
    call english_trans%load("app_en.qm")
    call spanish_trans%load("app_es.qm")

    ! Switch languages at runtime
    if (user_selected_spanish()) then
        call app%install_translator(spanish_trans)
    else
        call app%install_translator(english_trans)
    end if

    ! UI updates automatically
    call refresh_ui()

end program multi_language_app
```

## Integration with Build Systems

### CMake Integration

```cmake
# Find ForGE
find_package(ForGE REQUIRED)

# Generate QM files from TS files
forge_lrelease_generate(QM_FILES
    translations/app_en.ts
    translations/app_es.ts
    translations/app_fr.ts
)

# Add to application
add_executable(my_app main.f90 ${QM_FILES})
target_link_libraries(my_app ForGE::forge)
```

### FPM Integration

```toml
# fpm.toml
[build]
# Pre-build commands
prebuild = [
    "forge_lrelease translations/app_en.ts -qm app_en.qm",
    "forge_lrelease translations/app_es.ts -qm app_es.qm"
]

[[executable]]
name = "my_app"
sources = ["main.f90"]
```

### Make Integration

```makefile
# Makefile
LRELEASE = forge_lrelease
FORTRAN = gfortran

# Generate QM files
%.qm: %.ts
	$(LRELEASE) $< -qm $@

# Translation files
QM_FILES = app_en.qm app_es.qm app_fr.qm

# Build application
my_app: main.f90 $(QM_FILES)
	$(FORTRAN) $^ -o $@ -lforge
```

## Qt Linguist Workflow

### Creating Translation Files

1. **Mark translatable strings in code**
   ```fortran
   title = tr("Application Title")
   error_msg = tr("File not found: %1", filename)
   ```

2. **Generate/update TS files**
   ```bash
   forge_lupdate source_files -ts translations/app_en.ts
   ```

3. **Edit translations with Qt Linguist**
   ```
   linguist translations/app_en.ts
   ```

4. **Generate QM files**
   ```bash
   forge_lrelease translations/app_en.ts
   ```

### Translation Context

```fortran
! Different contexts for same source text
menu_text = tr("Open", "menu")
button_text = tr("Open", "button")
tooltip_text = tr("Open", "tooltip")
```

## Performance Considerations

### File Size Optimization

- **Compression**: Reduces QM file sizes by ~50%
- **Duplicate elimination**: Shared translations stored once
- **Efficient encoding**: UTF-8 with length prefixes

### Runtime Performance

- **Fast lookups**: Hash-based translation indexing
- **Memory efficient**: Translations loaded on demand
- **Minimal overhead**: Fallback to source text when no translation

### Loading Strategies

```fortran
! Lazy loading
subroutine load_language(language_code)
    character(len=*), intent(in) :: language_code

    select case (language_code)
    case ('en')
        call load_translation("app_en.qm")
    case ('es')
        call load_translation("app_es.qm")
    case ('fr')
        call load_translation("app_fr.qm")
    end select
end subroutine load_language

! Preloading for performance
subroutine preload_translations()
    call load_translation("app_en.qm")  ! Default/fallback
    ! Load other languages in background
end subroutine preload_translations
```

## Error Handling

### Common Errors

1. **Invalid TS file syntax**
   ```
   Error: Unable to parse TS file - invalid XML
   Solution: Validate XML syntax and structure
   ```

2. **Missing translations**
   ```
   Warning: Empty translation for "Hello World"
   Solution: Provide translation text or mark as unfinished
   ```

3. **Encoding issues**
   ```
   Error: Invalid UTF-8 sequence
   Solution: Ensure proper encoding in TS files
   ```

### Verbose Mode

Use `-v` for detailed processing information:

```bash
forge_lrelease -v app_en.ts
```

Shows:
- Files being processed
- Messages parsed and translated
- Compression ratios
- Generated file statistics

## Best Practices

### Translation Management

1. **Consistent source strings**: Use identical strings for same concepts
2. **Context information**: Provide context for ambiguous strings
3. **Parameter placeholders**: Use %1, %2, etc. for variable content
4. **Regular updates**: Keep translations current with source changes

### Build System Integration

1. **Dependency tracking**: Rebuild QM files when TS files change
2. **Clean builds**: Remove generated files on clean
3. **Version control**: Track TS files, not generated QM files
4. **Testing**: Validate translations load correctly

### Runtime Usage

1. **Graceful fallback**: Always provide source text fallback
2. **Dynamic switching**: Allow runtime language changes
3. **Resource management**: Load/unload translations as needed
4. **Error handling**: Handle missing translation files

## Examples

### Simple Application Translation

```xml
<!-- translations/app_en.ts -->
<?xml version="1.0" encoding="utf-8"?>
<!DOCTYPE TS>
<TS version="2.1" language="en_US">
<context>
    <name>MainWindow</name>
    <message>
        <source>Hello World</source>
        <translation>Hello World</translation>
    </message>
    <message>
        <source>Click me</source>
        <translation>Click me</translation>
    </message>
</context>
</TS>
```

```xml
<!-- translations/app_es.ts -->
<?xml version="1.0" encoding="utf-8"?>
<!DOCTYPE TS>
<TS version="2.1" language="es_ES">
<context>
    <name>MainWindow</name>
    <message>
        <source>Hello World</source>
        <translation>Hola Mundo</translation>
    </message>
    <message>
        <source>Click me</source>
        <translation>Haz clic</translation>
    </message>
</context>
</TS>
```

```fortran
program translated_app
    use forge_translator

    type(forge_translator) :: translator

    ! Load appropriate translation
    select case (get_system_language())
    case ('es', 'es_ES')
        call translator%load("app_es.qm")
    case default
        call translator%load("app_en.qm")
    end select

    call app%install_translator(translator)

    ! UI automatically uses translated strings
    call create_ui()

end program translated_app
```

### Plural Forms

```xml
<!-- Handling plural forms -->
<message>
    <source>%n file(s)</source>
    <translation>
        <numerusform>%n file</numerusform>
        <numerusform>%n files</numerusform>
    </translation>
</message>
```

```fortran
! Usage in code
file_count = get_file_count()
message = tr("%n file(s)", "", file_count)
```

## Troubleshooting

### Build Issues

**Problem**: QM files not generated
**Solution**: Check TS file syntax and lrelease installation

**Problem**: Translations not loading
**Solution**: Verify QM file paths and format

**Problem**: Encoding problems
**Solution**: Ensure consistent UTF-8 encoding

### Runtime Issues

**Problem**: Wrong language displayed
**Solution**: Check translator installation order

**Problem**: Missing translations
**Solution**: Verify TS files are up-to-date and compiled

**Problem**: Performance issues
**Solution**: Profile translation lookups and optimize

## See Also

- [Qt Linguist Manual](https://doc.qt.io/qt-5/linguist-manual.html)
- [lupdate Tool](lupdate_tool.md) - Translation source generator
- [MOC Tool](moc_tool.md) - Meta-object compiler
- [Internationalization Tutorial](../tutorials/internationalization.md)