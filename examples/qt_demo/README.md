# Qt Demo Example

This example demonstrates the ForGE Qt tooling features:

## Features Demonstrated

- **Meta-Object Compiler (moc)**: Automatic signal/slot introspection
- **Resource Compiler (rcc)**: Embedding resources into the binary
- **UI Compiler (uic)**: Generating widget code from .ui files
- **Translation System**: Internationalization support with .ts/.qm files
- **Qt-style QObject**: Signals, slots, and properties

## Files

- `qt_demo.f90` - Main application with Qt-style QObject
- `qt_demo.qrc` - Resource file (processed by rcc)
- `qt_demo.ui` - UI definition file (processed by uic)
- `translations/qt_demo_en.ts` - Translation source file (processed by lupdate/lrelease)
- `icons/` - Directory for embedded icons
- `translations/` - Directory for compiled translations

## Building with Qt Tooling

### Using CMake

```bash
# Generate meta-object code
forge_moc qt_demo.f90 -o moc_qt_demo.f90

# Generate resource code
forge_rcc qt_demo.qrc -o qrc_qt_demo.f90

# Generate UI code
forge_uic qt_demo.ui -o ui_qt_demo.f90

# Update translations
forge_lupdate . -ts translations/qt_demo_en.ts

# Release translations
forge_lrelease translations/qt_demo_en.ts -qm qt_demo_en.qm

# Build the application
cmake ..
make
```

### Using FPM

```bash
# Pre-build step (would be automatic in real build)
forge_moc qt_demo.f90 -o moc_qt_demo.f90
forge_rcc qt_demo.qrc -o qrc_qt_demo.f90
forge_uic qt_demo.ui -o ui_qt_demo.f90
forge_lrelease translations/qt_demo_en.ts -qm qt_demo_en.qm

# Build
fpm build
```

## Qt Concepts Demonstrated

1. **QObject Inheritance**: Classes extending forge_qobject
2. **Signals and Slots**: Type-safe event system
3. **Properties**: Dynamic properties system
4. **Meta-object System**: Runtime introspection
5. **Resources**: Embedded binary resources
6. **UI Compilation**: Declarative UI definition
7. **Internationalization**: Translation support

## Running the Example

```bash
./qt_demo
```

The application shows a simple window with a button, label, and checkbox demonstrating the Qt-style features.