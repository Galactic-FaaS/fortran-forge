# Qt Demo README

This example demonstrates the ForGE Qt tooling features and Qt-style programming patterns.

## Overview

The Qt demo showcases ForGE's Qt compatibility layer with:
- Meta-Object Compiler (moc) for signal/slot introspection
- Resource Compiler (rcc) for embedding resources
- UI Compiler (uic) for declarative UI definition
- Translation system for internationalization
- Qt-style QObject inheritance

## Features Demonstrated

### Meta-Object Compiler (moc)
Automatic generation of meta-object code for runtime introspection:

```bash
forge_moc qt_demo.f90 -o moc_qt_demo.f90
```

### Resource Compiler (rcc)
Embedding resources directly into the binary:

```bash
forge_rcc qt_demo.qrc -o qrc_qt_demo.f90
```

### UI Compiler (uic)
Converting declarative .ui files to Fortran widget code:

```bash
forge_uic qt_demo.ui -o ui_qt_demo.f90
```

### Translation System
Internationalization support with .ts/.qm files:

```bash
forge_lupdate . -ts translations/qt_demo_en.ts
forge_lrelease translations/qt_demo_en.ts -qm qt_demo_en.qm
```

## Qt Concepts in ForGE

### QObject Inheritance

```fortran
type, extends(forge_mainwindow) :: main_window
    ! Q_OBJECT  ! Processed by moc
    type(forge_button) :: click_button
    type(signal_void) :: button_clicked_signal
contains
    procedure :: on_button_clicked
end type main_window
```

### Signals and Slots

```fortran
! Define signals
type(signal_void) :: clicked

! Define slots
procedure(on_clicked) :: handle_click

! Connect
call clicked%connect(handle_click)

! Emit
call clicked%emit()
```

### Properties System

```fortran
! Qt-style properties (planned)
call object%set_property("enabled", .true.)
enabled = object%get_property("enabled")
```

## File Structure

```
qt_demo/
├── qt_demo.f90          # Main application
├── qt_demo.qrc          # Resource definition
├── qt_demo.ui           # UI definition
├── icons/               # Embedded icons
│   └── app.png
└── translations/        # Translation files
    ├── qt_demo_en.ts    # Translation source
    └── qt_demo_en.qm    # Compiled translations
```

## Resource File (qt_demo.qrc)

```xml
<!DOCTYPE RCC>
<RCC version="1.0">
<qresource>
    <file>icons/app.png</file>
    <file>styles/main.qss</file>
</qresource>
</RCC>
```

## UI File (qt_demo.ui)

```xml
<?xml version="1.0" encoding="UTF-8"?>
<ui version="4.0">
 <class>MainWindow</class>
 <widget class="QMainWindow" name="MainWindow">
  <property name="windowTitle">
   <string>ForGE Qt Demo</string>
  </property>
  <widget class="QWidget" name="centralwidget">
   <layout class="QVBoxLayout">
    <item>
     <widget class="QPushButton" name="clickButton">
      <property name="text">
       <string>Click Me!</string>
      </property>
     </widget>
    </item>
   </layout>
  </widget>
 </widget>
</ui>
```

## Translation File (qt_demo_en.ts)

```xml
<?xml version="1.0" encoding="utf-8"?>
<!DOCTYPE TS>
<TS version="2.1" language="en_US">
<context>
    <name>MainWindow</name>
    <message>
        <source>Click Me!</source>
        <translation>Click Me!</translation>
    </message>
</context>
</TS>
```

## Building the Example

### Manual Build Process

```bash
# 1. Generate meta-object code
forge_moc qt_demo.f90 -o moc_qt_demo.f90

# 2. Generate resource code
forge_rcc qt_demo.qrc -o qrc_qt_demo.f90

# 3. Generate UI code
forge_uic qt_demo.ui -o ui_qt_demo.f90

# 4. Update translations
forge_lupdate qt_demo.f90 -ts translations/qt_demo_en.ts

# 5. Release translations
forge_lrelease translations/qt_demo_en.ts -qm translations/qt_demo_en.qm

# 6. Compile
gfortran qt_demo.f90 moc_qt_demo.f90 qrc_qt_demo.f90 ui_qt_demo.f90 -lforge_qt -o qt_demo
```

### CMake Integration

```cmake
# CMakeLists.txt
find_package(ForGE REQUIRED)

# Generate Qt files
forge_moc_generate(MOC_SOURCES qt_demo.f90)
forge_rcc_generate(RCC_SOURCES qt_demo.qrc)
forge_uic_generate(UI_HEADERS qt_demo.ui)

add_executable(qt_demo
    qt_demo.f90
    ${MOC_SOURCES}
    ${RCC_SOURCES}
    ${UI_HEADERS}
)

target_link_libraries(qt_demo ForGE::forge_qt)
```

### FPM Integration

```toml
# fpm.toml
[dependencies]
forge = { git = "https://github.com/your-org/fortran-forge.git" }

[build]
# Pre-build commands for Qt tools
prebuild = [
    "forge_moc qt_demo.f90 -o moc_qt_demo.f90",
    "forge_rcc qt_demo.qrc -o qrc_qt_demo.f90",
    "forge_uic qt_demo.ui -o ui_qt_demo.f90",
    "forge_lrelease translations/qt_demo_en.ts -qm translations/qt_demo_en.qm"
]

[[executable]]
name = "qt_demo"
sources = ["qt_demo.f90", "moc_qt_demo.f90", "qrc_qt_demo.f90", "ui_qt_demo.f90"]
```

## Qt Compatibility Features

### Signal/Slot System

ForGE provides Qt-compatible signals and slots:

```fortran
! Signals
type(signal_void) :: void_signal
type(signal_int) :: int_signal
type(signal_string) :: string_signal

! Slots
type(slot_void_proc) :: void_slot
type(slot_int_proc) :: int_slot

! Connection
call signal%connect(slot)
call signal%disconnect(slot)

! Emission
call void_signal%emit()
call int_signal%emit(42)
call string_signal%emit("hello")
```

### QObject Features

```fortran
type, extends(forge_qobject) :: my_object
    ! Q_OBJECT
    character(len=:), allocatable :: object_name
contains
    procedure :: set_object_name
    procedure :: object_name
end type my_object

! Usage
type(my_object) :: obj
call obj%set_object_name("my_widget")
name = obj%object_name()
```

### Property System

```fortran
! Dynamic properties (planned)
call obj%set_property("custom_prop", 42)
value = obj%get_property("custom_prop")

! Static properties with getters/setters
call obj%set_enabled(.true.)
enabled = obj%is_enabled()
```

## Internationalization

### Translation Loading

```fortran
type(forge_translator) :: translator

! Load compiled translation
call translator%load("translations/qt_demo_en.qm")

! Install translator
call app%install_translator(translator)
```

### Translatable Strings

```fortran
! In code
text = tr("Hello, World!")

! In UI files
<string>Hello, World!</string>
```

## Running the Example

```bash
./qt_demo
```

The application displays a window with Qt-style widgets and demonstrates the signal/slot system.

## Key Benefits

1. **Qt Compatibility**: Familiar patterns for Qt developers
2. **Tool Integration**: Full Qt toolchain support
3. **Resource Management**: Embedded assets
4. **Internationalization**: Built-in translation support
5. **Meta-object System**: Runtime introspection capabilities

## Next Steps

- Read the [Qt backend guide](../tutorials/qt_backend_guide.md)
- Learn about [signals and slots](../tutorials/signals_slots.md)
- Explore [resource management](../tutorials/resources.md)
- See the [calculator](../examples/calculator.md) for complex Qt applications