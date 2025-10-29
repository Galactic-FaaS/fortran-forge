# Migration Guide

This guide helps developers migrate existing applications to ForGE from other GUI frameworks like Qt, GTK, or custom solutions.

## Migrating from Qt

### Key Differences

Qt and ForGE have different design philosophies:

- **Qt**: C++ framework with extensive features, steep learning curve
- **ForGE**: Fortran-native, simpler API, focused on essential features

### Application Structure

**Qt Application:**
```cpp
#include <QApplication>
#include <QMainWindow>

int main(int argc, char *argv[]) {
    QApplication app(argc, argv);
    QMainWindow window;
    window.show();
    return app.exec();
}
```

**ForGE Application:**
```fortran
program my_app
    use forge
    implicit none

    type(forge_application) :: app
    type(forge_window_t) :: window
    type(forge_status) :: status

    call app%init(BACKEND_QT, status)  ! Can use Qt backend
    window = app%create_window("My App", 800, 600)
    call window%show()
    call app%run()
end program my_app
```

### Widget Mapping

| Qt Widget | ForGE Widget | Notes |
|-----------|--------------|-------|
| `QPushButton` | `forge_button` | Direct equivalent |
| `QLabel` | `forge_label` | Direct equivalent |
| `QLineEdit` | `forge_entry` | Single-line text input |
| `QTextEdit` | `forge_text_view` | Multi-line text |
| `QSpinBox` | `forge_spin_button` | Numeric input |
| `QComboBox` | `forge_combo_box` | Drop-down selection |
| `QCheckBox` | `forge_check_box` | Boolean toggle |
| `QRadioButton` | `forge_radio_button` | Exclusive selection |
| `QSlider` | `forge_slider` | Range selection |
| `QProgressBar` | `forge_progress_bar` | Progress display |

### Layout Migration

**Qt Layouts:**
```cpp
QVBoxLayout *layout = new QVBoxLayout;
layout->addWidget(button1);
layout->addWidget(button2);
window->setLayout(layout);
```

**ForGE Layouts:**
```fortran
type(forge_box_layout) :: layout

call layout%set_orientation(LAYOUT_VERTICAL)
call layout%add_widget(button1)
call layout%add_widget(button2)
call layout%set_parent_size(400, 300)
call layout%compute()
```

### Signal/Slot to Event Handling

**Qt Signals/Slots:**
```cpp
connect(button, &QPushButton::clicked, this, &MyClass::handleClick);
```

**ForGE Events:**
```fortran
call button%on_click(handle_click)

subroutine handle_click(event)
    type(forge_event), intent(in) :: event
    ! Handle click
end subroutine handle_click
```

### Common Qt Patterns in ForGE

**Qt Model/View:**
```cpp
QStringListModel *model = new QStringListModel;
model->setStringList(list);
QListView *view = new QListView;
view->setModel(model);
```

**ForGE Equivalent:**
```fortran
! ForGE uses direct widget manipulation
! No built-in model/view framework yet
type(forge_combo_box) :: combo

do i = 1, size(list)
    call combo%add_item(list(i))
end do
```

## Migrating from GTK

### Key Differences

- **GTK**: C-based, extensive widget catalog, complex API
- **ForGE**: Fortran-native, simpler widget set, consistent API

### Application Structure

**GTK Application:**
```c
#include <gtk/gtk.h>

int main(int argc, char *argv[]) {
    gtk_init(&argc, &argv);
    GtkWidget *window = gtk_window_new(GTK_WINDOW_TOPLEVEL);
    gtk_widget_show(window);
    gtk_main();
    return 0;
}
```

**ForGE Application:**
```fortran
program my_app
    use forge
    implicit none

    type(forge_application) :: app
    type(forge_window_t) :: window
    type(forge_status) :: status

    call app%init(BACKEND_GTK4, status)  ! Can use GTK backend
    window = app%create_window("My App", 800, 600)
    call window%show()
    call app%run()
end program my_app
```

### Widget Mapping

| GTK Widget | ForGE Widget | Notes |
|------------|--------------|-------|
| `GtkButton` | `forge_button` | Direct equivalent |
| `GtkLabel` | `forge_label` | Direct equivalent |
| `GtkEntry` | `forge_entry` | Single-line text |
| `GtkTextView` | `forge_text_view` | Multi-line text |
| `GtkSpinButton` | `forge_spin_button` | Numeric input |
| `GtkComboBox` | `forge_combo_box` | Drop-down selection |
| `GtkCheckButton` | `forge_check_box` | Boolean toggle |
| `GtkRadioButton` | `forge_radio_button` | Exclusive selection |
| `GtkScale` | `forge_slider` | Range selection |
| `GtkProgressBar` | `forge_progress_bar` | Progress display |

### Container Migration

**GTK Containers:**
```c
GtkWidget *vbox = gtk_box_new(GTK_ORIENTATION_VERTICAL, 5);
gtk_box_pack_start(GTK_BOX(vbox), button1, TRUE, TRUE, 0);
gtk_box_pack_start(GTK_BOX(vbox), button2, TRUE, TRUE, 0);
gtk_container_add(GTK_CONTAINER(window), vbox);
```

**ForGE Containers:**
```fortran
type(forge_box_layout) :: layout

call layout%set_orientation(LAYOUT_VERTICAL)
call layout%set_spacing(5)
call layout%add_widget(button1)
call layout%add_widget(button2)
```

## Migrating from Custom GUI

### From Win32 API

**Win32 Window Creation:**
```cpp
HWND hwnd = CreateWindowEx(
    0, "MyClass", "Title",
    WS_OVERLAPPEDWINDOW,
    CW_USEDEFAULT, CW_USEDEFAULT, 800, 600,
    NULL, NULL, hInstance, NULL
);
```

**ForGE Window Creation:**
```fortran
type(forge_window_t) :: window
type(forge_status) :: status

call app%init(BACKEND_CUSTOM, status)
window = app%create_window("Title", 800, 600)
call window%show()
```

### From Custom Drawing

**Custom Drawing Code:**
```cpp
case WM_PAINT:
    hdc = BeginPaint(hwnd, &ps);
    // Custom drawing code
    Rectangle(hdc, 10, 10, 100, 100);
    EndPaint(hwnd, &ps);
    break;
```

**ForGE Custom Widgets:**
```fortran
type, extends(forge_widget) :: custom_drawing_widget
contains
    procedure :: draw => custom_drawing_widget_draw
end type custom_drawing_widget

subroutine custom_drawing_widget_draw(this)
    class(custom_drawing_widget), intent(inout) :: this

    ! Custom drawing using ForGE rendering API
    ! (Implementation depends on backend capabilities)
end subroutine custom_drawing_widget_draw
```

## Language-Specific Migration

### From C/C++ GUI Applications

**Memory Management:**
- **C/C++**: Manual memory management with `new`/`delete`
- **Fortran**: Automatic allocation/deallocation, or explicit with `allocate`/`deallocate`

**Object Lifetime:**
```cpp
// C++ - explicit destruction
MyWidget* widget = new MyWidget();
delete widget;
```

```fortran
! Fortran - automatic or explicit
type(forge_button) :: button  ! Automatic
! or
type(forge_button), allocatable :: button
allocate(button)
deallocate(button)
```

### From Other Fortran GUI Libraries

**From FTK (Fortran Tool Kit):**
```fortran
! FTK style
call ftk_init()
window = ftk_window("Title", 800, 600)
call ftk_show(window)
call ftk_run()
```

**To ForGE:**
```fortran
! ForGE style
type(forge_application) :: app
type(forge_window_t) :: window
type(forge_status) :: status

call app%init(BACKEND_CUSTOM, status)
window = app%create_window("Title", 800, 600)
call window%show()
call app%run()
```

## Data Model Migration

### From MVC to ForGE Patterns

**Traditional MVC:**
```
Model <-> Controller <-> View
```

**ForGE Approach:**
```fortran
! Direct data binding
type(person) :: model
type(forge_entry) :: name_field

call bind_text_field(name_field, model, "name")
```

### State Management

**Qt State Management:**
```cpp
// Qt properties/signals
Q_PROPERTY(QString name READ name WRITE setName NOTIFY nameChanged)
```

**ForGE State Management:**
```fortran
! Direct property access with observers
type :: observable_model
    character(len=:), allocatable :: name
contains
    procedure :: set_name => observable_model_set_name
end type observable_model

subroutine observable_model_set_name(this, name)
    class(observable_model), intent(inout) :: this
    character(len=*), intent(in) :: name

    this%name = name
    call notify_observers("name", name)
end subroutine observable_model_set_name
```

## Event System Migration

### From Callback-Based Systems

**C-Style Callbacks:**
```c
void button_callback(GtkWidget *widget, gpointer data) {
    // Handle click
}

g_signal_connect(button, "clicked", G_CALLBACK(button_callback), NULL);
```

**ForGE Events:**
```fortran
call button%on_click(button_callback)

subroutine button_callback(event)
    type(forge_event), intent(in) :: event
    ! Handle click
end subroutine button_callback
```

### From Message-Based Systems

**Windows Messages:**
```cpp
LRESULT CALLBACK WndProc(HWND hwnd, UINT msg, WPARAM wParam, LPARAM lParam) {
    switch (msg) {
        case WM_COMMAND:
            // Handle command
            break;
    }
    return DefWindowProc(hwnd, msg, wParam, lParam);
}
```

**ForGE Events:**
```fortran
subroutine handle_event(event)
    type(forge_event), intent(in) :: event

    select case (event%type)
    case (EVENT_BUTTON_CLICKED)
        ! Handle button click
    case (EVENT_WINDOW_CLOSED)
        ! Handle window close
    end select
end subroutine handle_event
```

## Layout System Migration

### From Absolute Positioning

**Absolute Positioning:**
```cpp
button1->setGeometry(10, 10, 100, 30);
button2->setGeometry(10, 50, 100, 30);
```

**ForGE Layouts:**
```fortran
type(forge_box_layout) :: layout

call layout%set_orientation(LAYOUT_VERTICAL)
call layout%add_widget(button1)
call layout%add_widget(button2)
call layout%set_parent_size(400, 300)
call layout%compute()  ! Automatic positioning
```

### From Complex Layout Hierarchies

**Qt Complex Layouts:**
```cpp
QVBoxLayout *mainLayout = new QVBoxLayout;
QHBoxLayout *buttonLayout = new QHBoxLayout;

buttonLayout->addWidget(okButton);
buttonLayout->addWidget(cancelButton);
mainLayout->addLayout(buttonLayout);
```

**ForGE Nested Layouts:**
```fortran
type(forge_box_layout) :: main_layout, button_layout

call main_layout%set_orientation(LAYOUT_VERTICAL)
call button_layout%set_orientation(LAYOUT_HORIZONTAL)

call button_layout%add_widget(ok_button)
call button_layout%add_widget(cancel_button)
call main_layout%add_widget(button_layout)  ! Nested layout
```

## Configuration and Settings

### From INI/Config Files

**Qt Settings:**
```cpp
QSettings settings("MyCompany", "MyApp");
settings.setValue("window/width", 800);
int width = settings.value("window/width", 600).toInt();
```

**ForGE Configuration:**
```fortran
module app_config
    integer :: window_width = 800
    integer :: window_height = 600
contains
    subroutine load_config()
        ! Load from file/INI
    end subroutine load_config

    subroutine save_config()
        ! Save to file/INI
    end subroutine save_config
end module app_config
```

## Testing Migration

### Unit Testing Migration

**Qt Test:**
```cpp
void TestButton::testClick() {
    QPushButton button("Click me");
    QSignalSpy spy(&button, &QPushButton::clicked);
    QTest::mouseClick(&button, Qt::LeftButton);
    QCOMPARE(spy.count(), 1);
}
```

**ForGE Test:**
```fortran
subroutine test_button_click()
    type(forge_button) :: button
    logical :: clicked = .false.

    call button%set_label("Click me")
    call button%on_click(test_click_handler)

    ! Simulate click (implementation-specific)
    call simulate_button_click(button)

    call assert(clicked, "Button should have been clicked")
end subroutine test_button_click

subroutine test_click_handler(event)
    type(forge_event), intent(in) :: event
    clicked = .true.
end subroutine test_click_handler
```

## Performance Considerations

### Memory Usage

- **Qt/GTK**: Heavy memory footprint, extensive features
- **ForGE**: Lightweight, minimal memory usage
- **Custom Backend**: Very low memory footprint

### Startup Time

- **Qt**: Slow startup due to extensive initialization
- **GTK**: Moderate startup time
- **ForGE Custom**: Fast startup

### Runtime Performance

- **Qt**: Excellent performance with hardware acceleration
- **GTK**: Good performance
- **ForGE**: Good performance, depends on backend

## Common Migration Issues

### API Differences

1. **No Direct Widget Parenting:**
   - Qt/GTK: `parent->addChild(child)`
   - ForGE: Use layouts for widget relationships

2. **No Built-in Model/View:**
   - Qt: Rich model/view framework
   - ForGE: Direct widget manipulation (for now)

3. **Limited Styling Options:**
   - Qt/GTK: Extensive styling/theming
   - ForGE: Basic styling, backend-dependent

### Missing Features

1. **Advanced Widgets:**
   - ForGE has fewer built-in widgets than Qt/GTK
   - Create custom widgets by extending `forge_widget`

2. **Complex Layouts:**
   - ForGE layouts are simpler than Qt's
   - Use nested layouts for complex arrangements

3. **Internationalization:**
   - ForGE has basic i18n support
   - More limited than Qt's comprehensive i18n framework

## Migration Strategy

### Phase 1: Core Application

1. Set up basic ForGE application structure
2. Migrate main window and basic widgets
3. Implement core functionality
4. Test basic operation

### Phase 2: UI Enhancement

1. Migrate complex layouts
2. Add custom widgets where needed
3. Implement advanced event handling
4. Polish UI appearance

### Phase 3: Advanced Features

1. Add data binding and models
2. Implement custom drawing/rendering
3. Add platform-specific features
4. Performance optimization

### Phase 4: Testing and Deployment

1. Comprehensive testing
2. Cross-platform validation
3. Performance benchmarking
4. Deployment preparation

## Tools and Utilities

### Migration Assistant

```fortran
! Conceptual migration helper
module migration_assistant
    implicit none

contains

    subroutine convert_qt_layout(qt_layout, forge_layout)
        ! Convert Qt layout to ForGE layout
        ! This would be a complex conversion utility
    end subroutine convert_qt_layout

    subroutine generate_forge_equivalent(qt_code)
        character(len=*), intent(in) :: qt_code
        character(len=:), allocatable :: forge_code

        ! Convert Qt code snippets to ForGE
        ! Implementation would parse and convert
    end subroutine generate_forge_equivalent

end module migration_assistant
```

### Compatibility Layers

```fortran
! Qt compatibility layer for ForGE
module qt_compatibility
    implicit none

contains

    ! Qt-style API wrappers around ForGE
    subroutine qApp_init()
        use forge
        type(forge_application), save :: app
        type(forge_status) :: status
        call app%init(BACKEND_QT, status)
    end subroutine qApp_init

    function QPushButton(label) result(button)
        character(len=*), intent(in) :: label
        type(forge_button) :: button
        call button%set_label(label)
    end function QPushButton

end module qt_compatibility
```

## Next Steps

- Read the [getting started guide](getting_started.md) for ForGE basics
- Explore the [API documentation](../api/) for detailed method references
- Check the [examples](../../examples/) directory for working code
- Review the [backend guide](backend_guide.md) for platform considerations