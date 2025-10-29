# Widget Examples

This section contains comprehensive examples of ForGE Qt's widget library, showcasing advanced widget usage patterns and complex user interface components.

## Container Widgets

### Group Box

**File:** `examples/widget_groupbox/widget_groupbox.f90`

Demonstrates:
- Logical grouping of related controls
- Group box titles and borders
- Checkable group boxes
- Enabling/disabling grouped controls

```fortran
type(forge_group_box) :: group1
call group1%set_title("Display Options")
call group1%set_checkable(.true.)
```

### Tab Widget

**File:** `examples/widget_tabwidget/widget_tabwidget.f90`

Demonstrates:
- Multi-page interfaces
- Tab navigation
- Dynamic tab management
- Closable and movable tabs

### Stacked Widget

**File:** `examples/widget_stackedwidget/widget_stackedwidget.f90`

Demonstrates:
- Layered content display
- Programmatic page switching
- Navigation controls

## Item View Widgets

### List View

**File:** `examples/widget_listview/widget_listview.f90`

Demonstrates:
- Item lists with icons and text
- Selection modes (single, multi, extended)
- Sorting and filtering
- Dynamic item management

### Table Widget

**File:** `examples/widget_tablewidget/widget_tablewidget.f90`

Demonstrates:
- Tabular data display
- Column headers and sorting
- Cell editing
- Row and column operations

## Menu and Toolbar Widgets

### Menu Bar

**File:** `examples/widget_menubar/widget_menubar.f90`

Demonstrates:
- Application menu systems
- Keyboard shortcuts
- Menu hierarchies
- Action management

## Feedback Widgets

### Progress Bar

**File:** `examples/widget_progressbar/widget_progressbar.f90`

Demonstrates:
- Operation progress indication
- Determinate and indeterminate modes
- Custom formatting
- Progress updates

## Dialog Widgets

### Message Box

**File:** `examples/widget_messagebox/widget_messagebox.f90`

Demonstrates:
- User notifications
- Different message types (info, warning, error, question)
- Standard button configurations
- Modal dialogs

### File Dialog

**File:** `examples/widget_filedialog/widget_filedialog.f90`

Demonstrates:
- File selection dialogs
- Directory browsing
- File type filtering
- Multiple file selection

## Layout Widgets

### Scroll Area

**File:** `examples/widget_scrollarea/widget_scrollarea.f90`

Demonstrates:
- Scrollable content areas
- Automatic scrollbar management
- Viewport control

### Splitter

**File:** `examples/widget_splitter/widget_splitter.f90`

Demonstrates:
- Resizable panel layouts
- Horizontal and vertical splitting
- Proportional sizing

## Advanced Widget Concepts

### Widget Communication

Widgets in ForGE Qt communicate through:
- **Signals and Slots:** Event-driven programming
- **Properties:** Declarative state management
- **Parent-Child Relationships:** Automatic cleanup and layout

### Widget Lifecycle

```fortran
! Create
type(forge_button) :: button
call button%set_text("Click me")

! Configure
call button%on_clicked(my_handler)

! Use in layout
call layout%add_widget(button)

! Cleanup (automatic when parent destroyed)
```

### Custom Widget Creation

ForGE Qt supports custom widget development through:
- Inheritance from base widget classes
- Event handling overrides
- Custom painting and rendering

## Running Widget Examples

```bash
# Build and run a specific widget example
cmake --build build --target widget_groupbox
./build/examples/widget_groupbox/widget_groupbox

# Or build all examples
cmake --build build --target all
```

## Widget Categories

| Category | Examples | Purpose |
|----------|----------|---------|
| **Containers** | Group Box, Tab Widget, Stacked Widget | Organizing interface elements |
| **Item Views** | List View, Table Widget | Data presentation |
| **Menus** | Menu Bar | Application commands |
| **Feedback** | Progress Bar | User feedback |
| **Dialogs** | Message Box, File Dialog | User interaction |
| **Layout** | Scroll Area, Splitter | Content organization |

## Best Practices

1. **Consistent Naming:** Use descriptive names for widgets
2. **Proper Parenting:** Set appropriate parent-child relationships
3. **Event Handling:** Connect signals to appropriate handlers
4. **Resource Management:** Let ForGE handle widget cleanup
5. **Accessibility:** Provide meaningful labels and descriptions

These examples demonstrate ForGE Qt's comprehensive widget library and provide patterns for building sophisticated user interfaces.