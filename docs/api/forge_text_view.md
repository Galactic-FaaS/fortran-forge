# forge_text_view

Multi-line text display and editing widget for ForGE applications.

## Synopsis

```fortran
type, extends(forge_widget) :: forge_text_view
    private
    type(forge_string) :: text
    logical :: editable = .true.
    logical :: word_wrap = .true.
    type(forge_event_handler) :: change_handler
contains
    procedure :: set_text => forge_text_view_set_text
    procedure :: get_text => forge_text_view_get_text
    procedure :: set_editable => forge_text_view_set_editable
    procedure :: set_word_wrap => forge_text_view_set_word_wrap
    procedure :: on_change => forge_text_view_on_change
end type forge_text_view
```

## Description

The `forge_text_view` widget provides multi-line text display and editing capabilities. It supports:

- Multi-line text display
- Text editing (when enabled)
- Word wrapping
- Scrollable content
- Text selection and manipulation
- Change event notifications

Text views are ideal for:
- Document editing
- Log display
- Code editing
- Form text areas
- README or help display

## Properties

- `text`: The text content
- `editable`: Whether text can be modified
- `word_wrap`: Whether text wraps at widget boundary
- `change_handler`: Event handler for text changes

## Methods

### set_text(text)

Sets the text content of the text view.

**Parameters:**
- `text` (character): Text to display

**Example:**
```fortran
type(forge_text_view) :: text_view

call text_view%set_text("This is line 1\nThis is line 2\nThis is line 3")
```

### get_text()

Gets the current text content.

**Returns:** character - Current text

**Example:**
```fortran
character(len=:), allocatable :: content

content = text_view%get_text()
print *, "Text view contains: ", content
```

### set_editable(editable)

Sets whether the text can be edited by the user.

**Parameters:**
- `editable` (logical): True for editable, false for read-only

**Example:**
```fortran
! Read-only text display
call text_view%set_editable(.false.)

! Editable text area
call text_view%set_editable(.true.)
```

### set_word_wrap(word_wrap)

Sets whether text should wrap at the widget boundary.

**Parameters:**
- `word_wrap` (logical): True to enable word wrapping

**Example:**
```fortran
! Enable word wrapping for better readability
call text_view%set_word_wrap(.true.)
```

### on_change(callback)

Registers a callback for text change events.

**Parameters:**
- `callback` (procedure): Event callback subroutine

**Example:**
```fortran
call text_view%on_change(handle_text_change)

subroutine handle_text_change(event)
    type(forge_event), intent(in) :: event
    ! Text was modified
    call update_word_count()
    call save_draft()
end subroutine handle_text_change
```

## Text Operations

### Multi-line Text
```fortran
! Set multi-line content
call text_view%set_text("Line 1" // new_line("a") // &
                       "Line 2" // new_line("a") // &
                       "Line 3")
```

### Large Documents
```fortran
! Load file content
call text_view%set_text(read_file_content("document.txt"))
```

### Dynamic Updates
```fortran
! Append text
character(len=:), allocatable :: current_text

current_text = text_view%get_text()
call text_view%set_text(current_text // new_line("a") // "New line")
```

## Display Modes

### Read-only Display
```fortran
type(forge_text_view) :: log_view

call log_view%set_editable(.false.)
call log_view%set_word_wrap(.true.)
call log_view%set_text(get_application_log())
```

### Code Editor
```fortran
type(forge_text_view) :: code_editor

call code_editor%set_editable(.true.)
call code_editor%set_word_wrap(.false.)  ! Code typically doesn't wrap
call code_editor%set_text("program hello\n    print *, 'Hello'\nend program")
```

### Form Text Area
```fortran
type(forge_text_view) :: comments_field

call comments_field%set_editable(.true.)
call comments_field%set_word_wrap(.true.)
call comments_field%set_placeholder("Enter your comments here...")
```

## Events

Text views generate `EVENT_TEXT_CHANGED` events:

```fortran
subroutine text_modified(event)
    type(forge_event), intent(in) :: event

    select case (event%type)
    case (EVENT_TEXT_CHANGED)
        ! Text content was modified
        call validate_content()
        call update_save_status()
    end select
end subroutine text_modified
```

## Size and Layout

Text views work well with layout managers:

```fortran
! Expand to fill available space
call text_view%set_size_policy(QSizePolicy_Expanding, QSizePolicy_Expanding)

! Fixed size for specific content
call text_view%set_size_hint(400, 300)
```

## Scrolling

Text views automatically provide scrolling for large content:

- Vertical scrolling for long documents
- Horizontal scrolling when word wrap is disabled
- Scrollbar visibility depends on backend

## Text Selection

Text views support text selection operations:

- Mouse selection
- Keyboard selection (Shift+Arrow keys)
- Copy/paste operations
- Find and replace (backend dependent)

## Performance

Text views handle large documents efficiently:

- Virtual scrolling for very large files
- Incremental updates
- Memory management for large content
- Fast rendering of visible text only

## Word Wrapping

Word wrapping behavior:

- **Enabled**: Text wraps at widget boundary
- **Disabled**: Horizontal scrolling for long lines
- Automatic re-wrapping on widget resize

## Encoding

Text views handle text encoding:

- UTF-8 support (backend dependent)
- Platform-specific encoding handling
- Proper display of international characters

## Thread Safety

Text view operations are not thread-safe. Text updates should occur on the main GUI thread.

## Example Usage

```fortran
program text_view_demo
    use forge
    implicit none

    type(forge_application) :: app
    type(forge_window_t) :: window
    type(forge_text_view) :: editor, log_view
    type(forge_box_layout) :: layout
    type(forge_status) :: status

    ! Initialize
    call app%init(BACKEND_CUSTOM, status)
    window = app%create_window("Text View Demo", 600, 400)

    ! Create editable text editor
    call editor%set_editable(.true.)
    call editor%set_word_wrap(.true.)
    call editor%set_text("Welcome to the text editor!\n\nStart typing...")
    call editor%on_change(on_text_changed)

    ! Create read-only log view
    call log_view%set_editable(.false.)
    call log_view%set_word_wrap(.true.)
    call log_view%set_text("Application started at " // get_timestamp())

    ! Layout widgets
    call layout%set_orientation(LAYOUT_VERTICAL)
    call layout%add_widget(editor)
    call layout%add_widget(log_view)

    ! Show window
    call window%show()
    call app%run()

contains

    subroutine on_text_changed(event)
        type(forge_event), intent(in) :: event
        character(len=:), allocatable :: text
        integer :: line_count

        text = editor%get_text()
        line_count = count_lines(text)

        call log_view%set_text("Text changed. Lines: " // int_to_string(line_count))
    end subroutine on_text_changed

    function count_lines(text) result(count)
        character(len=*), intent(in) :: text
        integer :: count, i

        count = 1
        do i = 1, len(text)
            if (text(i:i) == new_line("a")) count = count + 1
        end do
    end function count_lines

end program text_view_demo
```

## Advanced Usage

### Syntax Highlighting
```fortran
subroutine apply_syntax_highlighting(text_view, code)
    type(forge_text_view), intent(inout) :: text_view
    character(len=*), intent(in) :: code

    ! Apply syntax highlighting (backend-specific)
    call text_view%set_highlighted_text(code, "fortran")
end subroutine apply_syntax_highlighting
```

### Auto-save
```fortran
subroutine setup_auto_save(text_view, filename)
    type(forge_text_view), intent(inout) :: text_view
    character(len=*), intent(in) :: filename

    call text_view%on_change(auto_save_callback)

contains

    subroutine auto_save_callback(event)
        character(len=:), allocatable :: content

        content = text_view%get_text()
        call save_to_file(filename, content)
    end subroutine auto_save_callback

end subroutine setup_auto_save
```

### Find and Replace
```fortran
subroutine find_and_replace(text_view, find_text, replace_text)
    type(forge_text_view), intent(inout) :: text_view
    character(len=*), intent(in) :: find_text, replace_text

    character(len=:), allocatable :: content, new_content

    content = text_view%get_text()
    new_content = replace_all(content, find_text, replace_text)
    call text_view%set_text(new_content)
end subroutine find_and_replace
```

### Line Numbers
```fortran
subroutine enable_line_numbers(text_view)
    type(forge_text_view), intent(inout) :: text_view

    ! Enable line number display (backend-specific)
    call text_view%set_line_numbers_visible(.true.)
end subroutine enable_line_numbers
```

## See Also

- [forge_widget](forge_widget.md) - Base widget class
- [forge_entry](forge_entry.md) - Single-line text input
- [forge_label](forge_label.md) - Non-editable text display
- [forge_event](forge_event.md) - Event handling