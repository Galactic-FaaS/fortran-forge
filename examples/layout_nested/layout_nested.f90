!> @brief Nested Layouts Example
!> @details Demonstrates nesting layouts within other layouts
!> @author ForGE Contributors
!> @license GPL-3.0-or-later

program layout_nested_example
    use forge
    use forge_stub_backend
    implicit none

    type(forge_application) :: app
    type(forge_window_t) :: window
    type(forge_box_layout) :: main_layout, top_layout, bottom_layout
    type(forge_form_layout) :: left_form, right_form
    type(forge_label) :: title_label, status_label
    type(forge_label) :: name_label, age_label, email_label, phone_label
    type(forge_entry) :: name_entry, email_entry, phone_entry
    type(forge_spin_button) :: age_spin
    type(forge_button) :: save_button, cancel_button, help_button
    type(forge_status) :: status
    type(forge_stub_backend_t), target :: stub_backend

    print '(A)', "=== Nested Layouts Example ==="
    print '(A)', ""
    print '(A)', "This example demonstrates:"
    print '(A)', "  - Nesting layouts within layouts"
    print '(A)', "  - Combining different layout types"
    print '(A)', "  - Complex widget hierarchies"
    print '(A)', "  - Layout composition patterns"
    print '(A)', ""
    print '(A)', "NOTE: Using stub backend - no actual GUI will appear"
    print '(A)', ""

    ! Initialize stub backend
    call stub_backend%init(status)
    if (status%is_error()) then
        print '(A)', "ERROR: Failed to initialize stub backend"
        call status%print()
        stop 1
    end if

    ! Create window
    block
        type(forge_window_builder) :: builder
        call builder%set_title("Nested Layouts Example")
        call builder%set_size(600, 400)
        call builder%set_backend(stub_backend)
        window = builder%build(status)
    end block

    if (status%is_error()) then
        print '(A)', "ERROR: Failed to create window"
        call status%print()
        call stub_backend%shutdown()
        stop 1
    end if

    ! Create main vertical layout
    print '(A)', "Creating nested layout structure..."
    call main_layout%set_orientation(1)  ! Vertical
    call main_layout%set_spacing(10)
    call main_layout%set_margin(15)

    ! Title
    call title_label%set_text("User Information Form")
    call title_label%set_name("title")

    ! Top horizontal layout (contains two forms side by side)
    call top_layout%set_orientation(0)  ! Horizontal
    call top_layout%set_spacing(20)

    ! Left form layout
    call left_form%set_name("left_form")
    call left_form%set_spacing(5)
    call left_form%set_label_alignment(1)  ! Right align labels

    call name_label%set_text("Name:")
    call name_entry%set_placeholder_text("Full name")

    call age_label%set_text("Age:")
    call age_spin%set_range(1, 120)
    call age_spin%set_value(25)

    call left_form%add_row(name_label, name_entry)
    call left_form%add_row(age_label, age_spin)

    ! Right form layout
    call right_form%set_name("right_form")
    call right_form%set_spacing(5)
    call right_form%set_label_alignment(1)  ! Right align labels

    call email_label%set_text("Email:")
    call email_entry%set_placeholder_text("email@example.com")

    call phone_label%set_text("Phone:")
    call phone_entry%set_placeholder_text("(123) 456-7890")

    call right_form%add_row(email_label, email_entry)
    call right_form%add_row(phone_label, phone_entry)

    ! Add forms to top layout
    call top_layout%add_layout(left_form)
    call top_layout%add_layout(right_form)

    ! Bottom horizontal layout (buttons)
    call bottom_layout%set_orientation(0)  ! Horizontal
    call bottom_layout%set_spacing(10)

    call save_button%set_label("Save")
    call cancel_button%set_label("Cancel")
    call help_button%set_label("Help")

    call bottom_layout%add_widget(save_button)
    call bottom_layout%add_stretch()  ! Push buttons to right
    call bottom_layout%add_widget(cancel_button)
    call bottom_layout%add_widget(help_button)

    ! Status label
    call status_label%set_name("status_label")
    call status_label%set_text("Nested layouts: Main vertical → Top horizontal → Two forms + Bottom horizontal → Buttons")

    ! Assemble the nested structure
    call main_layout%add_widget(title_label)
    call main_layout%add_layout(top_layout)
    call main_layout%add_layout(bottom_layout)
    call main_layout%add_widget(status_label)

    ! Set main layout on window
    call window%set_layout(main_layout)

    print '(A)', "  Layout hierarchy:"
    print '(A)', "    Main Layout (Vertical)"
    print '(A)', "      ├── Title Label"
    print '(A)', "      ├── Top Layout (Horizontal)"
    print '(A)', "      │     ├── Left Form (Name, Age)"
    print '(A)', "      │     └── Right Form (Email, Phone)"
    print '(A)', "      ├── Bottom Layout (Horizontal)"
    print '(A)', "      │     ├── Save Button"
    print '(A)', "      │     ├── Stretch"
    print '(A)', "      │     ├── Cancel Button"
    print '(A)', "      │     └── Help Button"
    print '(A)', "      └── Status Label"

    ! Show window
    print '(A)', ""
    print '(A)', "Showing window with nested layouts..."
    call window%show()

    ! Simulate form filling
    print '(A)', ""
    print '(A)', "Simulating form filling:"

    call name_entry%set_text("Jane Smith")
    call age_spin%set_value(32)
    call email_entry%set_text("jane.smith@company.com")
    call phone_entry%set_text("(555) 123-4567")

    print '(A)', "  Form data entered in nested layout structure"

    ! Run event loop (stub version just returns)
    call stub_backend%run()

    ! Cleanup
    print '(A)', ""
    print '(A)', "Cleaning up..."
    call window%close()
    call stub_backend%shutdown()

    print '(A)', ""
    print '(A)', "=== Example Complete ==="

end program layout_nested_example