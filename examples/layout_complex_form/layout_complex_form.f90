!> @brief Complex Form Layout Example
!> @details Demonstrates a complex form with multiple sections and layouts
!> @author ForGE Contributors
!> @license GPL-3.0-or-later

program layout_complex_form_example
    use forge
    use forge_stub_backend
    implicit none

    type(forge_application) :: app
    type(forge_window_t) :: window
    type(forge_box_layout) :: main_layout, button_layout
    type(forge_group_box) :: personal_group, address_group, preferences_group
    type(forge_form_layout) :: personal_form, address_form
    type(forge_box_layout) :: preferences_layout
    type(forge_label) :: title_label
    type(forge_label) :: name_label, email_label, phone_label
    type(forge_label) :: street_label, city_label, zip_label
    type(forge_entry) :: name_entry, email_entry, phone_entry
    type(forge_entry) :: street_entry, city_entry, zip_entry
    type(forge_checkbox) :: newsletter_check, updates_check
    type(forge_button) :: save_button, cancel_button, reset_button
    type(forge_status) :: status
    type(forge_stub_backend_t), target :: stub_backend

    print '(A)', "=== Complex Form Layout Example ==="
    print '(A)', ""
    print '(A)', "This example demonstrates:"
    print '(A)', "  - Complex nested layout structures"
    print '(A)', "  - Group boxes for organizing sections"
    print '(A)', "  - Multiple layout types in one form"
    print '(A)', "  - Professional form design patterns"
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
        call builder%set_title("Complex Form Layout Example")
        call builder%set_size(500, 600)
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
    print '(A)', "Creating complex nested form layout..."
    call main_layout%set_orientation(1)  ! Vertical
    call main_layout%set_spacing(15)
    call main_layout%set_margin(20)

    ! Title
    call title_label%set_text("User Registration Form")
    call title_label%set_name("title")

    ! Personal Information Group
    call personal_group%set_title("Personal Information")
    call personal_group%set_name("group_personal")

    call personal_form%set_name("form_personal")
    call personal_form%set_spacing(5)
    call personal_form%set_label_alignment(1)  ! Right align

    call name_label%set_text("Full Name:")
    call name_entry%set_placeholder_text("Enter your full name")

    call email_label%set_text("Email:")
    call email_entry%set_placeholder_text("your.email@example.com")

    call phone_label%set_text("Phone:")
    call phone_entry%set_placeholder_text("(123) 456-7890")

    call personal_form%add_row(name_label, name_entry)
    call personal_form%add_row(email_label, email_entry)
    call personal_form%add_row(phone_label, phone_entry)

    call personal_group%set_widget(personal_form)

    ! Address Information Group
    call address_group%set_title("Address Information")
    call address_group%set_name("group_address")

    call address_form%set_name("form_address")
    call address_form%set_spacing(5)
    call address_form%set_label_alignment(1)  ! Right align

    call street_label%set_text("Street:")
    call street_entry%set_placeholder_text("123 Main Street")

    call city_label%set_text("City:")
    call city_entry%set_placeholder_text("Anytown")

    call zip_label%set_text("ZIP Code:")
    call zip_entry%set_placeholder_text("12345")

    call address_form%add_row(street_label, street_entry)
    call address_form%add_row(city_label, city_entry)
    call address_form%add_row(zip_label, zip_entry)

    call address_group%set_widget(address_form)

    ! Preferences Group
    call preferences_group%set_title("Preferences")
    call preferences_group%set_name("group_preferences")

    call preferences_layout%set_orientation(1)  ! Vertical
    call preferences_layout%set_spacing(5)
    call preferences_layout%set_margin(10)

    call newsletter_check%set_label("Subscribe to newsletter")
    call newsletter_check%set_checked(.true.)

    call updates_check%set_label("Receive product updates")
    call updates_check%set_checked(.false.)

    call preferences_layout%add_widget(newsletter_check)
    call preferences_layout%add_widget(updates_check)

    call preferences_group%set_widget(preferences_layout)

    ! Button layout
    call button_layout%set_orientation(0)  ! Horizontal
    call button_layout%set_spacing(10)

    call save_button%set_label("Save")
    call cancel_button%set_label("Cancel")
    call reset_button%set_label("Reset")

    call button_layout%add_widget(save_button)
    call button_layout%add_stretch()
    call button_layout%add_widget(cancel_button)
    call button_layout%add_widget(reset_button)

    ! Assemble main layout
    call main_layout%add_widget(title_label)
    call main_layout%add_widget(personal_group)
    call main_layout%add_widget(address_group)
    call main_layout%add_widget(preferences_group)
    call main_layout%add_layout(button_layout)

    ! Set main layout on window
    call window%set_layout(main_layout)

    print '(A)', "  Complex form structure:"
    print '(A)', "    ├── Title"
    print '(A)', "    ├── Personal Info Group (Form Layout)"
    print '(A)', "    ├── Address Info Group (Form Layout)"
    print '(A)', "    ├── Preferences Group (Vertical Layout)"
    print '(A)', "    └── Button Layout (Horizontal with stretch)"

    ! Show window
    print '(A)', ""
    print '(A)', "Showing window with complex form layout..."
    call window%show()

    ! Simulate form filling
    print '(A)', ""
    print '(A)', "Simulating form completion:"

    call name_entry%set_text("John Doe")
    call email_entry%set_text("john.doe@example.com")
    call phone_entry%set_text("(555) 123-4567")
    call street_entry%set_text("123 Oak Street")
    call city_entry%set_text("Springfield")
    call zip_entry%set_text("12345")
    call newsletter_check%set_checked(.false.)
    call updates_check%set_checked(.true.)

    print '(A)', "  Form filled with sample data"

    ! Run event loop (stub version just returns)
    call stub_backend%run()

    ! Cleanup
    print '(A)', ""
    print '(A)', "Cleaning up..."
    call window%close()
    call stub_backend%shutdown()

    print '(A)', ""
    print '(A)', "=== Example Complete ==="

end program layout_complex_form_example