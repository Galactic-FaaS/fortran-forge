!> @brief Form Layout Example
!> @details Demonstrates form layout for label-field pairs
!> @author ForGE Contributors
!> @license GPL-3.0-or-later

program layout_form_example
    use forge
    use forge_stub_backend
    implicit none

    type(forge_application) :: app
    type(forge_window_t) :: window
    type(forge_form_layout) :: form_layout
    type(forge_label) :: name_label, email_label, age_label, country_label
    type(forge_entry) :: name_entry, email_entry
    type(forge_spin_button) :: age_spin
    type(forge_combo_box) :: country_combo
    type(forge_button) :: submit_button, cancel_button
    type(forge_label) :: status_label
    type(forge_status) :: status
    type(forge_stub_backend_t), target :: stub_backend

    print '(A)', "=== Form Layout Example ==="
    print '(A)', ""
    print '(A)', "This example demonstrates:"
    print '(A)', "  - Creating form layouts"
    print '(A)', "  - Adding label-field pairs"
    print '(A)', "  - Form field alignment"
    print '(A)', "  - Row and column spanning"
    print '(A)', "  - Form validation layout"
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
        call builder%set_title("Form Layout Example")
        call builder%set_size(400, 300)
        call builder%set_backend(stub_backend)
        window = builder%build(status)
    end block

    if (status%is_error()) then
        print '(A)', "ERROR: Failed to create window"
        call status%print()
        call stub_backend%shutdown()
        stop 1
    end if

    ! Create form layout
    print '(A)', "Creating form layout..."
    call form_layout%set_name("user_form")
    call form_layout%set_spacing(10)
    call form_layout%set_margin(20)
    call form_layout%set_label_alignment(1)  ! Align labels to the right
    call form_layout%set_form_alignment(0)   ! Align fields to the left

    ! Create form fields
    print '(A)', "Creating form fields..."

    ! Name field
    call name_label%set_text("Name:")
    call name_label%set_name("label_name")

    call name_entry%set_placeholder_text("Enter your full name")
    call name_entry%set_name("entry_name")

    ! Email field
    call email_label%set_text("Email:")
    call email_label%set_name("label_email")

    call email_entry%set_placeholder_text("Enter your email address")
    call email_entry%set_name("entry_email")

    ! Age field
    call age_label%set_text("Age:")
    call age_label%set_name("label_age")

    call age_spin%set_range(1, 120)
    call age_spin%set_value(25)
    call age_spin%set_name("spin_age")

    ! Country field
    call country_label%set_text("Country:")
    call country_label%set_name("label_country")

    call country_combo%set_name("combo_country")
    call country_combo%set_editable(.false.)
    call country_combo%add_item("United States")
    call country_combo%add_item("Canada")
    call country_combo%add_item("United Kingdom")
    call country_combo%add_item("Australia")
    call country_combo%add_item("Germany")
    call country_combo%set_current_index(0)

    ! Buttons (span across two columns)
    call submit_button%set_label("Submit")
    call submit_button%set_name("button_submit")

    call cancel_button%set_label("Cancel")
    call cancel_button%set_name("button_cancel")

    ! Add fields to form layout
    print '(A)', "Adding fields to form layout..."
    call form_layout%add_row(name_label, name_entry)
    call form_layout%add_row(email_label, email_entry)
    call form_layout%add_row(age_label, age_spin)
    call form_layout%add_row(country_label, country_combo)
    call form_layout%add_row(submit_button, cancel_button)  ! Buttons span both columns

    ! Status label
    call status_label%set_name("status_label")
    call status_label%set_text("Fill out the form above")

    print '(A,I0,A)', "  Form has ", form_layout%get_row_count(), " rows"
    print '(A,I0)', "  Label alignment: ", form_layout%get_label_alignment()
    print '(A,I0)', "  Form alignment: ", form_layout%get_form_alignment()

    ! Set layout on window
    call window%set_layout(form_layout)

    ! Show window
    print '(A)', ""
    print '(A)', "Showing window with form layout..."
    call window%show()

    ! Simulate form interaction
    print '(A)', ""
    print '(A)', "Simulating form interaction:"

    call name_entry%set_text("John Doe")
    print '(A,A)', "  Name entered: ", trim(name_entry%get_text())

    call email_entry%set_text("john.doe@example.com")
    print '(A,A)', "  Email entered: ", trim(email_entry%get_text())

    call age_spin%set_value(30)
    print '(A,I0)', "  Age set to: ", age_spin%get_value()

    call country_combo%set_current_index(2)  ! United Kingdom
    print '(A,A)', "  Country selected: ", trim(country_combo%get_current_text())

    ! Update status
    call update_status()

    ! Run event loop (stub version just returns)
    call stub_backend%run()

    ! Cleanup
    print '(A)', ""
    print '(A)', "Cleaning up..."
    call window%close()
    call stub_backend%shutdown()

    print '(A)', ""
    print '(A)', "=== Example Complete ==="

contains

    subroutine update_status()
        character(len:150) :: status_text

        write(status_text, '(A,A,A,I0,A,A)') &
            "Form data: ", trim(name_entry%get_text()), &
            ", Age ", age_spin%get_value(), &
            ", ", trim(country_combo%get_current_text())
        call status_label%set_text(trim(status_text))
        print '(A,A)', "  Status: ", trim(status_text)
    end subroutine update_status

end program layout_form_example