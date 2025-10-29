!> @brief Wizard Layout Example
!> @details Demonstrates a wizard-style interface with step-by-step navigation
!> @author ForGE Contributors
!> @license GPL-3.0-or-later

program layout_wizard_example
    use forge
    use forge_stub_backend
    implicit none

    type(forge_application) :: app
    type(forge_window_t) :: window
    type(forge_box_layout) :: main_layout, content_layout, button_layout
    type(forge_stacked_widget) :: wizard_pages
    type(forge_label) :: title_label, step_indicator
    type(forge_label) :: welcome_content, config_content, confirm_content, finish_content
    type(forge_button) :: back_button, next_button, cancel_button, finish_button
    type(forge_checkbox) :: option1_check, option2_check, option3_check
    type(forge_label) :: summary_label
    type(forge_status) :: status
    type(forge_stub_backend_t), target :: stub_backend
    integer :: current_step = 1

    print '(A)', "=== Wizard Layout Example ==="
    print '(A)', ""
    print '(A)', "This example demonstrates:"
    print '(A)', "  - Wizard-style interface design"
    print '(A)', "  - Stacked widget for page navigation"
    print '(A)', "  - Step-by-step user guidance"
    print '(A)', "  - Dynamic button states"
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
        call builder%set_title("Setup Wizard")
        call builder%set_size(500, 400)
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
    print '(A)', "Creating wizard layout..."
    call main_layout%set_orientation(1)  ! Vertical
    call main_layout%set_spacing(15)
    call main_layout%set_margin(20)

    ! Header
    call title_label%set_text("Setup Wizard")
    call title_label%set_name("title")

    call step_indicator%set_text("Step 1 of 4: Welcome")
    call step_indicator%set_name("step_indicator")

    ! Content area with stacked widget
    call content_layout%set_orientation(1)  ! Vertical
    call content_layout%set_spacing(10)

    call wizard_pages%set_name("wizard_pages")

    ! Page 1: Welcome
    call welcome_content%set_text("Welcome to the Setup Wizard!\n\n" // &
                                 "This wizard will guide you through the\n" // &
                                 "configuration process.\n\n" // &
                                 "Click 'Next' to continue.")
    call welcome_content%set_name("welcome_content")

    ! Page 2: Configuration
    call config_content%set_text("Configuration Options:\n\n" // &
                                "Please select the features you want to enable:")
    call config_content%set_name("config_content")

    call option1_check%set_label("Enable feature A")
    call option1_check%set_name("option1")
    call option1_check%set_checked(.true.)

    call option2_check%set_label("Enable feature B")
    call option2_check%set_name("option2")
    call option2_check%set_checked(.false.)

    call option3_check%set_label("Enable feature C")
    call option3_check%set_name("option3")
    call option3_check%set_checked(.true.)

    ! Page 3: Confirmation
    call confirm_content%set_text("Confirmation:\n\n" // &
                                "Please review your selections before\n" // &
                                "completing the setup.")
    call confirm_content%set_name("confirm_content")

    call summary_label%set_text("Selected options will be shown here.")
    call summary_label%set_name("summary")

    ! Page 4: Finish
    call finish_content%set_text("Setup Complete!\n\n" // &
                               "The configuration has been applied successfully.\n\n" // &
                               "Click 'Finish' to close the wizard.")
    call finish_content%set_name("finish_content")

    ! Add pages to stacked widget
    call wizard_pages%add_widget(welcome_content)
    call wizard_pages%add_widget(config_content)
    call wizard_pages%add_widget(option1_check)
    call wizard_pages%add_widget(option2_check)
    call wizard_pages%add_widget(option3_check)
    call wizard_pages%add_widget(confirm_content)
    call wizard_pages%add_widget(summary_label)
    call wizard_pages%add_widget(finish_content)

    ! Button layout
    call button_layout%set_orientation(0)  ! Horizontal
    call button_layout%set_spacing(10)

    call back_button%set_label("< Back")
    call back_button%set_name("button_back")
    call back_button%set_enabled(.false.)  ! Disabled on first page

    call next_button%set_label("Next >")
    call next_button%set_name("button_next")

    call cancel_button%set_label("Cancel")
    call cancel_button%set_name("button_cancel")

    call finish_button%set_label("Finish")
    call finish_button%set_name("button_finish")
    call finish_button%set_visible(.false.)  ! Hidden until last page

    call button_layout%add_widget(back_button)
    call button_layout%add_widget(next_button)
    call button_layout%add_stretch()
    call button_layout%add_widget(cancel_button)
    call button_layout%add_widget(finish_button)

    ! Assemble layout
    call main_layout%add_widget(title_label)
    call main_layout%add_widget(step_indicator)
    call main_layout%add_layout(content_layout)
    call main_layout%add_layout(button_layout)

    ! Set main layout on window
    call window%set_layout(main_layout)

    print '(A)', "  Wizard structure:"
    print '(A)', "    ├── Header (Title + Step indicator)"
    print '(A)', "    ├── Content Area (Stacked Widget)"
    print '(A)', "    │     ├── Welcome Page"
    print '(A)', "    │     ├── Configuration Page"
    print '(A)', "    │     ├── Confirmation Page"
    print '(A)', "    │     └── Finish Page"
    print '(A)', "    └── Button Bar (Back/Next/Cancel/Finish)"

    ! Show window
    print '(A)', ""
    print '(A)', "Showing wizard interface..."
    call window%show()

    ! Simulate wizard navigation
    print '(A)', ""
    print '(A)', "Simulating wizard navigation:"

    ! Go to configuration page
    call wizard_pages%set_current_index(1)  ! Config page
    call step_indicator%set_text("Step 2 of 4: Configuration")
    call back_button%set_enabled(.true.)
    print '(A)', "  Navigated to Configuration step"

    ! Go to confirmation page
    call wizard_pages%set_current_index(5)  ! Confirmation page
    call step_indicator%set_text("Step 3 of 4: Confirmation")
    call update_summary()
    print '(A)', "  Navigated to Confirmation step"

    ! Go to finish page
    call wizard_pages%set_current_index(7)  ! Finish page
    call step_indicator%set_text("Step 4 of 4: Complete")
    call next_button%set_visible(.false.)
    call finish_button%set_visible(.true.)
    print '(A)', "  Navigated to Finish step"

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

    subroutine update_summary()
        character(len:100) :: summary_text

        write(summary_text, '(A,L1,A,L1,A,L1,A)') &
            "Selected options: A=", option1_check%is_checked(), &
            ", B=", option2_check%is_checked(), &
            ", C=", option3_check%is_checked()
        call summary_label%set_text(trim(summary_text))
        print '(A,A)', "  Summary: ", trim(summary_text)
    end subroutine update_summary

end program layout_wizard_example