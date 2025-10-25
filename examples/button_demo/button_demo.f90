!> @brief Button demonstration for ForGE
!> @details Shows button creation and click event handling
!> @author ForGE Contributors
!> @license GPL-3.0-or-later

program button_demo_example
    use forge
    use forge_stub_backend
    implicit none
    
    type(forge_application) :: app
    type(forge_window_t) :: window
    type(forge_button) :: button1, button2, button3
    type(forge_status) :: status
    type(forge_stub_backend_t), target :: stub_backend
    
    print '(A)', "=== Button Demo Example ==="
    print '(A)', ""
    print '(A)', "This example demonstrates:"
    print '(A)', "  - Creating multiple buttons"
    print '(A)', "  - Attaching click event handlers"
    print '(A)', "  - Button layout and positioning"
    print '(A)', ""
    print '(A)', "NOTE: Using stub backend - events won't fire in demo mode"
    print '(A)', ""
    
    ! Initialize backend
    call stub_backend%init(status)
    call forge_check_status(status, abort_on_error=.true.)
    
    ! Create window
    block
        type(forge_window_builder) :: builder
        call builder%set_title("Button Demo")
        call builder%set_size(500, 400)
        call builder%set_backend(stub_backend)
        window = builder%build(status)
    end block
    call forge_check_status(status, abort_on_error=.true.)
    
    ! Create buttons (backend integration would happen here)
    print '(A)', "Creating buttons..."
    call button1%set_name("btn_click_me")
    call button1%set_label("Click Me!")
    call button1%on_click(on_button1_clicked)
    
    call button2%set_name("btn_increment")
    call button2%set_label("Increment Counter")
    call button2%on_click(on_button2_clicked)
    
    call button3%set_name("btn_quit")
    call button3%set_label("Quit")
    call button3%on_click(on_quit_clicked)
    
    print '(A)', "  Button 1: ", trim(button1%get_label())
    print '(A)', "  Button 2: ", trim(button2%get_label())
    print '(A)', "  Button 3: ", trim(button3%get_label())
    print '(A)', ""
    
    ! Show window and run
    call window%show()
    call stub_backend%run()
    
    ! Cleanup
    call window%close()
    call stub_backend%shutdown()
    
    print '(A)', ""
    print '(A)', "=== Demo Complete ==="
    
contains

    !> @brief Callback for button 1 click
    subroutine on_button1_clicked(event)
        type(forge_event), intent(in) :: event
        print '(A)', "[EVENT] Button 1 clicked!"
    end subroutine on_button1_clicked
    
    !> @brief Callback for button 2 click
    subroutine on_button2_clicked(event)
        type(forge_event), intent(in) :: event
        integer, save :: counter = 0
        counter = counter + 1
        print '(A,I0)', "[EVENT] Button 2 clicked! Counter: ", counter
    end subroutine on_button2_clicked
    
    !> @brief Callback for quit button
    subroutine on_quit_clicked(event)
        type(forge_event), intent(in) :: event
        print '(A)', "[EVENT] Quit button clicked - application would exit"
    end subroutine on_quit_clicked

end program button_demo_example

