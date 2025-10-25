!> @brief Hello World example for ForGE
!> @details Demonstrates creating a simple window with a label
!> @author ForGE Contributors
!> @license GPL-3.0-or-later

program hello_world_example
    use forge
    use forge_stub_backend
    implicit none
    
    type(forge_application) :: app
    type(forge_window_t) :: window
    type(forge_status) :: status
    type(forge_stub_backend_t), target :: stub_backend
    
    ! Print ForGE version
    print '(A,A)', "ForGE Version: ", FORGE_VERSION
    print '(A)', ""
    print '(A)', "=== Hello World Example ==="
    print '(A)', ""
    
    ! Note: Using stub backend for now since real backends not yet implemented
    print '(A)', "NOTE: Using stub backend (no actual GUI will appear)"
    print '(A)', "      Real GUI backends (Tcl/Tk, GTK4, Qt) coming soon!"
    print '(A)', ""
    
    ! Initialize stub backend manually
    call stub_backend%init(status)
    if (status%is_error()) then
        print '(A)', "ERROR: Failed to initialize stub backend"
        call status%print()
        stop 1
    end if
    
    ! Create window using builder pattern
    print '(A)', "Creating window..."
    block
        type(forge_window_builder) :: builder
        call builder%set_title("Hello, ForGE!")
        call builder%set_size(400, 300)
        call builder%set_position(100, 100)
        call builder%set_backend(stub_backend)
        window = builder%build(status)
    end block
    
    if (status%is_error()) then
        print '(A)', "ERROR: Failed to create window"
        call status%print()
        call stub_backend%shutdown()
        stop 1
    end if
    
    ! Show the window
    print '(A)', "Showing window..."
    call window%show()
    
    ! In a real application, this would run the event loop
    ! For now with stub backend, it just prints and exits
    print '(A)', ""
    print '(A)', "With a real backend, the window would appear and the event loop would run."
    print '(A)', "Press Ctrl+C to close (or the window close button in real backends)."
    print '(A)', ""
    
    ! "Run" the event loop (stub version just returns immediately)
    call stub_backend%run()
    
    ! Cleanup
    print '(A)', ""
    print '(A)', "Cleaning up..."
    call window%close()
    call stub_backend%shutdown()
    
    print '(A)', ""
    print '(A)', "=== Example Complete ==="
    
end program hello_world_example

