!> @brief Custom GUI Framework demonstration
!> @details Shows native window creation using the custom backend
!> @author ForGE Contributors
!> @license GPL-3.0-or-later

program custom_window_example
    use forge
    implicit none
    
    type(forge_application) :: app
    type(forge_window_t) :: window
    type(forge_status) :: status
    
    print '(A)', "========================================"
    print '(A)', "ForGE Custom GUI Framework Demo"
    print '(A)', "========================================"
    print '(A)', ""
    print '(A)', "This example demonstrates:"
    print '(A)', "  - Custom GUI framework (no external GUI deps)"
    print '(A)', "  - Native Windows window via Win32 API"
    print '(A)', "  - Event loop handling"
    print '(A)', "  - Proper cleanup"
    print '(A)', ""
    
    ! Initialize application with custom backend
    print '(A)', "Initializing custom GUI backend..."
    call app%init(BACKEND_CUSTOM, status)
    
    if (status%is_error()) then
        print '(A)', "ERROR: Failed to initialize custom backend"
        call status%print()
        stop 1
    end if
    
    print '(A)', "Backend initialized successfully!"
    print '(A)', ""
    
    ! Create window
    print '(A)', "Creating window..."
    window = app%create_window("ForGE Custom GUI Demo", 800, 600)
    
    ! Show the window
    print '(A)', "Showing window..."
    call window%show()
    
    print '(A)', ""
    print '(A)', "Window is now visible!"
    print '(A)', "Close the window to exit the application."
    print '(A)', ""
    
    ! Run the event loop
    print '(A)', "Entering event loop..."
    call app%run()
    
    ! Cleanup
    print '(A)', ""
    print '(A)', "Cleaning up..."
    call window%close()
    call app%shutdown()
    
    print '(A)', ""
    print '(A)', "========================================"
    print '(A)', "Example complete!"
    print '(A)', "========================================"
    
end program custom_window_example

