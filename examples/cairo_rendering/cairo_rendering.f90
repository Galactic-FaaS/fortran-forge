!> @brief Cairo Rendering demonstration
!> @details Shows actual rendering using Cairo in custom backend
!> @author ForGE Contributors
!> @license GPL-3.0-or-later

program cairo_rendering_example
    use forge
    use forge_platform
    use forge_platform_windows
    use forge_cairo_bindings
    use forge_rendering
    implicit none
    
    type(forge_windows_platform), target :: platform
    type(platform_window_handle) :: window_handle
    type(platform_surface_handle) :: surface
    type(forge_status) :: status
    logical(c_bool) :: should_quit
    type(c_ptr) :: hdc
    integer :: i
    
    print '(A)', "========================================"
    print '(A)', "ForGE Cairo Rendering Demo"
    print '(A)', "========================================"
    print '(A)', ""
    
    ! Initialize platform
    print '(A)', "Initializing Windows platform..."
    call platform%init(status)
    if (status%is_error()) then
        print '(A)', "ERROR: Failed to initialize platform"
        call status%print()
        stop 1
    end if
    
    ! Create window
    print '(A)', "Creating window..."
    call platform%create_window(window_handle, "Cairo Rendering Test", 800, 600, status)
    if (status%is_error()) then
        print '(A)', "ERROR: Failed to create window"
        call status%print()
        stop 1
    end if
    
    ! Show window
    print '(A)', "Showing window..."
    call platform%show_window(window_handle)
    
    print '(A)', ""
    print '(A)', "Rendering test pattern..."
    
    ! Get Cairo surface and render
    call platform%get_surface(window_handle, surface, status)
    if (status%is_error()) then
        print '(A)', "ERROR: Failed to get surface"
        call status%print()
    else
        ! Render test pattern
        call render_test_pattern(surface%context, window_handle%width, window_handle%height)
        call cairo_surface_flush(surface%surface)
        
        ! Cleanup Cairo
        call cairo_destroy(surface%context)
        call cairo_surface_destroy(surface%surface)
        
        print '(A)', "Test pattern rendered!"
    end if
    
    print '(A)', ""
    print '(A)', "Window displayed with rendered content!"
    print '(A)', "Close the window to exit."
    print '(A)', ""
    
    ! Simple event loop (process events for a while)
    print '(A)', "Running event loop..."
    do i = 1, 1000  ! Run for limited iterations for demo
        call platform%process_events(should_quit)
        if (should_quit) exit
        
        ! Sleep briefly
        call sleep(10)  ! milliseconds
    end do
    
    ! Cleanup
    print '(A)', ""
    print '(A)', "Cleaning up..."
    call platform%destroy_window(window_handle)
    call platform%shutdown()
    
    print '(A)', ""
    print '(A)', "========================================"
    print '(A)', "Demo complete!"
    print '(A)', "========================================"
    
contains

    ! Simple sleep function (platform-specific)
    subroutine sleep(milliseconds)
        integer, intent(in) :: milliseconds
        
        interface
            subroutine Sleep(dwMilliseconds) bind(C, name="Sleep")
                use iso_c_binding
                integer(c_int), value :: dwMilliseconds
            end subroutine Sleep
        end interface
        
        call Sleep(milliseconds)
    end subroutine sleep
    
end program cairo_rendering_example

