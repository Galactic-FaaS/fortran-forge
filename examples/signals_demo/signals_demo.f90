!> @brief Signals and Slots demonstration
!> @details Shows the signals/slots system in action
!> @author ForGE Contributors
!> @license GPL-3.0-or-later

program signals_demo
    use forge_signals
    use iso_c_binding
    implicit none
    
    type(signal_void) :: button_clicked
    type(signal_int) :: value_changed
    type(signal_string) :: text_changed
    type(signal_bool) :: toggled
    type(forge_connection) :: conn1, conn2, conn3
    
    integer :: total_clicks = 0
    
    print '(A)', "========================================"
    print '(A)', "ForGE Qt - Signals & Slots Demo"
    print '(A)', "========================================"
    print '(A)', ""
    print '(A)', "Demonstrating Qt-style signals and slots!"
    print '(A)', ""
    
    ! ========== Example 1: Void Signal ==========
    print '(A)', "Example 1: Button Click (void signal)"
    print '(A)', "---------------------------------------"
    
    ! Connect multiple slots to same signal
    conn1 = button_clicked%connect(on_button_clicked_1)
    conn2 = button_clicked%connect(on_button_clicked_2)
    
    print '(A)', "Emitting button_clicked signal..."
    call button_clicked%emit()
    
    print '(A)', ""
    
    ! ========== Example 2: Integer Signal ==========
    print '(A)', "Example 2: Value Changed (int signal)"
    print '(A)', "---------------------------------------"
    
    conn3 = value_changed%connect(on_value_changed)
    
    print '(A)', "Setting value to 42..."
    call value_changed%emit(42)
    
    print '(A)', "Setting value to 100..."
    call value_changed%emit(100)
    
    print '(A)', ""
    
    ! ========== Example 3: String Signal ==========
    print '(A)', "Example 3: Text Changed (string signal)"
    print '(A)', "---------------------------------------"
    
    call text_changed%connect(on_text_changed)
    
    print '(A)', 'Setting text to "Hello, ForGE Qt!"...'
    call text_changed%emit("Hello, ForGE Qt!")
    
    print '(A)', 'Setting text to "Signals work great!"...'
    call text_changed%emit("Signals work great!")
    
    print '(A)', ""
    
    ! ========== Example 4: Boolean Signal ==========
    print '(A)', "Example 4: Toggle State (bool signal)"
    print '(A)', "---------------------------------------"
    
    call toggled%connect(on_toggled)
    
    print '(A)', "Toggling to TRUE..."
    call toggled%emit(.true.)
    
    print '(A)', "Toggling to FALSE..."
    call toggled%emit(.false.)
    
    print '(A)', ""
    
    ! ========== Example 5: Disconnecting ==========
    print '(A)', "Example 5: Disconnecting Slots"
    print '(A)', "---------------------------------------"
    
    print '(A)', "Disconnecting first button callback..."
    call button_clicked%disconnect(conn1)
    
    print '(A)', "Emitting button_clicked (only second callback should fire)..."
    call button_clicked%emit()
    
    print '(A)', ""
    
    ! ========== Example 6: Disconnect All ==========
    print '(A)', "Example 6: Disconnect All"
    print '(A)', "---------------------------------------"
    
    print '(A)', "Disconnecting all button callbacks..."
    call button_clicked%disconnect_all()
    
    print '(A)', "Emitting button_clicked (no callbacks should fire)..."
    call button_clicked%emit()
    
    print '(A)', ""
    
    ! ========== Summary ==========
    print '(A)', "========================================"
    print '(A,I0)', "Total button clicks detected: ", total_clicks
    print '(A)', "========================================"
    print '(A)', ""
    print '(A)', "Signals & Slots Features Demonstrated:"
    print '(A)', "  ✓ Multiple slots per signal"
    print '(A)', "  ✓ Type-safe signal parameters"
    print '(A)', "  ✓ Disconnecting individual slots"
    print '(A)', "  ✓ Disconnecting all slots"
    print '(A)', "  ✓ Callback execution"
    print '(A)', ""
    print '(A)', "This is the foundation for Qt-style"
    print '(A)', "event handling in ForGE Qt!"
    print '(A)', ""
    print '(A)', "========================================"
    print '(A)', "Demo Complete!"
    print '(A)', "========================================"
    
contains

    subroutine on_button_clicked_1()
        print '(A)', "  → Slot 1: Button was clicked!"
        total_clicks = total_clicks + 1
    end subroutine on_button_clicked_1
    
    subroutine on_button_clicked_2()
        print '(A)', "  → Slot 2: I also detected the click!"
        total_clicks = total_clicks + 1
    end subroutine on_button_clicked_2
    
    subroutine on_value_changed(value)
        integer(c_int), intent(in) :: value
        print '(A,I0)', "  → Value changed to: ", value
    end subroutine on_value_changed
    
    subroutine on_text_changed(text)
        character(len=*), intent(in) :: text
        print '(A,A,A)', '  → Text changed to: "', trim(text), '"'
    end subroutine on_text_changed
    
    subroutine on_toggled(state)
        logical, intent(in) :: state
        if (state) then
            print '(A)', "  → Toggled ON"
        else
            print '(A)', "  → Toggled OFF"
        end if
    end subroutine on_toggled

end program signals_demo

