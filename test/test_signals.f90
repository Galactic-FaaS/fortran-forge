!> @brief Unit tests for forge_signals module
!> @author ForGE Contributors
!> @license GPL-3.0-or-later

program test_signals
    use iso_c_binding
    use forge_signals
    implicit none
    
    integer :: num_tests = 0
    integer :: num_passed = 0
    integer :: callback_count
    integer(c_int) :: last_int_value
    character(len=256) :: last_string_value
    logical :: last_bool_value
    
    print '(A)', "=========================================="
    print '(A)', "ForGE Signals & Slots Test Suite"
    print '(A)', "=========================================="
    print '(A)', ""
    
    call test_signal_void()
    call test_signal_int()
    call test_signal_string()
    call test_signal_bool()
    call test_multiple_connections()
    call test_disconnect()
    
    print '(A)', ""
    print '(A)', "=========================================="
    print '(A,I0,A,I0)', "Tests passed: ", num_passed, " / ", num_tests
    print '(A)', "=========================================="
    
    if (num_passed /= num_tests) then
        error stop "Some tests failed!"
    end if
    
contains

    subroutine test_signal_void()
        type(signal_void) :: sig
        type(forge_connection) :: conn
        
        print '(A)', "Testing signal_void..."
        callback_count = 0
        
        ! Test connect
        conn = sig%connect(void_slot_callback)
        num_tests = num_tests + 1
        if (conn%active) then
            num_passed = num_passed + 1
            print '(A)', "  PASS: connect"
        else
            print '(A)', "  FAIL: connect"
        end if
        
        ! Test emit
        call sig%emit()
        num_tests = num_tests + 1
        if (callback_count == 1) then
            num_passed = num_passed + 1
            print '(A)', "  PASS: emit (callback executed)"
        else
            print '(A)', "  FAIL: emit"
        end if
        
        ! Test is_connected
        num_tests = num_tests + 1
        if (sig%is_connected()) then
            num_passed = num_passed + 1
            print '(A)', "  PASS: is_connected"
        else
            print '(A)', "  FAIL: is_connected"
        end if
        
        ! Test disconnect_all
        call sig%disconnect_all()
        num_tests = num_tests + 1
        if (.not. sig%is_connected()) then
            num_passed = num_passed + 1
            print '(A)', "  PASS: disconnect_all"
        else
            print '(A)', "  FAIL: disconnect_all"
        end if
    end subroutine test_signal_void
    
    subroutine test_signal_int()
        type(signal_int) :: sig
        type(forge_connection) :: conn
        
        print '(A)', "Testing signal_int..."
        last_int_value = 0
        
        conn = sig%connect(int_slot_callback)
        call sig%emit(42_c_int)
        
        num_tests = num_tests + 1
        if (last_int_value == 42) then
            num_passed = num_passed + 1
            print '(A)', "  PASS: emit with integer argument"
        else
            print '(A)', "  FAIL: emit with integer argument"
        end if
    end subroutine test_signal_int
    
    subroutine test_signal_string()
        type(signal_string) :: sig
        type(forge_connection) :: conn
        
        print '(A)', "Testing signal_string..."
        last_string_value = ""
        
        conn = sig%connect(string_slot_callback)
        call sig%emit("Hello, ForGE!")
        
        num_tests = num_tests + 1
        if (trim(last_string_value) == "Hello, ForGE!") then
            num_passed = num_passed + 1
            print '(A)', "  PASS: emit with string argument"
        else
            print '(A)', "  FAIL: emit with string argument"
        end if
    end subroutine test_signal_string
    
    subroutine test_signal_bool()
        type(signal_bool) :: sig
        type(forge_connection) :: conn
        
        print '(A)', "Testing signal_bool..."
        last_bool_value = .false.
        
        conn = sig%connect(bool_slot_callback)
        call sig%emit(.true.)
        
        num_tests = num_tests + 1
        if (last_bool_value .eqv. .true.) then
            num_passed = num_passed + 1
            print '(A)', "  PASS: emit with boolean argument"
        else
            print '(A)', "  FAIL: emit with boolean argument"
        end if
    end subroutine test_signal_bool
    
    subroutine test_multiple_connections()
        type(signal_void) :: sig
        type(forge_connection) :: conn1, conn2
        
        print '(A)', "Testing multiple connections..."
        callback_count = 0
        
        conn1 = sig%connect(void_slot_callback)
        conn2 = sig%connect(void_slot_callback)
        
        call sig%emit()
        
        num_tests = num_tests + 1
        if (callback_count == 2) then
            num_passed = num_passed + 1
            print '(A)', "  PASS: multiple slots called"
        else
            print '(A)', "  FAIL: multiple slots called"
        end if
    end subroutine test_multiple_connections
    
    subroutine test_disconnect()
        type(signal_void) :: sig
        type(forge_connection) :: conn
        
        print '(A)', "Testing disconnect..."
        callback_count = 0
        
        conn = sig%connect(void_slot_callback)
        call sig%disconnect(conn)
        call sig%emit()
        
        num_tests = num_tests + 1
        if (callback_count == 0) then
            num_passed = num_passed + 1
            print '(A)', "  PASS: disconnect prevents callback"
        else
            print '(A)', "  FAIL: disconnect"
        end if
    end subroutine test_disconnect
    
    ! ========== Slot Callbacks ==========
    
    subroutine void_slot_callback()
        callback_count = callback_count + 1
    end subroutine void_slot_callback
    
    subroutine int_slot_callback(value)
        integer(c_int), intent(in) :: value
        last_int_value = value
    end subroutine int_slot_callback
    
    subroutine string_slot_callback(value)
        character(len=*), intent(in) :: value
        last_string_value = value
    end subroutine string_slot_callback
    
    subroutine bool_slot_callback(value)
        logical, intent(in) :: value
        last_bool_value = value
    end subroutine bool_slot_callback

end program test_signals

