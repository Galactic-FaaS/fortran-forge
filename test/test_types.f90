!> @brief Unit tests for forge_types module
!> @author ForGE Contributors
!> @license GPL-3.0-or-later

program test_types
    use forge_types
    use iso_c_binding
    implicit none
    
    integer :: num_tests = 0
    integer :: num_passed = 0
    
    print '(A)', "=========================================="
    print '(A)', "ForGE Types Test Suite"
    print '(A)', "=========================================="
    print '(A)', ""
    
    call test_forge_string()
    call test_forge_color()
    call test_forge_size()
    call test_forge_position()
    
    print '(A)', ""
    print '(A)', "=========================================="
    print '(A,I0,A,I0)', "Tests passed: ", num_passed, " / ", num_tests
    print '(A)', "=========================================="
    
    if (num_passed /= num_tests) then
        error stop "Some tests failed!"
    end if
    
contains

    subroutine test_forge_string()
        type(forge_string) :: str
        character(len=:), allocatable :: result
        
        print '(A)', "Testing forge_string..."
        
        ! Test set and get
        call str%set("Hello, ForGE!")
        result = str%get()
        num_tests = num_tests + 1
        if (result == "Hello, ForGE!") then
            num_passed = num_passed + 1
            print '(A)', "  PASS: set/get"
        else
            print '(A)', "  FAIL: set/get"
        end if
        
        ! Test length
        num_tests = num_tests + 1
        if (str%length() == 13) then
            num_passed = num_passed + 1
            print '(A)', "  PASS: length"
        else
            print '(A)', "  FAIL: length"
        end if
        
        ! Test to_c (null-terminated)
        result = str%to_c()
        num_tests = num_tests + 1
        if (result(len(result):len(result)) == c_null_char) then
            num_passed = num_passed + 1
            print '(A)', "  PASS: to_c"
        else
            print '(A)', "  FAIL: to_c"
        end if
    end subroutine test_forge_string
    
    subroutine test_forge_color()
        type(forge_color) :: color
        
        print '(A)', "Testing forge_color..."
        
        ! Test RGB
        call color%set_rgb(0.5_c_double, 0.7_c_double, 0.9_c_double)
        num_tests = num_tests + 1
        if (abs(color%r - 0.5_c_double) < 1.0e-10 .and. &
            abs(color%g - 0.7_c_double) < 1.0e-10 .and. &
            abs(color%b - 0.9_c_double) < 1.0e-10 .and. &
            abs(color%a - 1.0_c_double) < 1.0e-10) then
            num_passed = num_passed + 1
            print '(A)', "  PASS: set_rgb"
        else
            print '(A)', "  FAIL: set_rgb"
        end if
        
        ! Test RGBA
        call color%set_rgba(0.1_c_double, 0.2_c_double, 0.3_c_double, 0.5_c_double)
        num_tests = num_tests + 1
        if (abs(color%r - 0.1_c_double) < 1.0e-10 .and. &
            abs(color%g - 0.2_c_double) < 1.0e-10 .and. &
            abs(color%b - 0.3_c_double) < 1.0e-10 .and. &
            abs(color%a - 0.5_c_double) < 1.0e-10) then
            num_passed = num_passed + 1
            print '(A)', "  PASS: set_rgba"
        else
            print '(A)', "  FAIL: set_rgba"
        end if
    end subroutine test_forge_color
    
    subroutine test_forge_size()
        type(forge_size) :: size
        
        print '(A)', "Testing forge_size..."
        
        call size%set(800, 600)
        num_tests = num_tests + 1
        if (size%width == 800 .and. size%height == 600) then
            num_passed = num_passed + 1
            print '(A)', "  PASS: set"
        else
            print '(A)', "  FAIL: set"
        end if
    end subroutine test_forge_size
    
    subroutine test_forge_position()
        type(forge_position) :: pos
        
        print '(A)', "Testing forge_position..."
        
        call pos%set(100, 200)
        num_tests = num_tests + 1
        if (pos%x == 100 .and. pos%y == 200) then
            num_passed = num_passed + 1
            print '(A)', "  PASS: set"
        else
            print '(A)', "  FAIL: set"
        end if
    end subroutine test_forge_position

end program test_types

