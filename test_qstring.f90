!> @brief Test program for QString implementation
program test_qstring
    use forge_string_utils
    implicit none

    type(QString) :: str1, str2, str3
    character(len=:), allocatable :: result
    logical :: success

    print *, "Testing QString implementation..."

    ! Test basic set/get
    call str1%set("Hello World")
    result = str1%get()
    print *, "Set/Get test: ", result

    ! Test length
    print *, "Length test: ", str1%length()

    ! Test append
    call str1%append(" from Fortran")
    result = str1%get()
    print *, "Append test: ", result

    ! Test substring
    str2 = str1%mid(7, 5)
    result = str2%get()
    print *, "Substring test: ", result

    ! Test case conversion
    call str1%to_upper()
    result = str1%get()
    print *, "Upper case test: ", result

    ! Test number conversion
    call str1%set("42")
    print *, "String to int test: ", str1%to_int(success), " success: ", success

    ! Test operators
    str1 = "Hello"
    str2 = " World"
    str3 = str1 + str2
    result = str3%get()
    print *, "Concatenation test: ", result

    print *, "All tests completed!"
end program test_qstring