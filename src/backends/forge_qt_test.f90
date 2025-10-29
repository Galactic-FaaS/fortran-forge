!> @brief High-level Fortran interface for Qt Test framework
!> @details Provides object-oriented Fortran interface to Qt Test functionality
!> @author ForGE Contributors
!> @date 2025
!> @license GPL-3.0-or-later

module forge_qt_test
    use iso_c_binding
    use forge_qt_test_bindings
    use forge_qt_bindings
    implicit none
    private

    public :: qtest_object, qtest_case, qtest_suite, qsignal_spy, qbenchmark_measurement
    public :: qtest_assertion, qtest_data_table, qtest_event_simulator
    public :: qtest_init, qtest_cleanup, qtest_run, qtest_run_all
    public :: qtest_assert, qtest_assert_equal, qtest_assert_not_equal
    public :: qtest_assert_true, qtest_assert_false, qtest_assert_null
    public :: qtest_assert_not_null, qtest_skip, qtest_fail, qtest_expect_fail
    public :: qtest_warn, qtest_benchmark, qtest_benchmark_once
    public :: qtest_sleep, qtest_wait, qtest_wait_for
    public :: qtest_wait_for_window_active, qtest_wait_for_window_shown
    public :: qtest_wait_for_window_closed, qtest_wait_for_window_exposed
    public :: qtest_key_click, qtest_key_press, qtest_key_release
    public :: qtest_mouse_click, qtest_mouse_press, qtest_mouse_release
    public :: qtest_mouse_move, qtest_mouse_wheel, qtest_touch_begin
    public :: qtest_touch_update, qtest_touch_end, qtest_touch_cancel
    public :: qtest_data_add_column, qtest_data_new_row, qtest_data_fetch
    public :: qtest_spy_create, qtest_spy_count, qtest_spy_wait
    public :: qtest_spy_is_valid, qtest_spy_signal, qtest_spy_take_first
    public :: qtest_spy_take_last, qtest_spy_at
    public :: qtest_benchmark_start, qtest_benchmark_stop, qtest_benchmark_elapsed
    public :: qtest_benchmark_iterations

    !> Test result enumeration
    integer, parameter :: QTEST_PASS = 0
    integer, parameter :: QTEST_FAIL = 1
    integer, parameter :: QTEST_SKIP = 2
    integer, parameter :: QTEST_EXPECT_FAIL = 3

    !> Qt key codes
    integer, parameter :: QT_KEY_ESCAPE = 16777216
    integer, parameter :: QT_KEY_TAB = 16777217
    integer, parameter :: QT_KEY_BACKTAB = 16777218
    integer, parameter :: QT_KEY_BACKSPACE = 16777219
    integer, parameter :: QT_KEY_RETURN = 16777220
    integer, parameter :: QT_KEY_ENTER = 16777221
    integer, parameter :: QT_KEY_INSERT = 16777222
    integer, parameter :: QT_KEY_DELETE = 16777223
    integer, parameter :: QT_KEY_PAUSE = 16777224
    integer, parameter :: QT_KEY_PRINT = 16777225
    integer, parameter :: QT_KEY_SYSREQ = 16777226
    integer, parameter :: QT_KEY_CLEAR = 16777227
    integer, parameter :: QT_KEY_HOME = 16777232
    integer, parameter :: QT_KEY_END = 16777233
    integer, parameter :: QT_KEY_LEFT = 16777234
    integer, parameter :: QT_KEY_UP = 16777235
    integer, parameter :: QT_KEY_RIGHT = 16777236
    integer, parameter :: QT_KEY_DOWN = 16777237
    integer, parameter :: QT_KEY_PAGEUP = 16777238
    integer, parameter :: QT_KEY_PAGEDOWN = 16777239
    integer, parameter :: QT_KEY_SHIFT = 16777248
    integer, parameter :: QT_KEY_CONTROL = 16777249
    integer, parameter :: QT_KEY_META = 16777250
    integer, parameter :: QT_KEY_ALT = 16777251
    integer, parameter :: QT_KEY_CAPSLOCK = 16777252
    integer, parameter :: QT_KEY_NUMLOCK = 16777253
    integer, parameter :: QT_KEY_SCROLLLOCK = 16777254
    integer, parameter :: QT_KEY_F1 = 16777264
    integer, parameter :: QT_KEY_F2 = 16777265
    integer, parameter :: QT_KEY_F3 = 16777266
    integer, parameter :: QT_KEY_F4 = 16777267
    integer, parameter :: QT_KEY_F5 = 16777268
    integer, parameter :: QT_KEY_F6 = 16777269
    integer, parameter :: QT_KEY_F7 = 16777270
    integer, parameter :: QT_KEY_F8 = 16777271
    integer, parameter :: QT_KEY_F9 = 16777272
    integer, parameter :: QT_KEY_F10 = 16777273
    integer, parameter :: QT_KEY_F11 = 16777274
    integer, parameter :: QT_KEY_F12 = 16777275
    integer, parameter :: QT_KEY_F13 = 16777276
    integer, parameter :: QT_KEY_F14 = 16777277
    integer, parameter :: QT_KEY_F15 = 16777278
    integer, parameter :: QT_KEY_F16 = 16777279
    integer, parameter :: QT_KEY_F17 = 16777280
    integer, parameter :: QT_KEY_F18 = 16777281
    integer, parameter :: QT_KEY_F19 = 16777282
    integer, parameter :: QT_KEY_F20 = 16777283
    integer, parameter :: QT_KEY_F21 = 16777284
    integer, parameter :: QT_KEY_F22 = 16777285
    integer, parameter :: QT_KEY_F23 = 16777286
    integer, parameter :: QT_KEY_F24 = 16777287
    integer, parameter :: QT_KEY_F25 = 16777288
    integer, parameter :: QT_KEY_F26 = 16777289
    integer, parameter :: QT_KEY_F27 = 16777290
    integer, parameter :: QT_KEY_F28 = 16777291
    integer, parameter :: QT_KEY_F29 = 16777292
    integer, parameter :: QT_KEY_F30 = 16777293
    integer, parameter :: QT_KEY_F31 = 16777294
    integer, parameter :: QT_KEY_F32 = 16777295
    integer, parameter :: QT_KEY_F33 = 16777296
    integer, parameter :: QT_KEY_F34 = 16777297
    integer, parameter :: QT_KEY_F35 = 16777298
    integer, parameter :: QT_KEY_F36 = 16777299
    integer, parameter :: QT_KEY_SUPER_L = 16777299
    integer, parameter :: QT_KEY_SUPER_R = 16777300
    integer, parameter :: QT_KEY_MENU = 16777301
    integer, parameter :: QT_KEY_HYPER_L = 16777302
    integer, parameter :: QT_KEY_HYPER_R = 16777303
    integer, parameter :: QT_KEY_HELP = 16777304
    integer, parameter :: QT_KEY_DIRECTION_L = 16777305
    integer, parameter :: QT_KEY_DIRECTION_R = 16777312
    integer, parameter :: QT_KEY_SPACE = 32
    integer, parameter :: QT_KEY_EXCLAM = 33
    integer, parameter :: QT_KEY_QUOTEDBL = 34
    integer, parameter :: QT_KEY_NUMBERSIGN = 35
    integer, parameter :: QT_KEY_DOLLAR = 36
    integer, parameter :: QT_KEY_PERCENT = 37
    integer, parameter :: QT_KEY_AMPERSAND = 38
    integer, parameter :: QT_KEY_APOSTROPHE = 39
    integer, parameter :: QT_KEY_PARENLEFT = 40
    integer, parameter :: QT_KEY_PARENRIGHT = 41
    integer, parameter :: QT_KEY_ASTERISK = 42
    integer, parameter :: QT_KEY_PLUS = 43
    integer, parameter :: QT_KEY_COMMA = 44
    integer, parameter :: QT_KEY_MINUS = 45
    integer, parameter :: QT_KEY_PERIOD = 46
    integer, parameter :: QT_KEY_SLASH = 47
    integer, parameter :: QT_KEY_0 = 48
    integer, parameter :: QT_KEY_1 = 49
    integer, parameter :: QT_KEY_2 = 50
    integer, parameter :: QT_KEY_3 = 51
    integer, parameter :: QT_KEY_4 = 52
    integer, parameter :: QT_KEY_5 = 53
    integer, parameter :: QT_KEY_6 = 54
    integer, parameter :: QT_KEY_7 = 55
    integer, parameter :: QT_KEY_8 = 56
    integer, parameter :: QT_KEY_9 = 57
    integer, parameter :: QT_KEY_COLON = 58
    integer, parameter :: QT_KEY_SEMICOLON = 59
    integer, parameter :: QT_KEY_LESS = 60
    integer, parameter :: QT_KEY_EQUAL = 61
    integer, parameter :: QT_KEY_GREATER = 62
    integer, parameter :: QT_KEY_QUESTION = 63
    integer, parameter :: QT_KEY_AT = 64
    integer, parameter :: QT_KEY_A = 65
    integer, parameter :: QT_KEY_B = 66
    integer, parameter :: QT_KEY_C = 67
    integer, parameter :: QT_KEY_D = 68
    integer, parameter :: QT_KEY_E = 69
    integer, parameter :: QT_KEY_F = 70
    integer, parameter :: QT_KEY_G = 71
    integer, parameter :: QT_KEY_H = 72
    integer, parameter :: QT_KEY_I = 73
    integer, parameter :: QT_KEY_J = 74
    integer, parameter :: QT_KEY_K = 75
    integer, parameter :: QT_KEY_L = 76
    integer, parameter :: QT_KEY_M = 77
    integer, parameter :: QT_KEY_N = 78
    integer, parameter :: QT_KEY_O = 79
    integer, parameter :: QT_KEY_P = 80
    integer, parameter :: QT_KEY_Q = 81
    integer, parameter :: QT_KEY_R = 82
    integer, parameter :: QT_KEY_S = 83
    integer, parameter :: QT_KEY_T = 84
    integer, parameter :: QT_KEY_U = 85
    integer, parameter :: QT_KEY_V = 86
    integer, parameter :: QT_KEY_W = 87
    integer, parameter :: QT_KEY_X = 88
    integer, parameter :: QT_KEY_Y = 89
    integer, parameter :: QT_KEY_Z = 90
    integer, parameter :: QT_KEY_BRACKETLEFT = 91
    integer, parameter :: QT_KEY_BACKSLASH = 92
    integer, parameter :: QT_KEY_BRACKETRIGHT = 93
    integer, parameter :: QT_KEY_ASCIICIRCUM = 94
    integer, parameter :: QT_KEY_UNDERSCORE = 95
    integer, parameter :: QT_KEY_QUOTELEFT = 96
    integer, parameter :: QT_KEY_BRACELEFT = 123
    integer, parameter :: QT_KEY_BAR = 124
    integer, parameter :: QT_KEY_BRACERIGHT = 125
    integer, parameter :: QT_KEY_ASCIITILDE = 126

    !> Mouse buttons
    integer, parameter :: QT_LEFTBUTTON = 1
    integer, parameter :: QT_RIGHTBUTTON = 2
    integer, parameter :: QT_MIDBUTTON = 4
    integer, parameter :: QT_XBUTTON1 = 8
    integer, parameter :: QT_XBUTTON2 = 16

    !> Keyboard modifiers
    integer, parameter :: QT_NOMODIFIER = 0
    integer, parameter :: QT_SHIFTMODIFIER = 33554432
    integer, parameter :: QT_CONTROLMODIFIER = 67108864
    integer, parameter :: QT_ALTMODIFIER = 134217728
    integer, parameter :: QT_METAMODIFIER = 268435456
    integer, parameter :: QT_KEYPADMODIFIER = 536870912
    integer, parameter :: QT_GROUPMODIFIERSHIFT = 24

    !> Test data types
    integer, parameter :: QTEST_TYPE_BOOL = 1
    integer, parameter :: QTEST_TYPE_INT = 2
    integer, parameter :: QTEST_TYPE_UINT = 3
    integer, parameter :: QTEST_TYPE_LONG = 4
    integer, parameter :: QTEST_TYPE_ULONG = 5
    integer, parameter :: QTEST_TYPE_DOUBLE = 6
    integer, parameter :: QTEST_TYPE_FLOAT = 7
    integer, parameter :: QTEST_TYPE_CHAR = 8
    integer, parameter :: QTEST_TYPE_STRING = 9
    integer, parameter :: QTEST_TYPE_QPOINT = 10
    integer, parameter :: QTEST_TYPE_QPOINTF = 11
    integer, parameter :: QTEST_TYPE_QSIZE = 12
    integer, parameter :: QTEST_TYPE_QSIZEF = 13
    integer, parameter :: QTEST_TYPE_QRECT = 14
    integer, parameter :: QTEST_TYPE_QRECTF = 15

    !> Base test object class
    type :: qtest_object
        private
        type(c_ptr) :: ptr = c_null_ptr
        character(len=:), allocatable :: name
        integer :: test_count = 0
        integer :: pass_count = 0
        integer :: fail_count = 0
        integer :: skip_count = 0
    contains
        procedure :: init => qtest_object_init
        procedure :: cleanup => qtest_object_cleanup
        procedure :: run => qtest_object_run
        procedure :: get_name => qtest_object_get_name
        procedure :: get_test_count => qtest_object_get_test_count
        procedure :: get_pass_count => qtest_object_get_pass_count
        procedure :: get_fail_count => qtest_object_get_fail_count
        procedure :: get_skip_count => qtest_object_get_skip_count
        procedure :: is_valid => qtest_object_is_valid
    end type qtest_object

    !> Test case class
    type, extends(qtest_object) :: qtest_case
        private
        type(qtestdata) :: current_data
        logical :: has_data = .false.
    contains
        procedure :: set_data => qtest_case_set_data
        procedure :: has_current_data => qtest_case_has_current_data
        procedure :: fetch_data => qtest_case_fetch_data
        procedure :: init_test_case => qtest_case_init_test_case
        procedure :: cleanup_test_case => qtest_case_cleanup_test_case
        procedure :: init_test => qtest_case_init_test
        procedure :: cleanup_test => qtest_case_cleanup_test
    end type qtest_case

    !> Test suite class
    type :: qtest_suite
        private
        type(qtest_object), dimension(:), allocatable :: tests
        character(len=:), allocatable :: name
        integer :: test_count = 0
    contains
        procedure :: init => qtest_suite_init
        procedure :: add_test => qtest_suite_add_test
        procedure :: run_all => qtest_suite_run_all
        procedure :: get_name => qtest_suite_get_name
        procedure :: get_test_count => qtest_suite_get_test_count
    end type qtest_suite

    !> Signal spy class
    type :: qsignal_spy
        private
        type(qsignalspy) :: spy
    contains
        procedure :: init => qsignal_spy_init
        procedure :: cleanup => qsignal_spy_cleanup
        procedure :: count => qsignal_spy_count
        procedure :: wait => qsignal_spy_wait
        procedure :: is_valid => qsignal_spy_is_valid
        procedure :: signal => qsignal_spy_signal
        procedure :: take_first => qsignal_spy_take_first
        procedure :: take_last => qsignal_spy_take_last
        procedure :: at => qsignal_spy_at
    end type qsignal_spy

    !> Benchmark measurement class
    type :: qbenchmark_measurement
        private
        type(qbenchmarkmeasurement) :: measurement
    contains
        procedure :: init => qbenchmark_measurement_init
        procedure :: cleanup => qbenchmark_measurement_cleanup
        procedure :: start => qbenchmark_measurement_start
        procedure :: stop => qbenchmark_measurement_stop
        procedure :: elapsed => qbenchmark_measurement_elapsed
        procedure :: iterations => qbenchmark_measurement_iterations
    end type qbenchmark_measurement

    !> Test assertion class
    type :: qtest_assertion
        private
        character(len=:), allocatable :: message
        character(len=:), allocatable :: file
        integer :: line = 0
        integer :: result = QTEST_PASS
    contains
        procedure :: init => qtest_assertion_init
        procedure :: get_message => qtest_assertion_get_message
        procedure :: get_file => qtest_assertion_get_file
        procedure :: get_line => qtest_assertion_get_line
        procedure :: get_result => qtest_assertion_get_result
        procedure :: set_result => qtest_assertion_set_result
    end type qtest_assertion

    !> Test data table class
    type :: qtest_data_table
        private
        type(qtesttable) :: table
        character(len=:), allocatable :: name
    contains
        procedure :: init => qtest_data_table_init
        procedure :: cleanup => qtest_data_table_cleanup
        procedure :: add_column => qtest_data_table_add_column
        procedure :: new_row => qtest_data_table_new_row
        procedure :: fetch => qtest_data_table_fetch
        procedure :: get_name => qtest_data_table_get_name
    end type qtest_data_table

    !> Event simulator class
    type :: qtest_event_simulator
        private
    contains
        procedure :: key_click => qtest_event_simulator_key_click
        procedure :: key_press => qtest_event_simulator_key_press
        procedure :: key_release => qtest_event_simulator_key_release
        procedure :: mouse_click => qtest_event_simulator_mouse_click
        procedure :: mouse_press => qtest_event_simulator_mouse_press
        procedure :: mouse_release => qtest_event_simulator_mouse_release
        procedure :: mouse_move => qtest_event_simulator_mouse_move
        procedure :: mouse_wheel => qtest_event_simulator_mouse_wheel
        procedure :: touch_begin => qtest_event_simulator_touch_begin
        procedure :: touch_update => qtest_event_simulator_touch_update
        procedure :: touch_end => qtest_event_simulator_touch_end
        procedure :: touch_cancel => qtest_event_simulator_touch_cancel
    end type qtest_event_simulator

contains

    !> Initialize Qt Test framework
    subroutine qtest_init()
        ! Initialize Qt Test framework
        ! This would typically set up the test environment
    end subroutine qtest_init

    !> Cleanup Qt Test framework
    subroutine qtest_cleanup()
        ! Cleanup Qt Test framework
        ! This would typically clean up test resources
    end subroutine qtest_cleanup

    !> Run a single test object
    function qtest_run(test) result(success)
        class(qtest_object), intent(inout) :: test
        logical :: success
        success = test%run()
    end function qtest_run

    !> Run all tests in a test suite
    function qtest_run_all(suite) result(success)
        type(qtest_suite), intent(inout) :: suite
        logical :: success
        success = suite%run_all()
    end function qtest_run_all

    !> Generic assertion
    subroutine qtest_assert(condition, message, file, line)
        logical, intent(in) :: condition
        character(len=*), intent(in), optional :: message
        character(len=*), intent(in), optional :: file
        integer, intent(in), optional :: line

        character(len=:), allocatable :: msg, f
        integer :: l

        if (.not. condition) then
            msg = "Assertion failed"
            if (present(message)) msg = message
            f = "unknown"
            if (present(file)) f = file
            l = 0
            if (present(line)) l = line

            call qtest_qfail(trim(msg)//c_null_char, trim(f)//c_null_char, l)
        end if
    end subroutine qtest_assert

    !> Assert equality
    subroutine qtest_assert_equal(actual, expected, message, file, line)
        class(*), intent(in) :: actual, expected
        character(len=*), intent(in), optional :: message
        character(len=*), intent(in), optional :: file
        integer, intent(in), optional :: line

        character(len=:), allocatable :: msg, f
        integer :: l

        select type (actual)
        type is (integer)
            select type (expected)
            type is (integer)
                if (actual /= expected) then
                    msg = "Values not equal"
                    if (present(message)) msg = message
                    f = "unknown"
                    if (present(file)) f = file
                    l = 0
                    if (present(line)) l = line
                    call qtest_qfail(trim(msg)//c_null_char, trim(f)//c_null_char, l)
                end if
            end select
        type is (real)
            select type (expected)
            type is (real)
                if (actual /= expected) then
                    msg = "Values not equal"
                    if (present(message)) msg = message
                    f = "unknown"
                    if (present(file)) f = file
                    l = 0
                    if (present(line)) l = line
                    call qtest_qfail(trim(msg)//c_null_char, trim(f)//c_null_char, l)
                end if
            end select
        type is (character(len=*))
            select type (expected)
            type is (character(len=*))
                if (actual /= expected) then
                    msg = "Strings not equal"
                    if (present(message)) msg = message
                    f = "unknown"
                    if (present(file)) f = file
                    l = 0
                    if (present(line)) l = line
                    call qtest_qfail(trim(msg)//c_null_char, trim(f)//c_null_char, l)
                end if
            end select
        type is (logical)
            select type (expected)
            type is (logical)
                if (actual .neqv. expected) then
                    msg = "Booleans not equal"
                    if (present(message)) msg = message
                    f = "unknown"
                    if (present(file)) f = file
                    l = 0
                    if (present(line)) l = line
                    call qtest_qfail(trim(msg)//c_null_char, trim(f)//c_null_char, l)
                end if
            end select
        end select
    end subroutine qtest_assert_equal

    !> Assert inequality
    subroutine qtest_assert_not_equal(actual, expected, message, file, line)
        class(*), intent(in) :: actual, expected
        character(len=*), intent(in), optional :: message
        character(len=*), intent(in), optional :: file
        integer, intent(in), optional :: line

        character(len=:), allocatable :: msg, f
        integer :: l

        select type (actual)
        type is (integer)
            select type (expected)
            type is (integer)
                if (actual == expected) then
                    msg = "Values are equal"
                    if (present(message)) msg = message
                    f = "unknown"
                    if (present(file)) f = file
                    l = 0
                    if (present(line)) l = line
                    call qtest_qfail(trim(msg)//c_null_char, trim(f)//c_null_char, l)
                end if
            end select
        type is (real)
            select type (expected)
            type is (real)
                if (actual == expected) then
                    msg = "Values are equal"
                    if (present(message)) msg = message
                    f = "unknown"
                    if (present(file)) f = file
                    l = 0
                    if (present(line)) l = line
                    call qtest_qfail(trim(msg)//c_null_char, trim(f)//c_null_char, l)
                end if
            end select
        type is (character(len=*))
            select type (expected)
            type is (character(len=*))
                if (actual == expected) then
                    msg = "Strings are equal"
                    if (present(message)) msg = message
                    f = "unknown"
                    if (present(file)) f = file
                    l = 0
                    if (present(line)) l = line
                    call qtest_qfail(trim(msg)//c_null_char, trim(f)//c_null_char, l)
                end if
            end select
        type is (logical)
            select type (expected)
            type is (logical)
                if (actual .eqv. expected) then
                    msg = "Booleans are equal"
                    if (present(message)) msg = message
                    f = "unknown"
                    if (present(file)) f = file
                    l = 0
                    if (present(line)) l = line
                    call qtest_qfail(trim(msg)//c_null_char, trim(f)//c_null_char, l)
                end if
            end select
        end select
    end subroutine qtest_assert_not_equal

    !> Assert true
    subroutine qtest_assert_true(condition, message, file, line)
        logical, intent(in) :: condition
        character(len=*), intent(in), optional :: message
        character(len=*), intent(in), optional :: file
        integer, intent(in), optional :: line

        character(len=:), allocatable :: msg, f
        integer :: l

        if (.not. condition) then
            msg = "Expected true, got false"
            if (present(message)) msg = message
            f = "unknown"
            if (present(file)) f = file
            l = 0
            if (present(line)) l = line
            call qtest_qfail(trim(msg)//c_null_char, trim(f)//c_null_char, l)
        end if
    end subroutine qtest_assert_true

    !> Assert false
    subroutine qtest_assert_false(condition, message, file, line)
        logical, intent(in) :: condition
        character(len=*), intent(in), optional :: message
        character(len=*), intent(in), optional :: file
        integer, intent(in), optional :: line

        character(len=:), allocatable :: msg, f
        integer :: l

        if (condition) then
            msg = "Expected false, got true"
            if (present(message)) msg = message
            f = "unknown"
            if (present(file)) f = file
            l = 0
            if (present(line)) l = line
            call qtest_qfail(trim(msg)//c_null_char, trim(f)//c_null_char, l)
        end if
    end subroutine qtest_assert_false

    !> Assert null
    subroutine qtest_assert_null(ptr, message, file, line)
        type(c_ptr), intent(in) :: ptr
        character(len=*), intent(in), optional :: message
        character(len=*), intent(in), optional :: file
        integer, intent(in), optional :: line

        character(len=:), allocatable :: msg, f
        integer :: l

        if (.not. c_associated(ptr)) then
            msg = "Pointer is not null"
            if (present(message)) msg = message
            f = "unknown"
            if (present(file)) f = file
            l = 0
            if (present(line)) l = line
            call qtest_qfail(trim(msg)//c_null_char, trim(f)//c_null_char, l)
        end if
    end subroutine qtest_assert_null

    !> Assert not null
    subroutine qtest_assert_not_null(ptr, message, file, line)
        type(c_ptr), intent(in) :: ptr
        character(len=*), intent(in), optional :: message
        character(len=*), intent(in), optional :: file
        integer, intent(in), optional :: line

        character(len=:), allocatable :: msg, f
        integer :: l

        if (c_associated(ptr)) then
            msg = "Pointer is null"
            if (present(message)) msg = message
            f = "unknown"
            if (present(file)) f = file
            l = 0
            if (present(line)) l = line
            call qtest_qfail(trim(msg)//c_null_char, trim(f)//c_null_char, l)
        end if
    end subroutine qtest_assert_not_null

    !> Skip test
    subroutine qtest_skip(message, file, line)
        character(len=*), intent(in), optional :: message
        character(len=*), intent(in), optional :: file
        integer, intent(in), optional :: line

        character(len=:), allocatable :: msg, f
        integer :: l

        msg = "Test skipped"
        if (present(message)) msg = message
        f = "unknown"
        if (present(file)) f = file
        l = 0
        if (present(line)) l = line

        call qtest_qskip(trim(msg)//c_null_char, trim(f)//c_null_char, l)
    end subroutine qtest_skip

    !> Fail test
    subroutine qtest_fail(message, file, line)
        character(len=*), intent(in), optional :: message
        character(len=*), intent(in), optional :: file
        integer, intent(in), optional :: line

        character(len=:), allocatable :: msg, f
        integer :: l

        msg = "Test failed"
        if (present(message)) msg = message
        f = "unknown"
        if (present(file)) f = file
        l = 0
        if (present(line)) l = line

        call qtest_qfail(trim(msg)//c_null_char, trim(f)//c_null_char, l)
    end subroutine qtest_fail

    !> Expect test to fail
    subroutine qtest_expect_fail(data_index, comment, mode, file, line)
        character(len=*), intent(in) :: data_index
        character(len=*), intent(in) :: comment
        integer, intent(in) :: mode
        character(len=*), intent(in), optional :: file
        integer, intent(in), optional :: line

        character(len=:), allocatable :: f
        integer :: l

        f = "unknown"
        if (present(file)) f = file
        l = 0
        if (present(line)) l = line

        call qtest_qexpect_fail(trim(data_index)//c_null_char, trim(comment)//c_null_char, mode, trim(f)//c_null_char, l)
    end subroutine qtest_expect_fail

    !> Warning message
    subroutine qtest_warn(message)
        character(len=*), intent(in) :: message
        call qtest_qwarn(trim(message)//c_null_char)
    end subroutine qtest_warn

    !> Benchmark block
    subroutine qtest_benchmark(block, file, line)
        procedure() :: block
        character(len=*), intent(in), optional :: file
        integer, intent(in), optional :: line

        character(len=:), allocatable :: f
        integer :: l

        f = "unknown"
        if (present(file)) f = file
        l = 0
        if (present(line)) l = line

        call qtest_qbenchmark(c_funloc(block), trim(f)//c_null_char, l)
    end subroutine qtest_benchmark

    !> Benchmark once
    subroutine qtest_benchmark_once(block, file, line)
        procedure() :: block
        character(len=*), intent(in), optional :: file
        integer, intent(in), optional :: line

        character(len=:), allocatable :: f
        integer :: l

        f = "unknown"
        if (present(file)) f = file
        l = 0
        if (present(line)) l = line

        call qtest_qbenchmark_once(c_funloc(block), trim(f)//c_null_char, l)
    end subroutine qtest_benchmark_once

    !> Sleep for milliseconds
    subroutine qtest_sleep(ms)
        integer, intent(in) :: ms
        call qtest_qsleep(ms)
    end subroutine qtest_sleep

    !> Wait for milliseconds
    subroutine qtest_wait(ms)
        integer, intent(in) :: ms
        call qtest_qwait(ms)
    end subroutine qtest_wait

    !> Wait for condition
    function qtest_wait_for(condition, timeout) result(success)
        procedure(logical) :: condition
        integer, intent(in) :: timeout
        logical :: success

        success = logical(qtest_qwaitfor(c_funloc(condition), timeout))
    end function qtest_wait_for

    !> Wait for window to be active
    function qtest_wait_for_window_active(window, timeout) result(success)
        type(qwidget), intent(in) :: window
        integer, intent(in) :: timeout
        logical :: success

        success = logical(qtest_qwaitforwindowactive(window%ptr, timeout))
    end function qtest_wait_for_window_active

    !> Wait for window to be shown
    function qtest_wait_for_window_shown(window, timeout) result(success)
        type(qwidget), intent(in) :: window
        integer, intent(in) :: timeout
        logical :: success

        success = logical(qtest_qwaitforwindowshown(window%ptr, timeout))
    end function qtest_wait_for_window_shown

    !> Wait for window to be closed
    function qtest_wait_for_window_closed(window, timeout) result(success)
        type(qwidget), intent(in) :: window
        integer, intent(in) :: timeout
        logical :: success

        success = logical(qtest_qwaitforwindowclosed(window%ptr, timeout))
    end function qtest_wait_for_window_closed

    !> Wait for window to be exposed
    function qtest_wait_for_window_exposed(window, timeout) result(success)
        type(qwidget), intent(in) :: window
        integer, intent(in) :: timeout
        logical :: success

        success = logical(qtest_qwaitforwindowexposed(window%ptr, timeout))
    end function qtest_wait_for_window_exposed

    !> Simulate key click
    subroutine qtest_key_click(widget, key, modifier, delay)
        type(qwidget), intent(in) :: widget
        integer, intent(in) :: key
        integer, intent(in), optional :: modifier
        integer, intent(in), optional :: delay

        integer :: mod, del

        mod = QT_NOMODIFIER
        if (present(modifier)) mod = modifier
        del = -1
        if (present(delay)) del = delay

        call qtest_keyclicks(widget%ptr, char(key), mod, del)
    end subroutine qtest_key_click

    !> Simulate key press
    subroutine qtest_key_press(widget, key, modifier, delay)
        type(qwidget), intent(in) :: widget
        integer, intent(in) :: key
        integer, intent(in), optional :: modifier
        integer, intent(in), optional :: delay

        integer :: mod, del

        mod = QT_NOMODIFIER
        if (present(modifier)) mod = modifier
        del = -1
        if (present(delay)) del = delay

        call qtest_keypress(widget%ptr, key, mod, del)
    end subroutine qtest_key_press

    !> Simulate key release
    subroutine qtest_key_release(widget, key, modifier, delay)
        type(qwidget), intent(in) :: widget
        integer, intent(in) :: key
        integer, intent(in), optional :: modifier
        integer, intent(in), optional :: delay

        integer :: mod, del

        mod = QT_NOMODIFIER
        if (present(modifier)) mod = modifier
        del = -1
        if (present(delay)) del = delay

        call qtest_keyrelease(widget%ptr, key, mod, del)
    end subroutine qtest_key_release

    !> Simulate mouse click
    subroutine qtest_mouse_click(widget, button, modifier, pos, delay)
        type(qwidget), intent(in) :: widget
        integer, intent(in) :: button
        integer, intent(in), optional :: modifier
        integer, intent(in), dimension(2), optional :: pos
        integer, intent(in), optional :: delay

        integer :: mod, del
        integer, dimension(2) :: p

        mod = QT_NOMODIFIER
        if (present(modifier)) mod = modifier
        p = [0, 0]
        if (present(pos)) p = pos
        del = -1
        if (present(delay)) del = delay

        call qtest_mouseclick(widget%ptr, button, mod, c_loc(p), del)
    end subroutine qtest_mouse_click

    !> Simulate mouse press
    subroutine qtest_mouse_press(widget, button, modifier, pos, delay)
        type(qwidget), intent(in) :: widget
        integer, intent(in) :: button
        integer, intent(in), optional :: modifier
        integer, intent(in), dimension(2), optional :: pos
        integer, intent(in), optional :: delay

        integer :: mod, del
        integer, dimension(2) :: p

        mod = QT_NOMODIFIER
        if (present(modifier)) mod = modifier
        p = [0, 0]
        if (present(pos)) p = pos
        del = -1
        if (present(delay)) del = delay

        call qtest_mousepress(widget%ptr, button, mod, c_loc(p), del)
    end subroutine qtest_mouse_press

    !> Simulate mouse release
    subroutine qtest_mouse_release(widget, button, modifier, pos, delay)
        type(qwidget), intent(in) :: widget
        integer, intent(in) :: button
        integer, intent(in), optional :: modifier
        integer, intent(in), dimension(2), optional :: pos
        integer, intent(in), optional :: delay

        integer :: mod, del
        integer, dimension(2) :: p

        mod = QT_NOMODIFIER
        if (present(modifier)) mod = modifier
        p = [0, 0]
        if (present(pos)) p = pos
        del = -1
        if (present(delay)) del = delay

        call qtest_mouserelease(widget%ptr, button, mod, c_loc(p), del)
    end subroutine qtest_mouse_release

    !> Simulate mouse move
    subroutine qtest_mouse_move(widget, pos, delay)
        type(qwidget), intent(in) :: widget
        integer, intent(in), dimension(2) :: pos
        integer, intent(in), optional :: delay

        integer :: del

        del = -1
        if (present(delay)) del = delay

        call qtest_mousemove(widget%ptr, c_loc(pos), del)
    end subroutine qtest_mouse_move

    !> Simulate mouse wheel
    subroutine qtest_mouse_wheel(widget, pos, delta, button, modifier, delay)
        type(qwidget), intent(in) :: widget
        integer, intent(in), dimension(2) :: pos
        integer, intent(in) :: delta
        integer, intent(in), optional :: button
        integer, intent(in), optional :: modifier
        integer, intent(in), optional :: delay

        integer :: btn, mod, del

        btn = QT_NOMODIFIER
        if (present(button)) btn = button
        mod = QT_NOMODIFIER
        if (present(modifier)) mod = modifier
        del = -1
        if (present(delay)) del = delay

        call qtest_mousewheel(widget%ptr, c_loc(pos), delta, btn, mod, del)
    end subroutine qtest_mouse_wheel

    !> Simulate touch begin
    subroutine qtest_touch_begin(widget, touch_id, pos, device)
        type(qwidget), intent(in) :: widget
        integer, intent(in) :: touch_id
        integer, intent(in), dimension(2) :: pos
        type(c_ptr), intent(in), optional :: device

        type(c_ptr) :: dev

        dev = c_null_ptr
        if (present(device)) dev = device

        call qtest_touchbegin(widget%ptr, touch_id, c_loc(pos), dev)
    end subroutine qtest_touch_begin

    !> Simulate touch update
    subroutine qtest_touch_update(widget, touch_id, pos, device)
        type(qwidget), intent(in) :: widget
        integer, intent(in) :: touch_id
        integer, intent(in), dimension(2) :: pos
        type(c_ptr), intent(in), optional :: device

        type(c_ptr) :: dev

        dev = c_null_ptr
        if (present(device)) dev = device

        call qtest_touchupdate(widget%ptr, touch_id, c_loc(pos), dev)
    end subroutine qtest_touch_update

    !> Simulate touch end
    subroutine qtest_touch_end(widget, touch_id, device)
        type(qwidget), intent(in) :: widget
        integer, intent(in) :: touch_id
        type(c_ptr), intent(in), optional :: device

        type(c_ptr) :: dev

        dev = c_null_ptr
        if (present(device)) dev = device

        call qtest_touchend(widget%ptr, touch_id, dev)
    end subroutine qtest_touch_end

    !> Simulate touch cancel
    subroutine qtest_touch_cancel(widget, touch_id, device)
        type(qwidget), intent(in) :: widget
        integer, intent(in) :: touch_id
        type(c_ptr), intent(in), optional :: device

        type(c_ptr) :: dev

        dev = c_null_ptr
        if (present(device)) dev = device

        call qtest_touchcancel(widget%ptr, touch_id, dev)
    end subroutine qtest_touch_cancel

    !> Add column to data table
    subroutine qtest_data_add_column(table, name, type_id)
        type(qtesttable), intent(in) :: table
        character(len=*), intent(in) :: name
        integer, intent(in) :: type_id

        call qtest_addcolumn(table%ptr, trim(name)//c_null_char, type_id)
    end subroutine qtest_data_add_column

    !> Add new row to data table
    subroutine qtest_data_new_row(table, name)
        type(qtesttable), intent(in) :: table
        character(len=*), intent(in) :: name

        call qtest_newrow(table%ptr, trim(name)//c_null_char)
    end subroutine qtest_data_new_row

    !> Fetch data from current row
    function qtest_data_fetch(table, column) result(data)
        type(qtesttable), intent(in) :: table
        character(len=*), intent(in) :: column
        type(c_ptr) :: data

        data = qtest_fetch(table%ptr, trim(column)//c_null_char)
    end function qtest_data_fetch

    !> Create signal spy
    subroutine qtest_spy_create(spy, object, signal)
        type(qsignal_spy), intent(out) :: spy
        type(c_ptr), intent(in) :: object
        character(len=*), intent(in) :: signal

        spy%spy%ptr = qsignalspy_new(object, trim(signal)//c_null_char)
    end subroutine qtest_spy_create

    !> Get signal count
    function qtest_spy_count(spy) result(count)
        type(qsignal_spy), intent(in) :: spy
        integer :: count

        count = qsignalspy_count(spy%spy%ptr)
    end function qtest_spy_count

    !> Wait for signal
    function qtest_spy_wait(spy, timeout) result(success)
        type(qsignal_spy), intent(in) :: spy
        integer, intent(in) :: timeout
        logical :: success

        success = logical(qsignalspy_wait(spy%spy%ptr, timeout))
    end function qtest_spy_wait

    !> Check if spy is valid
    function qtest_spy_is_valid(spy) result(valid)
        type(qsignal_spy), intent(in) :: spy
        logical :: valid

        valid = logical(qsignalspy_isvalid(spy%spy%ptr))
    end function qtest_spy_is_valid

    !> Get signal signature
    function qtest_spy_signal(spy) result(signal)
        type(qsignal_spy), intent(in) :: spy
        character(len=:), allocatable :: signal

        character(len=256) :: buffer

        ! Simplified implementation
        signal = "signal"
    end function qtest_spy_signal

    !> Take first signal
    function qtest_spy_take_first(spy) result(signal)
        type(qsignal_spy), intent(in) :: spy
        type(c_ptr) :: signal

        signal = qsignalspy_takefirst(spy%spy%ptr)
    end function qtest_spy_take_first

    !> Take last signal
    function qtest_spy_take_last(spy) result(signal)
        type(qsignal_spy), intent(in) :: spy
        type(c_ptr) :: signal

        signal = qsignalspy_takelast(spy%spy%ptr)
    end function qtest_spy_take_last

    !> Get signal at index
    function qtest_spy_at(spy, index) result(signal)
        type(qsignal_spy), intent(in) :: spy
        integer, intent(in) :: index
        type(c_ptr) :: signal

        signal = qsignalspy_at(spy%spy%ptr, index)
    end function qtest_spy_at

    !> Start benchmark measurement
    subroutine qtest_benchmark_start(measurement)
        type(qbenchmark_measurement), intent(in) :: measurement

        call qbenchmarkmeasurement_start(measurement%measurement%ptr)
    end subroutine qtest_benchmark_start

    !> Stop benchmark measurement
    subroutine qtest_benchmark_stop(measurement)
        type(qbenchmark_measurement), intent(in) :: measurement

        call qbenchmarkmeasurement_stop(measurement%measurement%ptr)
    end subroutine qtest_benchmark_stop

    !> Get elapsed time
    function qtest_benchmark_elapsed(measurement) result(time)
        type(qbenchmark_measurement), intent(in) :: measurement
        integer(c_long_long) :: time

        time = qbenchmarkmeasurement_elapsed(measurement%measurement%ptr)
    end function qtest_benchmark_elapsed

    !> Get iterations count
    function qtest_benchmark_iterations(measurement) result(count)
        type(qbenchmark_measurement), intent