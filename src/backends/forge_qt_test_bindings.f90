!> @brief C bindings for Qt Test framework
!> @details Provides Fortran interfaces to Qt Test C API
!> @author ForGE Contributors
!> @date 2025
!> @license GPL-3.0-or-later

module forge_qt_test_bindings
    use iso_c_binding
    implicit none
    private

    public :: qtest, qtestevent, qtesteventlist, qtestmouseevent, qtestkeyevent
    public :: qtestkeyclicks, qtestkeypress, qtestkeyrelease, qtestmouseclick
    public :: qtestmousepress, qtestmouserelease, qtestmousemove, qtestmousewheel
    public :: qtesttouchbegin, qtesttouchupdate, qtesttouchend, qtesttouchcancel
    public :: qsignalspy, qtestdata, qtesttable, qbenchmarkmeasurement
    public :: qtest_new, qtest_exec, qtest_qexec, qtest_addcolumn, qtest_newrow
    public :: qtest_fetch, qtest_qcompare, qtest_qverify, qtest_qfail, qtest_qskip
    public :: qtest_qexpect_fail, qtest_qwarn, qtest_qbenchmark, qtest_qbenchmark_once
    public :: qtest_qsleep, qtest_qwait, qtest_qwaitforwindowactive, qtest_qwaitforwindowexposed
    public :: qtest_qwaitfor, qtest_qwaitforwindowshown, qtest_qwaitforwindowclosed
    public :: qtest_qwaitforwindowactivated, qtest_qwaitforwindowdeactivated
    public :: qtest_qwaitforwindowexpose, qtest_qwaitforwindowunexposed
    public :: qtest_qwaitfor_timeout, qtest_qsleep_timeout, qtest_qwait_timeout
    public :: qtest_qwaitforwindowactive_timeout, qtest_qwaitforwindowexposed_timeout
    public :: qtest_qwaitforwindowshown_timeout, qtest_qwaitforwindowclosed_timeout
    public :: qtest_qwaitforwindowactivated_timeout, qtest_qwaitforwindowdeactivated_timeout
    public :: qtest_qwaitforwindowexpose_timeout, qtest_qwaitforwindowunexposed_timeout

    !> Opaque Qt Test handles
    type :: qtest
        type(c_ptr) :: ptr = c_null_ptr
    end type qtest

    type :: qtestevent
        type(c_ptr) :: ptr = c_null_ptr
    end type qtestevent

    type :: qtesteventlist
        type(c_ptr) :: ptr = c_null_ptr
    end type qtesteventlist

    type :: qtestmouseevent
        type(c_ptr) :: ptr = c_null_ptr
    end type qtestmouseevent

    type :: qtestkeyevent
        type(c_ptr) :: ptr = c_null_ptr
    end type qtestkeyevent

    type :: qsignalspy
        type(c_ptr) :: ptr = c_null_ptr
    end type qsignalspy

    type :: qtestdata
        type(c_ptr) :: ptr = c_null_ptr
    end type qtestdata

    type :: qtesttable
        type(c_ptr) :: ptr = c_null_ptr
    end type qtesttable

    type :: qbenchmarkmeasurement
        type(c_ptr) :: ptr = c_null_ptr
    end type qbenchmarkmeasurement

    !> Qt Test C API bindings

    interface
        !> QTest::qExec - Execute test object
        function qtest_qexec(test_object, argc, argv) bind(c, name="qtest_qexec")
            import :: c_ptr, c_int, c_char
            type(c_ptr), value :: test_object
            integer(c_int), value :: argc
            type(c_ptr), dimension(*) :: argv
            integer(c_int) :: qtest_qexec
        end function qtest_qexec

        !> QTest::addColumn - Add column to data table
        subroutine qtest_addcolumn(table, name, type_id) bind(c, name="qtest_addcolumn")
            import :: c_ptr, c_char, c_int
            type(c_ptr), value :: table
            character(kind=c_char), dimension(*) :: name
            integer(c_int), value :: type_id
        end subroutine qtest_addcolumn

        !> QTest::newRow - Add new row to data table
        subroutine qtest_newrow(table, name) bind(c, name="qtest_newrow")
            import :: c_ptr, c_char
            type(c_ptr), value :: table
            character(kind=c_char), dimension(*) :: name
        end subroutine qtest_newrow

        !> QTest::fetchData - Fetch data from current row
        function qtest_fetch(table, column) bind(c, name="qtest_fetch")
            import :: c_ptr, c_char
            type(c_ptr), value :: table
            character(kind=c_char), dimension(*) :: column
            type(c_ptr) :: qtest_fetch
        end function qtest_fetch

        !> QCOMPARE macro implementation
        subroutine qtest_qcompare(actual, expected, file, line) bind(c, name="qtest_qcompare")
            import :: c_ptr, c_char, c_int
            type(c_ptr), value :: actual
            type(c_ptr), value :: expected
            character(kind=c_char), dimension(*) :: file
            integer(c_int), value :: line
        end subroutine qtest_qcompare

        !> QVERIFY macro implementation
        subroutine qtest_qverify(condition, file, line) bind(c, name="qtest_qverify")
            import :: c_ptr, c_char, c_int
            type(c_ptr), value :: condition
            character(kind=c_char), dimension(*) :: file
            integer(c_int), value :: line
        end subroutine qtest_qverify

        !> QFAIL macro implementation
        subroutine qtest_qfail(message, file, line) bind(c, name="qtest_qfail")
            import :: c_char, c_int
            character(kind=c_char), dimension(*) :: message
            character(kind=c_char), dimension(*) :: file
            integer(c_int), value :: line
        end subroutine qtest_qfail

        !> QSKIP macro implementation
        subroutine qtest_qskip(message, file, line) bind(c, name="qtest_qskip")
            import :: c_char, c_int
            character(kind=c_char), dimension(*) :: message
            character(kind=c_char), dimension(*) :: file
            integer(c_int), value :: line
        end subroutine qtest_qskip

        !> QEXPECT_FAIL macro implementation
        subroutine qtest_qexpect_fail(data_index, comment, mode, file, line) bind(c, name="qtest_qexpect_fail")
            import :: c_char, c_int
            character(kind=c_char), dimension(*) :: data_index
            character(kind=c_char), dimension(*) :: comment
            integer(c_int), value :: mode
            character(kind=c_char), dimension(*) :: file
            integer(c_int), value :: line
        end subroutine qtest_qexpect_fail

        !> QWARN macro implementation
        subroutine qtest_qwarn(message) bind(c, name="qtest_qwarn")
            import :: c_char
            character(kind=c_char), dimension(*) :: message
        end subroutine qtest_qwarn

        !> QBENCHMARK macro implementation
        subroutine qtest_qbenchmark(block, file, line) bind(c, name="qtest_qbenchmark")
            import :: c_funptr, c_char, c_int
            type(c_funptr), value :: block
            character(kind=c_char), dimension(*) :: file
            integer(c_int), value :: line
        end subroutine qtest_qbenchmark

        !> QBENCHMARK_ONCE macro implementation
        subroutine qtest_qbenchmark_once(block, file, line) bind(c, name="qtest_qbenchmark_once")
            import :: c_funptr, c_char, c_int
            type(c_funptr), value :: block
            character(kind=c_char), dimension(*) :: file
            integer(c_int), value :: line
        end subroutine qtest_qbenchmark_once

        !> QTest::qSleep - Sleep for milliseconds
        subroutine qtest_qsleep(ms) bind(c, name="qtest_qsleep")
            import :: c_int
            integer(c_int), value :: ms
        end subroutine qtest_qsleep

        !> QTest::qWait - Wait for milliseconds
        subroutine qtest_qwait(ms) bind(c, name="qtest_qwait")
            import :: c_int
            integer(c_int), value :: ms
        end subroutine qtest_qwait

        !> QTest::qWaitFor - Wait for condition
        function qtest_qwaitfor(condition, timeout) bind(c, name="qtest_qwaitfor")
            import :: c_funptr, c_int, c_bool
            type(c_funptr), value :: condition
            integer(c_int), value :: timeout
            logical(c_bool) :: qtest_qwaitfor
        end function qtest_qwaitfor

        !> QTest::qWaitForWindowActive
        function qtest_qwaitforwindowactive(window, timeout) bind(c, name="qtest_qwaitforwindowactive")
            import :: c_ptr, c_int, c_bool
            type(c_ptr), value :: window
            integer(c_int), value :: timeout
            logical(c_bool) :: qtest_qwaitforwindowactive
        end function qtest_qwaitforwindowactive

        !> QTest::qWaitForWindowExposed
        function qtest_qwaitforwindowexposed(window, timeout) bind(c, name="qtest_qwaitforwindowexposed")
            import :: c_ptr, c_int, c_bool
            type(c_ptr), value :: window
            integer(c_int), value :: timeout
            logical(c_bool) :: qtest_qwaitforwindowexposed
        end function qtest_qwaitforwindowexposed

        !> QTest::qWaitForWindowShown
        function qtest_qwaitforwindowshown(window, timeout) bind(c, name="qtest_qwaitforwindowshown")
            import :: c_ptr, c_int, c_bool
            type(c_ptr), value :: window
            integer(c_int), value :: timeout
            logical(c_bool) :: qtest_qwaitforwindowshown
        end function qtest_qwaitforwindowshown

        !> QTest::qWaitForWindowClosed
        function qtest_qwaitforwindowclosed(window, timeout) bind(c, name="qtest_qwaitforwindowclosed")
            import :: c_ptr, c_int, c_bool
            type(c_ptr), value :: window
            integer(c_int), value :: timeout
            logical(c_bool) :: qtest_qwaitforwindowclosed
        end function qtest_qwaitforwindowclosed

        !> QTest::qWaitForWindowActivated
        function qtest_qwaitforwindowactivated(window, timeout) bind(c, name="qtest_qwaitforwindowactivated")
            import :: c_ptr, c_int, c_bool
            type(c_ptr), value :: window
            integer(c_int), value :: timeout
            logical(c_bool) :: qtest_qwaitforwindowactivated
        end function qtest_qwaitforwindowactivated

        !> QTest::qWaitForWindowDeactivated
        function qtest_qwaitforwindowdeactivated(window, timeout) bind(c, name="qtest_qwaitforwindowdeactivated")
            import :: c_ptr, c_int, c_bool
            type(c_ptr), value :: window
            integer(c_int), value :: timeout
            logical(c_bool) :: qtest_qwaitforwindowdeactivated
        end function qtest_qwaitforwindowdeactivated

        !> QTest::qWaitForWindowExposed
        function qtest_qwaitforwindowexpose(window, timeout) bind(c, name="qtest_qwaitforwindowexpose")
            import :: c_ptr, c_int, c_bool
            type(c_ptr), value :: window
            integer(c_int), value :: timeout
            logical(c_bool) :: qtest_qwaitforwindowexpose
        end function qtest_qwaitforwindowexpose

        !> QTest::qWaitForWindowUnexposed
        function qtest_qwaitforwindowunexposed(window, timeout) bind(c, name="qtest_qwaitforwindowunexposed")
            import :: c_ptr, c_int, c_bool
            type(c_ptr), value :: window
            integer(c_int), value :: timeout
            logical(c_bool) :: qtest_qwaitforwindowunexposed
        end function qtest_qwaitforwindowunexposed

        !> QTest::keyClicks - Simulate key clicks
        subroutine qtest_keyclicks(widget, sequence, modifier, delay) bind(c, name="qtest_keyclicks")
            import :: c_ptr, c_char, c_int
            type(c_ptr), value :: widget
            character(kind=c_char), dimension(*) :: sequence
            integer(c_int), value :: modifier
            integer(c_int), value :: delay
        end subroutine qtest_keyclicks

        !> QTest::keyPress - Simulate key press
        subroutine qtest_keypress(widget, key, modifier, delay) bind(c, name="qtest_keypress")
            import :: c_ptr, c_int
            type(c_ptr), value :: widget
            integer(c_int), value :: key
            integer(c_int), value :: modifier
            integer(c_int), value :: delay
        end subroutine qtest_keypress

        !> QTest::keyRelease - Simulate key release
        subroutine qtest_keyrelease(widget, key, modifier, delay) bind(c, name="qtest_keyrelease")
            import :: c_ptr, c_int
            type(c_ptr), value :: widget
            integer(c_int), value :: key
            integer(c_int), value :: modifier
            integer(c_int), value :: delay
        end subroutine qtest_keyrelease

        !> QTest::mouseClick - Simulate mouse click
        subroutine qtest_mouseclick(widget, button, modifier, pos, delay) bind(c, name="qtest_mouseclick")
            import :: c_ptr, c_int
            type(c_ptr), value :: widget
            integer(c_int), value :: button
            integer(c_int), value :: modifier
            type(c_ptr), value :: pos
            integer(c_int), value :: delay
        end subroutine qtest_mouseclick

        !> QTest::mousePress - Simulate mouse press
        subroutine qtest_mousepress(widget, button, modifier, pos, delay) bind(c, name="qtest_mousepress")
            import :: c_ptr, c_int
            type(c_ptr), value :: widget
            integer(c_int), value :: button
            integer(c_int), value :: modifier
            type(c_ptr), value :: pos
            integer(c_int), value :: delay
        end subroutine qtest_mousepress

        !> QTest::mouseRelease - Simulate mouse release
        subroutine qtest_mouserelease(widget, button, modifier, pos, delay) bind(c, name="qtest_mouserelease")
            import :: c_ptr, c_int
            type(c_ptr), value :: widget
            integer(c_int), value :: button
            integer(c_int), value :: modifier
            type(c_ptr), value :: pos
            integer(c_int), value :: delay
        end subroutine qtest_mouserelease

        !> QTest::mouseMove - Simulate mouse move
        subroutine qtest_mousemove(widget, pos, delay) bind(c, name="qtest_mousemove")
            import :: c_ptr, c_int
            type(c_ptr), value :: widget
            type(c_ptr), value :: pos
            integer(c_int), value :: delay
        end subroutine qtest_mousemove

        !> QTest::mouseWheel - Simulate mouse wheel
        subroutine qtest_mousewheel(widget, pos, delta, button, modifier, delay) bind(c, name="qtest_mousewheel")
            import :: c_ptr, c_int
            type(c_ptr), value :: widget
            type(c_ptr), value :: pos
            integer(c_int), value :: delta
            integer(c_int), value :: button
            integer(c_int), value :: modifier
            integer(c_int), value :: delay
        end subroutine qtest_mousewheel

        !> QTest::touchBegin - Simulate touch begin
        subroutine qtest_touchbegin(widget, touch_id, pos, device) bind(c, name="qtest_touchbegin")
            import :: c_ptr, c_int
            type(c_ptr), value :: widget
            integer(c_int), value :: touch_id
            type(c_ptr), value :: pos
            type(c_ptr), value :: device
        end subroutine qtest_touchbegin

        !> QTest::touchUpdate - Simulate touch update
        subroutine qtest_touchupdate(widget, touch_id, pos, device) bind(c, name="qtest_touchupdate")
            import :: c_ptr, c_int
            type(c_ptr), value :: widget
            integer(c_int), value :: touch_id
            type(c_ptr), value :: pos
            type(c_ptr), value :: device
        end subroutine qtest_touchupdate

        !> QTest::touchEnd - Simulate touch end
        subroutine qtest_touchend(widget, touch_id, device) bind(c, name="qtest_touchend")
            import :: c_ptr, c_int
            type(c_ptr), value :: widget
            integer(c_int), value :: touch_id
            type(c_ptr), value :: device
        end subroutine qtest_touchend

        !> QTest::touchCancel - Simulate touch cancel
        subroutine qtest_touchcancel(widget, touch_id, device) bind(c, name="qtest_touchcancel")
            import :: c_ptr, c_int
            type(c_ptr), value :: widget
            integer(c_int), value :: touch_id
            type(c_ptr), value :: device
        end subroutine qtest_touchcancel

        !> QSignalSpy constructor
        function qsignalspy_new(object, signal) bind(c, name="qsignalspy_new")
            import :: c_ptr, c_char
            type(c_ptr), value :: object
            character(kind=c_char), dimension(*) :: signal
            type(c_ptr) :: qsignalspy_new
        end function qsignalspy_new

        !> QSignalSpy::count - Get signal count
        function qsignalspy_count(spy) bind(c, name="qsignalspy_count")
            import :: c_ptr, c_int
            type(c_ptr), value :: spy
            integer(c_int) :: qsignalspy_count
        end function qsignalspy_count

        !> QSignalSpy::wait - Wait for signal
        function qsignalspy_wait(spy, timeout) bind(c, name="qsignalspy_wait")
            import :: c_ptr, c_int, c_bool
            type(c_ptr), value :: spy
            integer(c_int), value :: timeout
            logical(c_bool) :: qsignalspy_wait
        end function qsignalspy_wait

        !> QSignalSpy::isValid - Check if spy is valid
        function qsignalspy_isvalid(spy) bind(c, name="qsignalspy_isvalid")
            import :: c_ptr, c_bool
            type(c_ptr), value :: spy
            logical(c_bool) :: qsignalspy_isvalid
        end function qsignalspy_isvalid

        !> QSignalSpy::signal - Get signal signature
        function qsignalspy_signal(spy) bind(c, name="qsignalspy_signal")
            import :: c_ptr, c_char
            type(c_ptr), value :: spy
            character(kind=c_char), dimension(*) :: qsignalspy_signal
        end function qsignalspy_signal

        !> QSignalSpy::takeFirst - Take first signal
        function qsignalspy_takefirst(spy) bind(c, name="qsignalspy_takefirst")
            import :: c_ptr
            type(c_ptr), value :: spy
            type(c_ptr) :: qsignalspy_takefirst
        end function qsignalspy_takefirst

        !> QSignalSpy::takeLast - Take last signal
        function qsignalspy_takelast(spy) bind(c, name="qsignalspy_takelast")
            import :: c_ptr
            type(c_ptr), value :: spy
            type(c_ptr) :: qsignalspy_takelast
        end function qsignalspy_takelast

        !> QSignalSpy::at - Get signal at index
        function qsignalspy_at(spy, index) bind(c, name="qsignalspy_at")
            import :: c_ptr, c_int
            type(c_ptr), value :: spy
            integer(c_int), value :: index
            type(c_ptr) :: qsignalspy_at
        end function qsignalspy_at

        !> QTestData constructor
        function qtestdata_new() bind(c, name="qtestdata_new")
            import :: c_ptr
            type(c_ptr) :: qtestdata_new
        end function qtestdata_new

        !> QTestData destructor
        subroutine qtestdata_delete(data) bind(c, name="qtestdata_delete")
            import :: c_ptr
            type(c_ptr), value :: data
        end subroutine qtestdata_delete

        !> QBenchmarkMeasurement constructor
        function qbenchmarkmeasurement_new() bind(c, name="qbenchmarkmeasurement_new")
            import :: c_ptr
            type(c_ptr) :: qbenchmarkmeasurement_new
        end function qbenchmarkmeasurement_new

        !> QBenchmarkMeasurement::start
        subroutine qbenchmarkmeasurement_start(measurement) bind(c, name="qbenchmarkmeasurement_start")
            import :: c_ptr
            type(c_ptr), value :: measurement
        end subroutine qbenchmarkmeasurement_start

        !> QBenchmarkMeasurement::stop
        subroutine qbenchmarkmeasurement_stop(measurement) bind(c, name="qbenchmarkmeasurement_stop")
            import :: c_ptr
            type(c_ptr), value :: measurement
        end subroutine qbenchmarkmeasurement_stop

        !> QBenchmarkMeasurement::elapsed
        function qbenchmarkmeasurement_elapsed(measurement) bind(c, name="qbenchmarkmeasurement_elapsed")
            import :: c_ptr, c_long_long
            type(c_ptr), value :: measurement
            integer(c_long_long) :: qbenchmarkmeasurement_elapsed
        end function qbenchmarkmeasurement_elapsed

        !> QBenchmarkMeasurement::iterations
        function qbenchmarkmeasurement_iterations(measurement) bind(c, name="qbenchmarkmeasurement_iterations")
            import :: c_ptr, c_int
            type(c_ptr), value :: measurement
            integer(c_int) :: qbenchmarkmeasurement_iterations
        end function qbenchmarkmeasurement_iterations

    end interface

end module forge_qt_test_bindings