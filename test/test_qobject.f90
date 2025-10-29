!> @brief Test suite for QObject implementation
!> @details Comprehensive tests for all QObject features
!> @author ForGE Contributors
!> @date 2025
!> @license GPL-3.0-or-later

program test_qobject
    use iso_c_binding
    use forge_qobject
    use forge_types
    use forge_errors
    implicit none

    type(forge_qobject) :: root_obj, child1, child2, grandchild
    type(forge_qobject_ptr), dimension(:), allocatable :: children
    type(forge_string), dimension(:), allocatable :: prop_names
    integer :: i, child_count
    logical :: test_passed

    print *, "=== QObject Test Suite ==="

    ! Test 1: Basic object creation and initialization
    print *, "Test 1: Basic object creation..."
    call root_obj%init()
    call child1%init()
    call child2%init()
    call grandchild%init()

    test_passed = (root_obj%get_object_id() > 0)
    test_passed = test_passed .and. (child1%get_object_id() > 0)
    test_passed = test_passed .and. (child2%get_object_id() > 0)
    test_passed = test_passed .and. (grandchild%get_object_id() > 0)
    test_passed = test_passed .and. (root_obj%get_object_id() /= child1%get_object_id())

    if (test_passed) then
        print *, "✓ Object creation test passed"
    else
        print *, "✗ Object creation test failed"
    end if

    ! Test 2: Parent-child relationships
    print *, "Test 2: Parent-child relationships..."
    call child1%set_parent(root_obj)
    call child2%set_parent(root_obj)
    call grandchild%set_parent(child1)

    test_passed = associated(child1%get_parent(), root_obj)
    test_passed = test_passed .and. associated(child2%get_parent(), root_obj)
    test_passed = test_passed .and. associated(grandchild%get_parent(), child1)
    test_passed = test_passed .and. (root_obj%child_count() == 2)
    test_passed = test_passed .and. (child1%child_count() == 1)
    test_passed = test_passed .and. (child2%child_count() == 0)

    if (test_passed) then
        print *, "✓ Parent-child relationships test passed"
    else
        print *, "✗ Parent-child relationships test failed"
    end if

    ! Test 3: Object naming
    print *, "Test 3: Object naming..."
    call root_obj%set_object_name("root")
    call child1%set_object_name("child1")
    call child2%set_object_name("child2")
    call grandchild%set_object_name("grandchild")

    test_passed = (root_obj%get_object_name() == "root")
    test_passed = test_passed .and. (child1%get_object_name() == "child1")
    test_passed = test_passed .and. (child2%get_object_name() == "child2")
    test_passed = test_passed .and. (grandchild%get_object_name() == "grandchild")

    if (test_passed) then
        print *, "✓ Object naming test passed"
    else
        print *, "✗ Object naming test failed"
    end if

    ! Test 4: Find child by name
    print *, "Test 4: Find child by name..."
    test_passed = associated(root_obj%find_child("child1"), child1)
    test_passed = test_passed .and. associated(root_obj%find_child("child2"), child2)
    test_passed = test_passed .and. associated(child1%find_child("grandchild"), grandchild)
    test_passed = test_passed .and. (.not. associated(root_obj%find_child("nonexistent")))

    if (test_passed) then
        print *, "✓ Find child by name test passed"
    else
        print *, "✗ Find child by name test failed"
    end if

    ! Test 5: Dynamic properties
    print *, "Test 5: Dynamic properties..."
    call root_obj%set_property("width", 800)
    call root_obj%set_property("height", 600.0)
    call root_obj%set_property("title", "Main Window")
    call root_obj%set_property("visible", .true.)

    test_passed = root_obj%has_property("width")
    test_passed = test_passed .and. root_obj%has_property("height")
    test_passed = test_passed .and. root_obj%has_property("title")
    test_passed = test_passed .and. root_obj%has_property("visible")
    test_passed = test_passed .and. (.not. root_obj%has_property("nonexistent"))

    if (test_passed) then
        print *, "✓ Dynamic properties test passed"
    else
        print *, "✗ Dynamic properties test failed"
    end if

    ! Test 6: Property retrieval
    print *, "Test 6: Property retrieval..."
    test_passed = (root_obj%get_property("width") == 800)
    test_passed = test_passed .and. (abs(root_obj%get_property("height") - 600.0) < 1e-6)
    test_passed = test_passed .and. (root_obj%get_property("title") == "Main Window")
    test_passed = test_passed .and. (root_obj%get_property("visible") .eqv. .true.)

    if (test_passed) then
        print *, "✓ Property retrieval test passed"
    else
        print *, "✗ Property retrieval test failed"
    end if

    ! Test 7: Property names listing
    print *, "Test 7: Property names listing..."
    prop_names = root_obj%get_property_names()
    test_passed = (size(prop_names) == 4)
    test_passed = test_passed .and. any(prop_names(:)%get() == "width")
    test_passed = test_passed .and. any(prop_names(:)%get() == "height")
    test_passed = test_passed .and. any(prop_names(:)%get() == "title")
    test_passed = test_passed .and. any(prop_names(:)%get() == "visible")

    if (test_passed) then
        print *, "✓ Property names listing test passed"
    else
        print *, "✗ Property names listing test failed"
    end if

    ! Test 8: Meta-object system
    print *, "Test 8: Meta-object system..."
    test_passed = (root_obj%metaObject()%get_class_name() == "QObject")
    test_passed = test_passed .and. (root_obj%metaObject()%get_super_class_name() == "")
    test_passed = test_passed .and. (root_obj%metaObject()%get_property_count() == 0)  ! Meta properties, not dynamic

    if (test_passed) then
        print *, "✓ Meta-object system test passed"
    else
        print *, "✗ Meta-object system test failed"
    end if

    ! Test 9: Thread affinity
    print *, "Test 9: Thread affinity..."
    call root_obj%move_to_thread(1)
    test_passed = (root_obj%thread() == 1)

    if (test_passed) then
        print *, "✓ Thread affinity test passed"
    else
        print *, "✗ Thread affinity test failed"
    end if

    ! Test 10: Object tree dumping
    print *, "Test 10: Object tree dumping..."
    print *, "Object tree:"
    call root_obj%dump_object_tree()
    print *, "✓ Object tree dumping completed"

    ! Test 11: Automatic cleanup
    print *, "Test 11: Automatic cleanup..."
    call root_obj%cleanup()  ! This should recursively clean up all children

    test_passed = root_obj%destroyed
    test_passed = test_passed .and. child1%destroyed
    test_passed = test_passed .and. child2%destroyed
    test_passed = test_passed .and. grandchild%destroyed

    if (test_passed) then
        print *, "✓ Automatic cleanup test passed"
    else
        print *, "✗ Automatic cleanup test failed"
    end if

    print *, "=== QObject Test Suite Complete ==="

contains

    ! Helper function to check if string is in array
    function any(array, value) result(found)
        type(forge_string), dimension(:), intent(in) :: array
        character(len=*), intent(in) :: value
        logical :: found
        integer :: i

        found = .false.
        do i = 1, size(array)
            if (array(i)%get() == value) then
                found = .true.
                exit
            end if
        end do
    end function any

end program test_qobject