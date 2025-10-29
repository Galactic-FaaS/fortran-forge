!> @brief QObject demonstration program
!> @details Shows QObject features: parent-child relationships,
!> dynamic properties, signals, and automatic cleanup
!> @author ForGE Contributors
!> @date 2025
!> @license GPL-3.0-or-later

program qobject_demo
    use iso_c_binding
    use forge_qobject
    use forge_types
    use forge_signals
    use forge_errors
    implicit none

    type(forge_qobject) :: application, main_window, central_widget
    type(forge_qobject) :: button1, button2, label1
    type(forge_connection) :: conn1, conn2, conn3
    integer :: click_count = 0

    print *, "=== QObject Demonstration ==="

    ! Initialize the application object
    call application%init()
    call application%set_object_name("MyApplication")
    call application%set_property("version", "1.0.0")
    call application%set_property("debug_mode", .true.)

    print *, "Created application object: ", application%get_object_name()

    ! Create main window
    call main_window%init(application)
    call main_window%set_object_name("MainWindow")
    call main_window%set_property("width", 800)
    call main_window%set_property("height", 600)
    call main_window%set_property("title", "QObject Demo")

    print *, "Created main window: ", main_window%get_object_name()

    ! Create central widget
    call central_widget%init(main_window)
    call central_widget%set_object_name("CentralWidget")
    call central_widget%set_property("layout", "vertical")

    print *, "Created central widget: ", central_widget%get_object_name()

    ! Create buttons
    call button1%init(central_widget)
    call button1%set_object_name("OkButton")
    call button1%set_property("text", "OK")
    call button1%set_property("enabled", .true.)

    call button2%init(central_widget)
    call button2%set_object_name("CancelButton")
    call button2%set_property("text", "Cancel")
    call button2%set_property("enabled", .true.)

    print *, "Created buttons: ", button1%get_object_name(), ", ", button2%get_object_name()

    ! Create label
    call label1%init(central_widget)
    call label1%set_object_name("StatusLabel")
    call label1%set_property("text", "Ready")
    call label1%set_property("alignment", "center")

    print *, "Created label: ", label1%get_object_name()

    ! Connect signals
    print *, "Connecting signals..."
    conn1 = button1%destroyed_signal()%connect(button_destroyed_slot)
    conn2 = button2%destroyed_signal()%connect(button_destroyed_slot)
    conn3 = main_window%objectNameChanged_signal()%connect(object_name_changed_slot)

    ! Demonstrate property access
    print *, ""
    print *, "=== Property Access Demo ==="
    print *, "Application version: ", application%get_property("version")
    print *, "Main window title: ", main_window%get_property("title")
    print *, "Button1 text: ", button1%get_property("text")
    print *, "Button2 enabled: ", button2%get_property("enabled")

    ! Demonstrate finding objects
    print *, ""
    print *, "=== Object Finding Demo ==="
    print *, "Finding 'OkButton' from central widget: ", &
        associated(central_widget%find_child("OkButton"), button1)
    print *, "Finding 'MainWindow' from application: ", &
        associated(application%find_child("MainWindow"), main_window)

    ! Demonstrate object tree
    print *, ""
    print *, "=== Object Tree ==="
    call application%dump_object_tree()

    ! Demonstrate meta-object information
    print *, ""
    print *, "=== Meta-Object Information ==="
    print *, "Application class: ", application%metaObject()%get_class_name()
    print *, "Main window class: ", main_window%metaObject()%get_class_name()
    print *, "Button1 class: ", button1%metaObject()%get_class_name()

    ! Demonstrate thread affinity
    print *, ""
    print *, "=== Thread Affinity Demo ==="
    print *, "Application thread: ", application%thread()
    call application%move_to_thread(2)
    print *, "Application moved to thread: ", application%thread()

    ! Simulate some events
    print *, ""
    print *, "=== Event Simulation ==="
    call simulate_button_click(button1)
    call simulate_button_click(button2)

    ! Change object name to trigger signal
    call main_window%set_object_name("RenamedMainWindow")

    ! Cleanup demonstration
    print *, ""
    print *, "=== Cleanup Demo ==="
    print *, "Cleaning up application (should recursively cleanup all children)..."
    call application%cleanup()

    print *, "All objects cleaned up successfully!"
    print *, "=== QObject Demonstration Complete ==="

contains

    ! Slot for button destroyed signal
    subroutine button_destroyed_slot()
        print *, "Button destroyed signal received!"
    end subroutine button_destroyed_slot

    ! Slot for object name changed signal
    subroutine object_name_changed_slot(new_name)
        character(len=*), intent(in) :: new_name
        print *, "Object name changed to: ", new_name
    end subroutine object_name_changed_slot

    ! Simulate button click (for demonstration)
    subroutine simulate_button_click(button)
        type(forge_qobject), intent(in) :: button
        print *, "Simulating click on button: ", button%get_object_name()
    end subroutine simulate_button_click

end program qobject_demo