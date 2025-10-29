!> @brief Windows Registry Example
!> @details Demonstrates Windows registry access and manipulation
!> @author ForGE Contributors
!> @license GPL-3.0-or-later

program platform_windows_registry_example
    use forge
    use forge_stub_backend
    implicit none

    type(forge_application) :: app
    type(forge_window_t) :: window
    type(forge_label) :: status_label, key_label, value_label
    type(forge_button) :: read_button, write_button, delete_button, enum_button
    type(forge_entry) :: key_entry, value_entry
    type(forge_text_view) :: results_view
    type(forge_combo_box) :: hive_combo
    type(forge_status) :: status
    type(forge_stub_backend_t), target :: stub_backend

    print '(A)', "=== Windows Registry Example ==="
    print '(A)', ""
    print '(A)', "This example demonstrates:"
    print '(A)', "  - Windows registry key access"
    print '(A)', "  - Reading and writing registry values"
    print '(A)', "  - Registry key enumeration"
    print '(A)', "  - Different registry hives"
    print '(A)', "  - Error handling for registry operations"
    print '(A)', ""
    print '(A)', "NOTE: Using stub backend - registry operations are simulated"
    print '(A)', ""

    ! Initialize stub backend
    call stub_backend%init(status)
    if (status%is_error()) then
        print '(A)', "ERROR: Failed to initialize stub backend"
        call status%print()
        stop 1
    end if

    ! Create window
    block
        type(forge_window_builder) :: builder
        call builder%set_title("Windows Registry Access")
        call builder%set_size(600, 500)
        call builder%set_backend(stub_backend)
        window = builder%build(status)
    end block

    if (status%is_error()) then
        print '(A)', "ERROR: Failed to initialize window"
        call status%print()
        call stub_backend%shutdown()
        stop 1
    end if

    ! Create UI elements
    print '(A)', "Creating registry access interface..."

    ! Registry hive selection
    call hive_combo%set_name("hive_combo")
    call hive_combo%set_editable(.false.)
    call hive_combo%add_item("HKEY_CLASSES_ROOT")
    call hive_combo%add_item("HKEY_CURRENT_USER")
    call hive_combo%add_item("HKEY_LOCAL_MACHINE")
    call hive_combo%add_item("HKEY_USERS")
    call hive_combo%add_item("HKEY_CURRENT_CONFIG")
    call hive_combo%set_current_index(1)  ! HKEY_CURRENT_USER

    ! Key and value inputs
    call key_entry%set_placeholder_text("Registry key path (e.g., Software\\MyApp)")
    call key_entry%set_text("Software\\MyApp")
    call key_entry%set_name("key_entry")

    call value_entry%set_placeholder_text("Value name")
    call value_entry%set_text("Version")
    call value_entry%set_name("value_entry")

    ! Operation buttons
    call read_button%set_label("Read Value")
    call read_button%set_name("read_button")
    call read_button%on_click(on_read_clicked)

    call write_button%set_label("Write Value")
    call write_button%set_name("write_button")
    call write_button%on_click(on_write_clicked)

    call delete_button%set_label("Delete Value")
    call delete_button%set_name("delete_button")
    call delete_button%on_click(on_delete_clicked)

    call enum_button%set_label("Enumerate Keys")
    call enum_button%set_name("enum_button")
    call enum_button%on_click(on_enum_clicked)

    ! Status displays
    call status_label%set_name("status_label")
    call status_label%set_text("Registry access ready")

    call key_label%set_name("key_label")
    call key_label%set_text("Current Key: HKEY_CURRENT_USER\\Software\\MyApp")

    call value_label%set_name("value_label")
    call value_label%set_text("Current Value: Version")

    ! Results display
    call results_view%set_name("results_view")
    call results_view%set_editable(.false.)
    call results_view%set_text("Registry operation results will appear here...\n")

    ! Show window
    print '(A)', ""
    print '(A)', "Showing Windows registry access interface..."
    call window%show()

    ! Run event loop (stub version just returns)
    call stub_backend%run()

    ! Cleanup
    print '(A)', ""
    print '(A)', "Cleaning up..."
    call window%close()
    call stub_backend%shutdown()

    print '(A)', ""
    print '(A)', "=== Example Complete ==="

contains

    !> @brief Read registry value
    subroutine read_registry_value()
        character(len=:), allocatable :: hive, key_path, value_name
        character(len=100) :: result_text

        ! Get inputs
        select case (hive_combo%get_current_index())
        case (0)
            hive = "HKEY_CLASSES_ROOT"
        case (1)
            hive = "HKEY_CURRENT_USER"
        case (2)
            hive = "HKEY_LOCAL_MACHINE"
        case (3)
            hive = "HKEY_USERS"
        case (4)
            hive = "HKEY_CURRENT_CONFIG"
        end select

        key_path = trim(key_entry%get_text())
        value_name = trim(value_entry%get_text())

        print '(A,A,A,A,A,A)', "  Reading registry: ", hive, "\\", key_path, "\\", value_name

        ! Simulate registry read
        result_text = "Registry Read Result:\n" // &
                     "Hive: " // trim(hive) // "\n" // &
                     "Key: " // trim(key_path) // "\n" // &
                     "Value: " // trim(value_name) // "\n" // &
                     "Type: REG_SZ\n" // &
                     "Data: 1.0.0\n"

        call results_view%set_text(trim(result_text))
        call status_label%set_text("Registry value read successfully")

        print '(A)', "  → Registry value read"
    end subroutine read_registry_value

    !> @brief Write registry value
    subroutine write_registry_value()
        character(len=:), allocatable :: hive, key_path, value_name
        character(len=100) :: result_text

        ! Get inputs
        select case (hive_combo%get_current_index())
        case (0)
            hive = "HKEY_CLASSES_ROOT"
        case (1)
            hive = "HKEY_CURRENT_USER"
        case (2)
            hive = "HKEY_LOCAL_MACHINE"
        case (3)
            hive = "HKEY_USERS"
        case (4)
            hive = "HKEY_CURRENT_CONFIG"
        end select

        key_path = trim(key_entry%get_text())
        value_name = trim(value_entry%get_text())

        print '(A,A,A,A,A,A)', "  Writing registry: ", hive, "\\", key_path, "\\", value_name

        ! Simulate registry write
        result_text = "Registry Write Result:\n" // &
                     "Hive: " // trim(hive) // "\n" // &
                     "Key: " // trim(key_path) // "\n" // &
                     "Value: " // trim(value_name) // "\n" // &
                     "Type: REG_SZ\n" // &
                     "Data: 2.0.0\n" // &
                     "Status: Success\n"

        call results_view%set_text(trim(result_text))
        call status_label%set_text("Registry value written successfully")

        print '(A)', "  → Registry value written"
    end subroutine write_registry_value

    !> @brief Delete registry value
    subroutine delete_registry_value()
        character(len=:), allocatable :: hive, key_path, value_name
        character(len=100) :: result_text

        ! Get inputs
        select case (hive_combo%get_current_index())
        case (0)
            hive = "HKEY_CLASSES_ROOT"
        case (1)
            hive = "HKEY_CURRENT_USER"
        case (2)
            hive = "HKEY_LOCAL_MACHINE"
        case (3)
            hive = "HKEY_USERS"
        case (4)
            hive = "HKEY_CURRENT_CONFIG"
        end select

        key_path = trim(key_entry%get_text())
        value_name = trim(value_entry%get_text())

        print '(A,A,A,A,A,A)', "  Deleting registry: ", hive, "\\", key_path, "\\", value_name

        ! Simulate registry delete
        result_text = "Registry Delete Result:\n" // &
                     "Hive: " // trim(hive) // "\n" // &
                     "Key: " // trim(key_path) // "\n" // &
                     "Value: " // trim(value_name) // "\n" // &
                     "Status: Value deleted successfully\n"

        call results_view%set_text(trim(result_text))
        call status_label%set_text("Registry value deleted successfully")

        print '(A)', "  → Registry value deleted"
    end subroutine delete_registry_value

    !> @brief Enumerate registry keys
    subroutine enumerate_registry_keys()
        character(len=:), allocatable :: hive, key_path
        character(len=300) :: result_text

        ! Get inputs
        select case (hive_combo%get_current_index())
        case (0)
            hive = "HKEY_CLASSES_ROOT"
        case (1)
            hive = "HKEY_CURRENT_USER"
        case (2)
            hive = "HKEY_LOCAL_MACHINE"
        case (3)
            hive = "HKEY_USERS"
        case (4)
            hive = "HKEY_CURRENT_CONFIG"
        end select

        key_path = trim(key_entry%get_text())

        print '(A,A,A,A,A)', "  Enumerating registry keys in: ", hive, "\\", key_path

        ! Simulate registry enumeration
        result_text = "Registry Key Enumeration:\n" // &
                     "Hive: " // trim(hive) // "\n" // &
                     "Parent Key: " // trim(key_path) // "\n\n" // &
                     "Subkeys found:\n" // &
                     "  - Settings\n" // &
                     "  - Preferences\n" // &
                     "  - Cache\n" // &
                     "  - Logs\n\n" // &
                     "Values found:\n" // &
                     "  - Version (REG_SZ): 1.0.0\n" // &
                     "  - InstallDate (REG_SZ): 2024-01-01\n" // &
                     "  - AutoStart (REG_DWORD): 1\n"

        call results_view%set_text(trim(result_text))
        call status_label%set_text("Registry keys enumerated successfully")

        print '(A)', "  → Registry keys enumerated"
    end subroutine enumerate_registry_keys

    !> @brief Update key path display
    subroutine update_key_display()
        character(len=:), allocatable :: hive, key_path, display_text

        select case (hive_combo%get_current_index())
        case (0)
            hive = "HKEY_CLASSES_ROOT"
        case (1)
            hive = "HKEY_CURRENT_USER"
        case (2)
            hive = "HKEY_LOCAL_MACHINE"
        case (3)
            hive = "HKEY_USERS"
        case (4)
            hive = "HKEY_CURRENT_CONFIG"
        end select

        key_path = trim(key_entry%get_text())
        display_text = "Current Key: " // trim(hive) // "\\" // trim(key_path)
        call key_label%set_text(trim(display_text))
    end subroutine update_key_display

    !> @brief Handler for read button click
    subroutine on_read_clicked(event)
        type(forge_event), intent(in) :: event
        call read_registry_value()
    end subroutine on_read_clicked

    !> @brief Handler for write button click
    subroutine on_write_clicked(event)
        type(forge_event), intent(in) :: event
        call write_registry_value()
    end subroutine on_write_clicked

    !> @brief Handler for delete button click
    subroutine on_delete_clicked(event)
        type(forge_event), intent(in) :: event
        call delete_registry_value()
    end subroutine on_delete_clicked

    !> @brief Handler for enumerate button click
    subroutine on_enum_clicked(event)
        type(forge_event), intent(in) :: event
        call enumerate_registry_keys()
    end subroutine on_enum_clicked

end program platform_windows_registry_example