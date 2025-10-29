!> @brief Linux System Information Example
!> @details Demonstrates accessing Linux system information and resources
!> @author ForGE Contributors
!> @license GPL-3.0-or-later

program platform_linux_system_info_example
    use forge
    use forge_stub_backend
    implicit none

    type(forge_application) :: app
    type(forge_window_t) :: window
    type(forge_label) :: status_label, cpu_label, memory_label, disk_label
    type(forge_button) :: refresh_button, system_button, process_button, network_button
    type(forge_text_view) :: info_view
    type(forge_progress_bar) :: cpu_progress, memory_progress
    type(forge_status) :: status
    type(forge_stub_backend_t), target :: stub_backend

    print '(A)', "=== Linux System Information Example ==="
    print '(A)', ""
    print '(A)', "This example demonstrates:"
    print '(A)', "  - Reading Linux system information"
    print '(A)', "  - CPU usage and load monitoring"
    print '(A)', "  - Memory and swap statistics"
    print '(A)', "  - Disk usage information"
    print '(A)', "  - Process and network monitoring"
    print '(A)', ""
    print '(A)', "NOTE: Using stub backend - system info is simulated"
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
        call builder%set_title("Linux System Information")
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
    print '(A)', "Creating system information interface..."

    ! Control buttons
    call refresh_button%set_label("Refresh Info")
    call refresh_button%set_name("refresh_button")
    call refresh_button%on_click(on_refresh_clicked)

    call system_button%set_label("System Info")
    call system_button%set_name("system_button")
    call system_button%on_click(on_system_clicked)

    call process_button%set_label("Process Info")
    call process_button%set_name("process_button")
    call process_button%on_click(on_process_clicked)

    call network_button%set_label("Network Info")
    call network_button%set_name("network_button")
    call network_button%on_click(on_network_clicked)

    ! Progress bars for system resources
    call cpu_progress%set_name("cpu_progress")
    call cpu_progress%set_range(0, 100)
    call cpu_progress%set_value(45)
    call cpu_progress%set_text_visible(.true.)
    call cpu_progress%set_format("CPU Usage: %p%")

    call memory_progress%set_name("memory_progress")
    call memory_progress%set_range(0, 100)
    call memory_progress%set_value(67)
    call memory_progress%set_text_visible(.true.)
    call memory_progress%set_format("Memory Usage: %p%")

    ! Status labels
    call status_label%set_name("status_label")
    call status_label%set_text("System information monitor ready")

    call cpu_label%set_name("cpu_label")
    call cpu_label%set_text("CPU: 4 cores, 3.2 GHz")

    call memory_label%set_name("memory_label")
    call memory_label%set_text("Memory: 8 GB total, 5.4 GB used")

    call disk_label%set_name("disk_label")
    call disk_label%set_text("Disk: 256 GB SSD, 45% used")

    ! Information display
    call info_view%set_name("info_view")
    call info_view%set_editable(.false.)
    call info_view%set_text("Click buttons above to view different system information...\n")

    ! Show window
    print '(A)', ""
    print '(A)', "Showing Linux system information monitor..."
    call window%show()

    ! Initial system information display
    call display_system_info()

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

    !> @brief Refresh system information
    subroutine refresh_system_info()
        integer :: cpu_usage, memory_usage

        ! Simulate reading current system stats
        cpu_usage = 45 + mod(int(secnds(0.0) * 100), 30)  ! Vary CPU usage
        memory_usage = 60 + mod(int(secnds(0.0) * 50), 20)  ! Vary memory usage

        call cpu_progress%set_value(cpu_usage)
        call memory_progress%set_value(memory_usage)

        write(cpu_text, '("CPU: ", I0, "% usage, 4 cores active")') cpu_usage
        call cpu_label%set_text(trim(cpu_text))

        write(memory_text, '("Memory: ", I0, "% usage, ", I0, " MB free")') memory_usage, 8000 - (8000 * memory_usage / 100)
        call memory_label%set_text(trim(memory_text))

        call status_label%set_text("System information refreshed")

        print '(A,I0,A,I0,A)', "  → System info refreshed: CPU=", cpu_usage, "%, Memory=", memory_usage, "%"
    end subroutine refresh_system_info

    !> @brief Display general system information
    subroutine display_system_info()
        character(len=500) :: info_text

        info_text = "Linux System Information:\n" // &
                   "========================\n\n" // &
                   "Operating System: Ubuntu 22.04.3 LTS\n" // &
                   "Kernel Version: 5.15.0-78-generic\n" // &
                   "Architecture: x86_64\n" // &
                   "Hostname: ubuntu-workstation\n\n" // &
                   "CPU Information:\n" // &
                   "  Model: Intel Core i7-8700K\n" // &
                   "  Cores: 6 physical, 12 logical\n" // &
                   "  Frequency: 3.7 GHz base, 4.7 GHz turbo\n" // &
                   "  Cache: 12MB L3\n\n" // &
                   "Memory Information:\n" // &
                   "  Total: 16 GB\n" // &
                   "  Type: DDR4-3200\n" // &
                   "  Speed: 3200 MHz\n\n" // &
                   "Storage Information:\n" // &
                   "  Primary: 512GB NVMe SSD\n" // &
                   "  File System: ext4\n" // &
                   "  Mount Point: /\n"

        call info_view%set_text(trim(info_text))
        call status_label%set_text("Displaying system information")

        print '(A)', "  → Displaying system information"
    end subroutine display_system_info

    !> @brief Display process information
    subroutine display_process_info()
        character(len=500) :: info_text

        info_text = "Process Information:\n" // &
                   "===================\n\n" // &
                   "Top Processes by CPU Usage:\n" // &
                   "  1. chrome (45.2%)\n" // &
                   "  2. code (23.1%)\n" // &
                   "  3. firefox (12.8%)\n" // &
                   "  4. gnome-shell (8.4%)\n" // &
                   "  5. Xorg (5.2%)\n\n" // &
                   "Top Processes by Memory Usage:\n" // &
                   "  1. chrome (2.1 GB)\n" // &
                   "  2. code (1.8 GB)\n" // &
                   "  3. firefox (1.2 GB)\n" // &
                   "  4. gnome-shell (0.8 GB)\n" // &
                   "  5. docker (0.6 GB)\n\n" // &
                   "System Load:\n" // &
                   "  1 minute: 2.45\n" // &
                   "  5 minutes: 2.12\n" // &
                   "  15 minutes: 1.98\n\n" // &
                   "Total Processes: 284\n" // &
                   "Running Processes: 2\n"

        call info_view%set_text(trim(info_text))
        call status_label%set_text("Displaying process information")

        print '(A)', "  → Displaying process information"
    end subroutine display_process_info

    !> @brief Display network information
    subroutine display_network_info()
        character(len=500) :: info_text

        info_text = "Network Information:\n" // &
                   "===================\n\n" // &
                   "Network Interfaces:\n" // &
                   "  eth0 (Ethernet):\n" // &
                   "    IP Address: 192.168.1.100\n" // &
                   "    Netmask: 255.255.255.0\n" // &
                   "    Gateway: 192.168.1.1\n" // &
                   "    MAC: 00:1B:44:11:3A:B7\n" // &
                   "    Status: Up\n\n" // &
                   "  wlan0 (Wireless):\n" // &
                   "    IP Address: Not configured\n" // &
                   "    Status: Down\n\n" // &
                   "Network Statistics:\n" // &
                   "  Received: 1.2 GB\n" // &
                   "  Transmitted: 856 MB\n" // &
                   "  Packets Received: 1,245,678\n" // &
                   "  Packets Transmitted: 987,654\n\n" // &
                   "DNS Servers:\n" // &
                   "  8.8.8.8 (Google)\n" // &
                   "  8.8.4.4 (Google)\n" // &
                   "  192.168.1.1 (Local)\n\n" // &
                   "Hostname: ubuntu-workstation.local\n"

        call info_view%set_text(trim(info_text))
        call status_label%set_text("Displaying network information")

        print '(A)', "  → Displaying network information"
    end subroutine display_network_info

    !> @brief Handler for refresh button click
    subroutine on_refresh_clicked(event)
        type(forge_event), intent(in) :: event
        call refresh_system_info()
    end subroutine on_refresh_clicked

    !> @brief Handler for system info button click
    subroutine on_system_clicked(event)
        type(forge_event), intent(in) :: event
        call display_system_info()
    end subroutine on_system_clicked

    !> @brief Handler for process info button click
    subroutine on_process_clicked(event)
        type(forge_event), intent(in) :: event
        call display_process_info()
    end subroutine on_process_clicked

    !> @brief Handler for network info button click
    subroutine on_network_clicked(event)
        type(forge_event), intent(in) :: event
        call display_network_info()
    end subroutine on_network_clicked

end program platform_linux_system_info_example