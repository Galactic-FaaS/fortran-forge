!> @brief Basic Grid Layout Example
!> @details Demonstrates grid layout for arranging widgets in rows and columns
!> @author ForGE Contributors
!> @license GPL-3.0-or-later

program basic_layout_grid_example
    use forge
    use forge_stub_backend
    implicit none

    type(forge_application) :: app
    type(forge_window_t) :: window
    type(forge_grid_layout) :: layout
    type(forge_label) :: labels(3, 2)
    type(forge_button) :: buttons(3, 2)
    type(forge_entry) :: entries(3, 2)
    type(forge_status) :: status
    type(forge_stub_backend_t), target :: stub_backend
    integer :: i, j

    print '(A)', "=== Basic Grid Layout Example ==="
    print '(A)', ""
    print '(A)', "This example demonstrates:"
    print '(A)', "  - Creating a grid layout (3x2)"
    print '(A)', "  - Adding widgets at specific grid positions"
    print '(A)', "  - Row and column spanning"
    print '(A)', "  - Grid spacing and margins"
    print '(A)', ""
    print '(A)', "NOTE: Using stub backend - no actual GUI will appear"
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
        call builder%set_title("Basic Grid Layout Example")
        call builder%set_size(400, 300)
        call builder%set_backend(stub_backend)
        window = builder%build(status)
    end block

    if (status%is_error()) then
        print '(A)', "ERROR: Failed to create window"
        call status%print()
        call stub_backend%shutdown()
        stop 1
    end if

    ! Create grid layout
    print '(A)', "Creating 3x2 grid layout..."
    call layout%set_rows(3)
    call layout%set_columns(2)
    call layout%set_spacing(10)
    call layout%set_margin(15)

    ! Create and add widgets to grid positions
    print '(A)', "Creating and placing widgets..."

    do i = 1, 3
        do j = 1, 2
            ! Create label
            call labels(i,j)%set_text("Label " // char(48+i) // "," // char(48+j))
            call layout%add_widget_at(labels(i,j), i, j)

            ! Create button
            call buttons(i,j)%set_label("Btn " // char(48+i) // "," // char(48+j))
            call layout%add_widget_at(buttons(i,j), i, j+2)  ! Offset by 2 columns

            ! Create entry
            call entries(i,j)%set_placeholder_text("Entry " // char(48+i) // "," // char(48+j))
            call layout%add_widget_at(entries(i,j), i, j+4)  ! Offset by 4 columns
        end do
    end do

    print '(A,I0,A,I0)', "  Grid size: ", layout%get_rows(), "x", layout%get_columns()
    print '(A,I0)', "  Total widgets: ", layout%get_widget_count()
    print '(A,I0)', "  Spacing: ", layout%get_spacing()
    print '(A,I0)', "  Margin: ", layout%get_margin()

    ! Set layout on window
    call window%set_layout(layout)

    ! Show window
    print '(A)', ""
    print '(A)', "Showing window with grid layout..."
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

end program basic_layout_grid_example