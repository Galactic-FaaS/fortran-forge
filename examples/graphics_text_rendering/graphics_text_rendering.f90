!> @brief Text Rendering Example
!> @details Demonstrates text rendering in graphics scenes
!> @author ForGE Contributors
!> @license GPL-3.0-or-later

program graphics_text_rendering_example
    use forge
    use forge_stub_backend
    implicit none

    type(forge_application) :: app
    type(forge_window_t) :: window
    type(forge_graphics_view) :: graphics_view
    type(forge_scene) :: scene
    type(forge_label) :: status_label
    type(forge_button) :: fonts_button, styles_button, alignment_button
    type(forge_status) :: status
    type(forge_stub_backend_t), target :: stub_backend

    print '(A)', "=== Graphics Text Rendering Example ==="
    print '(A)', ""
    print '(A)', "This example demonstrates:"
    print '(A)', "  - Text rendering in graphics scenes"
    print '(A)', "  - Font selection and sizing"
    print '(A)', "  - Text styling (bold, italic, underline)"
    print '(A)', "  - Text alignment and positioning"
    print '(A)', ""
    print '(A)', "NOTE: Using stub backend - graphics won't actually render"
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
        call builder%set_title("Graphics Text Rendering Example")
        call builder%set_size(700, 500)
        call builder%set_backend(stub_backend)
        window = builder%build(status)
    end block

    if (status%is_error()) then
        print '(A)', "ERROR: Failed to create window"
        call status%print()
        call stub_backend%shutdown()
        stop 1
    end if

    ! Create graphics scene
    print '(A)', "Creating graphics scene for text rendering..."
    call scene%set_scene_rect(0.0d0, 0.0d0, 600.0d0, 400.0d0)
    call scene%set_name("text_scene")

    ! Create graphics view
    call graphics_view%set_scene(scene)
    call graphics_view%set_name("graphics_view")

    ! Create control buttons
    call fonts_button%set_label("Different Fonts")
    call fonts_button%set_name("fonts_button")
    call fonts_button%on_click(on_fonts_clicked)

    call styles_button%set_label("Text Styles")
    call styles_button%set_name("styles_button")
    call styles_button%on_click(on_styles_clicked)

    call alignment_button%set_label("Text Alignment")
    call alignment_button%set_name("alignment_button")
    call alignment_button%on_click(on_alignment_clicked)

    ! Status label
    call status_label%set_name("status_label")
    call status_label%set_text("Click buttons to see different text rendering examples")

    ! Show window
    print '(A)', ""
    print '(A)', "Showing window with text rendering capabilities..."
    call window%show()

    ! Draw initial text
    print '(A)', ""
    print '(A)', "Rendering initial text examples:"
    call draw_font_examples()

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

    !> @brief Draw different font examples
    subroutine draw_font_examples()
        type(forge_font) :: font
        type(forge_graphics_item) :: text_item

        call scene%clear()

        ! Large serif font
        call font%set_family("Times New Roman")
        call font%set_size(24)
        call font%set_weight(0)  ! Normal
        call text_item%set_text("Times New Roman 24pt")
        call text_item%set_font(font)
        call text_item%set_pos(50.0d0, 50.0d0)
        call scene%add_item(text_item)
        print '(A)', "  Added Times New Roman text"

        ! Medium sans-serif font
        call font%set_family("Arial")
        call font%set_size(18)
        call text_item%set_text("Arial 18pt")
        call text_item%set_font(font)
        call text_item%set_pos(50.0d0, 100.0d0)
        call scene%add_item(text_item)
        print '(A)', "  Added Arial text"

        ! Small monospace font
        call font%set_family("Courier New")
        call font%set_size(12)
        call text_item%set_text("Courier New 12pt")
        call text_item%set_font(font)
        call text_item%set_pos(50.0d0, 140.0d0)
        call scene%add_item(text_item)
        print '(A)', "  Added Courier New text"

        ! Very large decorative font
        call font%set_family("Impact")
        call font%set_size(36)
        call text_item%set_text("Impact 36pt")
        call text_item%set_font(font)
        call text_item%set_pos(50.0d0, 200.0d0)
        call scene%add_item(text_item)
        print '(A)', "  Added Impact text"

        call status_label%set_text("Font Examples: Times, Arial, Courier, Impact")
    end subroutine draw_font_examples

    !> @brief Draw text styling examples
    subroutine draw_style_examples()
        type(forge_font) :: font
        type(forge_graphics_item) :: text_item

        call scene%clear()

        ! Normal text
        call font%set_family("Arial")
        call font%set_size(16)
        call font%set_weight(0)  ! Normal
        call font%set_style(0)   ! Normal
        call text_item%set_text("Normal Text")
        call text_item%set_font(font)
        call text_item%set_pos(50.0d0, 50.0d0)
        call scene%add_item(text_item)
        print '(A)', "  Added normal text"

        ! Bold text
        call font%set_weight(1)  ! Bold
        call text_item%set_text("Bold Text")
        call text_item%set_font(font)
        call text_item%set_pos(50.0d0, 80.0d0)
        call scene%add_item(text_item)
        print '(A)', "  Added bold text"

        ! Italic text
        call font%set_weight(0)  ! Normal
        call font%set_style(1)   ! Italic
        call text_item%set_text("Italic Text")
        call text_item%set_font(font)
        call text_item%set_pos(50.0d0, 110.0d0)
        call scene%add_item(text_item)
        print '(A)', "  Added italic text"

        ! Bold italic text
        call font%set_weight(1)  ! Bold
        call font%set_style(1)   ! Italic
        call text_item%set_text("Bold Italic Text")
        call text_item%set_font(font)
        call text_item%set_pos(50.0d0, 140.0d0)
        call scene%add_item(text_item)
        print '(A)', "  Added bold italic text"

        ! Underlined text
        call font%set_weight(0)  ! Normal
        call font%set_style(0)   ! Normal
        call font%set_underline(.true.)
        call text_item%set_text("Underlined Text")
        call text_item%set_font(font)
        call text_item%set_pos(50.0d0, 170.0d0)
        call scene%add_item(text_item)
        print '(A)', "  Added underlined text"

        ! Colored text
        call font%set_underline(.false.)
        call text_item%set_text("Colored Text")
        call text_item%set_font(font)
        call text_item%set_pos(50.0d0, 200.0d0)
        ! Set text color (would be implemented in real graphics item)
        call scene%add_item(text_item)
        print '(A)', "  Added colored text"

        call status_label%set_text("Text Styles: Normal, Bold, Italic, Underline, Color")
    end subroutine draw_style_examples

    !> @brief Draw text alignment examples
    subroutine draw_alignment_examples()
        type(forge_font) :: font
        type(forge_graphics_item) :: text_item

        call scene%clear()

        call font%set_family("Arial")
        call font%set_size(14)

        ! Left aligned text
        call text_item%set_text("Left Aligned Text\nSecond line left")
        call text_item%set_font(font)
        call text_item%set_pos(50.0d0, 50.0d0)
        call text_item%set_text_alignment(0)  ! Left
        call scene%add_item(text_item)
        print '(A)', "  Added left-aligned text"

        ! Center aligned text
        call text_item%set_text("Center Aligned Text\nSecond line center")
        call text_item%set_font(font)
        call text_item%set_pos(300.0d0, 50.0d0)
        call text_item%set_text_alignment(1)  ! Center
        call scene%add_item(text_item)
        print '(A)', "  Added center-aligned text"

        ! Right aligned text
        call text_item%set_text("Right Aligned Text\nSecond line right")
        call text_item%set_font(font)
        call text_item%set_pos(550.0d0, 50.0d0)
        call text_item%set_text_alignment(2)  ! Right
        call scene%add_item(text_item)
        print '(A)', "  Added right-aligned text"

        ! Justified text
        call text_item%set_text("Justified text spreads evenly across the width of the container it is in.")
        call text_item%set_font(font)
        call text_item%set_pos(50.0d0, 150.0d0)
        call text_item%set_text_width(500.0d0)  ! Width for justification
        call text_item%set_text_alignment(3)  ! Justified
        call scene%add_item(text_item)
        print '(A)', "  Added justified text"

        ! Multi-line with different alignments
        call text_item%set_text("Line 1 (Left)\nLine 2 (Center)\nLine 3 (Right)")
        call text_item%set_font(font)
        call text_item%set_pos(50.0d0, 250.0d0)
        call text_item%set_text_alignment(0)  ! Left for whole block
        call scene%add_item(text_item)
        print '(A)', "  Added multi-line text"

        call status_label%set_text("Text Alignment: Left, Center, Right, Justified")
    end subroutine draw_alignment_examples

    !> @brief Handler for fonts button click
    subroutine on_fonts_clicked(event)
        type(forge_event), intent(in) :: event
        call draw_font_examples()
    end subroutine on_fonts_clicked

    !> @brief Handler for styles button click
    subroutine on_styles_clicked(event)
        type(forge_event), intent(in) :: event
        call draw_style_examples()
    end subroutine on_styles_clicked

    !> @brief Handler for alignment button click
    subroutine on_alignment_clicked(event)
        type(forge_event), intent(in) :: event
        call draw_alignment_examples()
    end subroutine on_alignment_clicked

end program graphics_text_rendering_example