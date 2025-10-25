!> @brief Widget rendering routines
!> @details Implements drawing functions for all widget types
!> @author ForGE Contributors
!> @date 2025
!> @license GPL-3.0-or-later

module forge_rendering
    use iso_c_binding
    use forge_types
    use forge_cairo_bindings
    implicit none
    private

    public :: render_clear_background, render_test_pattern
    public :: render_button, render_label, render_entry
    public :: render_progress_bar, render_separator

    ! Common constants
    real(c_double), parameter :: PI = 3.14159265358979323846_c_double

    ! Default colors
    type(forge_color), parameter :: COLOR_BACKGROUND = forge_color(0.95, 0.95, 0.95, 1.0)
    type(forge_color), parameter :: COLOR_BUTTON_NORMAL = forge_color(0.9, 0.9, 0.9, 1.0)
    type(forge_color), parameter :: COLOR_BUTTON_HOVER = forge_color(0.85, 0.85, 0.95, 1.0)
    type(forge_color), parameter :: COLOR_BUTTON_PRESSED = forge_color(0.7, 0.7, 0.8, 1.0)
    type(forge_color), parameter :: COLOR_BORDER = forge_color(0.6, 0.6, 0.6, 1.0)
    type(forge_color), parameter :: COLOR_TEXT = forge_color(0.0, 0.0, 0.0, 1.0)

contains

    !> @brief Clear background with solid color
    subroutine render_clear_background(cr, width, height, color)
        type(c_ptr), intent(in) :: cr
        integer(c_int), intent(in) :: width, height
        type(forge_color), intent(in), optional :: color
        type(forge_color) :: bg_color

        if (present(color)) then
            bg_color = color
        else
            bg_color = COLOR_BACKGROUND
        end if

        call cairo_set_source_rgb(cr, bg_color%r, bg_color%g, bg_color%b)
        call cairo_rectangle(cr, 0.0_c_double, 0.0_c_double, &
                            real(width, c_double), real(height, c_double))
        call cairo_fill(cr)
    end subroutine render_clear_background

    !> @brief Render a test pattern (for debugging)
    subroutine render_test_pattern(cr, width, height)
        type(c_ptr), intent(in) :: cr
        integer(c_int), intent(in) :: width, height
        real(c_double) :: w, h
        character(len=30, kind=c_char) :: text

        w = real(width, c_double)
        h = real(height, c_double)

        ! Clear to white
        call render_clear_background(cr, width, height)

        ! Draw colored rectangles
        ! Red rectangle
        call cairo_set_source_rgb(cr, 1.0_c_double, 0.0_c_double, 0.0_c_double)
        call cairo_rectangle(cr, 50.0_c_double, 50.0_c_double, 100.0_c_double, 100.0_c_double)
        call cairo_fill(cr)

        ! Green rectangle
        call cairo_set_source_rgb(cr, 0.0_c_double, 1.0_c_double, 0.0_c_double)
        call cairo_rectangle(cr, w - 150.0_c_double, 50.0_c_double, 100.0_c_double, 100.0_c_double)
        call cairo_fill(cr)

        ! Blue rectangle
        call cairo_set_source_rgb(cr, 0.0_c_double, 0.0_c_double, 1.0_c_double)
        call cairo_rectangle(cr, 50.0_c_double, h - 150.0_c_double, 100.0_c_double, 100.0_c_double)
        call cairo_fill(cr)

        ! Yellow rectangle (center)
        call cairo_set_source_rgb(cr, 1.0_c_double, 1.0_c_double, 0.0_c_double)
        call cairo_rectangle(cr, w/2.0 - 50.0, h/2.0 - 50.0, 100.0_c_double, 100.0_c_double)
        call cairo_fill(cr)

        ! Draw text
        call cairo_set_source_rgb(cr, 0.0_c_double, 0.0_c_double, 0.0_c_double)
        call cairo_select_font_face(cr, c_loc("Sans"//c_null_char), &
                                    CAIRO_FONT_SLANT_NORMAL, CAIRO_FONT_WEIGHT_BOLD)
        call cairo_set_font_size(cr, 24.0_c_double)
        call cairo_move_to(cr, w/2.0 - 100.0, 30.0_c_double)
        text = "ForGE Custom GUI!"//c_null_char
        call cairo_show_text(cr, c_loc(text))

        ! Draw border around window
        call cairo_set_source_rgb(cr, 0.0_c_double, 0.0_c_double, 0.0_c_double)
        call cairo_set_line_width(cr, 2.0_c_double)
        call cairo_rectangle(cr, 1.0_c_double, 1.0_c_double, w - 2.0, h - 2.0)
        call cairo_stroke(cr)
    end subroutine render_test_pattern

    !> @brief Render a button widget
    subroutine render_button(cr, rect, label, state)
        type(c_ptr), intent(in) :: cr
        type(forge_rect), intent(in) :: rect
        character(len=*), intent(in) :: label
        integer, intent(in) :: state  ! 0=normal, 1=hover, 2=pressed
        real(c_double) :: x, y, w, h, corner_radius
        type(forge_color) :: bg_color, border_color
        character(len=:), allocatable :: label_c

        x = real(rect%pos%x, c_double)
        y = real(rect%pos%y, c_double)
        w = real(rect%size%width, c_double)
        h = real(rect%size%height, c_double)
        corner_radius = 5.0_c_double

        ! Select color based on state
        select case (state)
        case (1)
            bg_color = COLOR_BUTTON_HOVER
        case (2)
            bg_color = COLOR_BUTTON_PRESSED
        case default
            bg_color = COLOR_BUTTON_NORMAL
        end select
        border_color = COLOR_BORDER

        ! Draw rounded rectangle background
        call draw_rounded_rect(cr, x, y, w, h, corner_radius)
        call cairo_set_source_rgb(cr, bg_color%r, bg_color%g, bg_color%b)
        call cairo_fill_preserve(cr)

        ! Draw border
        call cairo_set_source_rgb(cr, border_color%r, border_color%g, border_color%b)
        call cairo_set_line_width(cr, 1.0_c_double)
        call cairo_stroke(cr)

        ! Draw label (centered)
        if (len_trim(label) > 0) then
            label_c = trim(label) // c_null_char
            call cairo_set_source_rgb(cr, COLOR_TEXT%r, COLOR_TEXT%g, COLOR_TEXT%b)
            call cairo_select_font_face(cr, c_loc("Sans"//c_null_char), &
                                        CAIRO_FONT_SLANT_NORMAL, CAIRO_FONT_WEIGHT_NORMAL)
            call cairo_set_font_size(cr, 14.0_c_double)
            ! TODO: Measure text for proper centering
            call cairo_move_to(cr, x + 10.0, y + h/2.0 + 5.0)
            call cairo_show_text(cr, c_loc(label_c))
        end if
    end subroutine render_button

    !> @brief Render a label widget
    subroutine render_label(cr, rect, text, alignment)
        type(c_ptr), intent(in) :: cr
        type(forge_rect), intent(in) :: rect
        character(len=*), intent(in) :: text
        integer, intent(in), optional :: alignment  ! 0=left, 1=center, 2=right
        real(c_double) :: x, y
        integer :: align
        character(len=:), allocatable :: text_c

        align = 0
        if (present(alignment)) align = alignment

        x = real(rect%pos%x, c_double)
        y = real(rect%pos%y, c_double)

        ! Adjust x based on alignment (simplified)
        select case (align)
        case (1)  ! center
            x = x + real(rect%size%width, c_double) / 2.0 - 20.0  ! TODO: measure text
        case (2)  ! right
            x = x + real(rect%size%width, c_double) - 40.0  ! TODO: measure text
        end select

        ! Draw text
        text_c = trim(text) // c_null_char
        call cairo_set_source_rgb(cr, COLOR_TEXT%r, COLOR_TEXT%g, COLOR_TEXT%b)
        call cairo_select_font_face(cr, c_loc("Sans"//c_null_char), &
                                    CAIRO_FONT_SLANT_NORMAL, CAIRO_FONT_WEIGHT_NORMAL)
        call cairo_set_font_size(cr, 14.0_c_double)
        call cairo_move_to(cr, x, y + 15.0_c_double)
        call cairo_show_text(cr, c_loc(text_c))
    end subroutine render_label

    !> @brief Render a text entry widget
    subroutine render_entry(cr, rect, text, cursor_pos, has_focus)
        type(c_ptr), intent(in) :: cr
        type(forge_rect), intent(in) :: rect
        character(len=*), intent(in) :: text
        integer, intent(in), optional :: cursor_pos
        logical, intent(in), optional :: has_focus
        real(c_double) :: x, y, w, h
        type(forge_color) :: bg_color, border_color
        character(len=:), allocatable :: text_c

        x = real(rect%pos%x, c_double)
        y = real(rect%pos%y, c_double)
        w = real(rect%size%width, c_double)
        h = real(rect%size%height, c_double)

        bg_color = forge_color(1.0, 1.0, 1.0, 1.0)  ! white
        border_color = COLOR_BORDER

        ! Draw background
        call cairo_rectangle(cr, x, y, w, h)
        call cairo_set_source_rgb(cr, bg_color%r, bg_color%g, bg_color%b)
        call cairo_fill_preserve(cr)

        ! Draw border (thicker if focused)
        if (present(has_focus) .and. has_focus) then
            border_color = forge_color(0.3, 0.3, 0.8, 1.0)  ! blue
            call cairo_set_line_width(cr, 2.0_c_double)
        else
            call cairo_set_line_width(cr, 1.0_c_double)
        end if
        call cairo_set_source_rgb(cr, border_color%r, border_color%g, border_color%b)
        call cairo_stroke(cr)

        ! Draw text
        if (len_trim(text) > 0) then
            text_c = trim(text) // c_null_char
            call cairo_set_source_rgb(cr, COLOR_TEXT%r, COLOR_TEXT%g, COLOR_TEXT%b)
            call cairo_select_font_face(cr, c_loc("Monospace"//c_null_char), &
                                        CAIRO_FONT_SLANT_NORMAL, CAIRO_FONT_WEIGHT_NORMAL)
            call cairo_set_font_size(cr, 13.0_c_double)
            call cairo_move_to(cr, x + 5.0, y + h/2.0 + 5.0)
            call cairo_show_text(cr, c_loc(text_c))
        end if

        ! TODO: Draw cursor if has_focus and cursor_pos provided
    end subroutine render_entry

    !> @brief Render a progress bar widget
    subroutine render_progress_bar(cr, rect, value, show_text, text)
        type(c_ptr), intent(in) :: cr
        type(forge_rect), intent(in) :: rect
        real(c_double), intent(in) :: value  ! 0.0 to 1.0
        logical, intent(in), optional :: show_text
        character(len=*), intent(in), optional :: text
        real(c_double) :: x, y, w, h, filled_width
        type(forge_color) :: bg_color, fill_color, border_color
        real(c_double) :: clamped_value
        character(len=:), allocatable :: display_text

        x = real(rect%pos%x, c_double)
        y = real(rect%pos%y, c_double)
        w = real(rect%size%width, c_double)
        h = real(rect%size%height, c_double)

        clamped_value = max(0.0_c_double, min(1.0_c_double, value))
        filled_width = w * clamped_value

        bg_color = forge_color(0.85, 0.85, 0.85, 1.0)
        fill_color = forge_color(0.3, 0.7, 0.3, 1.0)  ! green
        border_color = COLOR_BORDER

        ! Draw background
        call cairo_rectangle(cr, x, y, w, h)
        call cairo_set_source_rgb(cr, bg_color%r, bg_color%g, bg_color%b)
        call cairo_fill(cr)

        ! Draw filled portion
        if (filled_width > 0.0) then
            call cairo_rectangle(cr, x, y, filled_width, h)
            call cairo_set_source_rgb(cr, fill_color%r, fill_color%g, fill_color%b)
            call cairo_fill(cr)
        end if

        ! Draw border
        call cairo_rectangle(cr, x, y, w, h)
        call cairo_set_source_rgb(cr, border_color%r, border_color%g, border_color%b)
        call cairo_set_line_width(cr, 1.0_c_double)
        call cairo_stroke(cr)

        ! Draw text if requested
        if (present(show_text) .and. show_text) then
            if (present(text)) then
                display_text = trim(text) // c_null_char
            else
                ! Format percentage
                write(display_text, '(I0,A)') int(clamped_value * 100), "%"//c_null_char
            end if
            call cairo_set_source_rgb(cr, COLOR_TEXT%r, COLOR_TEXT%g, COLOR_TEXT%b)
            call cairo_select_font_face(cr, c_loc("Sans"//c_null_char), &
                                        CAIRO_FONT_SLANT_NORMAL, CAIRO_FONT_WEIGHT_NORMAL)
            call cairo_set_font_size(cr, 12.0_c_double)
            call cairo_move_to(cr, x + w/2.0 - 15.0, y + h/2.0 + 4.0)
            call cairo_show_text(cr, c_loc(display_text))
        end if
    end subroutine render_progress_bar

    !> @brief Render a separator widget
    subroutine render_separator(cr, rect, vertical)
        type(c_ptr), intent(in) :: cr
        type(forge_rect), intent(in) :: rect
        logical, intent(in), optional :: vertical
        real(c_double) :: x, y, w, h
        logical :: is_vertical

        x = real(rect%pos%x, c_double)
        y = real(rect%pos%y, c_double)
        w = real(rect%size%width, c_double)
        h = real(rect%size%height, c_double)

        is_vertical = .false.
        if (present(vertical)) is_vertical = vertical

        call cairo_set_source_rgb(cr, COLOR_BORDER%r, COLOR_BORDER%g, COLOR_BORDER%b)
        call cairo_set_line_width(cr, 1.0_c_double)

        if (is_vertical) then
            call cairo_move_to(cr, x + w/2.0, y)
            call cairo_line_to(cr, x + w/2.0, y + h)
        else
            call cairo_move_to(cr, x, y + h/2.0)
            call cairo_line_to(cr, x + w, y + h/2.0)
        end if
        call cairo_stroke(cr)
    end subroutine render_separator

    !> @brief Helper: Draw rounded rectangle path
    subroutine draw_rounded_rect(cr, x, y, width, height, radius)
        type(c_ptr), intent(in) :: cr
        real(c_double), intent(in) :: x, y, width, height, radius
        real(c_double) :: degrees

        degrees = PI / 180.0_c_double

        call cairo_new_path(cr)
        call cairo_arc(cr, x + width - radius, y + radius, radius, -90.0 * degrees, 0.0 * degrees)
        call cairo_arc(cr, x + width - radius, y + height - radius, radius, 0.0 * degrees, 90.0 * degrees)
        call cairo_arc(cr, x + radius, y + height - radius, radius, 90.0 * degrees, 180.0 * degrees)
        call cairo_arc(cr, x + radius, y + radius, radius, 180.0 * degrees, 270.0 * degrees)
        call cairo_close_path(cr)
    end subroutine draw_rounded_rect

end module forge_rendering

