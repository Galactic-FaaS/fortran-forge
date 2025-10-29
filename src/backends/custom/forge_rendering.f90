!> @brief Widget rendering routines
!> @details Implements drawing functions for all widget types
!> @author ForGE Contributors
!> @date 2025
!> @license GPL-3.0-or-later

module forge_rendering
    use iso_c_binding
    use forge_types
    use forge_qpen
    use forge_qbrush
    use forge_qpixmap
    use forge_qpainter
    use forge_qfont
    use forge_qpath
    use forge_cairo_bindings
    implicit none
    private

    public :: render_clear_background, render_test_pattern
    public :: render_button, render_label, render_entry
    public :: render_progress_bar, render_separator
    ! Export new Qt-like classes
    public :: forge_qpen_t, forge_qbrush_t, forge_qpixmap_t, forge_qpainter_t
    public :: forge_qfont_t, forge_qpath_t
    public :: PEN_STYLE_SOLID, PEN_STYLE_DASH, PEN_STYLE_DOT, PEN_STYLE_DASHDOT
    public :: PEN_STYLE_DASHDOTDOT, PEN_STYLE_CUSTOM
    public :: PEN_CAP_FLAT, PEN_CAP_SQUARE, PEN_CAP_ROUND
    public :: PEN_JOIN_MITER, PEN_JOIN_BEVEL, PEN_JOIN_ROUND
    public :: BRUSH_STYLE_SOLID, BRUSH_STYLE_DENSE1, BRUSH_STYLE_DENSE2, BRUSH_STYLE_DENSE3
    public :: BRUSH_STYLE_DENSE4, BRUSH_STYLE_DENSE5, BRUSH_STYLE_DENSE6, BRUSH_STYLE_DENSE7
    public :: BRUSH_STYLE_NO_BRUSH, BRUSH_STYLE_HORIZONTAL, BRUSH_STYLE_VERTICAL
    public :: BRUSH_STYLE_CROSS, BRUSH_STYLE_BDIAGONAL, BRUSH_STYLE_FDIAGONAL
    public :: BRUSH_STYLE_DIAGCROSS, BRUSH_STYLE_LINEAR_GRADIENT, BRUSH_STYLE_RADIAL_GRADIENT
    public :: BRUSH_STYLE_CONICAL_GRADIENT, BRUSH_STYLE_TEXTURE
    public :: PIXMAP_FORMAT_INVALID, PIXMAP_FORMAT_MONO, PIXMAP_FORMAT_MONO_LSB
    public :: PIXMAP_FORMAT_INDEXED8, PIXMAP_FORMAT_RGB32, PIXMAP_FORMAT_ARGB32
    public :: PIXMAP_FORMAT_ARGB32_PREMULTIPLIED, PIXMAP_FORMAT_RGB16, PIXMAP_FORMAT_ARGB8565_PREMULTIPLIED
    public :: PIXMAP_FORMAT_RGB666, PIXMAP_FORMAT_ARGB6666_PREMULTIPLIED, PIXMAP_FORMAT_RGB555
    public :: PIXMAP_FORMAT_ARGB8555_PREMULTIPLIED, PIXMAP_FORMAT_RGB888, PIXMAP_FORMAT_RGB444
    public :: PIXMAP_FORMAT_ARGB4444_PREMULTIPLIED, PIXMAP_FORMAT_RGBX8888, PIXMAP_FORMAT_RGBA8888
    public :: PIXMAP_FORMAT_RGBA8888_PREMULTIPLIED
    public :: PAINTER_ANTIALIASING_NONE, PAINTER_ANTIALIASING_DEFAULT, PAINTER_ANTIALIASING_GRAY
    public :: PAINTER_ANTIALIASING_SUBPIXEL, PAINTER_ANTIALIASING_FAST, PAINTER_ANTIALIASING_GOOD
    public :: PAINTER_ANTIALIASING_BEST
    public :: PAINTER_COMPOSITION_MODE_SOURCE_OVER, PAINTER_COMPOSITION_MODE_DESTINATION_OVER
    public :: PAINTER_COMPOSITION_MODE_CLEAR, PAINTER_COMPOSITION_MODE_SOURCE
    public :: PAINTER_COMPOSITION_MODE_DESTINATION, PAINTER_COMPOSITION_MODE_SOURCE_IN
    public :: PAINTER_COMPOSITION_MODE_DESTINATION_IN, PAINTER_COMPOSITION_MODE_SOURCE_OUT
    public :: PAINTER_COMPOSITION_MODE_DESTINATION_OUT, PAINTER_COMPOSITION_MODE_SOURCE_ATOP
    public :: PAINTER_COMPOSITION_MODE_DESTINATION_ATOP, PAINTER_COMPOSITION_MODE_XOR
    public :: PAINTER_COMPOSITION_MODE_PLUS, PAINTER_COMPOSITION_MODE_MULTIPLY
    public :: PAINTER_COMPOSITION_MODE_SCREEN, PAINTER_COMPOSITION_MODE_OVERLAY
    public :: PAINTER_COMPOSITION_MODE_DARKEN, PAINTER_COMPOSITION_MODE_LIGHTEN
    public :: PAINTER_COMPOSITION_MODE_COLOR_DODGE, PAINTER_COMPOSITION_MODE_COLOR_BURN
    public :: PAINTER_COMPOSITION_MODE_HARD_LIGHT, PAINTER_COMPOSITION_MODE_SOFT_LIGHT
    public :: PAINTER_COMPOSITION_MODE_DIFFERENCE, PAINTER_COMPOSITION_MODE_EXCLUSION
    public :: FONT_STYLE_NORMAL, FONT_STYLE_ITALIC, FONT_STYLE_OBLIQUE
    public :: FONT_WEIGHT_THIN, FONT_WEIGHT_LIGHT, FONT_WEIGHT_NORMAL, FONT_WEIGHT_MEDIUM
    public :: FONT_WEIGHT_BOLD, FONT_WEIGHT_BLACK
    public :: FONT_HINTING_NONE, FONT_HINTING_VERTICAL, FONT_HINTING_FULL
    public :: FONT_ANTIALIAS_NONE, FONT_ANTIALIAS_GRAY, FONT_ANTIALIAS_SUBPIXEL
    public :: PATH_ELEMENT_MOVE_TO, PATH_ELEMENT_LINE_TO, PATH_ELEMENT_CURVE_TO
    public :: PATH_ELEMENT_CLOSE_SUBPATH
    public :: render_checkbox, render_radiobutton, render_combobox
    public :: render_spinbox, render_double_spinbox, render_slider
    public :: render_dateedit, render_timeedit
    public :: render_groupbox, render_tabwidget, render_scrollarea, render_scrollbar
    public :: render_menubar, render_menu, render_action
    public :: render_listview, render_listwidget
    public :: render_messagebox, render_statusbar

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

            ! Measure text for proper centering
            type(cairo_text_extents_t) :: extents
            call cairo_text_extents(cr, c_loc(label_c), extents)
            call cairo_move_to(cr, x + (w - extents%width)/2.0, y + h/2.0 + extents%height/2.0)
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

        ! Draw text
        text_c = trim(text) // c_null_char
        call cairo_set_source_rgb(cr, COLOR_TEXT%r, COLOR_TEXT%g, COLOR_TEXT%b)
        call cairo_select_font_face(cr, c_loc("Sans"//c_null_char), &
                                    CAIRO_FONT_SLANT_NORMAL, CAIRO_FONT_WEIGHT_NORMAL)
        call cairo_set_font_size(cr, 14.0_c_double)

        ! Measure text for proper alignment
        type(cairo_text_extents_t) :: extents
        call cairo_text_extents(cr, c_loc(text_c), extents)

        ! Adjust x based on alignment
        select case (align)
        case (1)  ! center
            x = x + (real(rect%size%width, c_double) - extents%width) / 2.0
        case (2)  ! right
            x = x + real(rect%size%width, c_double) - extents%width - 5.0
        end select

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

        ! Draw cursor if has_focus and cursor_pos provided
        if (present(has_focus) .and. has_focus .and. present(cursor_pos)) then
            if (cursor_pos >= 0 .and. cursor_pos <= len_trim(text)) then
                ! Calculate cursor position based on text width up to cursor_pos
                character(len=cursor_pos) :: text_before_cursor
                real(c_double) :: cursor_x
                type(cairo_text_extents_t) :: cursor_extents

                if (cursor_pos > 0) then
                    text_before_cursor = text(1:cursor_pos)
                    text_before_cursor = trim(text_before_cursor) // c_null_char
                    call cairo_text_extents(cr, c_loc(text_before_cursor), cursor_extents)
                    cursor_x = x + 5.0 + cursor_extents%x_advance
                else
                    cursor_x = x + 5.0
                end if

                ! Draw cursor as a vertical line
                call cairo_set_source_rgb(cr, 0.0_c_double, 0.0_c_double, 0.0_c_double)
                call cairo_set_line_width(cr, 1.0_c_double)
                call cairo_move_to(cr, cursor_x, y + 3.0)
                call cairo_line_to(cr, cursor_x, y + h - 3.0)
                call cairo_stroke(cr)
            end if
        end if
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

    !> @brief Render a checkbox widget
    subroutine render_checkbox(cr, rect, text, checked, has_focus)
        type(c_ptr), intent(in) :: cr
        type(forge_rect), intent(in) :: rect
        character(len=*), intent(in) :: text
        logical, intent(in) :: checked
        logical, intent(in), optional :: has_focus
        real(c_double) :: x, y, w, h, box_size, box_x, box_y
        type(forge_color) :: bg_color, border_color
        character(len=:), allocatable :: text_c

        x = real(rect%pos%x, c_double)
        y = real(rect%pos%y, c_double)
        w = real(rect%size%width, c_double)
        h = real(rect%size%height, c_double)
        box_size = 16.0_c_double
        box_x = x + 2.0
        box_y = y + (h - box_size) / 2.0

        ! Draw checkbox box
        bg_color = forge_color(1.0, 1.0, 1.0, 1.0)
        border_color = COLOR_BORDER
        if (present(has_focus) .and. has_focus) then
            border_color = forge_color(0.3, 0.3, 0.8, 1.0)
        end if

        call cairo_rectangle(cr, box_x, box_y, box_size, box_size)
        call cairo_set_source_rgb(cr, bg_color%r, bg_color%g, bg_color%b)
        call cairo_fill_preserve(cr)
        call cairo_set_source_rgb(cr, border_color%r, border_color%g, border_color%b)
        call cairo_set_line_width(cr, 1.0_c_double)
        call cairo_stroke(cr)

        ! Draw check mark if checked
        if (checked) then
            call cairo_set_source_rgb(cr, 0.0_c_double, 0.0_c_double, 0.0_c_double)
            call cairo_set_line_width(cr, 2.0_c_double)
            call cairo_move_to(cr, box_x + 3.0, box_y + box_size/2.0)
            call cairo_line_to(cr, box_x + box_size/2.0 - 1.0, box_y + box_size - 3.0)
            call cairo_line_to(cr, box_x + box_size - 3.0, box_y + 3.0)
            call cairo_stroke(cr)
        end if

        ! Draw text
        if (len_trim(text) > 0) then
            text_c = trim(text) // c_null_char
            call cairo_set_source_rgb(cr, COLOR_TEXT%r, COLOR_TEXT%g, COLOR_TEXT%b)
            call cairo_select_font_face(cr, c_loc("Sans"//c_null_char), &
                                        CAIRO_FONT_SLANT_NORMAL, CAIRO_FONT_WEIGHT_NORMAL)
            call cairo_set_font_size(cr, 14.0_c_double)
            call cairo_move_to(cr, box_x + box_size + 8.0, y + h/2.0 + 5.0)
            call cairo_show_text(cr, c_loc(text_c))
        end if
    end subroutine render_checkbox

    !> @brief Render a radiobutton widget
    subroutine render_radiobutton(cr, rect, text, checked, has_focus)
        type(c_ptr), intent(in) :: cr
        type(forge_rect), intent(in) :: rect
        character(len=*), intent(in) :: text
        logical, intent(in) :: checked
        logical, intent(in), optional :: has_focus
        real(c_double) :: x, y, w, h, radius, center_x, center_y
        type(forge_color) :: border_color
        character(len=:), allocatable :: text_c

        x = real(rect%pos%x, c_double)
        y = real(rect%pos%y, c_double)
        w = real(rect%size%width, c_double)
        h = real(rect%size%height, c_double)
        radius = 8.0_c_double
        center_x = x + 2.0 + radius
        center_y = y + h/2.0

        ! Draw radio button circle
        border_color = COLOR_BORDER
        if (present(has_focus) .and. has_focus) then
            border_color = forge_color(0.3, 0.3, 0.8, 1.0)
        end if

        call cairo_arc(cr, center_x, center_y, radius, 0.0_c_double, 2.0_c_double * PI)
        call cairo_set_source_rgb(cr, 1.0_c_double, 1.0_c_double, 1.0_c_double)
        call cairo_fill_preserve(cr)
        call cairo_set_source_rgb(cr, border_color%r, border_color%g, border_color%b)
        call cairo_set_line_width(cr, 1.0_c_double)
        call cairo_stroke(cr)

        ! Draw inner circle if checked
        if (checked) then
            call cairo_arc(cr, center_x, center_y, radius - 3.0, 0.0_c_double, 2.0_c_double * PI)
            call cairo_set_source_rgb(cr, 0.0_c_double, 0.0_c_double, 0.0_c_double)
            call cairo_fill(cr)
        end if

        ! Draw text
        if (len_trim(text) > 0) then
            text_c = trim(text) // c_null_char
            call cairo_set_source_rgb(cr, COLOR_TEXT%r, COLOR_TEXT%g, COLOR_TEXT%b)
            call cairo_select_font_face(cr, c_loc("Sans"//c_null_char), &
                                        CAIRO_FONT_SLANT_NORMAL, CAIRO_FONT_WEIGHT_NORMAL)
            call cairo_set_font_size(cr, 14.0_c_double)
            call cairo_move_to(cr, center_x + radius + 8.0, y + h/2.0 + 5.0)
            call cairo_show_text(cr, c_loc(text_c))
        end if
    end subroutine render_radiobutton

    !> @brief Render a combobox widget
    subroutine render_combobox(cr, rect, current_text, has_focus, dropdown_visible)
        type(c_ptr), intent(in) :: cr
        type(forge_rect), intent(in) :: rect
        character(len=*), intent(in) :: current_text
        logical, intent(in), optional :: has_focus, dropdown_visible
        real(c_double) :: x, y, w, h, arrow_width
        type(forge_color) :: bg_color, border_color
        character(len=:), allocatable :: text_c

        x = real(rect%pos%x, c_double)
        y = real(rect%pos%y, c_double)
        w = real(rect%size%width, c_double)
        h = real(rect%size%height, c_double)
        arrow_width = 20.0_c_double

        bg_color = forge_color(1.0, 1.0, 1.0, 1.0)
        border_color = COLOR_BORDER
        if (present(has_focus) .and. has_focus) then
            border_color = forge_color(0.3, 0.3, 0.8, 1.0)
        end if

        ! Draw background
        call cairo_rectangle(cr, x, y, w, h)
        call cairo_set_source_rgb(cr, bg_color%r, bg_color%g, bg_color%b)
        call cairo_fill_preserve(cr)
        call cairo_set_source_rgb(cr, border_color%r, border_color%g, border_color%b)
        call cairo_set_line_width(cr, 1.0_c_double)
        call cairo_stroke(cr)

        ! Draw dropdown arrow
        call cairo_set_source_rgb(cr, 0.0_c_double, 0.0_c_double, 0.0_c_double)
        call cairo_move_to(cr, x + w - arrow_width/2.0 - 5.0, y + h/2.0 - 2.0)
        call cairo_line_to(cr, x + w - arrow_width/2.0, y + h/2.0 + 2.0)
        call cairo_line_to(cr, x + w - arrow_width/2.0 + 5.0, y + h/2.0 - 2.0)
        call cairo_close_path(cr)
        call cairo_fill(cr)

        ! Draw text
        if (len_trim(current_text) > 0) then
            text_c = trim(current_text) // c_null_char
            call cairo_set_source_rgb(cr, COLOR_TEXT%r, COLOR_TEXT%g, COLOR_TEXT%b)
            call cairo_select_font_face(cr, c_loc("Sans"//c_null_char), &
                                        CAIRO_FONT_SLANT_NORMAL, CAIRO_FONT_WEIGHT_NORMAL)
            call cairo_set_font_size(cr, 14.0_c_double)
            call cairo_move_to(cr, x + 5.0, y + h/2.0 + 5.0)
            call cairo_show_text(cr, c_loc(text_c))
        end if
    end subroutine render_combobox

    !> @brief Render a spinbox widget
    subroutine render_spinbox(cr, rect, text, has_focus, up_pressed, down_pressed)
        type(c_ptr), intent(in) :: cr
        type(forge_rect), intent(in) :: rect
        character(len=*), intent(in) :: text
        logical, intent(in), optional :: has_focus, up_pressed, down_pressed
        real(c_double) :: x, y, w, h, button_width
        type(forge_color) :: bg_color, border_color, button_bg
        character(len=:), allocatable :: text_c

        x = real(rect%pos%x, c_double)
        y = real(rect%pos%y, c_double)
        w = real(rect%size%width, c_double)
        h = real(rect%size%height, c_double)
        button_width = 20.0_c_double

        bg_color = forge_color(1.0, 1.0, 1.0, 1.0)
        border_color = COLOR_BORDER
        if (present(has_focus) .and. has_focus) then
            border_color = forge_color(0.3, 0.3, 0.8, 1.0)
        end if

        ! Draw main background
        call cairo_rectangle(cr, x, y, w - button_width, h)
        call cairo_set_source_rgb(cr, bg_color%r, bg_color%g, bg_color%b)
        call cairo_fill_preserve(cr)
        call cairo_set_source_rgb(cr, border_color%r, border_color%g, border_color%b)
        call cairo_set_line_width(cr, 1.0_c_double)
        call cairo_stroke(cr)

        ! Draw up button
        button_bg = COLOR_BUTTON_NORMAL
        if (present(up_pressed) .and. up_pressed) button_bg = COLOR_BUTTON_PRESSED
        call cairo_rectangle(cr, x + w - button_width, y, button_width, h/2.0)
        call cairo_set_source_rgb(cr, button_bg%r, button_bg%g, button_bg%b)
        call cairo_fill_preserve(cr)
        call cairo_set_source_rgb(cr, COLOR_BORDER%r, COLOR_BORDER%g, COLOR_BORDER%b)
        call cairo_stroke(cr)

        ! Draw down button
        button_bg = COLOR_BUTTON_NORMAL
        if (present(down_pressed) .and. down_pressed) button_bg = COLOR_BUTTON_PRESSED
        call cairo_rectangle(cr, x + w - button_width, y + h/2.0, button_width, h/2.0)
        call cairo_set_source_rgb(cr, button_bg%r, button_bg%g, button_bg%b)
        call cairo_fill_preserve(cr)
        call cairo_set_source_rgb(cr, COLOR_BORDER%r, COLOR_BORDER%g, COLOR_BORDER%b)
        call cairo_stroke(cr)

        ! Draw arrows
        call cairo_set_source_rgb(cr, 0.0_c_double, 0.0_c_double, 0.0_c_double)
        ! Up arrow
        call cairo_move_to(cr, x + w - button_width/2.0 - 3.0, y + h/4.0 + 2.0)
        call cairo_line_to(cr, x + w - button_width/2.0, y + h/4.0 - 2.0)
        call cairo_line_to(cr, x + w - button_width/2.0 + 3.0, y + h/4.0 + 2.0)
        call cairo_close_path(cr)
        call cairo_fill(cr)
        ! Down arrow
        call cairo_move_to(cr, x + w - button_width/2.0 - 3.0, y + 3.0*h/4.0 - 2.0)
        call cairo_line_to(cr, x + w - button_width/2.0, y + 3.0*h/4.0 + 2.0)
        call cairo_line_to(cr, x + w - button_width/2.0 + 3.0, y + 3.0*h/4.0 - 2.0)
        call cairo_close_path(cr)
        call cairo_fill(cr)

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
    end subroutine render_spinbox

    !> @brief Render a double spinbox widget (same as spinbox for now)
    subroutine render_double_spinbox(cr, rect, text, has_focus, up_pressed, down_pressed)
        type(c_ptr), intent(in) :: cr
        type(forge_rect), intent(in) :: rect
        character(len=*), intent(in) :: text
        logical, intent(in), optional :: has_focus, up_pressed, down_pressed
        call render_spinbox(cr, rect, text, has_focus, up_pressed, down_pressed)
    end subroutine render_double_spinbox

    !> @brief Render a slider widget
    subroutine render_slider(cr, rect, value, min_val, max_val, orientation, has_focus)
        type(c_ptr), intent(in) :: cr
        type(forge_rect), intent(in) :: rect
        real(c_double), intent(in) :: value, min_val, max_val
        integer, intent(in) :: orientation  ! 1=horizontal, 2=vertical
        logical, intent(in), optional :: has_focus
        real(c_double) :: x, y, w, h, groove_width, handle_size, handle_pos
        type(forge_color) :: groove_color, handle_color, border_color

        x = real(rect%pos%x, c_double)
        y = real(rect%pos%y, c_double)
        w = real(rect%size%width, c_double)
        h = real(rect%size%height, c_double)
        groove_width = 4.0_c_double
        handle_size = 16.0_c_double

        groove_color = forge_color(0.8, 0.8, 0.8, 1.0)
        handle_color = COLOR_BUTTON_NORMAL
        border_color = COLOR_BORDER
        if (present(has_focus) .and. has_focus) then
            border_color = forge_color(0.3, 0.3, 0.8, 1.0)
        end if

        if (orientation == 1) then  ! Horizontal
            ! Draw groove
            call cairo_rectangle(cr, x, y + (h - groove_width)/2.0, w, groove_width)
            call cairo_set_source_rgb(cr, groove_color%r, groove_color%g, groove_color%b)
            call cairo_fill(cr)

            ! Calculate handle position
            handle_pos = x + (value - min_val) / (max_val - min_val) * (w - handle_size)
            handle_pos = max(x, min(handle_pos, x + w - handle_size))

            ! Draw handle
            call cairo_rectangle(cr, handle_pos, y + (h - handle_size)/2.0, handle_size, handle_size)
            call cairo_set_source_rgb(cr, handle_color%r, handle_color%g, handle_color%b)
            call cairo_fill_preserve(cr)
            call cairo_set_source_rgb(cr, border_color%r, border_color%g, border_color%b)
            call cairo_set_line_width(cr, 1.0_c_double)
            call cairo_stroke(cr)
        else  ! Vertical
            ! Draw groove
            call cairo_rectangle(cr, x + (w - groove_width)/2.0, y, groove_width, h)
            call cairo_set_source_rgb(cr, groove_color%r, groove_color%g, groove_color%b)
            call cairo_fill(cr)

            ! Calculate handle position
            handle_pos = y + (max_val - value) / (max_val - min_val) * (h - handle_size)
            handle_pos = max(y, min(handle_pos, y + h - handle_size))

            ! Draw handle
            call cairo_rectangle(cr, x + (w - handle_size)/2.0, handle_pos, handle_size, handle_size)
            call cairo_set_source_rgb(cr, handle_color%r, handle_color%g, handle_color%b)
            call cairo_fill_preserve(cr)
            call cairo_set_source_rgb(cr, border_color%r, border_color%g, border_color%b)
            call cairo_set_line_width(cr, 1.0_c_double)
            call cairo_stroke(cr)
        end if
    end subroutine render_slider

    !> @brief Render a dateedit widget
    subroutine render_dateedit(cr, rect, date_text, has_focus, calendar_visible)
        type(c_ptr), intent(in) :: cr
        type(forge_rect), intent(in) :: rect
        character(len=*), intent(in) :: date_text
        logical, intent(in), optional :: has_focus, calendar_visible
        real(c_double) :: x, y, w, h, button_width
        type(forge_color) :: bg_color, border_color, button_bg
        character(len=:), allocatable :: text_c

        x = real(rect%pos%x, c_double)
        y = real(rect%pos%y, c_double)
        w = real(rect%size%width, c_double)
        h = real(rect%size%height, c_double)
        button_width = 20.0_c_double

        bg_color = forge_color(1.0, 1.0, 1.0, 1.0)
        border_color = COLOR_BORDER
        if (present(has_focus) .and. has_focus) then
            border_color = forge_color(0.3, 0.3, 0.8, 1.0)
        end if

        ! Draw main background
        call cairo_rectangle(cr, x, y, w - button_width, h)
        call cairo_set_source_rgb(cr, bg_color%r, bg_color%g, bg_color%b)
        call cairo_fill_preserve(cr)
        call cairo_set_source_rgb(cr, border_color%r, border_color%g, border_color%b)
        call cairo_set_line_width(cr, 1.0_c_double)
        call cairo_stroke(cr)

        ! Draw calendar button
        button_bg = COLOR_BUTTON_NORMAL
        if (present(calendar_visible) .and. calendar_visible) button_bg = COLOR_BUTTON_PRESSED
        call cairo_rectangle(cr, x + w - button_width, y, button_width, h)
        call cairo_set_source_rgb(cr, button_bg%r, button_bg%g, button_bg%b)
        call cairo_fill_preserve(cr)
        call cairo_set_source_rgb(cr, COLOR_BORDER%r, COLOR_BORDER%g, COLOR_BORDER%b)
        call cairo_stroke(cr)

        ! Draw calendar icon (simple grid)
        call cairo_set_source_rgb(cr, 0.0_c_double, 0.0_c_double, 0.0_c_double)
        call cairo_set_line_width(cr, 1.0_c_double)
        call cairo_rectangle(cr, x + w - button_width + 3.0, y + 3.0, button_width - 6.0, h - 6.0)
        call cairo_stroke(cr)
        ! Simple calendar lines
        call cairo_move_to(cr, x + w - button_width + 3.0, y + 8.0)
        call cairo_line_to(cr, x + w - button_width + button_width - 3.0, y + 8.0)
        call cairo_move_to(cr, x + w - button_width + 3.0, y + 13.0)
        call cairo_line_to(cr, x + w - button_width + button_width - 3.0, y + 13.0)
        call cairo_stroke(cr)

        ! Draw text
        if (len_trim(date_text) > 0) then
            text_c = trim(date_text) // c_null_char
            call cairo_set_source_rgb(cr, COLOR_TEXT%r, COLOR_TEXT%g, COLOR_TEXT%b)
            call cairo_select_font_face(cr, c_loc("Monospace"//c_null_char), &
                                        CAIRO_FONT_SLANT_NORMAL, CAIRO_FONT_WEIGHT_NORMAL)
            call cairo_set_font_size(cr, 13.0_c_double)
            call cairo_move_to(cr, x + 5.0, y + h/2.0 + 5.0)
            call cairo_show_text(cr, c_loc(text_c))
        end if
    end subroutine render_dateedit

    !> @brief Render a timeedit widget
    subroutine render_timeedit(cr, rect, time_text, has_focus)
        type(c_ptr), intent(in) :: cr
        type(forge_rect), intent(in) :: rect
        character(len=*), intent(in) :: time_text
        logical, intent(in), optional :: has_focus
        real(c_double) :: x, y, w, h
        type(forge_color) :: bg_color, border_color
        character(len=:), allocatable :: text_c

        x = real(rect%pos%x, c_double)
        y = real(rect%pos%y, c_double)
        w = real(rect%size%width, c_double)
        h = real(rect%size%height, c_double)

        bg_color = forge_color(1.0, 1.0, 1.0, 1.0)
        border_color = COLOR_BORDER
        if (present(has_focus) .and. has_focus) then
            border_color = forge_color(0.3, 0.3, 0.8, 1.0)
        end if

        ! Draw background
        call cairo_rectangle(cr, x, y, w, h)
        call cairo_set_source_rgb(cr, bg_color%r, bg_color%g, bg_color%b)
        call cairo_fill_preserve(cr)
        call cairo_set_source_rgb(cr, border_color%r, border_color%g, border_color%b)
        call cairo_set_line_width(cr, 1.0_c_double)
        call cairo_stroke(cr)

        ! Draw text
        if (len_trim(time_text) > 0) then
            text_c = trim(time_text) // c_null_char
            call cairo_set_source_rgb(cr, COLOR_TEXT%r, COLOR_TEXT%g, COLOR_TEXT%b)
            call cairo_select_font_face(cr, c_loc("Monospace"//c_null_char), &
                                        CAIRO_FONT_SLANT_NORMAL, CAIRO_FONT_WEIGHT_NORMAL)
            call cairo_set_font_size(cr, 13.0_c_double)
            call cairo_move_to(cr, x + 5.0, y + h/2.0 + 5.0)
            call cairo_show_text(cr, c_loc(text_c))
        end if
    end subroutine render_timeedit

    !> @brief Render a groupbox widget
    subroutine render_groupbox(cr, rect, title, flat)
        type(c_ptr), intent(in) :: cr
        type(forge_rect), intent(in) :: rect
        character(len=*), intent(in) :: title
        logical, intent(in), optional :: flat
        real(c_double) :: x, y, w, h, text_width, text_height
        type(forge_color) :: border_color
        character(len=:), allocatable :: title_c
        logical :: is_flat

        x = real(rect%pos%x, c_double)
        y = real(rect%pos%y, c_double)
        w = real(rect%size%width, c_double)
        h = real(rect%size%height, c_double)

        is_flat = .false.
        if (present(flat)) is_flat = flat

        border_color = COLOR_BORDER

        ! Draw border
        if (.not. is_flat) then
            call cairo_set_source_rgb(cr, border_color%r, border_color%g, border_color%b)
            call cairo_set_line_width(cr, 1.0_c_double)
            call cairo_rectangle(cr, x, y, w, h)
            call cairo_stroke(cr)
        end if

        ! Draw title background and text
        if (len_trim(title) > 0) then
            title_c = trim(title) // c_null_char
            call cairo_set_source_rgb(cr, COLOR_BACKGROUND%r, COLOR_BACKGROUND%g, COLOR_BACKGROUND%b)
            call cairo_select_font_face(cr, c_loc("Sans"//c_null_char), &
                                        CAIRO_FONT_SLANT_NORMAL, CAIRO_FONT_WEIGHT_NORMAL)
            call cairo_set_font_size(cr, 12.0_c_double)

            ! Measure text width for proper background
            type(cairo_text_extents_t) :: title_extents
            call cairo_text_extents(cr, c_loc(title_c), title_extents)
            text_width = title_extents%width + 4.0_c_double  ! Add some padding
            text_height = 16.0_c_double

            ! Draw background for title
            call cairo_rectangle(cr, x + 10.0, y - text_height/2.0, text_width, text_height)
            call cairo_fill(cr)

            ! Draw title text
            call cairo_set_source_rgb(cr, COLOR_TEXT%r, COLOR_TEXT%g, COLOR_TEXT%b)
            call cairo_move_to(cr, x + 12.0, y + 4.0)
            call cairo_show_text(cr, c_loc(title_c))
        end if
    end subroutine render_groupbox

    !> @brief Render a tabwidget widget
    subroutine render_tabwidget(cr, rect, tab_texts, current_index, tab_position)
        type(c_ptr), intent(in) :: cr
        type(forge_rect), intent(in) :: rect
        character(len=*), intent(in) :: tab_texts(:)
        integer, intent(in) :: current_index, tab_position
        real(c_double) :: x, y, w, h, tab_height, tab_width, current_x
        type(forge_color) :: tab_bg, active_tab_bg, border_color
        integer :: i
        character(len=:), allocatable :: text_c

        x = real(rect%pos%x, c_double)
        y = real(rect%pos%y, c_double)
        w = real(rect%size%width, c_double)
        h = real(rect%size%height, c_double)
        tab_height = 25.0_c_double
        tab_width = w / size(tab_texts)

        tab_bg = forge_color(0.9, 0.9, 0.9, 1.0)
        active_tab_bg = forge_color(1.0, 1.0, 1.0, 1.0)
        border_color = COLOR_BORDER

        current_x = x

        ! Draw tabs
        do i = 1, size(tab_texts)
            ! Tab background
            if (i-1 == current_index) then
                call cairo_set_source_rgb(cr, active_tab_bg%r, active_tab_bg%g, active_tab_bg%b)
            else
                call cairo_set_source_rgb(cr, tab_bg%r, tab_bg%g, tab_bg%b)
            end if
            call cairo_rectangle(cr, current_x, y, tab_width, tab_height)
            call cairo_fill_preserve(cr)
            call cairo_set_source_rgb(cr, border_color%r, border_color%g, border_color%b)
            call cairo_set_line_width(cr, 1.0_c_double)
            call cairo_stroke(cr)

            ! Tab text
            if (len_trim(tab_texts(i)) > 0) then
                text_c = trim(tab_texts(i)) // c_null_char
                call cairo_set_source_rgb(cr, COLOR_TEXT%r, COLOR_TEXT%g, COLOR_TEXT%b)
                call cairo_select_font_face(cr, c_loc("Sans"//c_null_char), &
                                            CAIRO_FONT_SLANT_NORMAL, CAIRO_FONT_WEIGHT_NORMAL)
                call cairo_set_font_size(cr, 12.0_c_double)
                call cairo_move_to(cr, current_x + 5.0, y + tab_height/2.0 + 4.0)
                call cairo_show_text(cr, c_loc(text_c))
            end if

            current_x = current_x + tab_width
        end do

        ! Draw content area border
        call cairo_set_source_rgb(cr, border_color%r, border_color%g, border_color%b)
        call cairo_rectangle(cr, x, y + tab_height, w, h - tab_height)
        call cairo_stroke(cr)
    end subroutine render_tabwidget

    !> @brief Render a scrollarea widget
    subroutine render_scrollarea(cr, rect, content_rect, h_scroll_visible, v_scroll_visible)
        type(c_ptr), intent(in) :: cr
        type(forge_rect), intent(in) :: rect, content_rect
        logical, intent(in) :: h_scroll_visible, v_scroll_visible
        real(c_double) :: x, y, w, h, scrollbar_size
        type(forge_color) :: border_color

        x = real(rect%pos%x, c_double)
        y = real(rect%pos%y, c_double)
        w = real(rect%size%width, c_double)
        h = real(rect%size%height, c_double)
        scrollbar_size = 16.0_c_double

        border_color = COLOR_BORDER

        ! Draw main border
        call cairo_set_source_rgb(cr, border_color%r, border_color%g, border_color%b)
        call cairo_set_line_width(cr, 1.0_c_double)
        call cairo_rectangle(cr, x, y, w, h)
        call cairo_stroke(cr)

        ! Draw scrollbars if visible
        if (v_scroll_visible) then
            call cairo_rectangle(cr, x + w - scrollbar_size, y, scrollbar_size, h - scrollbar_size)
            call cairo_fill(cr)
        end if
        if (h_scroll_visible) then
            call cairo_rectangle(cr, x, y + h - scrollbar_size, w - scrollbar_size, scrollbar_size)
            call cairo_fill(cr)
        end if
    end subroutine render_scrollarea

    !> @brief Render a scrollbar widget
    subroutine render_scrollbar(cr, rect, value, min_val, max_val, orientation, has_focus)
        type(c_ptr), intent(in) :: cr
        type(forge_rect), intent(in) :: rect
        real(c_double), intent(in) :: value, min_val, max_val
        integer, intent(in) :: orientation  ! 1=horizontal, 2=vertical
        logical, intent(in), optional :: has_focus
        real(c_double) :: x, y, w, h, handle_size, handle_pos, button_size
        type(forge_color) :: bg_color, handle_color, border_color

        x = real(rect%pos%x, c_double)
        y = real(rect%pos%y, c_double)
        w = real(rect%size%width, c_double)
        h = real(rect%size%height, c_double)
        button_size = 16.0_c_double

        bg_color = forge_color(0.95, 0.95, 0.95, 1.0)
        handle_color = forge_color(0.8, 0.8, 0.8, 1.0)
        border_color = COLOR_BORDER
        if (present(has_focus) .and. has_focus) then
            border_color = forge_color(0.3, 0.3, 0.8, 1.0)
        end if

        ! Draw background
        call cairo_rectangle(cr, x, y, w, h)
        call cairo_set_source_rgb(cr, bg_color%r, bg_color%g, bg_color%b)
        call cairo_fill_preserve(cr)
        call cairo_set_source_rgb(cr, border_color%r, border_color%g, border_color%b)
        call cairo_set_line_width(cr, 1.0_c_double)
        call cairo_stroke(cr)

        if (orientation == 1) then  ! Horizontal
            handle_size = max(button_size, w * 0.3_c_double)
            handle_pos = x + button_size + (value - min_val) / (max_val - min_val) * (w - 2.0*button_size - handle_size)
            handle_pos = max(x + button_size, min(handle_pos, x + w - button_size - handle_size))

            ! Draw handle
            call cairo_rectangle(cr, handle_pos, y, handle_size, h)
            call cairo_set_source_rgb(cr, handle_color%r, handle_color%g, handle_color%b)
            call cairo_fill_preserve(cr)
            call cairo_set_source_rgb(cr, border_color%r, border_color%g, border_color%b)
            call cairo_stroke(cr)

            ! Draw arrows (simplified)
            call cairo_set_source_rgb(cr, 0.0_c_double, 0.0_c_double, 0.0_c_double)
            ! Left arrow
            call cairo_move_to(cr, x + button_size/2.0 - 3.0, y + h/2.0)
            call cairo_line_to(cr, x + button_size/2.0 + 3.0, y + h/2.0 - 3.0)
            call cairo_line_to(cr, x + button_size/2.0 + 3.0, y + h/2.0 + 3.0)
            call cairo_close_path(cr)
            call cairo_fill(cr)
            ! Right arrow
            call cairo_move_to(cr, x + w - button_size/2.0 + 3.0, y + h/2.0)
            call cairo_line_to(cr, x + w - button_size/2.0 - 3.0, y + h/2.0 - 3.0)
            call cairo_line_to(cr, x + w - button_size/2.0 - 3.0, y + h/2.0 + 3.0)
            call cairo_close_path(cr)
            call cairo_fill(cr)
        else  ! Vertical
            handle_size = max(button_size, h * 0.3_c_double)
            handle_pos = y + button_size + (value - min_val) / (max_val - min_val) * (h - 2.0*button_size - handle_size)
            handle_pos = max(y + button_size, min(handle_pos, y + h - button_size - handle_size))

            ! Draw handle
            call cairo_rectangle(cr, x, handle_pos, w, handle_size)
            call cairo_set_source_rgb(cr, handle_color%r, handle_color%g, handle_color%b)
            call cairo_fill_preserve(cr)
            call cairo_set_source_rgb(cr, border_color%r, border_color%g, border_color%b)
            call cairo_stroke(cr)

            ! Draw arrows (simplified)
            call cairo_set_source_rgb(cr, 0.0_c_double, 0.0_c_double, 0.0_c_double)
            ! Up arrow
            call cairo_move_to(cr, x + w/2.0, y + button_size/2.0 - 3.0)
            call cairo_line_to(cr, x + w/2.0 - 3.0, y + button_size/2.0 + 3.0)
            call cairo_line_to(cr, x + w/2.0 + 3.0, y + button_size/2.0 + 3.0)
            call cairo_close_path(cr)
            call cairo_fill(cr)
            ! Down arrow
            call cairo_move_to(cr, x + w/2.0, y + h - button_size/2.0 + 3.0)
            call cairo_line_to(cr, x + w/2.0 - 3.0, y + h - button_size/2.0 - 3.0)
            call cairo_line_to(cr, x + w/2.0 + 3.0, y + h - button_size/2.0 - 3.0)
            call cairo_close_path(cr)
            call cairo_fill(cr)
        end if
    end subroutine render_scrollbar

    !> @brief Render a menubar widget
    subroutine render_menubar(cr, rect, menu_titles, active_menu)
        type(c_ptr), intent(in) :: cr
        type(forge_rect), intent(in) :: rect
        character(len=*), intent(in) :: menu_titles(:)
        integer, intent(in), optional :: active_menu
        real(c_double) :: x, y, w, h, menu_width, current_x
        type(forge_color) :: bg_color, active_bg_color, border_color
        integer :: i
        character(len=:), allocatable :: text_c

        x = real(rect%pos%x, c_double)
        y = real(rect%pos%y, c_double)
        w = real(rect%size%width, c_double)
        h = real(rect%size%height, c_double)
        menu_width = w / size(menu_titles)

        bg_color = forge_color(0.95, 0.95, 0.95, 1.0)
        active_bg_color = forge_color(0.85, 0.85, 0.95, 1.0)
        border_color = COLOR_BORDER

        current_x = x

        ! Draw menubar background
        call cairo_rectangle(cr, x, y, w, h)
        call cairo_set_source_rgb(cr, bg_color%r, bg_color%g, bg_color%b)
        call cairo_fill_preserve(cr)
        call cairo_set_source_rgb(cr, border_color%r, border_color%g, border_color%b)
        call cairo_set_line_width(cr, 1.0_c_double)
        call cairo_stroke(cr)

        ! Draw menu items
        do i = 1, size(menu_titles)
            ! Highlight active menu
            if (present(active_menu) .and. i-1 == active_menu) then
                call cairo_set_source_rgb(cr, active_bg_color%r, active_bg_color%g, active_bg_color%b)
                call cairo_rectangle(cr, current_x, y, menu_width, h)
                call cairo_fill(cr)
            end if

            ! Menu text
            if (len_trim(menu_titles(i)) > 0) then
                text_c = trim(menu_titles(i)) // c_null_char
                call cairo_set_source_rgb(cr, COLOR_TEXT%r, COLOR_TEXT%g, COLOR_TEXT%b)
                call cairo_select_font_face(cr, c_loc("Sans"//c_null_char), &
                                            CAIRO_FONT_SLANT_NORMAL, CAIRO_FONT_WEIGHT_NORMAL)
                call cairo_set_font_size(cr, 12.0_c_double)
                call cairo_move_to(cr, current_x + 5.0, y + h/2.0 + 4.0)
                call cairo_show_text(cr, c_loc(text_c))
            end if

            current_x = current_x + menu_width
        end do
    end subroutine render_menubar

    !> @brief Render a menu widget
    subroutine render_menu(cr, rect, menu_items, checked_items, separator_items)
        type(c_ptr), intent(in) :: cr
        type(forge_rect), intent(in) :: rect
        character(len=*), intent(in) :: menu_items(:)
        logical, intent(in), optional :: checked_items(:), separator_items(:)
        real(c_double) :: x, y, w, h, item_height, current_y
        type(forge_color) :: bg_color, border_color, separator_color
        integer :: i
        character(len=:), allocatable :: text_c

        x = real(rect%pos%x, c_double)
        y = real(rect%pos%y, c_double)
        w = real(rect%size%width, c_double)
        h = real(rect%size%height, c_double)
        item_height = 24.0_c_double

        bg_color = forge_color(1.0, 1.0, 1.0, 1.0)
        border_color = COLOR_BORDER
        separator_color = forge_color(0.8, 0.8, 0.8, 1.0)

        ! Draw menu background
        call cairo_rectangle(cr, x, y, w, h)
        call cairo_set_source_rgb(cr, bg_color%r, bg_color%g, bg_color%b)
        call cairo_fill_preserve(cr)
        call cairo_set_source_rgb(cr, border_color%r, border_color%g, border_color%b)
        call cairo_set_line_width(cr, 1.0_c_double)
        call cairo_stroke(cr)

        current_y = y

        ! Draw menu items
        do i = 1, size(menu_items)
            ! Check if separator
            if (present(separator_items) .and. i <= size(separator_items) .and. separator_items(i)) then
                call cairo_set_source_rgb(cr, separator_color%r, separator_color%g, separator_color%b)
                call cairo_move_to(cr, x + 5.0, current_y + item_height/2.0)
                call cairo_line_to(cr, x + w - 5.0, current_y + item_height/2.0)
                call cairo_set_line_width(cr, 1.0_c_double)
                call cairo_stroke(cr)
            else
                ! Draw check mark if checked
                if (present(checked_items) .and. i <= size(checked_items) .and. checked_items(i)) then
                    call cairo_set_source_rgb(cr, 0.0_c_double, 0.0_c_double, 0.0_c_double)
                    call cairo_set_line_width(cr, 2.0_c_double)
                    call cairo_move_to(cr, x + 5.0, current_y + item_height/2.0 - 3.0)
                    call cairo_line_to(cr, x + 12.0, current_y + item_height/2.0 + 3.0)
                    call cairo_line_to(cr, x + 19.0, current_y + item_height/2.0 - 3.0)
                    call cairo_stroke(cr)
                end if

                ! Menu item text
                if (len_trim(menu_items(i)) > 0) then
                    text_c = trim(menu_items(i)) // c_null_char
                    call cairo_set_source_rgb(cr, COLOR_TEXT%r, COLOR_TEXT%g, COLOR_TEXT%b)
                    call cairo_select_font_face(cr, c_loc("Sans"//c_null_char), &
                                                CAIRO_FONT_SLANT_NORMAL, CAIRO_FONT_WEIGHT_NORMAL)
                    call cairo_set_font_size(cr, 12.0_c_double)
                    call cairo_move_to(cr, x + 25.0, current_y + item_height/2.0 + 4.0)
                    call cairo_show_text(cr, c_loc(text_c))
                end if
            end if

            current_y = current_y + item_height
        end do
    end subroutine render_menu

    !> @brief Render an action (menu item) widget
    subroutine render_action(cr, rect, text, checked, enabled, has_submenu)
        type(c_ptr), intent(in) :: cr
        type(forge_rect), intent(in) :: rect
        character(len=*), intent(in) :: text
        logical, intent(in), optional :: checked, enabled, has_submenu
        real(c_double) :: x, y, w, h
        type(forge_color) :: text_color
        character(len=:), allocatable :: text_c

        x = real(rect%pos%x, c_double)
        y = real(rect%pos%y, c_double)
        w = real(rect%size%width, c_double)
        h = real(rect%size%height, c_double)

        text_color = COLOR_TEXT
        if (present(enabled) .and. .not. enabled) then
            text_color = forge_color(0.5, 0.5, 0.5, 1.0)
        end if

        ! Draw check mark if checked
        if (present(checked) .and. checked) then
            call cairo_set_source_rgb(cr, 0.0_c_double, 0.0_c_double, 0.0_c_double)
            call cairo_set_line_width(cr, 2.0_c_double)
            call cairo_move_to(cr, x + 5.0, y + h/2.0 - 3.0)
            call cairo_line_to(cr, x + 12.0, y + h/2.0 + 3.0)
            call cairo_line_to(cr, x + 19.0, y + h/2.0 - 3.0)
            call cairo_stroke(cr)
        end if

        ! Draw submenu arrow if has submenu
        if (present(has_submenu) .and. has_submenu) then
            call cairo_set_source_rgb(cr, 0.0_c_double, 0.0_c_double, 0.0_c_double)
            call cairo_move_to(cr, x + w - 15.0, y + h/2.0 - 3.0)
            call cairo_line_to(cr, x + w - 10.0, y + h/2.0)
            call cairo_line_to(cr, x + w - 15.0, y + h/2.0 + 3.0)
            call cairo_stroke(cr)
        end if

        ! Draw text
        if (len_trim(text) > 0) then
            text_c = trim(text) // c_null_char
            call cairo_set_source_rgb(cr, text_color%r, text_color%g, text_color%b)
            call cairo_select_font_face(cr, c_loc("Sans"//c_null_char), &
                                        CAIRO_FONT_SLANT_NORMAL, CAIRO_FONT_WEIGHT_NORMAL)
            call cairo_set_font_size(cr, 12.0_c_double)
            call cairo_move_to(cr, x + 25.0, y + h/2.0 + 4.0)
            call cairo_show_text(cr, c_loc(text_c))
        end if
    end subroutine render_action

    !> @brief Render a listview widget
    subroutine render_listview(cr, rect, items, current_index, selected_indices, alternating_row_colors)
        type(c_ptr), intent(in) :: cr
        type(forge_rect), intent(in) :: rect
        character(len=*), intent(in) :: items(:)
        integer, intent(in) :: current_index
        integer, intent(in), optional :: selected_indices(:)
        logical, intent(in), optional :: alternating_row_colors
        real(c_double) :: x, y, w, h, item_height, current_y
        type(forge_color) :: bg_color, alt_bg_color, selected_bg_color, border_color
        integer :: i, j
        logical :: is_selected, use_alternating
        character(len=:), allocatable :: text_c

        x = real(rect%pos%x, c_double)
        y = real(rect%pos%y, c_double)
        w = real(rect%size%width, c_double)
        h = real(rect%size%height, c_double)
        item_height = 20.0_c_double

        bg_color = forge_color(1.0, 1.0, 1.0, 1.0)
        alt_bg_color = forge_color(0.95, 0.95, 0.95, 1.0)
        selected_bg_color = forge_color(0.7, 0.85, 1.0, 1.0)
        border_color = COLOR_BORDER

        use_alternating = .false.
        if (present(alternating_row_colors)) use_alternating = alternating_row_colors

        ! Draw list background
        call cairo_rectangle(cr, x, y, w, h)
        call cairo_set_source_rgb(cr, bg_color%r, bg_color%g, bg_color%b)
        call cairo_fill_preserve(cr)
        call cairo_set_source_rgb(cr, border_color%r, border_color%g, border_color%b)
        call cairo_set_line_width(cr, 1.0_c_double)
        call cairo_stroke(cr)

        current_y = y

        ! Draw items
        do i = 1, size(items)
            ! Check if selected
            is_selected = .false.
            if (present(selected_indices)) then
                do j = 1, size(selected_indices)
                    if (selected_indices(j) == i-1) then
                        is_selected = .true.
                        exit
                    end if
                end do
            end if

            ! Draw item background
            if (is_selected) then
                call cairo_set_source_rgb(cr, selected_bg_color%r, selected_bg_color%g, selected_bg_color%b)
            else if (use_alternating .and. mod(i, 2) == 0) then
                call cairo_set_source_rgb(cr, alt_bg_color%r, alt_bg_color%g, alt_bg_color%b)
            else
                call cairo_set_source_rgb(cr, bg_color%r, bg_color%g, bg_color%b)
            end if
            call cairo_rectangle(cr, x, current_y, w, item_height)
            call cairo_fill(cr)

            ! Draw item text
            if (len_trim(items(i)) > 0) then
                text_c = trim(items(i)) // c_null_char
                call cairo_set_source_rgb(cr, COLOR_TEXT%r, COLOR_TEXT%g, COLOR_TEXT%b)
                call cairo_select_font_face(cr, c_loc("Sans"//c_null_char), &
                                            CAIRO_FONT_SLANT_NORMAL, CAIRO_FONT_WEIGHT_NORMAL)
                call cairo_set_font_size(cr, 12.0_c_double)
                call cairo_move_to(cr, x + 5.0, current_y + item_height/2.0 + 4.0)
                call cairo_show_text(cr, c_loc(text_c))
            end if

            current_y = current_y + item_height
        end do
    end subroutine render_listview

    !> @brief Render a listwidget (convenience wrapper for listview)
    subroutine render_listwidget(cr, rect, items, current_index, selected_indices, alternating_row_colors)
        type(c_ptr), intent(in) :: cr
        type(forge_rect), intent(in) :: rect
        character(len=*), intent(in) :: items(:)
        integer, intent(in) :: current_index
        integer, intent(in), optional :: selected_indices(:)
        logical, intent(in), optional :: alternating_row_colors
        call render_listview(cr, rect, items, current_index, selected_indices, alternating_row_colors)
    end subroutine render_listwidget

    !> @brief Render a messagebox dialog
    subroutine render_messagebox(cr, rect, title, text, icon_type, buttons)
        type(c_ptr), intent(in) :: cr
        type(forge_rect), intent(in) :: rect
        character(len=*), intent(in) :: title, text
        integer, intent(in) :: icon_type
        integer, intent(in) :: buttons(:)
        real(c_double) :: x, y, w, h, icon_size, button_width, button_height, current_x
        type(forge_color) :: bg_color, border_color
        integer :: i
        character(len=:), allocatable :: text_c, button_text

        x = real(rect%pos%x, c_double)
        y = real(rect%pos%y, c_double)
        w = real(rect%size%width, c_double)
        h = real(rect%size%height, c_double)
        icon_size = 32.0_c_double
        button_width = 80.0_c_double
        button_height = 25.0_c_double

        bg_color = forge_color(1.0, 1.0, 1.0, 1.0)
        border_color = COLOR_BORDER

        ! Draw dialog background
        call cairo_rectangle(cr, x, y, w, h)
        call cairo_set_source_rgb(cr, bg_color%r, bg_color%g, bg_color%b)
        call cairo_fill_preserve(cr)
        call cairo_set_source_rgb(cr, border_color%r, border_color%g, border_color%b)
        call cairo_set_line_width(cr, 2.0_c_double)
        call cairo_stroke(cr)

        ! Draw title bar
        call cairo_rectangle(cr, x, y, w, 30.0_c_double)
        call cairo_set_source_rgb(cr, 0.9_c_double, 0.9_c_double, 0.9_c_double)
        call cairo_fill_preserve(cr)
        call cairo_set_source_rgb(cr, border_color%r, border_color%g, border_color%b)
        call cairo_stroke(cr)

        ! Draw title
        if (len_trim(title) > 0) then
            text_c = trim(title) // c_null_char
            call cairo_set_source_rgb(cr, COLOR_TEXT%r, COLOR_TEXT%g, COLOR_TEXT%b)
            call cairo_select_font_face(cr, c_loc("Sans"//c_null_char), &
                                        CAIRO_FONT_SLANT_NORMAL, CAIRO_FONT_WEIGHT_BOLD)
            call cairo_set_font_size(cr, 14.0_c_double)
            call cairo_move_to(cr, x + 10.0, y + 20.0)
            call cairo_show_text(cr, c_loc(text_c))
        end if

        ! Draw icon
        select case (icon_type)
        case (1)  ! Information
            call cairo_set_source_rgb(cr, 0.2_c_double, 0.4_c_double, 0.8_c_double)
        case (2)  ! Warning
            call cairo_set_source_rgb(cr, 0.8_c_double, 0.6_c_double, 0.0_c_double)
        case (3)  ! Critical
            call cairo_set_source_rgb(cr, 0.8_c_double, 0.2_c_double, 0.2_c_double)
        case (4)  ! Question
            call cairo_set_source_rgb(cr, 0.4_c_double, 0.6_c_double, 0.2_c_double)
        end select
        call cairo_rectangle(cr, x + 20.0, y + 50.0, icon_size, icon_size)
        call cairo_fill(cr)

        ! Draw text
        if (len_trim(text) > 0) then
            text_c = trim(text) // c_null_char
            call cairo_set_source_rgb(cr, COLOR_TEXT%r, COLOR_TEXT%g, COLOR_TEXT%b)
            call cairo_select_font_face(cr, c_loc("Sans"//c_null_char), &
                                        CAIRO_FONT_SLANT_NORMAL, CAIRO_FONT_WEIGHT_NORMAL)
            call cairo_set_font_size(cr, 12.0_c_double)
            call cairo_move_to(cr, x + 70.0, y + 70.0)
            call cairo_show_text(cr, c_loc(text_c))
        end if

        ! Draw buttons
        current_x = x + w - size(buttons) * (button_width + 10.0) - 10.0
        do i = 1, size(buttons)
            ! Button background
            call cairo_rectangle(cr, current_x, y + h - button_height - 20.0, button_width, button_height)
            call cairo_set_source_rgb(cr, COLOR_BUTTON_NORMAL%r, COLOR_BUTTON_NORMAL%g, COLOR_BUTTON_NORMAL%b)
            call cairo_fill_preserve(cr)
            call cairo_set_source_rgb(cr, border_color%r, border_color%g, border_color%b)
            call cairo_stroke(cr)

            ! Button text
            select case (buttons(i))
            case (1)
                button_text = "OK"//c_null_char
            case (2)
                button_text = "Cancel"//c_null_char
            case (4)
                button_text = "Yes"//c_null_char
            case (8)
                button_text = "No"//c_null_char
            case default
                button_text = "Button"//c_null_char
            end select
            call cairo_set_source_rgb(cr, COLOR_TEXT%r, COLOR_TEXT%g, COLOR_TEXT%b)
            call cairo_select_font_face(cr, c_loc("Sans"//c_null_char), &
                                        CAIRO_FONT_SLANT_NORMAL, CAIRO_FONT_WEIGHT_NORMAL)
            call cairo_set_font_size(cr, 11.0_c_double)
            call cairo_move_to(cr, current_x + button_width/2.0 - 15.0, y + h - 35.0)
            call cairo_show_text(cr, c_loc(button_text))

            current_x = current_x + button_width + 10.0
        end do
    end subroutine render_messagebox

    !> @brief Render a statusbar widget
    subroutine render_statusbar(cr, rect, message, permanent_widgets)
        type(c_ptr), intent(in) :: cr
        type(forge_rect), intent(in) :: rect
        character(len=*), intent(in) :: message
        character(len=*), intent(in), optional :: permanent_widgets(:)
        real(c_double) :: x, y, w, h
        type(forge_color) :: bg_color, border_color
        character(len=:), allocatable :: text_c

        x = real(rect%pos%x, c_double)
        y = real(rect%pos%y, c_double)
        w = real(rect%size%width, c_double)
        h = real(rect%size%height, c_double)

        bg_color = forge_color(0.95, 0.95, 0.95, 1.0)
        border_color = COLOR_BORDER

        ! Draw statusbar background
        call cairo_rectangle(cr, x, y, w, h)
        call cairo_set_source_rgb(cr, bg_color%r, bg_color%g, bg_color%b)
        call cairo_fill_preserve(cr)
        call cairo_set_source_rgb(cr, border_color%r, border_color%g, border_color%b)
        call cairo_set_line_width(cr, 1.0_c_double)
        call cairo_stroke(cr)

        ! Draw message
        if (len_trim(message) > 0) then
            text_c = trim(message) // c_null_char
            call cairo_set_source_rgb(cr, COLOR_TEXT%r, COLOR_TEXT%g, COLOR_TEXT%b)
            call cairo_select_font_face(cr, c_loc("Sans"//c_null_char), &
                                        CAIRO_FONT_SLANT_NORMAL, CAIRO_FONT_WEIGHT_NORMAL)
            call cairo_set_font_size(cr, 11.0_c_double)
            call cairo_move_to(cr, x + 5.0, y + h/2.0 + 4.0)
            call cairo_show_text(cr, c_loc(text_c))
        end if

        ! Draw permanent widgets on the right side
        if (present(permanent_widgets)) then
            real(c_double) :: widget_x, widget_spacing
            integer :: i
            character(len=:), allocatable :: widget_text

            widget_spacing = 80.0_c_double
            widget_x = x + w - widget_spacing

            do i = 1, size(permanent_widgets)
                if (len_trim(permanent_widgets(i)) > 0) then
                    widget_text = trim(permanent_widgets(i)) // c_null_char
                    call cairo_set_source_rgb(cr, COLOR_TEXT%r, COLOR_TEXT%g, COLOR_TEXT%b)
                    call cairo_select_font_face(cr, c_loc("Sans"//c_null_char), &
                                                CAIRO_FONT_SLANT_NORMAL, CAIRO_FONT_WEIGHT_NORMAL)
                    call cairo_set_font_size(cr, 10.0_c_double)
                    call cairo_move_to(cr, widget_x, y + h/2.0 + 4.0)
                    call cairo_show_text(cr, c_loc(widget_text))

                    widget_x = widget_x - widget_spacing
                end if
            end do
        end if
    end subroutine render_statusbar

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

