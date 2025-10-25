!> @brief Cairo 2D graphics library bindings
!> @details Fortran bindings to Cairo C API for 2D rendering
!> @author ForGE Contributors
!> @date 2025
!> @license GPL-3.0-or-later

module forge_cairo_bindings
    use iso_c_binding
    implicit none
    private

    public :: cairo_t, cairo_surface_t, cairo_font_slant_t, cairo_font_weight_t
    public :: CAIRO_FORMAT_ARGB32, CAIRO_FORMAT_RGB24
    public :: CAIRO_FONT_SLANT_NORMAL, CAIRO_FONT_SLANT_ITALIC, CAIRO_FONT_SLANT_OBLIQUE
    public :: CAIRO_FONT_WEIGHT_NORMAL, CAIRO_FONT_WEIGHT_BOLD
    
    ! Cairo opaque types
    type, bind(C) :: cairo_t
        private
        type(c_ptr) :: ptr
    end type cairo_t

    type, bind(C) :: cairo_surface_t
        private
        type(c_ptr) :: ptr
    end type cairo_surface_t

    ! Cairo enums
    enum, bind(C)
        enumerator :: CAIRO_FORMAT_INVALID = -1
        enumerator :: CAIRO_FORMAT_ARGB32 = 0
        enumerator :: CAIRO_FORMAT_RGB24 = 1
        enumerator :: CAIRO_FORMAT_A8 = 2
        enumerator :: CAIRO_FORMAT_A1 = 3
    end enum

    enum, bind(C)
        enumerator :: CAIRO_FONT_SLANT_NORMAL = 0
        enumerator :: CAIRO_FONT_SLANT_ITALIC = 1
        enumerator :: CAIRO_FONT_SLANT_OBLIQUE = 2
    end enum

    enum, bind(C)
        enumerator :: CAIRO_FONT_WEIGHT_NORMAL = 0
        enumerator :: CAIRO_FONT_WEIGHT_BOLD = 1
    end enum

    type, bind(C) :: cairo_font_slant_t
        integer(c_int) :: value
    end type cairo_font_slant_t

    type, bind(C) :: cairo_font_weight_t
        integer(c_int) :: value
    end type cairo_font_weight_t

    !> Cairo C API bindings
    interface

        ! Surface creation and destruction
        function cairo_image_surface_create(format, width, height) &
                bind(C, name="cairo_image_surface_create")
            import :: c_ptr, c_int
            integer(c_int), value :: format, width, height
            type(c_ptr) :: cairo_image_surface_create
        end function cairo_image_surface_create

        function cairo_win32_surface_create(hdc) &
                bind(C, name="cairo_win32_surface_create")
            import :: c_ptr
            type(c_ptr), value :: hdc
            type(c_ptr) :: cairo_win32_surface_create
        end function cairo_win32_surface_create

        subroutine cairo_surface_destroy(surface) &
                bind(C, name="cairo_surface_destroy")
            import :: c_ptr
            type(c_ptr), value :: surface
        end subroutine cairo_surface_destroy

        subroutine cairo_surface_flush(surface) &
                bind(C, name="cairo_surface_flush")
            import :: c_ptr
            type(c_ptr), value :: surface
        end subroutine cairo_surface_flush

        ! Context creation and destruction
        function cairo_create(target) bind(C, name="cairo_create")
            import :: c_ptr
            type(c_ptr), value :: target
            type(c_ptr) :: cairo_create
        end function cairo_create

        subroutine cairo_destroy(cr) bind(C, name="cairo_destroy")
            import :: c_ptr
            type(c_ptr), value :: cr
        end subroutine cairo_destroy

        ! State management
        subroutine cairo_save(cr) bind(C, name="cairo_save")
            import :: c_ptr
            type(c_ptr), value :: cr
        end subroutine cairo_save

        subroutine cairo_restore(cr) bind(C, name="cairo_restore")
            import :: c_ptr
            type(c_ptr), value :: cr
        end subroutine cairo_restore

        ! Color and source
        subroutine cairo_set_source_rgb(cr, red, green, blue) &
                bind(C, name="cairo_set_source_rgb")
            import :: c_ptr, c_double
            type(c_ptr), value :: cr
            real(c_double), value :: red, green, blue
        end subroutine cairo_set_source_rgb

        subroutine cairo_set_source_rgba(cr, red, green, blue, alpha) &
                bind(C, name="cairo_set_source_rgba")
            import :: c_ptr, c_double
            type(c_ptr), value :: cr
            real(c_double), value :: red, green, blue, alpha
        end subroutine cairo_set_source_rgba

        ! Path operations
        subroutine cairo_move_to(cr, x, y) bind(C, name="cairo_move_to")
            import :: c_ptr, c_double
            type(c_ptr), value :: cr
            real(c_double), value :: x, y
        end subroutine cairo_move_to

        subroutine cairo_line_to(cr, x, y) bind(C, name="cairo_line_to")
            import :: c_ptr, c_double
            type(c_ptr), value :: cr
            real(c_double), value :: x, y
        end subroutine cairo_line_to

        subroutine cairo_rectangle(cr, x, y, width, height) &
                bind(C, name="cairo_rectangle")
            import :: c_ptr, c_double
            type(c_ptr), value :: cr
            real(c_double), value :: x, y, width, height
        end subroutine cairo_rectangle

        subroutine cairo_arc(cr, xc, yc, radius, angle1, angle2) &
                bind(C, name="cairo_arc")
            import :: c_ptr, c_double
            type(c_ptr), value :: cr
            real(c_double), value :: xc, yc, radius, angle1, angle2
        end subroutine cairo_arc

        subroutine cairo_close_path(cr) bind(C, name="cairo_close_path")
            import :: c_ptr
            type(c_ptr), value :: cr
        end subroutine cairo_close_path

        subroutine cairo_new_path(cr) bind(C, name="cairo_new_path")
            import :: c_ptr
            type(c_ptr), value :: cr
        end subroutine cairo_new_path

        ! Drawing operations
        subroutine cairo_stroke(cr) bind(C, name="cairo_stroke")
            import :: c_ptr
            type(c_ptr), value :: cr
        end subroutine cairo_stroke

        subroutine cairo_stroke_preserve(cr) bind(C, name="cairo_stroke_preserve")
            import :: c_ptr
            type(c_ptr), value :: cr
        end subroutine cairo_stroke_preserve

        subroutine cairo_fill(cr) bind(C, name="cairo_fill")
            import :: c_ptr
            type(c_ptr), value :: cr
        end subroutine cairo_fill

        subroutine cairo_fill_preserve(cr) bind(C, name="cairo_fill_preserve")
            import :: c_ptr
            type(c_ptr), value :: cr
        end subroutine cairo_fill_preserve

        subroutine cairo_paint(cr) bind(C, name="cairo_paint")
            import :: c_ptr
            type(c_ptr), value :: cr
        end subroutine cairo_paint

        ! Line properties
        subroutine cairo_set_line_width(cr, width) &
                bind(C, name="cairo_set_line_width")
            import :: c_ptr, c_double
            type(c_ptr), value :: cr
            real(c_double), value :: width
        end subroutine cairo_set_line_width

        ! Text operations
        subroutine cairo_select_font_face(cr, family, slant, weight) &
                bind(C, name="cairo_select_font_face")
            import :: c_ptr, c_int
            type(c_ptr), value :: cr, family
            integer(c_int), value :: slant, weight
        end subroutine cairo_select_font_face

        subroutine cairo_set_font_size(cr, size) &
                bind(C, name="cairo_set_font_size")
            import :: c_ptr, c_double
            type(c_ptr), value :: cr
            real(c_double), value :: size
        end subroutine cairo_set_font_size

        subroutine cairo_show_text(cr, utf8) bind(C, name="cairo_show_text")
            import :: c_ptr
            type(c_ptr), value :: cr, utf8
        end subroutine cairo_show_text

        ! Transformations
        subroutine cairo_translate(cr, tx, ty) bind(C, name="cairo_translate")
            import :: c_ptr, c_double
            type(c_ptr), value :: cr
            real(c_double), value :: tx, ty
        end subroutine cairo_translate

        subroutine cairo_scale(cr, sx, sy) bind(C, name="cairo_scale")
            import :: c_ptr, c_double
            type(c_ptr), value :: cr
            real(c_double), value :: sx, sy
        end subroutine cairo_scale

        subroutine cairo_rotate(cr, angle) bind(C, name="cairo_rotate")
            import :: c_ptr, c_double
            type(c_ptr), value :: cr
            real(c_double), value :: angle
        end subroutine cairo_rotate

    end interface

    ! Public wrapper functions for convenience
    public :: cairo_create, cairo_destroy
    public :: cairo_image_surface_create, cairo_win32_surface_create
    public :: cairo_surface_destroy, cairo_surface_flush
    public :: cairo_save, cairo_restore
    public :: cairo_set_source_rgb, cairo_set_source_rgba
    public :: cairo_move_to, cairo_line_to, cairo_rectangle, cairo_arc
    public :: cairo_close_path, cairo_new_path
    public :: cairo_stroke, cairo_stroke_preserve, cairo_fill, cairo_fill_preserve
    public :: cairo_paint, cairo_set_line_width
    public :: cairo_select_font_face, cairo_set_font_size, cairo_show_text
    public :: cairo_translate, cairo_scale, cairo_rotate

end module forge_cairo_bindings

