!> @brief QFont-like module for font rendering and text layout
!> @details Implements Qt-style font for controlling text appearance
!> @author ForGE Contributors
!> @date 2025
!> @license GPL-3.0-or-later

module forge_qfont
    use iso_c_binding
    use forge_types
    implicit none
    private

    public :: forge_qfont_t
    public :: FONT_STYLE_NORMAL, FONT_STYLE_ITALIC, FONT_STYLE_OBLIQUE
    public :: FONT_WEIGHT_THIN, FONT_WEIGHT_LIGHT, FONT_WEIGHT_NORMAL, FONT_WEIGHT_MEDIUM
    public :: FONT_WEIGHT_BOLD, FONT_WEIGHT_BLACK
    public :: FONT_HINTING_NONE, FONT_HINTING_VERTICAL, FONT_HINTING_FULL
    public :: FONT_ANTIALIAS_NONE, FONT_ANTIALIAS_GRAY, FONT_ANTIALIAS_SUBPIXEL

    ! Font style constants
    integer, parameter :: FONT_STYLE_NORMAL = 0
    integer, parameter :: FONT_STYLE_ITALIC = 1
    integer, parameter :: FONT_STYLE_OBLIQUE = 2

    ! Font weight constants
    integer, parameter :: FONT_WEIGHT_THIN = 0
    integer, parameter :: FONT_WEIGHT_LIGHT = 12
    integer, parameter :: FONT_WEIGHT_NORMAL = 25
    integer, parameter :: FONT_WEIGHT_MEDIUM = 37
    integer, parameter :: FONT_WEIGHT_BOLD = 50
    integer, parameter :: FONT_WEIGHT_BLACK = 62

    ! Font hinting constants
    integer, parameter :: FONT_HINTING_NONE = 0
    integer, parameter :: FONT_HINTING_VERTICAL = 1
    integer, parameter :: FONT_HINTING_FULL = 2

    ! Font antialiasing constants
    integer, parameter :: FONT_ANTIALIAS_NONE = 0
    integer, parameter :: FONT_ANTIALIAS_GRAY = 1
    integer, parameter :: FONT_ANTIALIAS_SUBPIXEL = 2

    !> @brief QFont-like class for controlling text appearance
    type :: forge_qfont_t
        private
        character(len=:), allocatable :: family_
        real(c_double) :: point_size_ = 12.0_c_double
        integer :: style_ = FONT_STYLE_NORMAL
        integer :: weight_ = FONT_WEIGHT_NORMAL
        logical :: underline_ = .false.
        logical :: overline_ = .false.
        logical :: strikeout_ = .false.
        integer :: hinting_preference_ = FONT_HINTING_FULL
        integer :: antialias_preference_ = FONT_ANTIALIAS_GRAY
        logical :: kerning_ = .true.
        real(c_double) :: letter_spacing_ = 0.0_c_double
        real(c_double) :: word_spacing_ = 0.0_c_double
    contains
        procedure :: set_family => forge_qfont_set_family
        procedure :: family => forge_qfont_get_family
        procedure :: set_point_size => forge_qfont_set_point_size
        procedure :: point_size => forge_qfont_get_point_size
        procedure :: set_pixel_size => forge_qfont_set_pixel_size
        procedure :: pixel_size => forge_qfont_get_pixel_size
        procedure :: set_style => forge_qfont_set_style
        procedure :: style => forge_qfont_get_style
        procedure :: set_weight => forge_qfont_set_weight
        procedure :: weight => forge_qfont_get_weight
        procedure :: set_underline => forge_qfont_set_underline
        procedure :: underline => forge_qfont_get_underline
        procedure :: set_overline => forge_qfont_set_overline
        procedure :: overline => forge_qfont_get_overline
        procedure :: set_strikeout => forge_qfont_set_strikeout
        procedure :: strikeout => forge_qfont_get_strikeout
        procedure :: set_hinting_preference => forge_qfont_set_hinting_preference
        procedure :: hinting_preference => forge_qfont_get_hinting_preference
        procedure :: set_antialias_preference => forge_qfont_set_antialias_preference
        procedure :: antialias_preference => forge_qfont_get_antialias_preference
        procedure :: set_kerning => forge_qfont_set_kerning
        procedure :: kerning => forge_qfont_get_kerning
        procedure :: set_letter_spacing => forge_qfont_set_letter_spacing
        procedure :: letter_spacing => forge_qfont_get_letter_spacing
        procedure :: set_word_spacing => forge_qfont_set_word_spacing
        procedure :: word_spacing => forge_qfont_get_word_spacing
        procedure :: apply_to_cairo => forge_qfont_apply_to_cairo
        procedure :: measure_text => forge_qfont_measure_text
        procedure :: ascent => forge_qfont_ascent
        procedure :: descent => forge_qfont_descent
        procedure :: height => forge_qfont_height
        procedure :: leading => forge_qfont_leading
        procedure :: max_width => forge_qfont_max_width
        procedure :: average_char_width => forge_qfont_average_char_width
        procedure :: is_scalable => forge_qfont_is_scalable
        procedure :: exact_match => forge_qfont_exact_match
    end type forge_qfont_t

contains

    !> @brief Set font family
    subroutine forge_qfont_set_family(this, family)
        class(forge_qfont_t), intent(inout) :: this
        character(len=*), intent(in) :: family
        if (allocated(this%family_)) deallocate(this%family_)
        allocate(character(len=len(family)) :: this%family_)
        this%family_ = family
    end subroutine forge_qfont_set_family

    !> @brief Get font family
    function forge_qfont_get_family(this) result(family)
        class(forge_qfont_t), intent(in) :: this
        character(len=:), allocatable :: family
        if (allocated(this%family_)) then
            allocate(character(len=len(this%family_)) :: family)
            family = this%family_
        else
            allocate(character(len=5) :: family)
            family = "Sans"
        end if
    end function forge_qfont_get_family

    !> @brief Set point size
    subroutine forge_qfont_set_point_size(this, size)
        class(forge_qfont_t), intent(inout) :: this
        real(c_double), intent(in) :: size
        this%point_size_ = max(1.0_c_double, size)
    end subroutine forge_qfont_set_point_size

    !> @brief Get point size
    function forge_qfont_get_point_size(this) result(size)
        class(forge_qfont_t), intent(in) :: this
        real(c_double) :: size
        size = this%point_size_
    end function forge_qfont_get_point_size

    !> @brief Set pixel size (same as point size for simplicity)
    subroutine forge_qfont_set_pixel_size(this, size)
        class(forge_qfont_t), intent(inout) :: this
        real(c_double), intent(in) :: size
        this%point_size_ = max(1.0_c_double, size)
    end subroutine forge_qfont_set_pixel_size

    !> @brief Get pixel size
    function forge_qfont_get_pixel_size(this) result(size)
        class(forge_qfont_t), intent(in) :: this
        real(c_double) :: size
        size = this%point_size_
    end function forge_qfont_get_pixel_size

    !> @brief Set font style
    subroutine forge_qfont_set_style(this, style)
        class(forge_qfont_t), intent(inout) :: this
        integer, intent(in) :: style
        this%style_ = style
    end subroutine forge_qfont_set_style

    !> @brief Get font style
    function forge_qfont_get_style(this) result(style)
        class(forge_qfont_t), intent(in) :: this
        integer :: style
        style = this%style_
    end function forge_qfont_get_style

    !> @brief Set font weight
    subroutine forge_qfont_set_weight(this, weight)
        class(forge_qfont_t), intent(inout) :: this
        integer, intent(in) :: weight
        this%weight_ = weight
    end subroutine forge_qfont_set_weight

    !> @brief Get font weight
    function forge_qfont_get_weight(this) result(weight)
        class(forge_qfont_t), intent(in) :: this
        integer :: weight
        weight = this%weight_
    end function forge_qfont_get_weight

    !> @brief Set underline
    subroutine forge_qfont_set_underline(this, underline)
        class(forge_qfont_t), intent(inout) :: this
        logical, intent(in) :: underline
        this%underline_ = underline
    end subroutine forge_qfont_set_underline

    !> @brief Get underline
    function forge_qfont_get_underline(this) result(underline)
        class(forge_qfont_t), intent(in) :: this
        logical :: underline
        underline = this%underline_
    end function forge_qfont_get_underline

    !> @brief Set overline
    subroutine forge_qfont_set_overline(this, overline)
        class(forge_qfont_t), intent(inout) :: this
        logical, intent(in) :: overline
        this%overline_ = overline
    end subroutine forge_qfont_set_overline

    !> @brief Get overline
    function forge_qfont_get_overline(this) result(overline)
        class(forge_qfont_t), intent(in) :: this
        logical :: overline
        overline = this%overline_
    end function forge_qfont_get_overline

    !> @brief Set strikeout
    subroutine forge_qfont_set_strikeout(this, strikeout)
        class(forge_qfont_t), intent(inout) :: this
        logical, intent(in) :: strikeout
        this%strikeout_ = strikeout
    end subroutine forge_qfont_set_strikeout

    !> @brief Get strikeout
    function forge_qfont_get_strikeout(this) result(strikeout)
        class(forge_qfont_t), intent(in) :: this
        logical :: strikeout
        strikeout = this%strikeout_
    end function forge_qfont_get_strikeout

    !> @brief Set hinting preference
    subroutine forge_qfont_set_hinting_preference(this, hinting)
        class(forge_qfont_t), intent(inout) :: this
        integer, intent(in) :: hinting
        this%hinting_preference_ = hinting
    end subroutine forge_qfont_set_hinting_preference

    !> @brief Get hinting preference
    function forge_qfont_get_hinting_preference(this) result(hinting)
        class(forge_qfont_t), intent(in) :: this
        integer :: hinting
        hinting = this%hinting_preference_
    end function forge_qfont_get_hinting_preference

    !> @brief Set antialias preference
    subroutine forge_qfont_set_antialias_preference(this, antialias)
        class(forge_qfont_t), intent(inout) :: this
        integer, intent(in) :: antialias
        this%antialias_preference_ = antialias
    end subroutine forge_qfont_set_antialias_preference

    !> @brief Get antialias preference
    function forge_qfont_get_antialias_preference(this) result(antialias)
        class(forge_qfont_t), intent(in) :: this
        integer :: antialias
        antialias = this%antialias_preference_
    end function forge_qfont_get_antialias_preference

    !> @brief Set kerning
    subroutine forge_qfont_set_kerning(this, kerning)
        class(forge_qfont_t), intent(inout) :: this
        logical, intent(in) :: kerning
        this%kerning_ = kerning
    end subroutine forge_qfont_set_kerning

    !> @brief Get kerning
    function forge_qfont_get_kerning(this) result(kerning)
        class(forge_qfont_t), intent(in) :: this
        logical :: kerning
        kerning = this%kerning_
    end function forge_qfont_get_kerning

    !> @brief Set letter spacing
    subroutine forge_qfont_set_letter_spacing(this, spacing)
        class(forge_qfont_t), intent(inout) :: this
        real(c_double), intent(in) :: spacing
        this%letter_spacing_ = spacing
    end subroutine forge_qfont_set_letter_spacing

    !> @brief Get letter spacing
    function forge_qfont_get_letter_spacing(this) result(spacing)
        class(forge_qfont_t), intent(in) :: this
        real(c_double) :: spacing
        spacing = this%letter_spacing_
    end function forge_qfont_get_letter_spacing

    !> @brief Set word spacing
    subroutine forge_qfont_set_word_spacing(this, spacing)
        class(forge_qfont_t), intent(inout) :: this
        real(c_double), intent(in) :: spacing
        this%word_spacing_ = spacing
    end subroutine forge_qfont_set_word_spacing

    !> @brief Get word spacing
    function forge_qfont_get_word_spacing(this) result(spacing)
        class(forge_qfont_t), intent(in) :: this
        real(c_double) :: spacing
        spacing = this%word_spacing_
    end function forge_qfont_get_word_spacing

    !> @brief Apply font properties to Cairo context
    subroutine forge_qfont_apply_to_cairo(this, cr)
        use forge_cairo_bindings
        class(forge_qfont_t), intent(in) :: this
        type(c_ptr), intent(in) :: cr
        character(len=:), allocatable :: family
        integer(c_int) :: slant, weight

        ! Set font family
        family = this%family()
        call cairo_select_font_face(cr, c_loc(family//c_null_char), &
                                   CAIRO_FONT_SLANT_NORMAL, CAIRO_FONT_WEIGHT_NORMAL)

        ! Set font size
        call cairo_set_font_size(cr, this%point_size_)

        ! Note: Advanced font features like weight, style, hinting would require
        ! more sophisticated font handling in Cairo
    end subroutine forge_qfont_apply_to_cairo

    !> @brief Measure text dimensions
    function forge_qfont_measure_text(this, text) result(extents)
        use forge_cairo_bindings
        class(forge_qfont_t), intent(in) :: this
        character(len=*), intent(in) :: text
        type(cairo_text_extents_t) :: extents
        type(c_ptr) :: cr
        character(len=:), allocatable :: family

        ! Create temporary context for measurement
        cr = cairo_create(c_null_ptr)
        if (c_associated(cr)) then
            family = this%family()
            call cairo_select_font_face(cr, c_loc(family//c_null_char), &
                                       CAIRO_FONT_SLANT_NORMAL, CAIRO_FONT_WEIGHT_NORMAL)
            call cairo_set_font_size(cr, this%point_size_)
            call cairo_text_extents(cr, c_loc(text//c_null_char), extents)
            call cairo_destroy(cr)
        end if
    end function forge_qfont_measure_text

    !> @brief Get font ascent
    function forge_qfont_ascent(this) result(ascent)
        class(forge_qfont_t), intent(in) :: this
        real(c_double) :: ascent
        type(cairo_font_extents_t) :: extents
        type(c_ptr) :: cr
        character(len=:), allocatable :: family

        cr = cairo_create(c_null_ptr)
        if (c_associated(cr)) then
            family = this%family()
            call cairo_select_font_face(cr, c_loc(family//c_null_char), &
                                       CAIRO_FONT_SLANT_NORMAL, CAIRO_FONT_WEIGHT_NORMAL)
            call cairo_set_font_size(cr, this%point_size_)
            call cairo_font_extents(cr, extents)
            ascent = extents%ascent
            call cairo_destroy(cr)
        else
            ascent = this%point_size_ * 0.8_c_double
        end if
    end function forge_qfont_ascent

    !> @brief Get font descent
    function forge_qfont_descent(this) result(descent)
        class(forge_qfont_t), intent(in) :: this
        real(c_double) :: descent
        type(cairo_font_extents_t) :: extents
        type(c_ptr) :: cr
        character(len=:), allocatable :: family

        cr = cairo_create(c_null_ptr)
        if (c_associated(cr)) then
            family = this%family()
            call cairo_select_font_face(cr, c_loc(family//c_null_char), &
                                       CAIRO_FONT_SLANT_NORMAL, CAIRO_FONT_WEIGHT_NORMAL)
            call cairo_set_font_size(cr, this%point_size_)
            call cairo_font_extents(cr, extents)
            descent = extents%descent
            call cairo_destroy(cr)
        else
            descent = this%point_size_ * 0.2_c_double
        end if
    end function forge_qfont_descent

    !> @brief Get font height
    function forge_qfont_height(this) result(height)
        class(forge_qfont_t), intent(in) :: this
        real(c_double) :: height
        height = this%ascent() + this%descent()
    end function forge_qfont_height

    !> @brief Get font leading
    function forge_qfont_leading(this) result(leading)
        class(forge_qfont_t), intent(in) :: this
        real(c_double) :: leading
        type(cairo_font_extents_t) :: extents
        type(c_ptr) :: cr
        character(len=:), allocatable :: family

        cr = cairo_create(c_null_ptr)
        if (c_associated(cr)) then
            family = this%family()
            call cairo_select_font_face(cr, c_loc(family//c_null_char), &
                                       CAIRO_FONT_SLANT_NORMAL, CAIRO_FONT_WEIGHT_NORMAL)
            call cairo_set_font_size(cr, this%point_size_)
            call cairo_font_extents(cr, extents)
            leading = extents%height - (extents%ascent + extents%descent)
            call cairo_destroy(cr)
        else
            leading = this%point_size_ * 0.1_c_double
        end if
    end function forge_qfont_leading

    !> @brief Get maximum character width
    function forge_qfont_max_width(this) result(max_width)
        class(forge_qfont_t), intent(in) :: this
        real(c_double) :: max_width
        type(cairo_text_extents_t) :: extents
        type(c_ptr) :: cr
        character(len=:), allocatable :: family

        cr = cairo_create(c_null_ptr)
        if (c_associated(cr)) then
            family = this%family()
            call cairo_select_font_face(cr, c_loc(family//c_null_char), &
                                       CAIRO_FONT_SLANT_NORMAL, CAIRO_FONT_WEIGHT_NORMAL)
            call cairo_set_font_size(cr, this%point_size_)
            call cairo_text_extents(cr, c_loc("M"//c_null_char), extents)
            max_width = extents%x_advance
            call cairo_destroy(cr)
        else
            max_width = this%point_size_ * 0.8_c_double
        end if
    end function forge_qfont_max_width

    !> @brief Get average character width
    function forge_qfont_average_char_width(this) result(avg_width)
        class(forge_qfont_t), intent(in) :: this
        real(c_double) :: avg_width
        avg_width = this%max_width() * 0.8_c_double
    end function forge_qfont_average_char_width

    !> @brief Check if font is scalable
    function forge_qfont_is_scalable(this) result(scalable)
        class(forge_qfont_t), intent(in) :: this
        logical :: scalable
        scalable = .true.  ! Vector fonts are scalable
    end function forge_qfont_is_scalable

    !> @brief Check if font exactly matches request
    function forge_qfont_exact_match(this) result(exact)
        class(forge_qfont_t), intent(in) :: this
        logical :: exact
        exact = .true.  ! Assume exact match for simplicity
    end function forge_qfont_exact_match

end module forge_qfont