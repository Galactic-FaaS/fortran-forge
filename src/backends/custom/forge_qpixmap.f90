!> @brief QPixmap-like module for off-screen image representation
!> @details Implements Qt-style pixmap for image manipulation and drawing
!> @author ForGE Contributors
!> @date 2025
!> @license GPL-3.0-or-later

module forge_qpixmap
    use iso_c_binding
    use forge_types
    use forge_cairo_bindings
    implicit none
    private

    public :: forge_qpixmap_t
    public :: PIXMAP_FORMAT_INVALID, PIXMAP_FORMAT_MONO, PIXMAP_FORMAT_MONO_LSB
    public :: PIXMAP_FORMAT_INDEXED8, PIXMAP_FORMAT_RGB32, PIXMAP_FORMAT_ARGB32
    public :: PIXMAP_FORMAT_ARGB32_PREMULTIPLIED, PIXMAP_FORMAT_RGB16, PIXMAP_FORMAT_ARGB8565_PREMULTIPLIED
    public :: PIXMAP_FORMAT_RGB666, PIXMAP_FORMAT_ARGB6666_PREMULTIPLIED, PIXMAP_FORMAT_RGB555
    public :: PIXMAP_FORMAT_ARGB8555_PREMULTIPLIED, PIXMAP_FORMAT_RGB888, PIXMAP_FORMAT_RGB444
    public :: PIXMAP_FORMAT_ARGB4444_PREMULTIPLIED, PIXMAP_FORMAT_RGBX8888, PIXMAP_FORMAT_RGBA8888
    public :: PIXMAP_FORMAT_RGBA8888_PREMULTIPLIED

    ! Pixmap format constants (matching Qt)
    integer, parameter :: PIXMAP_FORMAT_INVALID = -1
    integer, parameter :: PIXMAP_FORMAT_MONO = 1
    integer, parameter :: PIXMAP_FORMAT_MONO_LSB = 2
    integer, parameter :: PIXMAP_FORMAT_INDEXED8 = 3
    integer, parameter :: PIXMAP_FORMAT_RGB32 = 4
    integer, parameter :: PIXMAP_FORMAT_ARGB32 = 5
    integer, parameter :: PIXMAP_FORMAT_ARGB32_PREMULTIPLIED = 6
    integer, parameter :: PIXMAP_FORMAT_RGB16 = 7
    integer, parameter :: PIXMAP_FORMAT_ARGB8565_PREMULTIPLIED = 8
    integer, parameter :: PIXMAP_FORMAT_RGB666 = 9
    integer, parameter :: PIXMAP_FORMAT_ARGB6666_PREMULTIPLIED = 10
    integer, parameter :: PIXMAP_FORMAT_RGB555 = 11
    integer, parameter :: PIXMAP_FORMAT_ARGB8555_PREMULTIPLIED = 12
    integer, parameter :: PIXMAP_FORMAT_RGB888 = 13
    integer, parameter :: PIXMAP_FORMAT_RGB444 = 14
    integer, parameter :: PIXMAP_FORMAT_ARGB4444_PREMULTIPLIED = 15
    integer, parameter :: PIXMAP_FORMAT_RGBX8888 = 16
    integer, parameter :: PIXMAP_FORMAT_RGBA8888 = 17
    integer, parameter :: PIXMAP_FORMAT_RGBA8888_PREMULTIPLIED = 18

    !> @brief QPixmap-like class for off-screen image representation
    type :: forge_qpixmap_t
        private
        type(c_ptr) :: surface_ = c_null_ptr  !< Cairo surface
        type(c_ptr) :: cr_ = c_null_ptr       !< Cairo context for drawing
        integer(c_int) :: width_ = 0
        integer(c_int) :: height_ = 0
        integer :: format_ = PIXMAP_FORMAT_ARGB32
        logical :: has_alpha_ = .true.
        logical :: premultiplied_alpha_ = .false.
    contains
        procedure :: create => forge_qpixmap_create
        procedure :: create_from_data => forge_qpixmap_create_from_data
        procedure :: create_from_file => forge_qpixmap_create_from_file
        procedure :: copy => forge_qpixmap_copy
        procedure :: width => forge_qpixmap_width
        procedure :: height => forge_qpixmap_height
        procedure :: size => forge_qpixmap_size
        procedure :: rect => forge_qpixmap_rect
        procedure :: format => forge_qpixmap_format
        procedure :: has_alpha => forge_qpixmap_has_alpha
        procedure :: is_null => forge_qpixmap_is_null
        procedure :: fill => forge_qpixmap_fill
        procedure :: scaled => forge_qpixmap_scaled
        procedure :: transformed => forge_qpixmap_transformed
        procedure :: to_image => forge_qpixmap_to_image
        procedure :: save => forge_qpixmap_save
        procedure :: load => forge_qpixmap_load
        procedure :: painter => forge_qpixmap_painter
        procedure :: detach => forge_qpixmap_detach
        procedure :: cleanup => forge_qpixmap_cleanup
        procedure :: convert_to_format => forge_qpixmap_convert_to_format
        procedure :: depth => forge_qpixmap_depth
        procedure :: device_pixel_ratio => forge_qpixmap_device_pixel_ratio
        procedure :: set_device_pixel_ratio => forge_qpixmap_set_device_pixel_ratio
        procedure :: cache_key => forge_qpixmap_cache_key
        procedure :: is_detached => forge_qpixmap_is_detached
        procedure :: scroll => forge_qpixmap_scroll
        procedure :: copy_rect => forge_qpixmap_copy_rect
        procedure :: set_mask => forge_qpixmap_set_mask
        procedure :: create_heuristic_mask => forge_qpixmap_create_heuristic_mask
        procedure :: create_mask_from_color => forge_qpixmap_create_mask_from_color
        procedure :: grab_widget => forge_qpixmap_grab_widget
        procedure :: grab_window => forge_qpixmap_grab_window
    end type forge_qpixmap_t

contains

    !> @brief Create a pixmap with specified size and format
    subroutine forge_qpixmap_create(this, width, height, format)
        class(forge_qpixmap_t), intent(inout) :: this
        integer(c_int), intent(in) :: width, height
        integer, intent(in), optional :: format
        integer :: fmt
        integer(c_int) :: cairo_format

        ! Cleanup existing resources
        call this%cleanup()

        this%width_ = width
        this%height_ = height
        fmt = PIXMAP_FORMAT_ARGB32
        if (present(format)) fmt = format
        this%format_ = fmt

        ! Determine if format has alpha
        select case (fmt)
        case (PIXMAP_FORMAT_RGB32, PIXMAP_FORMAT_RGB16, PIXMAP_FORMAT_RGB888, &
              PIXMAP_FORMAT_RGB555, PIXMAP_FORMAT_RGB444, PIXMAP_FORMAT_RGB666)
            this%has_alpha_ = .false.
            this%premultiplied_alpha_ = .false.
        case (PIXMAP_FORMAT_ARGB32_PREMULTIPLIED, PIXMAP_FORMAT_ARGB8565_PREMULTIPLIED, &
              PIXMAP_FORMAT_ARGB6666_PREMULTIPLIED, PIXMAP_FORMAT_ARGB8555_PREMULTIPLIED, &
              PIXMAP_FORMAT_ARGB4444_PREMULTIPLIED, PIXMAP_FORMAT_RGBA8888_PREMULTIPLIED)
            this%has_alpha_ = .true.
            this%premultiplied_alpha_ = .true.
        case default
            this%has_alpha_ = .true.
            this%premultiplied_alpha_ = .false.
        end select

        ! Map to Cairo format
        if (this%has_alpha_) then
            cairo_format = CAIRO_FORMAT_ARGB32
        else
            cairo_format = CAIRO_FORMAT_RGB24
        end if

        ! Create Cairo surface
        this%surface_ = cairo_image_surface_create(cairo_format, width, height)
        if (cairo_surface_status(this%surface_) /= CAIRO_STATUS_SUCCESS) then
            this%surface_ = c_null_ptr
            return
        end if

        ! Create Cairo context
        this%cr_ = cairo_create(this%surface_)
        if (cairo_status(this%cr_) /= CAIRO_STATUS_SUCCESS) then
            call cairo_surface_destroy(this%surface_)
            this%surface_ = c_null_ptr
            this%cr_ = c_null_ptr
        end if
    end subroutine forge_qpixmap_create

    !> @brief Create pixmap from raw data
    subroutine forge_qpixmap_create_from_data(this, data, width, height, format, bytes_per_line)
        class(forge_qpixmap_t), intent(inout) :: this
        type(c_ptr), intent(in) :: data
        integer(c_int), intent(in) :: width, height
        integer, intent(in) :: format
        integer(c_int), intent(in), optional :: bytes_per_line
        integer(c_int) :: stride

        ! Cleanup existing resources
        call this%cleanup()

        this%width_ = width
        this%height_ = height
        this%format_ = format

        ! Calculate stride if not provided
        if (present(bytes_per_line)) then
            stride = bytes_per_line
        else
            stride = width * 4  ! Assume 32-bit per pixel
        end if

        ! Create surface from data
        this%surface_ = cairo_image_surface_create_for_data(data, CAIRO_FORMAT_ARGB32, &
                                                           width, height, stride)
        if (cairo_surface_status(this%surface_) /= CAIRO_STATUS_SUCCESS) then
            this%surface_ = c_null_ptr
            return
        end if

        ! Create Cairo context
        this%cr_ = cairo_create(this%surface_)
    end subroutine forge_qpixmap_create_from_data

    !> @brief Create pixmap from file
    subroutine forge_qpixmap_create_from_file(this, filename, format)
        class(forge_qpixmap_t), intent(inout) :: this
        character(len=*), intent(in) :: filename
        character(len=*), intent(in), optional :: format

        ! Cleanup existing resources
        call this%cleanup()

        ! Load image using Cairo
        this%surface_ = cairo_image_surface_create_from_png(c_loc(trim(filename)//c_null_char))
        if (cairo_surface_status(this%surface_) /= CAIRO_STATUS_SUCCESS) then
            this%surface_ = c_null_ptr
            return
        end if

        ! Get dimensions
        this%width_ = cairo_image_surface_get_width(this%surface_)
        this%height_ = cairo_image_surface_get_height(this%surface_)
        this%format_ = PIXMAP_FORMAT_ARGB32
        this%has_alpha_ = .true.

        ! Create Cairo context
        this%cr_ = cairo_create(this%surface_)
    end subroutine forge_qpixmap_create_from_file

    !> @brief Create a copy of this pixmap
    function forge_qpixmap_copy(this) result(copy_pixmap)
        class(forge_qpixmap_t), intent(in) :: this
        type(forge_qpixmap_t) :: copy_pixmap

        if (this%is_null()) return

        call copy_pixmap%create(this%width_, this%height_, this%format_)
        if (.not. copy_pixmap%is_null()) then
            ! Copy surface data
            call cairo_set_source_surface(copy_pixmap%cr_, this%surface_, 0.0_c_double, 0.0_c_double)
            call cairo_paint(copy_pixmap%cr_)
        end if
    end function forge_qpixmap_copy

    !> @brief Get pixmap width
    function forge_qpixmap_width(this) result(width)
        class(forge_qpixmap_t), intent(in) :: this
        integer(c_int) :: width
        width = this%width_
    end function forge_qpixmap_width

    !> @brief Get pixmap height
    function forge_qpixmap_height(this) result(height)
        class(forge_qpixmap_t), intent(in) :: this
        integer(c_int) :: height
        height = this%height_
    end function forge_qpixmap_height

    !> @brief Get pixmap size
    function forge_qpixmap_size(this) result(size)
        class(forge_qpixmap_t), intent(in) :: this
        type(forge_size) :: size
        call size%set(this%width_, this%height_)
    end function forge_qpixmap_size

    !> @brief Get pixmap rectangle
    function forge_qpixmap_rect(this) result(rect)
        class(forge_qpixmap_t), intent(in) :: this
        type(forge_rect) :: rect
        rect%pos%x = 0
        rect%pos%y = 0
        rect%size%width = this%width_
        rect%size%height = this%height_
    end function forge_qpixmap_rect

    !> @brief Get pixmap format
    function forge_qpixmap_format(this) result(format)
        class(forge_qpixmap_t), intent(in) :: this
        integer :: format
        format = this%format_
    end function forge_qpixmap_format

    !> @brief Check if pixmap has alpha channel
    function forge_qpixmap_has_alpha(this) result(has_alpha)
        class(forge_qpixmap_t), intent(in) :: this
        logical :: has_alpha
        has_alpha = this%has_alpha_
    end function forge_qpixmap_has_alpha

    !> @brief Check if pixmap is null
    function forge_qpixmap_is_null(this) result(is_null)
        class(forge_qpixmap_t), intent(in) :: this
        logical :: is_null
        is_null = (.not. c_associated(this%surface_)) .or. (this%width_ <= 0 .or. this%height_ <= 0)
    end function forge_qpixmap_is_null

    !> @brief Fill pixmap with color
    subroutine forge_qpixmap_fill(this, color)
        class(forge_qpixmap_t), intent(inout) :: this
        type(forge_color), intent(in) :: color

        if (this%is_null()) return

        call cairo_set_source_rgba(this%cr_, color%r, color%g, color%b, color%a)
        call cairo_paint(this%cr_)
    end subroutine forge_qpixmap_fill

    !> @brief Create scaled version of pixmap
    function forge_qpixmap_scaled(this, width, height, aspect_ratio_mode, transform_mode) result(scaled_pixmap)
        class(forge_qpixmap_t), intent(in) :: this
        integer(c_int), intent(in) :: width, height
        integer, intent(in), optional :: aspect_ratio_mode, transform_mode
        type(forge_qpixmap_t) :: scaled_pixmap

        if (this%is_null()) return

        call scaled_pixmap%create(width, height, this%format_)
        if (.not. scaled_pixmap%is_null()) then
            ! Scale and draw
            call cairo_scale(scaled_pixmap%cr_, &
                           real(width, c_double)/real(this%width_, c_double), &
                           real(height, c_double)/real(this%height_, c_double))
            call cairo_set_source_surface(scaled_pixmap%cr_, this%surface_, 0.0_c_double, 0.0_c_double)
            call cairo_paint(scaled_pixmap%cr_)
        end if
    end function forge_qpixmap_scaled

    !> @brief Create transformed version of pixmap
    function forge_qpixmap_transformed(this, transform) result(transformed_pixmap)
        class(forge_qpixmap_t), intent(in) :: this
        real(c_double), intent(in) :: transform(6)  ! Affine transformation matrix
        type(forge_qpixmap_t) :: transformed_pixmap

        ! For now, return copy - full transformation implementation would be complex
        transformed_pixmap = this%copy()
    end function forge_qpixmap_transformed

    !> @brief Convert pixmap to image data
    function forge_qpixmap_to_image(this) result(image_data)
        class(forge_qpixmap_t), intent(in) :: this
        type(c_ptr) :: image_data

        if (this%is_null()) then
            image_data = c_null_ptr
        else
            image_data = cairo_image_surface_get_data(this%surface_)
        end if
    end function forge_qpixmap_to_image

    !> @brief Save pixmap to file
    function forge_qpixmap_save(this, filename, format, quality) result(success)
        class(forge_qpixmap_t), intent(in) :: this
        character(len=*), intent(in) :: filename
        character(len=*), intent(in), optional :: format
        integer, intent(in), optional :: quality
        logical :: success

        success = .false.
        if (this%is_null()) return

        ! For now, only PNG support
        if (cairo_surface_write_to_png(this%surface_, c_loc(trim(filename)//c_null_char)) == CAIRO_STATUS_SUCCESS) then
            success = .true.
        end if
    end function forge_qpixmap_save

    !> @brief Load pixmap from file
    function forge_qpixmap_load(this, filename) result(success)
        class(forge_qpixmap_t), intent(inout) :: this
        character(len=*), intent(in) :: filename
        logical :: success

        call this%create_from_file(filename)
        success = .not. this%is_null()
    end function forge_qpixmap_load

    !> @brief Get painter for drawing on pixmap
    function forge_qpixmap_painter(this) result(cr)
        class(forge_qpixmap_t), intent(in) :: this
        type(c_ptr) :: cr
        cr = this%cr_
    end function forge_qpixmap_painter

    !> @brief Detach pixmap (force copy on write)
    subroutine forge_qpixmap_detach(this)
        class(forge_qpixmap_t), intent(inout) :: this
        ! For now, do nothing - full copy-on-write would require reference counting
    end subroutine forge_qpixmap_detach

    !> @brief Cleanup pixmap resources
    subroutine forge_qpixmap_cleanup(this)
        class(forge_qpixmap_t), intent(inout) :: this
        if (c_associated(this%cr_)) then
            call cairo_destroy(this%cr_)
            this%cr_ = c_null_ptr
        end if
        if (c_associated(this%surface_)) then
            call cairo_surface_destroy(this%surface_)
            this%surface_ = c_null_ptr
        end if
        this%width_ = 0
        this%height_ = 0
    end subroutine forge_qpixmap_cleanup

    !> @brief Convert pixmap to different format
    function forge_qpixmap_convert_to_format(this, format) result(converted_pixmap)
        class(forge_qpixmap_t), intent(in) :: this
        integer, intent(in) :: format
        type(forge_qpixmap_t) :: converted_pixmap

        ! For now, return copy with new format - full conversion would be complex
        converted_pixmap = this%copy()
        converted_pixmap%format_ = format
    end function forge_qpixmap_convert_to_format

    !> @brief Get pixmap depth (bits per pixel)
    function forge_qpixmap_depth(this) result(depth)
        class(forge_qpixmap_t), intent(in) :: this
        integer :: depth

        select case (this%format_)
        case (PIXMAP_FORMAT_MONO, PIXMAP_FORMAT_MONO_LSB)
            depth = 1
        case (PIXMAP_FORMAT_INDEXED8)
            depth = 8
        case (PIXMAP_FORMAT_RGB16, PIXMAP_FORMAT_RGB555, PIXMAP_FORMAT_RGB444, &
              PIXMAP_FORMAT_ARGB4444_PREMULTIPLIED, PIXMAP_FORMAT_ARGB8555_PREMULTIPLIED)
            depth = 16
        case (PIXMAP_FORMAT_RGB32, PIXMAP_FORMAT_ARGB32, PIXMAP_FORMAT_ARGB32_PREMULTIPLIED, &
              PIXMAP_FORMAT_RGB888, PIXMAP_FORMAT_RGBX8888, PIXMAP_FORMAT_RGBA8888, &
              PIXMAP_FORMAT_RGBA8888_PREMULTIPLIED)
            depth = 32
        case default
            depth = 32
        end select
    end function forge_qpixmap_depth

    !> @brief Get device pixel ratio
    function forge_qpixmap_device_pixel_ratio(this) result(ratio)
        class(forge_qpixmap_t), intent(in) :: this
        real(c_double) :: ratio
        ratio = 1.0_c_double
    end function forge_qpixmap_device_pixel_ratio

    !> @brief Set device pixel ratio
    subroutine forge_qpixmap_set_device_pixel_ratio(this, ratio)
        class(forge_qpixmap_t), intent(inout) :: this
        real(c_double), intent(in) :: ratio
        ! For now, ignore - high DPI support would be complex
    end subroutine forge_qpixmap_set_device_pixel_ratio

    !> @brief Get cache key for pixmap
    function forge_qpixmap_cache_key(this) result(key)
        class(forge_qpixmap_t), intent(in) :: this
        integer(c_long_long) :: key
        ! Simple key based on surface pointer
        key = transfer(c_loc(this%surface_), key)
    end function forge_qpixmap_cache_key

    !> @brief Check if pixmap is detached
    function forge_qpixmap_is_detached(this) result(detached)
        class(forge_qpixmap_t), intent(in) :: this
        logical :: detached
        detached = .true.  ! For now, always detached
    end function forge_qpixmap_is_detached

    !> @brief Scroll pixmap contents
    subroutine forge_qpixmap_scroll(this, dx, dy, rect)
        class(forge_qpixmap_t), intent(inout) :: this
        integer(c_int), intent(in) :: dx, dy
        type(forge_rect), intent(in), optional :: rect
        ! Implementation would require complex surface manipulation
    end subroutine forge_qpixmap_scroll

    !> @brief Copy rectangle from pixmap
    function forge_qpixmap_copy_rect(this, rect) result(copy_pixmap)
        class(forge_qpixmap_t), intent(in) :: this
        type(forge_rect), intent(in) :: rect
        type(forge_qpixmap_t) :: copy_pixmap

        if (this%is_null()) return

        call copy_pixmap%create(rect%size%width, rect%size%height, this%format_)
        if (.not. copy_pixmap%is_null()) then
            call cairo_set_source_surface(copy_pixmap%cr_, this%surface_, &
                                        -real(rect%pos%x, c_double), -real(rect%pos%y, c_double))
            call cairo_paint(copy_pixmap%cr_)
        end if
    end function forge_qpixmap_copy_rect

    !> @brief Set transparency mask
    subroutine forge_qpixmap_set_mask(this, mask)
        class(forge_qpixmap_t), intent(inout) :: this
        type(forge_qpixmap_t), intent(in) :: mask
        ! Implementation would require complex masking operations
    end subroutine forge_qpixmap_set_mask

    !> @brief Create heuristic mask
    function forge_qpixmap_create_heuristic_mask(this, clip_tight) result(mask)
        class(forge_qpixmap_t), intent(in) :: this
        logical, intent(in), optional :: clip_tight
        type(forge_qpixmap_t) :: mask
        ! Implementation would require image analysis
    end function forge_qpixmap_create_heuristic_mask

    !> @brief Create mask from color
    function forge_qpixmap_create_mask_from_color(this, color, mode) result(mask)
        class(forge_qpixmap_t), intent(in) :: this
        type(forge_color), intent(in) :: color
        integer, intent(in), optional :: mode
        type(forge_qpixmap_t) :: mask
        ! Implementation would require color analysis
    end function forge_qpixmap_create_mask_from_color

    !> @brief Grab widget contents
    function forge_qpixmap_grab_widget(widget) result(pixmap)
        type(c_ptr), intent(in) :: widget
        type(forge_qpixmap_t) :: pixmap
        
        ! This would require platform-specific code to capture widget rendering
        ! For example, on X11: use XGetImage, on Windows: use BitBlt, etc.
        ! For now, return null pixmap
        
        ! Implementation would:
        ! 1. Get widget geometry (position, size)
        ! 2. Create pixmap with appropriate size
        ! 3. Use platform APIs to capture the widget pixels
        ! 4. Copy pixel data to the pixmap surface
    end function forge_qpixmap_grab_widget
end module forge_qpixmap
