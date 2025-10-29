!> @brief XML formatting and indentation
!> @details QXmlFormatter for XML formatting and indentation
!> @author ForGE Contributors
!> @date 2025
!> @license GPL-3.0-or-later

module forge_xml_formatter
    use forge_string_utils
    use forge_containers
    use forge_errors
    implicit none
    private

    public :: QXmlFormatter

    !> @brief XML formatter for indentation and formatting
    type :: QXmlFormatter
        private
        character(len=:), allocatable :: output
        integer :: indent_level = 0
        integer :: indent_size = 4
        logical :: auto_formatting = .true.
        character(len=1) :: indent_char = ' '
        type(forge_error) :: error
    contains
        procedure :: set_indent_size => xmlformatter_set_indent_size
        procedure :: indent_size => xmlformatter_indent_size
        procedure :: set_indent_char => xmlformatter_set_indent_char
        procedure :: indent_char => xmlformatter_indent_char
        procedure :: set_auto_formatting => xmlformatter_set_auto_formatting
        procedure :: auto_formatting => xmlformatter_auto_formatting
        procedure :: format => xmlformatter_format
        procedure :: format_to_string => xmlformatter_format_to_string
        procedure :: format_to_file => xmlformatter_format_to_file
        procedure :: has_error => xmlformatter_has_error
        procedure :: error_string => xmlformatter_error_string
        procedure, private :: write_indent => xmlformatter_write_indent
        procedure, private :: append => xmlformatter_append
        procedure, private :: format_element => xmlformatter_format_element
        procedure, private :: format_text => xmlformatter_format_text
        procedure, private :: format_comment => xmlformatter_format_comment
        procedure, private :: format_processing_instruction => xmlformatter_format_processing_instruction
        procedure, private :: format_cdata => xmlformatter_format_cdata
    end type QXmlFormatter

contains

    ! ========== QXmlFormatter Implementation ==========

    subroutine xmlformatter_set_indent_size(this, size)
        class(QXmlFormatter), intent(inout) :: this
        integer, intent(in) :: size
        this%indent_size = size
    end subroutine xmlformatter_set_indent_size

    function xmlformatter_indent_size(this) result(size)
        class(QXmlFormatter), intent(in) :: this
        integer :: size
        size = this%indent_size
    end function xmlformatter_indent_size

    subroutine xmlformatter_set_indent_char(this, char)
        class(QXmlFormatter), intent(inout) :: this
        character(len=1), intent(in) :: char
        this%indent_char = char
    end subroutine xmlformatter_set_indent_char

    function xmlformatter_indent_char(this) result(char)
        class(QXmlFormatter), intent(in) :: this
        character(len=1) :: char
        char = this%indent_char
    end function xmlformatter_indent_char

    subroutine xmlformatter_set_auto_formatting(this, enabled)
        class(QXmlFormatter), intent(inout) :: this
        logical, intent(in) :: enabled
        this%auto_formatting = enabled
    end subroutine xmlformatter_set_auto_formatting

    function xmlformatter_auto_formatting(this) result(enabled)
        class(QXmlFormatter), intent(in) :: this
        logical :: enabled
        enabled = this%auto_formatting
    end function xmlformatter_auto_formatting

    function xmlformatter_format(this, xml_content) result(success)
        class(QXmlFormatter), intent(inout) :: this
        character(len=*), intent(in) :: xml_content
        logical :: success

        success = .false.
        this%output = ""
        this%indent_level = 0

        ! Basic XML formatting - simplified
        ! Would parse and reformat XML with proper indentation
        call this%append(xml_content)
        success = .true.
    end function xmlformatter_format

    function xmlformatter_format_to_string(this, xml_content) result(formatted)
        class(QXmlFormatter), intent(inout) :: this
        character(len=*), intent(in) :: xml_content
        character(len=:), allocatable :: formatted

        if (this%format(xml_content)) then
            formatted = this%output
        else
            formatted = xml_content  ! Return original on error
        end if
    end function xmlformatter_format_to_string

    function xmlformatter_format_to_file(this, xml_content, filename) result(success)
        class(QXmlFormatter), intent(inout) :: this
        character(len=*), intent(in) :: xml_content, filename
        logical :: success

        integer :: unit, ios

        success = .false.
        if (.not. this%format(xml_content)) return

        ! Open file for writing with UTF-8 encoding
        open(newunit=unit, file=filename, status='replace', action='write', iostat=ios, encoding='UTF-8')
        if (ios /= 0) then
            call this%error%raise(FORGE_ERROR_FILE_NOT_FOUND, "Cannot create formatted output file: " // trim(filename))
            return
        end if

        ! Write formatted content to file
        write(unit, '(A)', iostat=ios) this%output
        close(unit)

        if (ios == 0) then
            success = .true.
        else
            call this%error%raise(FORGE_ERROR_IO, "Error writing formatted XML to file: " // trim(filename))
        end if
    end function xmlformatter_format_to_file

    function xmlformatter_has_error(this) result(has_err)
        class(QXmlFormatter), intent(in) :: this
        logical :: has_err
        has_err = this%error%status%is_error()
    end function xmlformatter_has_error

    function xmlformatter_error_string(this) result(err_str)
        class(QXmlFormatter), intent(in) :: this
        character(len=:), allocatable :: err_str
        err_str = trim(this%error%status%message)
    end function xmlformatter_error_string

    subroutine xmlformatter_write_indent(this)
        class(QXmlFormatter), intent(inout) :: this
        integer :: i

        if (.not. this%auto_formatting) return

        do i = 1, this%indent_level * this%indent_size
            call this%append(this%indent_char)
        end do
    end subroutine xmlformatter_write_indent

    subroutine xmlformatter_append(this, text)
        class(QXmlFormatter), intent(inout) :: this
        character(len=*), intent(in) :: text

        if (allocated(this%output)) then
            this%output = this%output // text
        else
            this%output = text
        end if
    end subroutine xmlformatter_append

    subroutine xmlformatter_format_element(this, element_name, attributes, has_children)
        class(QXmlFormatter), intent(inout) :: this
        character(len=*), intent(in) :: element_name
        type(QHashMap), intent(in) :: attributes
        logical, intent(in) :: has_children

        call this%write_indent()
        call this%append('<')
        call this%append(element_name)

        ! Add attributes - simplified
        call this%append('>')

        if (this%auto_formatting .and. has_children) then
            call this%append(achar(10))
            this%indent_level = this%indent_level + 1
        end if
    end subroutine xmlformatter_format_element

    subroutine xmlformatter_format_text(this, text)
        class(QXmlFormatter), intent(inout) :: this
        character(len=*), intent(in) :: text

        call this%write_indent()
        call this%append(text)
        if (this%auto_formatting) call this%append(achar(10))
    end subroutine xmlformatter_format_text

    subroutine xmlformatter_format_comment(this, comment)
        class(QXmlFormatter), intent(inout) :: this
        character(len=*), intent(in) :: comment

        call this%write_indent()
        call this%append('<!--')
        call this%append(comment)
        call this%append('-->')
        if (this%auto_formatting) call this%append(achar(10))
    end subroutine xmlformatter_format_comment

    subroutine xmlformatter_format_processing_instruction(this, target, data)
        class(QXmlFormatter), intent(inout) :: this
        character(len=*), intent(in) :: target, data

        call this%write_indent()
        call this%append('<?')
        call this%append(target)
        if (len_trim(data) > 0) then
            call this%append(' ')
            call this%append(data)
        end if
        call this%append('?>')
        if (this%auto_formatting) call this%append(achar(10))
    end subroutine xmlformatter_format_processing_instruction

    subroutine xmlformatter_format_cdata(this, cdata)
        class(QXmlFormatter), intent(inout) :: this
        character(len=*), intent(in) :: cdata

        call this%write_indent()
        call this%append('<![CDATA[')
        call this%append(cdata)
        call this%append(']]>')
        if (this%auto_formatting) call this%append(achar(10))
    end subroutine xmlformatter_format_cdata

end module forge_xml_formatter