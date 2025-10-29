!> @brief TS file parser and generator for Qt translation files
!> @details XML-based parsing and generation of .ts translation source files
!> @author ForGE Contributors
!> @date 2025
!> @license GPL-3.0-or-later

module forge_ts_parser
    use iso_c_binding
    use iso_fortran_env, only: int32, int64, real32, real64
    use forge_string_utils, only: QString, string_split
    use forge_translator, only: QTranslator, QTranslatorMessage
    implicit none
    private

    public :: TSParser, TSGenerator

    !> @brief TS file parser
    type :: TSParser
        private
        character(len=:), allocatable :: xml_content_
        integer :: position_ = 1
    contains
        ! Parsing methods
        procedure :: parse => tsparser_parse
        procedure :: parse_file => tsparser_parse_file
        procedure :: parse_string => tsparser_parse_string

        ! XML parsing helpers
        procedure, private :: skip_whitespace => tsparser_skip_whitespace
        procedure, private :: read_tag => tsparser_read_tag
        procedure, private :: read_attribute => tsparser_read_attribute
        procedure, private :: read_text_content => tsparser_read_text_content
        procedure, private :: parse_message => tsparser_parse_message
        procedure, private :: parse_context => tsparser_parse_context
    end type TSParser

    !> @brief TS file generator
    type :: TSGenerator
        private
        logical :: pretty_print_ = .true.
        integer :: indent_level_ = 0
    contains
        ! Generation methods
        procedure :: generate => tsgen_generate
        procedure :: generate_file => tsgen_generate_file
        procedure :: generate_string => tsgen_generate_string

        ! XML generation helpers
        procedure, private :: write_header => tsgen_write_header
        procedure, private :: write_context => tsgen_write_context
        procedure, private :: write_message => tsgen_write_message
        procedure, private :: write_footer => tsgen_write_footer
        procedure, private :: indent => tsgen_indent
        procedure, private :: escape_xml => tsgen_escape_xml
    end type TSGenerator

contains

    ! ========== TSParser Implementation ==========

    !> @brief Parse TS file
    function tsparser_parse(this, translator, filename) result(success)
        class(TSParser), intent(inout) :: this
        type(QTranslator), intent(inout) :: translator
        character(len=*), intent(in) :: filename
        logical :: success

        success = this%parse_file(filename)
        if (success) then
            success = this%parse_string(translator, this%xml_content_)
        end if
    end function tsparser_parse

    !> @brief Parse TS file from disk
    function tsparser_parse_file(this, filename) result(success)
        class(TSParser), intent(inout) :: this
        character(len=*), intent(in) :: filename
        logical :: success
        integer :: unit, iostat, file_size

        open(newunit=unit, file=filename, status='old', action='read', &
             iostat=iostat, form='unformatted', access='stream')
        if (iostat /= 0) then
            success = .false.
            return
        end if

        inquire(unit, size=file_size)
        if (file_size > 0) then
            allocate(character(len=file_size) :: this%xml_content_)
            read(unit, iostat=iostat) this%xml_content_
            success = (iostat == 0)
        else
            success = .false.
        end if

        close(unit)
        this%position_ = 1
    end function tsparser_parse_file

    !> @brief Parse TS string content
    function tsparser_parse_string(this, translator, xml_content) result(success)
        class(TSParser), intent(inout) :: this
        type(QTranslator), intent(inout) :: translator
        character(len=*), intent(in) :: xml_content
        logical :: success
        character(len=:), allocatable :: tag

        this%xml_content_ = xml_content
        this%position_ = 1

        ! Skip XML declaration
        call this%skip_whitespace()
        if (this%position_ <= len(this%xml_content_) .and. &
            this%xml_content_(this%position_:this%position_+4) == '<?xml') then
            do while (this%position_ <= len(this%xml_content_) .and. &
                     this%xml_content_(this%position_:this%position_+1) /= '?>')
                this%position_ = this%position_ + 1
            end do
            this%position_ = this%position_ + 2
        end if

        ! Skip DOCTYPE
        call this%skip_whitespace()
        if (this%position_ <= len(this%xml_content_) .and. &
            this%xml_content_(this%position_:this%position_+8) == '<!DOCTYPE') then
            do while (this%position_ <= len(this%xml_content_) .and. &
                     this%xml_content_(this%position_:this%position_) /= '>')
                this%position_ = this%position_ + 1
            end do
            this%position_ = this%position_ + 1
        end if

        ! Parse TS root element
        call this%skip_whitespace()
        tag = this%read_tag()
        if (tag /= 'TS') then
            success = .false.
            return
        end if

        ! Parse contexts
        do while (this%position_ <= len(this%xml_content_))
            call this%skip_whitespace()
            if (this%position_ > len(this%xml_content_)) exit

            if (this%xml_content_(this%position_:this%position_) == '<') then
                tag = this%read_tag()
                if (tag == '/TS') then
                    exit
                else if (tag == 'context') then
                    success = this%parse_context(translator)
                    if (.not. success) return
                end if
            else
                this%position_ = this%position_ + 1
            end if
        end do

        success = .true.
    end function tsparser_parse_string

    !> @brief Skip whitespace characters
    subroutine tsparser_skip_whitespace(this)
        class(TSParser), intent(inout) :: this

        do while (this%position_ <= len(this%xml_content_))
            select case (this%xml_content_(this%position_:this%position_))
            case (' ', achar(9), achar(10), achar(13))  ! space, tab, LF, CR
                this%position_ = this%position_ + 1
            case default
                exit
            end select
        end do
    end subroutine tsparser_skip_whitespace

    !> @brief Read XML tag name
    function tsparser_read_tag(this) result(tag)
        class(TSParser), intent(inout) :: this
        character(len=:), allocatable :: tag
        integer :: start_pos

        if (this%xml_content_(this%position_:this%position_) /= '<') then
            tag = ''
            return
        end if

        this%position_ = this%position_ + 1  ! Skip '<'
        start_pos = this%position_

        do while (this%position_ <= len(this%xml_content_) .and. &
                 this%xml_content_(this%position_:this%position_) /= '>' .and. &
                 this%xml_content_(this%position_:this%position_) /= ' ')
            this%position_ = this%position_ + 1
        end do

        tag = this%xml_content_(start_pos:this%position_-1)
    end function tsparser_read_tag

    !> @brief Read XML attribute
    function tsparser_read_attribute(this, attr_name) result(attr_value)
        class(TSParser), intent(inout) :: this
        character(len=*), intent(in) :: attr_name
        character(len=:), allocatable :: attr_value
        integer :: pos

        attr_value = ''
        pos = index(this%xml_content_(this%position_:), attr_name // '="')
        if (pos == 0) return

        this%position_ = this%position_ + pos + len(attr_name) + 1  ! Skip to quote
        pos = index(this%xml_content_(this%position_:), '"')
        if (pos > 0) then
            attr_value = this%xml_content_(this%position_:this%position_+pos-2)
            this%position_ = this%position_ + pos
        end if
    end function tsparser_read_attribute

    !> @brief Read text content between tags
    function tsparser_read_text_content(this) result(content)
        class(TSParser), intent(inout) :: this
        character(len=:), allocatable :: content
        integer :: start_pos

        start_pos = this%position_
        do while (this%position_ <= len(this%xml_content_) .and. &
                 this%xml_content_(this%position_:this%position_) /= '<')
            this%position_ = this%position_ + 1
        end do

        if (this%position_ > start_pos) then
            content = this%xml_content_(start_pos:this%position_-1)
        else
            content = ''
        end if
    end function tsparser_read_text_content

    !> @brief Parse context element
    function tsparser_parse_context(this, translator) result(success)
        class(TSParser), intent(inout) :: this
        type(QTranslator), intent(inout) :: translator
        logical :: success
        character(len=:), allocatable :: tag, context_name

        success = .true.

        ! Read context name
        call this%skip_whitespace()
        context_name = this%read_text_content()

        ! Skip to messages
        do while (this%position_ <= len(this%xml_content_))
            call this%skip_whitespace()
            if (this%position_ > len(this%xml_content_)) exit

            if (this%xml_content_(this%position_:this%position_) == '<') then
                tag = this%read_tag()
                if (tag == '/context') then
                    exit
                else if (tag == 'message') then
                    success = this%parse_message(translator, context_name)
                    if (.not. success) return
                end if
            else
                this%position_ = this%position_ + 1
            end if
        end do
    end function tsparser_parse_context

    !> @brief Parse message element
    function tsparser_parse_message(this, translator, context_name) result(success)
        class(TSParser), intent(inout) :: this
        type(QTranslator), intent(inout) :: translator
        character(len=*), intent(in) :: context_name
        logical :: success
        type(QTranslatorMessage) :: message
        character(len=:), allocatable :: tag, source_text, translation_text

        success = .true.
        call message%set_context(context_name)

        ! Parse message content
        do while (this%position_ <= len(this%xml_content_))
            call this%skip_whitespace()
            if (this%position_ > len(this%xml_content_)) exit

            if (this%xml_content_(this%position_:this%position_) == '<') then
                tag = this%read_tag()
                if (tag == '/message') then
                    exit
                else if (tag == 'source') then
                    source_text = this%read_text_content()
                    call message%set_source_text(source_text)
                else if (tag == 'translation') then
                    translation_text = this%read_text_content()
                    call message%set_translation(translation_text)
                end if
            else
                this%position_ = this%position_ + 1
            end if
        end do

        ! Add message to translator
        call translator%insert(message)
    end function tsparser_parse_message

    ! ========== TSGenerator Implementation ==========

    !> @brief Generate TS file
    function tsgen_generate(this, translator, filename, pretty_print) result(success)
        class(TSGenerator), intent(inout) :: this
        type(QTranslator), intent(in) :: translator
        character(len=*), intent(in) :: filename
        logical, intent(in), optional :: pretty_print
        logical :: success
        character(len=:), allocatable :: content

        if (present(pretty_print)) this%pretty_print_ = pretty_print

        content = this%generate_string(translator)
        success = this%generate_file(content, filename)
    end function tsgen_generate

    !> @brief Generate TS file content
    function tsgen_generate_string(this, translator) result(content)
        class(TSGenerator), intent(inout) :: this
        type(QTranslator), intent(in) :: translator
        character(len=:), allocatable :: content

        content = ''
        this%indent_level_ = 0

        ! Write header
        content = content // this%write_header()

        ! Write contexts and messages
        content = content // this%write_contexts(translator)

        ! Write footer
        content = content // this%write_footer()
    end function tsgen_generate_string

    !> @brief Write TS file to disk
    function tsgen_generate_file(this, content, filename) result(success)
        class(TSGenerator), intent(in) :: this
        character(len=*), intent(in) :: content
        character(len=*), intent(in) :: filename
        logical :: success
        integer :: unit, iostat

        open(newunit=unit, file=filename, status='replace', action='write', &
             iostat=iostat, encoding='utf-8')
        if (iostat /= 0) then
            success = .false.
            return
        end if

        write(unit, '(A)', iostat=iostat) content
        success = (iostat == 0)
        close(unit)
    end function tsgen_generate_file

    !> @brief Write TS header
    function tsgen_write_header(this) result(header)
        class(TSGenerator), intent(in) :: this
        character(len=:), allocatable :: header

        header = '<?xml version="1.0" encoding="utf-8"?>' // achar(10)
        header = header // '<!DOCTYPE TS>' // achar(10)
        header = header // '<TS version="2.1">' // achar(10)
    end function tsgen_write_header

    !> @brief Write contexts
    function tsgen_write_contexts(this, translator) result(contexts_str)
        class(TSGenerator), intent(inout) :: this
        type(QTranslator), intent(in) :: translator
        character(len=:), allocatable :: contexts_str
        ! Simplified - would iterate through messages and group by context
        contexts_str = this%indent() // '<context>' // achar(10)
        this%indent_level_ = this%indent_level_ + 1
        contexts_str = contexts_str // this%indent() // '<name>MainWindow</name>' // achar(10)
        ! Would write messages here
        this%indent_level_ = this%indent_level_ - 1
        contexts_str = contexts_str // this%indent() // '</context>' // achar(10)
    end function tsgen_write_contexts

    !> @brief Write TS footer
    function tsgen_write_footer(this) result(footer)
        class(TSGenerator), intent(in) :: this
        character(len=:), allocatable :: footer

        footer = '</TS>' // achar(10)
    end function tsgen_write_footer

    !> @brief Generate indentation
    function tsgen_indent(this) result(indent_str)
        class(TSGenerator), intent(in) :: this
        character(len=:), allocatable :: indent_str
        integer :: i

        indent_str = ''
        if (.not. this%pretty_print_) return

        do i = 1, this%indent_level_
            indent_str = indent_str // '    '
        end do
    end function tsgen_indent

    !> @brief Escape XML special characters
    function tsgen_escape_xml(this, text) result(escaped)
        class(TSGenerator), intent(in) :: this
        character(len=*), intent(in) :: text
        character(len=:), allocatable :: escaped

        escaped = text
        escaped = replace_all(escaped, '&', '&')
        escaped = replace_all(escaped, '<', '<')
        escaped = replace_all(escaped, '>', '>')
        escaped = replace_all(escaped, '"', '"')
        escaped = replace_all(escaped, "'", ''')
    end function tsgen_escape_xml

    !> @brief Replace all occurrences of a substring
    function replace_all(str, old, new) result(result_str)
        character(len=*), intent(in) :: str, old, new
        character(len=:), allocatable :: result_str
        integer :: pos

        result_str = str
        pos = index(result_str, old)
        do while (pos > 0)
            result_str = result_str(1:pos-1) // new // result_str(pos+len(old):)
            pos = index(result_str, old)
        end do
    end function replace_all

end module forge_ts_parser