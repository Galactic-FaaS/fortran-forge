!> @brief XML streaming parser and writer
!> @details High-performance XML streaming with QXmlStreamReader/QXmlStreamWriter
!> @author ForGE Contributors
!> @date 2025
!> @license GPL-3.0-or-later

module forge_xml_stream
    use forge_string_utils
    use forge_errors
    implicit none
    private

    public :: QXmlStreamReader, QXmlStreamWriter
    public :: QXmlStreamReader_TokenType
    public :: StartDocument, EndDocument, StartElement, EndElement
    public :: Characters, Comment, ProcessingInstruction, DTD
    public :: EntityReference, NoToken

    !> Token types for XML streaming
    integer, parameter :: NoToken = 0
    integer, parameter :: Invalid = 1
    integer, parameter :: StartDocument = 2
    integer, parameter :: EndDocument = 3
    integer, parameter :: StartElement = 4
    integer, parameter :: EndElement = 5
    integer, parameter :: Characters = 6
    integer, parameter :: Comment = 7
    integer, parameter :: DTD = 8
    integer, parameter :: EntityReference = 9
    integer, parameter :: ProcessingInstruction = 10

    !> @brief XML stream reader token type
    type :: QXmlStreamReader_TokenType
        integer :: token_type = NoToken
        character(len=:), allocatable :: name
        character(len=:), allocatable :: text
        type(QHashMap) :: attributes
        type(QHashMap) :: namespace_declarations
        logical :: is_whitespace = .false.
        logical :: is_cdata = .false.
    end type QXmlStreamReader_TokenType

    !> @brief High-performance XML stream reader
    type :: QXmlStreamReader
        private
        character(len=:), allocatable :: data
        integer :: position = 1
        type(QXmlStreamReader_TokenType) :: current_token
        type(forge_error) :: error
        logical :: at_end = .false.
    contains
        procedure :: set_data => xmlstreamreader_set_data
        procedure :: set_device => xmlstreamreader_set_device
        procedure :: read_next => xmlstreamreader_read_next
        procedure :: token_type => xmlstreamreader_token_type
        procedure :: token_string => xmlstreamreader_token_string
        procedure :: name => xmlstreamreader_name
        procedure :: text => xmlstreamreader_text
        procedure :: attributes => xmlstreamreader_attributes
        procedure :: is_whitespace => xmlstreamreader_is_whitespace
        procedure :: is_cdata => xmlstreamreader_is_cdata
        procedure :: at_end => xmlstreamreader_at_end
        procedure :: has_error => xmlstreamreader_has_error
        procedure :: error_string => xmlstreamreader_error_string
        procedure :: line_number => xmlstreamreader_line_number
        procedure :: column_number => xmlstreamreader_column_number
        procedure :: character_offset => xmlstreamreader_character_offset
        procedure, private :: parse_start_element => xmlstreamreader_parse_start_element
        procedure, private :: parse_end_element => xmlstreamreader_parse_end_element
        procedure, private :: parse_characters => xmlstreamreader_parse_characters
        procedure, private :: parse_comment => xmlstreamreader_parse_comment
        procedure, private :: parse_processing_instruction => xmlstreamreader_parse_processing_instruction
        procedure, private :: parse_dtd => xmlstreamreader_parse_dtd
        procedure, private :: skip_whitespace => xmlstreamreader_skip_whitespace
        procedure, private :: read_name => xmlstreamreader_read_name
        procedure, private :: read_attribute => xmlstreamreader_read_attribute
    end type QXmlStreamReader

    !> @brief XML stream writer
    type :: QXmlStreamWriter
        private
        character(len=:), allocatable :: buffer
        integer :: indent_level = 0
        logical :: auto_formatting = .false.
        integer :: indent_size = 4
        type(forge_error) :: error
        character(len=:), allocatable :: device_filename
        integer :: device_unit = -1
        logical :: writing_to_file = .false.
    contains
        procedure :: set_auto_formatting => xmlstreamwriter_set_auto_formatting
        procedure :: set_auto_formatting_enabled => xmlstreamwriter_set_auto_formatting_enabled
        procedure :: set_device => xmlstreamwriter_set_device
        procedure :: write_start_document => xmlstreamwriter_write_start_document
        procedure :: write_end_document => xmlstreamwriter_write_end_document
        procedure :: write_start_element => xmlstreamwriter_write_start_element
        procedure :: write_end_element => xmlstreamwriter_write_end_element
        procedure :: write_empty_element => xmlstreamwriter_write_empty_element
        procedure :: write_attribute => xmlstreamwriter_write_attribute
        procedure :: write_namespace => xmlstreamwriter_write_namespace
        procedure :: write_default_namespace => xmlstreamwriter_write_default_namespace
        procedure :: write_characters => xmlstreamwriter_write_characters
        procedure :: write_cdata => xmlstreamwriter_write_cdata
        procedure :: write_comment => xmlstreamwriter_write_comment
        procedure :: write_processing_instruction => xmlstreamwriter_write_processing_instruction
        procedure :: write_dtd => xmlstreamwriter_write_dtd
        procedure :: write_entity_reference => xmlstreamwriter_write_entity_reference
        procedure :: get_buffer => xmlstreamwriter_get_buffer
        procedure :: has_error => xmlstreamwriter_has_error
        procedure :: error_string => xmlstreamwriter_error_string
        procedure, private :: write_indent => xmlstreamwriter_write_indent
        procedure, private :: append => xmlstreamwriter_append
        procedure, private :: escape => xmlstreamwriter_escape
    end type QXmlStreamWriter

contains

    ! ========== QXmlStreamReader Implementation ==========

    subroutine xmlstreamreader_set_data(this, data)
        class(QXmlStreamReader), intent(inout) :: this
        character(len=*), intent(in) :: data

        this%data = data
        this%position = 1
        this%at_end = .false.
        this%current_token%token_type = NoToken
        call this%error%clear()
    end subroutine xmlstreamreader_set_data

    subroutine xmlstreamreader_set_device(this, device)
        class(QXmlStreamReader), intent(inout) :: this
        character(len=*), intent(in) :: device

        integer :: unit, ios
        character(len=1000) :: line
        character(len=:), allocatable :: file_content
        integer :: file_size, current_size

        file_content = ""
        current_size = 0

        ! First, check file size for memory allocation
        open(newunit=unit, file=device, status='old', action='read', iostat=ios, encoding='UTF-8')
        if (ios /= 0) then
            call this%error%raise(FORGE_ERROR_FILE_NOT_FOUND, "Cannot open XML file: " // trim(device))
            return
        end if

        ! Estimate file size by reading in chunks
        do
            read(unit, '(A)', iostat=ios, size=file_size) line
            if (ios /= 0) exit
            current_size = current_size + len_trim(line) + 1
        end do
        close(unit)

        ! Reopen and read content with pre-allocated memory
        if (current_size > 0) then
            file_content = repeat(' ', current_size)
        end if

        open(newunit=unit, file=device, status='old', action='read', iostat=ios, encoding='UTF-8')
        if (ios /= 0) then
            call this%error%raise(FORGE_ERROR_FILE_NOT_FOUND, "Cannot reopen XML file: " // trim(device))
            return
        end if

        ! Read file content efficiently
        current_size = 0
        do
            read(unit, '(A)', iostat=ios) line
            if (ios /= 0) exit
            if (current_size + len_trim(line) + 1 > len(file_content)) then
                ! Expand content if needed
                file_content = file_content // repeat(' ', len_trim(line) + 1)
            end if
            file_content(current_size+1:current_size+len_trim(line)+1) = trim(line) // achar(10)
            current_size = current_size + len_trim(line) + 1
        end do

        close(unit)

        ! Trim to actual content and set data
        if (current_size > 0) then
            file_content = file_content(1:current_size)
            call this%set_data(file_content)
        else
            call this%error%raise(FORGE_ERROR_XML_PARSE, "Empty XML file")
        end if
    end subroutine xmlstreamreader_set_device

    function xmlstreamreader_read_next(this) result(success)
        class(QXmlStreamReader), intent(inout) :: this
        logical :: success

        success = .false.
        if (this%at_end .or. this%position > len(this%data)) then
            this%at_end = .true.
            this%current_token%token_type = NoToken
            return
        end if

        call this%skip_whitespace()

        if (this%position > len(this%data)) then
            this%at_end = .true.
            this%current_token%token_type = NoToken
            return
        end if

        select case (this%data(this%position:this%position))
        case ('<')
            if (this%position + 1 <= len(this%data)) then
                select case (this%data(this%position+1:this%position+1))
                case ('/')
                    success = this%parse_end_element()
                case ('?')
                    success = this%parse_processing_instruction()
                case ('!')
                    if (this%position + 3 <= len(this%data) .and. &
                        this%data(this%position+1:this%position+3) == '!--') then
                        success = this%parse_comment()
                    else if (this%position + 8 <= len(this%data) .and. &
                            this%data(this%position+1:this%position+8) == '!DOCTYPE') then
                        success = this%parse_dtd()
                    else
                        call this%error%raise(FORGE_ERROR_XML_PARSE, "Invalid XML declaration")
                        return
                    end if
                case default
                    success = this%parse_start_element()
                end select
            else
                call this%error%raise(FORGE_ERROR_XML_PARSE, "Unexpected end of data")
                return
            end if
        case default
            success = this%parse_characters()
        end select
    end function xmlstreamreader_read_next

    function xmlstreamreader_token_type(this) result(token_type)
        class(QXmlStreamReader), intent(in) :: this
        integer :: token_type
        token_type = this%current_token%token_type
    end function xmlstreamreader_token_type

    function xmlstreamreader_token_string(this) result(str)
        class(QXmlStreamReader), intent(in) :: this
        character(len=:), allocatable :: str

        select case (this%current_token%token_type)
        case (StartDocument)
            str = "StartDocument"
        case (EndDocument)
            str = "EndDocument"
        case (StartElement)
            str = "StartElement"
        case (EndElement)
            str = "EndElement"
        case (Characters)
            str = "Characters"
        case (Comment)
            str = "Comment"
        case (DTD)
            str = "DTD"
        case (EntityReference)
            str = "EntityReference"
        case (ProcessingInstruction)
            str = "ProcessingInstruction"
        case default
            str = "NoToken"
        end select
    end function xmlstreamreader_token_string

    function xmlstreamreader_name(this) result(name)
        class(QXmlStreamReader), intent(in) :: this
        character(len=:), allocatable :: name
        if (allocated(this%current_token%name)) then
            name = this%current_token%name
        else
            name = ""
        end if
    end function xmlstreamreader_name

    function xmlstreamreader_text(this) result(text)
        class(QXmlStreamReader), intent(in) :: this
        character(len=:), allocatable :: text
        if (allocated(this%current_token%text)) then
            text = this%current_token%text
        else
            text = ""
        end if
    end function xmlstreamreader_text

    function xmlstreamreader_attributes(this) result(attrs)
        class(QXmlStreamReader), intent(in) :: this
        type(QHashMap) :: attrs
        attrs = this%current_token%attributes
    end function xmlstreamreader_attributes

    function xmlstreamreader_is_whitespace(this) result(is_ws)
        class(QXmlStreamReader), intent(in) :: this
        logical :: is_ws
        is_ws = this%current_token%is_whitespace
    end function xmlstreamreader_is_whitespace

    function xmlstreamreader_is_cdata(this) result(is_cd)
        class(QXmlStreamReader), intent(in) :: this
        logical :: is_cd
        is_cd = this%current_token%is_cdata
    end function xmlstreamreader_is_cdata

    function xmlstreamreader_at_end(this) result(at_end)
        class(QXmlStreamReader), intent(in) :: this
        logical :: at_end
        at_end = this%at_end
    end function xmlstreamreader_at_end

    function xmlstreamreader_has_error(this) result(has_err)
        class(QXmlStreamReader), intent(in) :: this
        logical :: has_err
        has_err = this%error%status%is_error()
    end function xmlstreamreader_has_error

    function xmlstreamreader_error_string(this) result(err_str)
        class(QXmlStreamReader), intent(in) :: this
        character(len=:), allocatable :: err_str
        err_str = trim(this%error%status%message)
    end function xmlstreamreader_error_string

    function xmlstreamreader_line_number(this) result(line)
        class(QXmlStreamReader), intent(in) :: this
        integer :: line
        ! Simplified line counting
        line = 1
        if (this%position <= len(this%data)) then
            line = line + count(achar(10) == this%data(1:this%position-1))
        end if
    end function xmlstreamreader_line_number

    function xmlstreamreader_column_number(this) result(col)
        class(QXmlStreamReader), intent(in) :: this
        integer :: col
        integer :: last_nl
        last_nl = index(this%data(1:this%position-1), achar(10), back=.true.)
        if (last_nl > 0) then
            col = this%position - last_nl
        else
            col = this%position
        end if
    end function xmlstreamreader_column_number

    function xmlstreamreader_character_offset(this) result(offset)
        class(QXmlStreamReader), intent(in) :: this
        integer :: offset
        offset = this%position - 1
    end function xmlstreamreader_character_offset

    function xmlstreamreader_parse_start_element(this) result(success)
        class(QXmlStreamReader), intent(inout) :: this
        logical :: success
        character(len=:), allocatable :: attr_name, attr_value

        success = .false.
        this%position = this%position + 1  ! Skip '<'

        this%current_token%name = this%read_name()
        if (.not. allocated(this%current_token%name)) then
            call this%error%raise(FORGE_ERROR_XML_PARSE, "Expected element name")
            return
        end if

        this%current_token%token_type = StartElement

        ! Parse attributes
        call this%skip_whitespace()
        do while (this%position <= len(this%data) .and. this%data(this%position:this%position) /= '>' .and. &
                  this%data(this%position:this%position) /= '/')
            attr_name = this%read_attribute(attr_value)
            if (allocated(attr_name)) then
                call this%current_token%attributes%insert(attr_name, attr_value)
            end if
            call this%skip_whitespace()
        end do

        ! Check for self-closing tag
        if (this%position <= len(this%data) .and. this%data(this%position:this%position) == '/') then
            this%position = this%position + 1
            this%current_token%token_type = EndElement
        end if

        if (this%position <= len(this%data) .and. this%data(this%position:this%position) == '>') then
            this%position = this%position + 1
            success = .true.
        else
            call this%error%raise(FORGE_ERROR_XML_PARSE, "Expected '>'")
        end if
    end function xmlstreamreader_parse_start_element

    function xmlstreamreader_parse_end_element(this) result(success)
        class(QXmlStreamReader), intent(inout) :: this
        logical :: success

        success = .false.
        this%position = this%position + 2  ! Skip '</'

        this%current_token%name = this%read_name()
        this%current_token%token_type = EndElement

        call this%skip_whitespace()
        if (this%position <= len(this%data) .and. this%data(this%position:this%position) == '>') then
            this%position = this%position + 1
            success = .true.
        else
            call this%error%raise(FORGE_ERROR_XML_PARSE, "Expected '>' in end element")
        end if
    end function xmlstreamreader_parse_end_element

    function xmlstreamreader_parse_characters(this) result(success)
        class(QXmlStreamReader), intent(inout) :: this
        logical :: success
        integer :: start_pos

        success = .false.
        start_pos = this%position

        do while (this%position <= len(this%data) .and. this%data(this%position:this%position) /= '<')
            this%position = this%position + 1
        end do

        if (this%position > start_pos) then
            this%current_token%text = this%data(start_pos:this%position-1)
            this%current_token%token_type = Characters
            this%current_token%is_whitespace = verify(trim(this%current_token%text), ' ') == 0
            success = .true.
        end if
    end function xmlstreamreader_parse_characters

    function xmlstreamreader_parse_comment(this) result(success)
        class(QXmlStreamReader), intent(inout) :: this
        logical :: success
        integer :: start_pos

        success = .false.
        this%position = this%position + 4  ! Skip '<!--'
        start_pos = this%position

        do while (this%position + 2 <= len(this%data))
            if (this%data(this%position:this%position+2) == '-->') then
                this%current_token%text = this%data(start_pos:this%position-1)
                this%current_token%token_type = Comment
                this%position = this%position + 3
                success = .true.
                return
            end if
            this%position = this%position + 1
        end do

        call this%error%raise(FORGE_ERROR_XML_PARSE, "Unterminated comment")
    end function xmlstreamreader_parse_comment

    function xmlstreamreader_parse_processing_instruction(this) result(success)
        class(QXmlStreamReader), intent(inout) :: this
        logical :: success
        integer :: start_pos

        success = .false.
        this%position = this%position + 2  ! Skip '<?'
        start_pos = this%position

        do while (this%position + 1 <= len(this%data))
            if (this%data(this%position:this%position+1) == '?>') then
                this%current_token%text = this%data(start_pos:this%position-1)
                this%current_token%token_type = ProcessingInstruction
                this%position = this%position + 2
                success = .true.
                return
            end if
            this%position = this%position + 1
        end do

        call this%error%raise(FORGE_ERROR_XML_PARSE, "Unterminated processing instruction")
    end function xmlstreamreader_parse_processing_instruction

    function xmlstreamreader_parse_dtd(this) result(success)
        class(QXmlStreamReader), intent(inout) :: this
        logical :: success
        integer :: start_pos

        success = .false.
        this%position = this%position + 9  ! Skip '<!DOCTYPE'
        start_pos = this%position

        do while (this%position <= len(this%data))
            if (this%data(this%position:this%position) == '>') then
                this%current_token%text = this%data(start_pos:this%position-1)
                this%current_token%token_type = DTD
                this%position = this%position + 1
                success = .true.
                return
            end if
            this%position = this%position + 1
        end do

        call this%error%raise(FORGE_ERROR_XML_PARSE, "Unterminated DTD")
    end function xmlstreamreader_parse_dtd

    subroutine xmlstreamreader_skip_whitespace(this)
        class(QXmlStreamReader), intent(inout) :: this

        do while (this%position <= len(this%data))
            select case (this%data(this%position:this%position))
            case (' ', achar(9), achar(10), achar(13))  ! space, tab, LF, CR
                this%position = this%position + 1
            case default
                exit
            end select
        end do
    end subroutine xmlstreamreader_skip_whitespace

    function xmlstreamreader_read_name(this) result(name)
        class(QXmlStreamReader), intent(inout) :: this
        character(len=:), allocatable :: name
        integer :: start_pos

        start_pos = this%position
        do while (this%position <= len(this%data))
            select case (this%data(this%position:this%position))
            case ('a':'z', 'A':'Z', '0':'9', '_', '-', '.', ':')
                this%position = this%position + 1
            case default
                exit
            end select
        end do

        if (this%position > start_pos) then
            name = this%data(start_pos:this%position-1)
        end if
    end function xmlstreamreader_read_name

    function xmlstreamreader_read_attribute(this, value) result(name)
        class(QXmlStreamReader), intent(inout) :: this
        character(len=:), allocatable, intent(out) :: value
        character(len=:), allocatable :: name
        integer :: start_pos

        name = this%read_name()
        if (.not. allocated(name)) return

        call this%skip_whitespace()
        if (this%position > len(this%data) .or. this%data(this%position:this%position) /= '=') then
            deallocate(name)
            return
        end if
        this%position = this%position + 1

        call this%skip_whitespace()
        if (this%position > len(this%data) .or. this%data(this%position:this%position) /= '"') then
            deallocate(name)
            return
        end if
        this%position = this%position + 1

        start_pos = this%position
        do while (this%position <= len(this%data) .and. this%data(this%position:this%position) /= '"')
            this%position = this%position + 1
        end do

        if (this%position <= len(this%data)) then
            value = this%data(start_pos:this%position-1)
            this%position = this%position + 1
        else
            deallocate(name)
        end if
    end function xmlstreamreader_read_attribute

    ! ========== QXmlStreamWriter Implementation ==========

    subroutine xmlstreamwriter_set_auto_formatting(this, indent_size)
        class(QXmlStreamWriter), intent(inout) :: this
        integer, intent(in) :: indent_size
        this%auto_formatting = .true.
        this%indent_size = indent_size
    end subroutine xmlstreamwriter_set_auto_formatting

    subroutine xmlstreamwriter_set_auto_formatting_enabled(this, enabled)
        class(QXmlStreamWriter), intent(inout) :: this
        logical, intent(in) :: enabled
        this%auto_formatting = enabled
    end subroutine xmlstreamwriter_set_auto_formatting_enabled

    subroutine xmlstreamwriter_set_device(this, device)
        class(QXmlStreamWriter), intent(inout) :: this
        character(len=*), intent(in) :: device

        integer :: ios

        ! Close any existing file
        if (this%device_unit /= -1) then
            close(this%device_unit)
            this%device_unit = -1
        end if

        this%device_filename = device
        this%writing_to_file = .true.

        ! Open file for writing with UTF-8 encoding and error handling
        open(newunit=this%device_unit, file=device, status='replace', action='write', &
             iostat=ios, encoding='UTF-8', position='rewind')
        if (ios /= 0) then
            call this%error%raise(FORGE_ERROR_FILE_NOT_FOUND, "Cannot create XML output file: " // trim(device) // &
                                 " (IOSTAT: " // trim(ios) // ")")
            this%writing_to_file = .false.
            this%device_unit = -1
        end if
    end subroutine xmlstreamwriter_set_device

    subroutine xmlstreamwriter_write_start_document(this, version, encoding, standalone)
        class(QXmlStreamWriter), intent(inout) :: this
        character(len=*), intent(in), optional :: version, encoding, standalone

        call this%append('<?xml version="')
        if (present(version)) then
            call this%append(version)
        else
            call this%append('1.0')
        end if
        call this%append('"')

        if (present(encoding)) then
            call this%append(' encoding="')
            call this%append(encoding)
            call this%append('"')
        end if

        if (present(standalone)) then
            call this%append(' standalone="')
            call this%append(standalone)
            call this%append('"')
        end if

        call this%append('?>')
        if (this%auto_formatting) call this%append(achar(10))
    end subroutine xmlstreamwriter_write_start_document

    subroutine xmlstreamwriter_write_end_document(this)
        class(QXmlStreamWriter), intent(inout) :: this
        ! Nothing to do for end document
    end subroutine xmlstreamwriter_write_end_document

    subroutine xmlstreamwriter_write_start_element(this, qualified_name)
        class(QXmlStreamWriter), intent(inout) :: this
        character(len=*), intent(in) :: qualified_name

        call this%write_indent()
        call this%append('<')
        call this%append(qualified_name)
        call this%append('>')
        if (this%auto_formatting) then
            call this%append(achar(10))
            this%indent_level = this%indent_level + 1
        end if
    end subroutine xmlstreamwriter_write_start_element

    subroutine xmlstreamwriter_write_end_element(this)
        class(QXmlStreamWriter), intent(inout) :: this

        if (this%auto_formatting) then
            this%indent_level = this%indent_level - 1
            call this%write_indent()
        end if
        call this%append('</')
        ! Note: In a full implementation, we'd track element names
        call this%append('>')
        if (this%auto_formatting) call this%append(achar(10))
    end subroutine xmlstreamwriter_write_end_element

    subroutine xmlstreamwriter_write_empty_element(this, qualified_name)
        class(QXmlStreamWriter), intent(inout) :: this
        character(len=*), intent(in) :: qualified_name

        call this%write_indent()
        call this%append('<')
        call this%append(qualified_name)
        call this%append(' />')
        if (this%auto_formatting) call this%append(achar(10))
    end subroutine xmlstreamwriter_write_empty_element

    subroutine xmlstreamwriter_write_attribute(this, qualified_name, value)
        class(QXmlStreamWriter), intent(inout) :: this
        character(len=*), intent(in) :: qualified_name, value

        call this%append(' ')
        call this%append(qualified_name)
        call this%append('="')
        call this%append(this%escape(value))
        call this%append('"')
    end subroutine xmlstreamwriter_write_attribute

    subroutine xmlstreamwriter_write_namespace(this, namespace_uri, prefix)
        class(QXmlStreamWriter), intent(inout) :: this
        character(len=*), intent(in) :: namespace_uri
        character(len=*), intent(in), optional :: prefix

        call this%append(' xmlns')
        if (present(prefix)) then
            call this%append(':')
            call this%append(prefix)
        end if
        call this%append('="')
        call this%append(namespace_uri)
        call this%append('"')
    end subroutine xmlstreamwriter_write_namespace

    subroutine xmlstreamwriter_write_default_namespace(this, namespace_uri)
        class(QXmlStreamWriter), intent(inout) :: this
        character(len=*), intent(in) :: namespace_uri

        call this%append(' xmlns="')
        call this%append(namespace_uri)
        call this%append('"')
    end subroutine xmlstreamwriter_write_default_namespace

    subroutine xmlstreamwriter_write_characters(this, text)
        class(QXmlStreamWriter), intent(inout) :: this
        character(len=*), intent(in) :: text

        call this%write_indent()
        call this%append(this%escape(text))
        if (this%auto_formatting) call this%append(achar(10))
    end subroutine xmlstreamwriter_write_characters

    subroutine xmlstreamwriter_write_cdata(this, text)
        class(QXmlStreamWriter), intent(inout) :: this
        character(len=*), intent(in) :: text

        call this%write_indent()
        call this%append('<![CDATA[')
        call this%append(text)
        call this%append(']]>')
        if (this%auto_formatting) call this%append(achar(10))
    end subroutine xmlstreamwriter_write_cdata

    subroutine xmlstreamwriter_write_comment(this, text)
        class(QXmlStreamWriter), intent(inout) :: this
        character(len=*), intent(in) :: text

        call this%write_indent()
        call this%append('<!--')
        call this%append(text)
        call this%append('-->')
        if (this%auto_formatting) call this%append(achar(10))
    end subroutine xmlstreamwriter_write_comment

    subroutine xmlstreamwriter_write_processing_instruction(this, target, data)
        class(QXmlStreamWriter), intent(inout) :: this
        character(len=*), intent(in) :: target
        character(len=*), intent(in), optional :: data

        call this%write_indent()
        call this%append('<?')
        call this%append(target)
        if (present(data)) then
            call this%append(' ')
            call this%append(data)
        end if
        call this%append('?>')
        if (this%auto_formatting) call this%append(achar(10))
    end subroutine xmlstreamwriter_write_processing_instruction

    subroutine xmlstreamwriter_write_dtd(this, dtd)
        class(QXmlStreamWriter), intent(inout) :: this
        character(len=*), intent(in) :: dtd

        call this%write_indent()
        call this%append('<!DOCTYPE ')
        call this%append(dtd)
        call this%append('>')
        if (this%auto_formatting) call this%append(achar(10))
    end subroutine xmlstreamwriter_write_dtd

    subroutine xmlstreamwriter_write_entity_reference(this, name)
        class(QXmlStreamWriter), intent(inout) :: this
        character(len=*), intent(in) :: name

        call this%append('&')
        call this%append(name)
        call this%append(';')
    end subroutine xmlstreamwriter_write_entity_reference

    function xmlstreamwriter_get_buffer(this) result(buffer)
        class(QXmlStreamWriter), intent(in) :: this
        character(len=:), allocatable :: buffer
        buffer = this%buffer
    end function xmlstreamwriter_get_buffer

    function xmlstreamwriter_has_error(this) result(has_err)
        class(QXmlStreamWriter), intent(in) :: this
        logical :: has_err
        has_err = this%error%status%is_error()
    end function xmlstreamwriter_has_error

    function xmlstreamwriter_error_string(this) result(err_str)
        class(QXmlStreamWriter), intent(in) :: this
        character(len=:), allocatable :: err_str
        err_str = trim(this%error%status%message)
    end function xmlstreamwriter_error_string

    subroutine xmlstreamwriter_write_indent(this)
        class(QXmlStreamWriter), intent(inout) :: this
        integer :: i

        if (.not. this%auto_formatting) return

        do i = 1, this%indent_level * this%indent_size
            call this%append(' ')
        end do
    end subroutine xmlstreamwriter_write_indent

    subroutine xmlstreamwriter_append(this, text)
        class(QXmlStreamWriter), intent(inout) :: this
        character(len=*), intent(in) :: text

        if (this%writing_to_file .and. this%device_unit /= -1) then
            ! Write directly to file
            write(this%device_unit, '(A)', advance='no') text
        else
            ! Write to buffer
            if (allocated(this%buffer)) then
                this%buffer = this%buffer // text
            else
                this%buffer = text
            end if
        end if
    end subroutine xmlstreamwriter_append

    function xmlstreamwriter_escape(this, text) result(escaped)
        class(QXmlStreamWriter), intent(in) :: this
        character(len=*), intent(in) :: text
        character(len=:), allocatable :: escaped

        escaped = text
        escaped = replace_all(escaped, '&', '&')
        escaped = replace_all(escaped, '<', '<')
        escaped = replace_all(escaped, '>', '>')
        escaped = replace_all(escaped, '"', '"')
        escaped = replace_all(escaped, "'", ''')
    end function xmlstreamwriter_escape

end module forge_xml_stream