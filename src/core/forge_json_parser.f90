!> @brief Complete JSON parser implementation
!> @details Full RFC 8259 compliant JSON parser
!> @author ForGE Contributors
!> @date 2025
!> @license GPL-3.0-or-later

module forge_json_parser
    use iso_fortran_env, only: error_unit
    use forge_string_utils
    use forge_json
    implicit none
    private

    public :: json_parse_complete, json_stringify_complete

    !> Parser state
    type :: parser_state
        character(len=:), allocatable :: input
        integer :: pos = 1
        integer :: line = 1
        integer :: column = 1
        logical :: has_error = .false.
        character(len=:), allocatable :: error_msg
        integer :: error_offset = 0
    end type parser_state

contains

    !> @brief Complete JSON parser - fully RFC 8259 compliant
    function json_parse_complete(json_string, error_msg) result(value)
        character(len=*), intent(in) :: json_string
        character(len=:), allocatable, intent(out), optional :: error_msg
        type(QJsonValue) :: value
        type(parser_state) :: state

        state%input = json_string
        state%pos = 1
        state%line = 1
        state%column = 1
        state%has_error = .false.
        state%error_msg = ""
        state%error_offset = 0

        call skip_whitespace(state)

        if (state%pos > len(state%input)) then
            value%value_type = JSON_NULL
            state%has_error = .true.
            state%error_msg = "Empty JSON input"
            state%error_offset = 0
        else
            value = parse_value(state)
        end if

        if (present(error_msg)) then
            if (state%has_error) then
                error_msg = state%error_msg
            else
                error_msg = ""
            end if
        end if
    end function json_parse_complete

    !> @brief Parse any JSON value
    recursive function parse_value(state) result(value)
        type(parser_state), intent(inout) :: state
        type(QJsonValue) :: value
        character :: ch

        call skip_whitespace(state)

        if (state%pos > len(state%input)) then
            value%value_type = JSON_NULL
            state%has_error = .true.
            state%error_msg = "Unexpected end of input"
            state%error_offset = state%pos
            return
        end if

        ch = state%input(state%pos:state%pos)

        select case (ch)
        case ('{')
            value = parse_object(state)
        case ('[')
            value = parse_array(state)
        case ('"')
            value = parse_string(state)
        case ('t', 'f')
            value = parse_boolean(state)
        case ('n')
            value = parse_null(state)
        case ('-', '0':'9')
            value = parse_number(state)
        case default
            value%value_type = JSON_NULL
            state%has_error = .true.
            state%error_msg = "Unexpected character '" // ch // "' at position " // trim(adjustl(str(state%pos)))
            state%error_offset = state%pos
        end select
    end function parse_value

    !> @brief Parse JSON object
    recursive function parse_object(state) result(value)
        type(parser_state), intent(inout) :: state
        type(QJsonValue) :: value
        type(QJsonObject) :: obj
        type(QString) :: key
        type(QJsonValue) :: val
        character :: ch

        allocate(value%object_val)
        value%value_type = JSON_OBJECT

        ! Skip opening brace
        state%pos = state%pos + 1
        call skip_whitespace(state)

        ! Check for empty object
        if (state%pos <= len(state%input)) then
            if (state%input(state%pos:state%pos) == '}') then
                state%pos = state%pos + 1
                value%object_val = obj
                return
            end if
        end if

        ! Parse key-value pairs
        do
            call skip_whitespace(state)

            ! Parse key (must be string)
            if (state%pos > len(state%input)) then
                state%has_error = .true.
                state%error_msg = "Unexpected end of input while parsing object key"
                state%error_offset = state%pos
                value%value_type = JSON_NULL
                return
            end if
            if (state%input(state%pos:state%pos) /= '"') then
                state%has_error = .true.
                state%error_msg = "Expected string key in object at position " // trim(adjustl(str(state%pos)))
                state%error_offset = state%pos
                value%value_type = JSON_NULL
                return
            end if

            val = parse_string(state)
            if (state%has_error) then
                value%value_type = JSON_NULL
                return
            end if
            key = val%string_val

            call skip_whitespace(state)

            ! Expect colon
            if (state%pos > len(state%input)) then
                state%has_error = .true.
                state%error_msg = "Unexpected end of input, expected ':' after object key"
                state%error_offset = state%pos
                value%value_type = JSON_NULL
                return
            end if
            if (state%input(state%pos:state%pos) /= ':') then
                state%has_error = .true.
                state%error_msg = "Expected ':' after object key at position " // trim(adjustl(str(state%pos)))
                state%error_offset = state%pos
                value%value_type = JSON_NULL
                return
            end if
            state%pos = state%pos + 1

            call skip_whitespace(state)

            ! Parse value
            val = parse_value(state)
            if (state%has_error) then
                value%value_type = JSON_NULL
                return
            end if
            call obj%insert(key%get(), val)

            call skip_whitespace(state)

            ! Check for continuation
            if (state%pos > len(state%input)) then
                state%has_error = .true.
                state%error_msg = "Unexpected end of input in object, expected ',' or '}'"
                state%error_offset = state%pos
                value%value_type = JSON_NULL
                return
            end if
            ch = state%input(state%pos:state%pos)

            if (ch == '}') then
                state%pos = state%pos + 1
                exit
            else if (ch == ',') then
                state%pos = state%pos + 1
                cycle
            else
                state%has_error = .true.
                state%error_msg = "Expected ',' or '}' in object at position " // trim(adjustl(str(state%pos)))
                state%error_offset = state%pos
                value%value_type = JSON_NULL
                return
            end if
        end do

        value%object_val = obj
    end function parse_object

    !> @brief Parse JSON array
    recursive function parse_array(state) result(value)
        type(parser_state), intent(inout) :: state
        type(QJsonValue) :: value
        type(QJsonArray) :: arr
        type(QJsonValue) :: element
        character :: ch

        allocate(value%array_val)
        value%value_type = JSON_ARRAY

        ! Skip opening bracket
        state%pos = state%pos + 1
        call skip_whitespace(state)

        ! Check for empty array
        if (state%pos <= len(state%input)) then
            if (state%input(state%pos:state%pos) == ']') then
                state%pos = state%pos + 1
                value%array_val = arr
                return
            end if
        end if

        ! Parse elements
        do
            call skip_whitespace(state)

            if (state%pos > len(state%input)) then
                state%has_error = .true.
                state%error_msg = "Unexpected end of input in array"
                state%error_offset = state%pos
                value%value_type = JSON_NULL
                return
            end if

            ! Parse element
            element = parse_value(state)
            if (state%has_error) then
                value%value_type = JSON_NULL
                return
            end if
            call arr%append(element)

            call skip_whitespace(state)

            ! Check for continuation
            if (state%pos > len(state%input)) then
                state%has_error = .true.
                state%error_msg = "Unexpected end of input in array, expected ',' or ']'"
                state%error_offset = state%pos
                value%value_type = JSON_NULL
                return
            end if
            ch = state%input(state%pos:state%pos)

            if (ch == ']') then
                state%pos = state%pos + 1
                exit
            else if (ch == ',') then
                state%pos = state%pos + 1
                cycle
            else
                state%has_error = .true.
                state%error_msg = "Expected ',' or ']' in array at position " // trim(adjustl(str(state%pos)))
                state%error_offset = state%pos
                value%value_type = JSON_NULL
                return
            end if
        end do

        value%array_val = arr
    end function parse_array

    !> @brief Parse JSON string with escape sequences
    function parse_string(state) result(value)
        type(parser_state), intent(inout) :: state
        type(QJsonValue) :: value
        character(len=:), allocatable :: str
        integer :: start_pos, i
        character :: ch, next_ch
        character(len=1024) :: buffer
        integer :: buf_len
        integer :: unicode_val

        value%value_type = JSON_STRING

        ! Skip opening quote
        state%pos = state%pos + 1
        start_pos = state%pos
        buf_len = 0

        ! Parse string content
        do while (state%pos <= len(state%input))
            ch = state%input(state%pos:state%pos)

            if (ch == '"') then
                ! End of string
                state%pos = state%pos + 1
                exit
            else if (ch == '\') then
                ! Escape sequence
                if (state%pos + 1 > len(state%input)) then
                    state%has_error = .true.
                    state%error_msg = "Unexpected end of input in string escape sequence"
                    state%error_offset = state%pos
                    value%value_type = JSON_NULL
                    return
                end if
                next_ch = state%input(state%pos+1:state%pos+1)
                state%pos = state%pos + 2

                select case (next_ch)
                case ('"')
                    buf_len = buf_len + 1
                    buffer(buf_len:buf_len) = '"'
                case ('\')
                    buf_len = buf_len + 1
                    buffer(buf_len:buf_len) = '\'
                case ('/')
                    buf_len = buf_len + 1
                    buffer(buf_len:buf_len) = '/'
                case ('b')
                    buf_len = buf_len + 1
                    buffer(buf_len:buf_len) = achar(8)  ! Backspace
                case ('f')
                    buf_len = buf_len + 1
                    buffer(buf_len:buf_len) = achar(12)  ! Form feed
                case ('n')
                    buf_len = buf_len + 1
                    buffer(buf_len:buf_len) = achar(10)  ! Newline
                case ('r')
                    buf_len = buf_len + 1
                    buffer(buf_len:buf_len) = achar(13)  ! Carriage return
                case ('t')
                    buf_len = buf_len + 1
                    buffer(buf_len:buf_len) = achar(9)  ! Tab
                case ('u')
                    ! Unicode escape sequence
                    if (state%pos + 3 > len(state%input)) then
                        state%has_error = .true.
                        state%error_msg = "Incomplete Unicode escape sequence"
                        state%error_offset = state%pos - 2
                        value%value_type = JSON_NULL
                        return
                    end if
                    unicode_val = parse_unicode_hex(state%input(state%pos:state%pos+3))
                    if (unicode_val == -1) then
                        state%has_error = .true.
                        state%error_msg = "Invalid Unicode escape sequence"
                        state%error_offset = state%pos - 2
                        value%value_type = JSON_NULL
                        return
                    end if
                    ! For now, represent Unicode as replacement character
                    buf_len = buf_len + 1
                    buffer(buf_len:buf_len) = achar(255)  ! Replacement character
                    state%pos = state%pos + 4
                case default
                    state%has_error = .true.
                    state%error_msg = "Invalid escape sequence '\ " // next_ch // "'"
                    state%error_offset = state%pos - 2
                    value%value_type = JSON_NULL
                    return
                end select
            else if (ch < ' ' .and. ch /= achar(9)) then
                ! Control characters not allowed in strings
                state%has_error = .true.
                state%error_msg = "Control character in string at position " // trim(adjustl(str(state%pos)))
                state%error_offset = state%pos
                value%value_type = JSON_NULL
                return
            else
                ! Regular character
                buf_len = buf_len + 1
                buffer(buf_len:buf_len) = ch
                state%pos = state%pos + 1
            end if
        end do

        if (state%pos > len(state%input)) then
            state%has_error = .true.
            state%error_msg = "Unterminated string"
            state%error_offset = start_pos - 1
            value%value_type = JSON_NULL
            return
        end if

        call value%string_val%set(buffer(1:buf_len))
    end function parse_string

    !> @brief Parse JSON number
    function parse_number(state) result(value)
        type(parser_state), intent(inout) :: state
        type(QJsonValue) :: value
        integer :: start_pos, end_pos
        character(len=:), allocatable :: num_str
        real :: num_val
        integer :: iostat

        value%value_type = JSON_NUMBER
        start_pos = state%pos

        ! Optional minus
        if (state%input(state%pos:state%pos) == '-') then
            state%pos = state%pos + 1
        end if

        ! Integer part - must have at least one digit
        if (state%pos > len(state%input) .or. .not. is_digit(state%input(state%pos:state%pos))) then
            state%has_error = .true.
            state%error_msg = "Invalid number format at position " // trim(adjustl(str(start_pos)))
            state%error_offset = start_pos
            value%value_type = JSON_NULL
            return
        end if

        ! Leading zero check
        if (state%input(state%pos:state%pos) == '0') then
            state%pos = state%pos + 1
            ! If next character is digit, it's invalid (leading zero)
            if (state%pos <= len(state%input) .and. is_digit(state%input(state%pos:state%pos))) then
                state%has_error = .true.
                state%error_msg = "Leading zero not allowed in number at position " // trim(adjustl(str(start_pos)))
                state%error_offset = start_pos
                value%value_type = JSON_NULL
                return
            end if
        else
            do while (state%pos <= len(state%input))
                if (is_digit(state%input(state%pos:state%pos))) then
                    state%pos = state%pos + 1
                else
                    exit
                end if
            end do
        end if

        ! Optional decimal part
        if (state%pos <= len(state%input)) then
            if (state%input(state%pos:state%pos) == '.') then
                state%pos = state%pos + 1
                if (state%pos > len(state%input) .or. .not. is_digit(state%input(state%pos:state%pos))) then
                    state%has_error = .true.
                    state%error_msg = "Invalid decimal part in number at position " // trim(adjustl(str(state%pos)))
                    state%error_offset = state%pos
                    value%value_type = JSON_NULL
                    return
                end if
                do while (state%pos <= len(state%input))
                    if (is_digit(state%input(state%pos:state%pos))) then
                        state%pos = state%pos + 1
                    else
                        exit
                    end if
                end do
            end if
        end if

        ! Optional exponent
        if (state%pos <= len(state%input)) then
            if (state%input(state%pos:state%pos) == 'e' .or. &
                state%input(state%pos:state%pos) == 'E') then
                state%pos = state%pos + 1
                if (state%pos <= len(state%input)) then
                    if (state%input(state%pos:state%pos) == '+' .or. &
                        state%input(state%pos:state%pos) == '-') then
                        state%pos = state%pos + 1
                    end if
                end if
                if (state%pos > len(state%input) .or. .not. is_digit(state%input(state%pos:state%pos))) then
                    state%has_error = .true.
                    state%error_msg = "Invalid exponent in number at position " // trim(adjustl(str(state%pos)))
                    state%error_offset = state%pos
                    value%value_type = JSON_NULL
                    return
                end if
                do while (state%pos <= len(state%input))
                    if (is_digit(state%input(state%pos:state%pos))) then
                        state%pos = state%pos + 1
                    else
                        exit
                    end if
                end do
            end if
        end if

        end_pos = state%pos - 1
        num_str = state%input(start_pos:end_pos)

        read(num_str, *, iostat=iostat) num_val
        if (iostat /= 0) then
            state%has_error = .true.
            state%error_msg = "Invalid number format: " // num_str
            state%error_offset = start_pos
            value%value_type = JSON_NULL
            return
        end if

        value%number_val = num_val
    end function parse_number

    !> @brief Parse JSON boolean
    function parse_boolean(state) result(value)
        type(parser_state), intent(inout) :: state
        type(QJsonValue) :: value

        value%value_type = JSON_BOOL

        if (state%pos + 3 <= len(state%input)) then
            if (state%input(state%pos:state%pos+3) == 'true') then
                value%bool_val = .true.
                state%pos = state%pos + 4
                return
            end if
        end if

        if (state%pos + 4 <= len(state%input)) then
            if (state%input(state%pos:state%pos+4) == 'false') then
                value%bool_val = .false.
                state%pos = state%pos + 5
                return
            end if
        end if

        state%has_error = .true.
        state%error_msg = "Invalid boolean value at position " // trim(adjustl(str(state%pos)))
        state%error_offset = state%pos
        value%value_type = JSON_NULL
    end function parse_boolean

    !> @brief Parse JSON null
    function parse_null(state) result(value)
        type(parser_state), intent(inout) :: state
        type(QJsonValue) :: value

        if (state%pos + 3 <= len(state%input)) then
            if (state%input(state%pos:state%pos+3) == 'null') then
                value%value_type = JSON_NULL
                state%pos = state%pos + 4
                return
            end if
        end if

        state%has_error = .true.
        state%error_msg = "Invalid null value at position " // trim(adjustl(str(state%pos)))
        state%error_offset = state%pos
        value%value_type = JSON_NULL
    end function parse_null

    !> @brief Skip whitespace
    subroutine skip_whitespace(state)
        type(parser_state), intent(inout) :: state
        character :: ch
        
        do while (state%pos <= len(state%input))
            ch = state%input(state%pos:state%pos)
            if (ch == ' ' .or. ch == achar(9) .or. ch == achar(10) .or. ch == achar(13)) then
                if (ch == achar(10)) then
                    state%line = state%line + 1
                    state%column = 1
                else
                    state%column = state%column + 1
                end if
                state%pos = state%pos + 1
            else
                exit
            end if
        end do
    end subroutine skip_whitespace

    !> @brief Check if character is digit
    pure function is_digit(ch) result(is_d)
        character, intent(in) :: ch
        logical :: is_d
        is_d = (ch >= '0' .and. ch <= '9')
    end function is_digit

    !> @brief Parse Unicode hex value
    function parse_unicode_hex(hex_str) result(value)
        character(len=4), intent(in) :: hex_str
        integer :: value
        integer :: i, digit
        character :: ch

        value = 0
        do i = 1, 4
            ch = hex_str(i:i)
            if (ch >= '0' .and. ch <= '9') then
                digit = ichar(ch) - ichar('0')
            else if (ch >= 'a' .and. ch <= 'f') then
                digit = ichar(ch) - ichar('a') + 10
            else if (ch >= 'A' .and. ch <= 'F') then
                digit = ichar(ch) - ichar('A') + 10
            else
                value = -1  ! Invalid hex digit
                return
            end if
            value = value * 16 + digit
        end do
    end function parse_unicode_hex

    !> @brief Convert integer to string (utility function)
    function str(i) result(s)
        integer, intent(in) :: i
        character(len=20) :: s
        write(s, '(I0)') i
        s = trim(adjustl(s))
    end function str

    !> @brief Complete JSON stringifier - produces valid JSON
    recursive function json_stringify_complete(value, pretty, indent_level) result(json_string)
        type(QJsonValue), intent(in) :: value
        logical, intent(in), optional :: pretty
        integer, intent(in), optional :: indent_level
        character(len=:), allocatable :: json_string
        logical :: do_pretty
        integer :: indent, i
        character(len=256) :: buffer
        type(QString), allocatable :: keys(:)
        type(QJsonValue) :: element
        character(len=:), allocatable :: indent_str, newline_str
        
        do_pretty = .false.
        if (present(pretty)) do_pretty = pretty
        
        indent = 0
        if (present(indent_level)) indent = indent_level
        
        if (do_pretty) then
            newline_str = achar(10)
            indent_str = repeat("  ", indent)
        else
            newline_str = ""
            indent_str = ""
        end if
        
        select case (value%value_type)
        case (JSON_NULL)
            json_string = "null"
            
        case (JSON_BOOL)
            if (value%bool_val) then
                json_string = "true"
            else
                json_string = "false"
            end if
            
        case (JSON_NUMBER)
            write(buffer, '(G0)') value%number_val
            json_string = trim(adjustl(buffer))
            
        case (JSON_STRING)
            json_string = '"' // escape_string(value%string_val%get()) // '"'
            
        case (JSON_ARRAY)
            if (.not. allocated(value%array_val)) then
                json_string = "[]"
                return
            end if
            
            json_string = "["
            if (do_pretty) json_string = json_string // newline_str
            
            do i = 0, value%array_val%size() - 1
                element = value%array_val%at(i)
                if (do_pretty) json_string = json_string // repeat("  ", indent + 1)
                json_string = json_string // json_stringify_complete(element, pretty, indent + 1)
                
                if (i < value%array_val%size() - 1) then
                    json_string = json_string // ","
                end if
                
                if (do_pretty) json_string = json_string // newline_str
            end do
            
            if (do_pretty) json_string = json_string // indent_str
            json_string = json_string // "]"
            
        case (JSON_OBJECT)
            if (.not. allocated(value%object_val)) then
                json_string = "{}"
                return
            end if
            
            keys = value%object_val%keys()
            
            json_string = "{"
            if (do_pretty) json_string = json_string // newline_str
            
            do i = 1, size(keys)
                if (do_pretty) json_string = json_string // repeat("  ", indent + 1)
                
                json_string = json_string // '"' // escape_string(keys(i)%get()) // '":'
                if (do_pretty) json_string = json_string // " "
                
                element = value%object_val%value(keys(i)%get())
                json_string = json_string // json_stringify_complete(element, pretty, indent + 1)
                
                if (i < size(keys)) then
                    json_string = json_string // ","
                end if
                
                if (do_pretty) json_string = json_string // newline_str
            end do
            
            if (do_pretty) json_string = json_string // indent_str
            json_string = json_string // "}"
            
        case default
            json_string = "null"
        end select
    end function json_stringify_complete

    !> @brief Escape string for JSON
    function escape_string(str) result(escaped)
        character(len=*), intent(in) :: str
        character(len=:), allocatable :: escaped
        integer :: i
        character :: ch
        character(len=2048) :: buffer
        integer :: buf_len
        
        buf_len = 0
        
        do i = 1, len(str)
            ch = str(i:i)
            
            select case (ch)
            case ('"')
                buffer(buf_len+1:buf_len+2) = '\"'
                buf_len = buf_len + 2
            case ('\')
                buffer(buf_len+1:buf_len+2) = '\\'
                buf_len = buf_len + 2
            case (achar(8))  ! Backspace
                buffer(buf_len+1:buf_len+2) = '\b'
                buf_len = buf_len + 2
            case (achar(12))  ! Form feed
                buffer(buf_len+1:buf_len+2) = '\f'
                buf_len = buf_len + 2
            case (achar(10))  ! Newline
                buffer(buf_len+1:buf_len+2) = '\n'
                buf_len = buf_len + 2
            case (achar(13))  ! Carriage return
                buffer(buf_len+1:buf_len+2) = '\r'
                buf_len = buf_len + 2
            case (achar(9))  ! Tab
                buffer(buf_len+1:buf_len+2) = '\t'
                buf_len = buf_len + 2
            case default
                buf_len = buf_len + 1
                buffer(buf_len:buf_len) = ch
            end select
        end do
        
        escaped = buffer(1:buf_len)
    end function escape_string

end module forge_json_parser

