!> @brief Complete HTTP/1.1 protocol implementation
!> @details Full HTTP client with proper request/response handling
!> @author ForGE Contributors
!> @date 2025
!> @license GPL-3.0-or-later

module forge_http_protocol
    use forge_socket
    use forge_string_utils
    use forge_http
    use forge_json
    implicit none
    private

    public :: http_send_request_complete, parse_url

    !> URL components
    type, public :: url_parts
        character(len=:), allocatable :: scheme    ! http or https
        character(len=:), allocatable :: host
        integer :: port
        character(len=:), allocatable :: path
        character(len=:), allocatable :: query
    end type url_parts

contains

    !> @brief Parse URL into components
    function parse_url(url) result(parts)
        character(len=*), intent(in) :: url
        type(url_parts) :: parts
        type(QString) :: url_str
        integer :: pos, start_pos
        
        call url_str%set(url)
        
        ! Extract scheme
        pos = index(url, "://")
        if (pos > 0) then
            parts%scheme = url(1:pos-1)
            start_pos = pos + 3
        else
            parts%scheme = "http"
            start_pos = 1
        end if
        
        ! Determine default port
        if (parts%scheme == "https") then
            parts%port = 443
        else
            parts%port = 80
        end if
        
        ! Extract host and optional port
        pos = index(url(start_pos:), "/")
        if (pos > 0) then
            parts%host = url(start_pos:start_pos+pos-2)
            parts%path = url(start_pos+pos-1:)
        else
            parts%host = url(start_pos:)
            parts%path = "/"
        end if
        
        ! Check for port in host
        pos = index(parts%host, ":")
        if (pos > 0) then
            read(parts%host(pos+1:), *) parts%port
            parts%host = parts%host(1:pos-1)
        end if
        
        ! Extract query string
        pos = index(parts%path, "?")
        if (pos > 0) then
            parts%query = parts%path(pos+1:)
            parts%path = parts%path(1:pos-1)
        else
            parts%query = ""
        end if
    end function parse_url

    !> @brief Send complete HTTP request and receive response
    function http_send_request_complete(request) result(response)
        type(QHttpRequest), intent(in) :: request
        type(QHttpResponse) :: response
        type(QTcpSocket) :: socket
        type(url_parts) :: url
        character(len=:), allocatable :: http_request, http_response
        character(len=:), allocatable :: method_str, line
        character(len=16384) :: buffer
        integer :: bytes_written, bytes_read, i, content_length
        logical :: reading_headers, success, chunked
        type(QString) :: response_str
        integer :: total_bytes_read

        ! Parse URL
        url = parse_url(request%get_url())

        ! Connect to server
        call socket%connect_to_host(url%host, url%port)
        success = socket%wait_for_connected(5000)  ! 5 second timeout

        if (.not. success) then
            response%status_code = 0
            call response%status_text%set("Connection failed")
            return
        end if

        ! Build HTTP request
        select case (request%method)
        case (HTTP_GET)
            method_str = "GET"
        case (HTTP_POST)
            method_str = "POST"
        case (HTTP_PUT)
            method_str = "PUT"
        case (HTTP_DELETE)
            method_str = "DELETE"
        case (HTTP_PATCH)
            method_str = "PATCH"
        case default
            method_str = "GET"
        end select

        ! Request line
        http_request = method_str // " " // url%path
        if (len(url%query) > 0) then
            http_request = http_request // "?" // url%query
        end if
        http_request = http_request // " HTTP/1.1" // achar(13) // achar(10)

        ! Host header (required in HTTP/1.1)
        http_request = http_request // "Host: " // url%host // achar(13) // achar(10)

        ! User-Agent header
        http_request = http_request // "User-Agent: ForGE/1.0" // achar(13) // achar(10)

        ! Accept header
        http_request = http_request // "Accept: */*" // achar(13) // achar(10)

        ! Add user headers
        if (allocated(request%headers_keys)) then
            do i = 1, request%header_count
                http_request = http_request // request%headers_keys(i)%get() // ": " // &
                              request%headers_values(i)%get() // achar(13) // achar(10)
            end do
        end if

        ! Add Content-Length if body present
        if (allocated(request%body)) then
            http_request = http_request // "Content-Length: " // &
                          int_to_string(len(request%body)) // achar(13) // achar(10)
            ! Content-Type if not specified
            if (.not. has_header(request, "Content-Type")) then
                http_request = http_request // "Content-Type: application/x-www-form-urlencoded" // achar(13) // achar(10)
            end if
        end if

        ! Connection header
        http_request = http_request // "Connection: close" // achar(13) // achar(10)

        ! End headers
        http_request = http_request // achar(13) // achar(10)

        ! Add body if present
        if (allocated(request%body)) then
            http_request = http_request // request%body
        end if

        ! Send request
        bytes_written = socket%write_data(http_request)
        if (bytes_written <= 0) then
            response%status_code = 0
            call response%status_text%set("Send failed")
            call socket%close()
            return
        end if

        ! Read response headers first
        http_response = ""
        total_bytes_read = 0
        content_length = -1
        chunked = .false.

        ! Read until we get all headers
        do
            buffer = ""
            bytes_read = socket_recv(socket%socket_handle, buffer, size(buffer))

            if (bytes_read <= 0) then
                response%status_code = 0
                call response%status_text%set("Receive failed")
                call socket%close()
                return
            end if

            http_response = http_response // buffer(1:bytes_read)
            total_bytes_read = total_bytes_read + bytes_read

            ! Check if we have complete headers (double CRLF)
            if (index(http_response, achar(13) // achar(10) // achar(13) // achar(10)) > 0) exit
        end do

        ! Parse headers to determine content length or chunked encoding
        call parse_response_headers(http_response, content_length, chunked)

        ! Read body based on content length or chunked encoding
        if (chunked) then
            http_response = read_chunked_body(socket, http_response)
        else if (content_length > 0) then
            http_response = read_content_length_body(socket, http_response, content_length)
        else
            ! Read until connection closes
            http_response = read_until_close(socket, http_response)
        end if

        call socket%close()

        ! Parse response
        call parse_http_response(http_response, response)

    end function http_send_request_complete

    function has_header(request, header_name) result(has_it)
        type(QHttpRequest), intent(in) :: request
        character(len=*), intent(in) :: header_name
        logical :: has_it
        integer :: i

        has_it = .false.
        if (.not. allocated(request%headers_keys)) return

        do i = 1, request%header_count
            if (request%headers_keys(i)%equals(header_name)) then
                has_it = .true.
                return
            end if
        end do
    end function has_header

    subroutine parse_response_headers(response_text, content_length, chunked)
        character(len=*), intent(in) :: response_text
        integer, intent(out) :: content_length
        logical, intent(out) :: chunked
        integer :: pos, line_start, line_end
        character(len=:), allocatable :: line, header_name, header_value

        content_length = -1
        chunked = .false.

        pos = index(response_text, achar(13) // achar(10) // achar(13) // achar(10))
        if (pos == 0) return

        line_start = index(response_text, achar(13) // achar(10)) + 2

        do while (line_start < pos)
            line_end = index(response_text(line_start:), achar(13) // achar(10))
            if (line_end == 0) exit

            line = response_text(line_start:line_start+line_end-2)
            line_start = line_start + line_end + 1

            ! Parse header
            pos = index(line, ":")
            if (pos > 0) then
                header_name = trim(line(1:pos-1))
                header_value = trim(adjustl(line(pos+1:)))

                if (header_name == "Content-Length") then
                    read(header_value, *) content_length
                else if (header_name == "Transfer-Encoding" .and. header_value == "chunked") then
                    chunked = .true.
                end if
            end if
        end do
    end subroutine parse_response_headers

    function read_chunked_body(socket, initial_response) result(full_response)
        type(QTcpSocket), intent(inout) :: socket
        character(len=*), intent(in) :: initial_response
        character(len=:), allocatable :: full_response
        character(len=16384) :: buffer
        integer :: bytes_read, chunk_size, pos
        character(len=:), allocatable :: chunk_data

        full_response = initial_response

        do
            ! Read chunk size line
            buffer = ""
            bytes_read = socket_recv(socket%socket_handle, buffer, size(buffer))
            if (bytes_read <= 0) exit

            full_response = full_response // buffer(1:bytes_read)

            ! Parse chunk size (hex)
            pos = index(buffer(1:bytes_read), achar(13) // achar(10))
            if (pos > 0) then
                read(buffer(1:pos-1), '(Z)', iostat=pos) chunk_size
                if (chunk_size == 0) exit  ! Last chunk

                ! Read chunk data
                chunk_data = ""
                do while (len(chunk_data) < chunk_size + 2)  ! +2 for CRLF
                    buffer = ""
                    bytes_read = socket_recv(socket%socket_handle, buffer, size(buffer))
                    if (bytes_read <= 0) exit
                    chunk_data = chunk_data // buffer(1:bytes_read)
                end do

                full_response = full_response // chunk_data
            end if
        end do
    end function read_chunked_body

    function read_content_length_body(socket, initial_response, content_length) result(full_response)
        type(QTcpSocket), intent(inout) :: socket
        character(len=*), intent(in) :: initial_response
        integer, intent(in) :: content_length
        character(len=:), allocatable :: full_response
        character(len=16384) :: buffer
        integer :: bytes_read, body_start, remaining

        full_response = initial_response

        ! Find where body starts
        body_start = index(full_response, achar(13) // achar(10) // achar(13) // achar(10)) + 4
        remaining = content_length - (len(full_response) - body_start + 1)

        ! Read remaining body
        do while (remaining > 0)
            buffer = ""
            bytes_read = socket_recv(socket%socket_handle, buffer, min(size(buffer), remaining))
            if (bytes_read <= 0) exit

            full_response = full_response // buffer(1:bytes_read)
            remaining = remaining - bytes_read
        end do
    end function read_content_length_body

    function read_until_close(socket, initial_response) result(full_response)
        type(QTcpSocket), intent(inout) :: socket
        character(len=*), intent(in) :: initial_response
        character(len=:), allocatable :: full_response
        character(len=16384) :: buffer
        integer :: bytes_read

        full_response = initial_response

        ! Read until connection closes
        do
            buffer = ""
            bytes_read = socket_recv(socket%socket_handle, buffer, size(buffer))
            if (bytes_read <= 0) exit

            full_response = full_response // buffer(1:bytes_read)
        end do
    end function read_until_close

    !> @brief Parse HTTP response
    subroutine parse_http_response(http_response, response)
        character(len=*), intent(in) :: http_response
        type(QHttpResponse), intent(inout) :: response
        type(QString) :: response_str, line
        type(QString), allocatable :: lines(:)
        integer :: i, pos, status_line_end
        character(len=:), allocatable :: status_line, header_line
        character(len=:), allocatable :: key, value
        
        ! Find end of status line
        status_line_end = index(http_response, achar(13) // achar(10))
        if (status_line_end == 0) then
            response%status_code = 0
            call response%status_text%set("Invalid response")
            return
        end if
        
        status_line = http_response(1:status_line_end-1)
        
        ! Parse status line: HTTP/1.1 200 OK
        pos = index(status_line, " ")
        if (pos > 0) then
            status_line = status_line(pos+1:)  ! Skip version
            pos = index(status_line, " ")
            if (pos > 0) then
                read(status_line(1:pos-1), *) response%status_code
                call response%status_text%set(status_line(pos+1:))
            end if
        end if
        
        ! Split into lines
        call response_str%set(http_response)
        lines = response_str%split(achar(13) // achar(10))
        
        ! Parse headers (skip first line which is status)
        do i = 2, size(lines)
            header_line = lines(i)%get()
            
            ! Empty line marks end of headers
            if (len(trim(header_line)) == 0) then
                ! Rest is body
                if (i < size(lines)) then
                    ! Reconstruct body from remaining lines
                    allocate(character(len=0) :: response%body)
                    do while (i < size(lines))
                        i = i + 1
                        if (len(response%body) > 0) then
                            response%body = response%body // achar(13) // achar(10) // lines(i)%get()
                        else
                            response%body = lines(i)%get()
                        end if
                    end do
                end if
                exit
            end if
            
            ! Parse header: Key: Value
            pos = index(header_line, ":")
            if (pos > 0) then
                key = trim(header_line(1:pos-1))
                value = trim(adjustl(header_line(pos+1:)))
                
                ! Store header
                if (.not. allocated(response%headers_keys)) then
                    allocate(response%headers_keys(20))
                    allocate(response%headers_values(20))
                end if
                
                if (response%header_count < size(response%headers_keys)) then
                    response%header_count = response%header_count + 1
                    call response%headers_keys(response%header_count)%set(key)
                    call response%headers_values(response%header_count)%set(value)
                end if
            end if
        end do
    end subroutine parse_http_response

end module forge_http_protocol

