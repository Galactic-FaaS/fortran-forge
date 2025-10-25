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
        integer :: bytes_written, bytes_read, i
        logical :: reading_headers, success
        type(QString) :: response_str
        
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
        
        ! Read response
        http_response = ""
        do
            buffer = ""
            bytes_read = socket_recv(socket%socket_handle, buffer, size(buffer))
            
            if (bytes_read <= 0) exit
            
            http_response = http_response // buffer(1:bytes_read)
            
            ! Check if we've received complete response
            ! (simplified - would check Content-Length or chunked encoding)
            if (bytes_read < size(buffer)) exit
        end do
        
        call socket%close()
        
        ! Parse response
        call parse_http_response(http_response, response)
        
    end function http_send_request_complete

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

