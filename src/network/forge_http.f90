!> @brief HTTP client
!> @details HTTP/HTTPS client for web requests
!> @author ForGE Contributors
!> @date 2025
!> @license GPL-3.0-or-later

module forge_http
    use iso_c_binding
    use forge_types
    use forge_errors
    use forge_signals
    use forge_string_utils
    use forge_socket
    use forge_json
    implicit none
    private

    public :: QHttpClient, QHttpRequest, QHttpResponse
    public :: HTTP_GET, HTTP_POST, HTTP_PUT, HTTP_DELETE, HTTP_PATCH

    !> HTTP methods
    integer, parameter :: HTTP_GET = 1
    integer, parameter :: HTTP_POST = 2
    integer, parameter :: HTTP_PUT = 3
    integer, parameter :: HTTP_DELETE = 4
    integer, parameter :: HTTP_PATCH = 5

    !> @brief HTTP request
    type :: QHttpRequest
        private
        integer :: method = HTTP_GET
        type(QString) :: url
        type(QString), allocatable :: headers_keys(:)
        type(QString), allocatable :: headers_values(:)
        integer :: header_count = 0
        character(len=:), allocatable :: body
    contains
        procedure :: set_url => httprequest_set_url
        procedure :: set_method => httprequest_set_method
        procedure :: add_header => httprequest_add_header
        procedure :: set_body => httprequest_set_body
        procedure :: get_url => httprequest_get_url
    end type QHttpRequest

    !> @brief HTTP response
    type :: QHttpResponse
        private
        integer :: status_code = 0
        type(QString) :: status_text
        type(QString), allocatable :: headers_keys(:)
        type(QString), allocatable :: headers_values(:)
        integer :: header_count = 0
        character(len=:), allocatable :: body
    contains
        procedure :: get_status_code => httpresponse_get_status
        procedure :: get_body => httpresponse_get_body
        procedure :: get_json => httpresponse_get_json
        procedure :: get_header => httpresponse_get_header
        procedure :: is_success => httpresponse_is_success
    end type QHttpResponse

    !> @brief HTTP client with SSL and timeout support
    type :: QHttpClient
        private
        type(QString) :: base_url
        integer :: timeout = 30000  ! milliseconds
        logical :: ssl_enabled = .false.
        character(len=256) :: ssl_cert_file = ""
        character(len=256) :: ssl_key_file = ""
        integer :: error_code = 0
        character(len=256) :: error_string = ""
        type(signal_void) :: finished
        type(signal_int) :: error_occurred
        type(signal_int) :: progress
        type(signal_void) :: ssl_errors
    contains
        procedure :: set_base_url => httpclient_set_base_url
        procedure :: set_timeout => httpclient_set_timeout
        procedure :: set_ssl_enabled => httpclient_set_ssl
        procedure :: set_ssl_cert => httpclient_set_ssl_cert
        procedure :: set_ssl_key => httpclient_set_ssl_key
        procedure :: get => httpclient_get
        procedure :: post => httpclient_post
        procedure :: put => httpclient_put
        procedure :: delete => httpclient_delete
        procedure :: patch => httpclient_patch
        procedure :: send_request => httpclient_send
        procedure :: get_error => httpclient_error
        procedure :: get_error_string => httpclient_error_string
        procedure :: abort => httpclient_abort
    end type QHttpClient

contains

    ! ========== QHttpRequest Implementation ==========

    subroutine httprequest_set_url(this, url)
        class(QHttpRequest), intent(inout) :: this
        character(len=*), intent(in) :: url
        call this%url%set(url)
    end subroutine httprequest_set_url

    subroutine httprequest_set_method(this, method)
        class(QHttpRequest), intent(inout) :: this
        integer, intent(in) :: method
        this%method = method
    end subroutine httprequest_set_method

    subroutine httprequest_add_header(this, key, value)
        class(QHttpRequest), intent(inout) :: this
        character(len=*), intent(in) :: key, value
        type(QString), allocatable :: temp_keys(:), temp_values(:)
        
        if (.not. allocated(this%headers_keys)) then
            allocate(this%headers_keys(10))
            allocate(this%headers_values(10))
        else if (this%header_count >= size(this%headers_keys)) then
            allocate(temp_keys(size(this%headers_keys) * 2))
            allocate(temp_values(size(this%headers_values) * 2))
            temp_keys(1:this%header_count) = this%headers_keys(1:this%header_count)
            temp_values(1:this%header_count) = this%headers_values(1:this%header_count)
            call move_alloc(temp_keys, this%headers_keys)
            call move_alloc(temp_values, this%headers_values)
        end if
        
        this%header_count = this%header_count + 1
        call this%headers_keys(this%header_count)%set(key)
        call this%headers_values(this%header_count)%set(value)
    end subroutine httprequest_add_header

    subroutine httprequest_set_body(this, body)
        class(QHttpRequest), intent(inout) :: this
        character(len=*), intent(in) :: body
        this%body = body
    end subroutine httprequest_set_body

    function httprequest_get_url(this) result(url)
        class(QHttpRequest), intent(in) :: this
        character(len=:), allocatable :: url
        url = this%url%get()
    end function httprequest_get_url

    ! ========== QHttpResponse Implementation ==========

    function httpresponse_get_status(this) result(status_code)
        class(QHttpResponse), intent(in) :: this
        integer :: status_code
        status_code = this%status_code
    end function httpresponse_get_status

    function httpresponse_get_body(this) result(body)
        class(QHttpResponse), intent(in) :: this
        character(len=:), allocatable :: body
        
        if (allocated(this%body)) then
            body = this%body
        else
            allocate(character(len=0) :: body)
        end if
    end function httpresponse_get_body

    function httpresponse_get_json(this) result(json_value)
        class(QHttpResponse), intent(in) :: this
        type(QJsonValue) :: json_value
        
        json_value = parse_json(this%get_body())
    end function httpresponse_get_json

    function httpresponse_get_header(this, key) result(value)
        class(QHttpResponse), intent(in) :: this
        character(len=*), intent(in) :: key
        character(len=:), allocatable :: value
        integer :: i
        
        do i = 1, this%header_count
            if (this%headers_keys(i)%equals(key)) then
                value = this%headers_values(i)%get()
                return
            end if
        end do
        
        allocate(character(len=0) :: value)
    end function httpresponse_get_header

    function httpresponse_is_success(this) result(success)
        class(QHttpResponse), intent(in) :: this
        logical :: success
        
        success = (this%status_code >= 200 .and. this%status_code < 300)
    end function httpresponse_is_success

    ! ========== QHttpClient Implementation ==========

    subroutine httpclient_set_base_url(this, url)
        class(QHttpClient), intent(inout) :: this
        character(len=*), intent(in) :: url
        call this%base_url%set(url)
    end subroutine httpclient_set_base_url

    subroutine httpclient_set_timeout(this, timeout)
        class(QHttpClient), intent(inout) :: this
        integer, intent(in) :: timeout
        this%timeout = timeout
    end subroutine httpclient_set_timeout

    function httpclient_get(this, url) result(response)
        class(QHttpClient), intent(inout) :: this
        character(len=*), intent(in) :: url
        type(QHttpResponse) :: response
        type(QHttpRequest) :: request
        
        call request%set_url(url)
        call request%set_method(HTTP_GET)
        response = this%send_request(request)
    end function httpclient_get

    function httpclient_post(this, url, body) result(response)
        class(QHttpClient), intent(inout) :: this
        character(len=*), intent(in) :: url, body
        type(QHttpResponse) :: response
        type(QHttpRequest) :: request
        
        call request%set_url(url)
        call request%set_method(HTTP_POST)
        call request%set_body(body)
        response = this%send_request(request)
    end function httpclient_post

    function httpclient_put(this, url, body) result(response)
        class(QHttpClient), intent(inout) :: this
        character(len=*), intent(in) :: url, body
        type(QHttpResponse) :: response
        type(QHttpRequest) :: request
        
        call request%set_url(url)
        call request%set_method(HTTP_PUT)
        call request%set_body(body)
        response = this%send_request(request)
    end function httpclient_put

    function httpclient_delete(this, url) result(response)
        class(QHttpClient), intent(inout) :: this
        character(len=*), intent(in) :: url
        type(QHttpResponse) :: response
        type(QHttpRequest) :: request

        call request%set_url(url)
        call request%set_method(HTTP_DELETE)
        response = this%send_request(request)
    end function httpclient_delete

    function httpclient_patch(this, url, body) result(response)
        class(QHttpClient), intent(inout) :: this
        character(len=*), intent(in) :: url, body
        type(QHttpResponse) :: response
        type(QHttpRequest) :: request

        call request%set_url(url)
        call request%set_method(HTTP_PATCH)
        call request%set_body(body)
        response = this%send_request(request)
    end function httpclient_patch

    subroutine httpclient_set_ssl(this, enabled)
        class(QHttpClient), intent(inout) :: this
        logical, intent(in) :: enabled
        this%ssl_enabled = enabled
    end subroutine httpclient_set_ssl

    subroutine httpclient_set_ssl_cert(this, cert_file)
        class(QHttpClient), intent(inout) :: this
        character(len=*), intent(in) :: cert_file
        this%ssl_cert_file = cert_file
    end subroutine httpclient_set_ssl_cert

    subroutine httpclient_set_ssl_key(this, key_file)
        class(QHttpClient), intent(inout) :: this
        character(len=*), intent(in) :: key_file
        this%ssl_key_file = key_file
    end subroutine httpclient_set_ssl_key

    function httpclient_error(this) result(error_code)
        class(QHttpClient), intent(in) :: this
        integer :: error_code
        error_code = this%error_code
    end function httpclient_error

    function httpclient_error_string(this) result(error_str)
        class(QHttpClient), intent(in) :: this
        character(len=:), allocatable :: error_str
        error_str = trim(this%error_string)
    end function httpclient_error_string

    subroutine httpclient_abort(this)
        class(QHttpClient), intent(inout) :: this
        ! In full implementation, would cancel ongoing requests
        this%error_code = -11
        this%error_string = "Request aborted"
        call this%error_occurred%emit(this%error_code)
    subroutine httpclient_set_ssl_configuration(this, config)
        class(QHttpClient), intent(inout) :: this
        type(QSslConfiguration), intent(in) :: config
        this%ssl_config = config
        this%ssl_enabled = .true.
    end subroutine httpclient_set_ssl_configuration

    function httpclient_ssl_configuration(this) result(config)
        class(QHttpClient), intent(in) :: this
        type(QSslConfiguration) :: config
        config = this%ssl_config
    end function httpclient_ssl_configuration
    end subroutine httpclient_abort

    function httpclient_send(this, request) result(response)
        use forge_http_protocol
        class(QHttpClient), intent(inout) :: this
        type(QHttpRequest), intent(in) :: request
        type(QHttpResponse) :: response
        type(QHttpRequest) :: full_request
        type(QString) :: full_url
        
        ! Build full URL if using base_url
        if (.not. this%base_url%is_empty()) then
            full_url = this%base_url
            call full_url%append(request%get_url())
            full_request = request
            call full_request%set_url(full_url%get())
        else
            full_request = request
        end if
        
        ! Send request using complete HTTP protocol implementation
        response = http_send_request_complete(full_request)
        
        call this%finished%emit()
    end function httpclient_send

end module forge_http

