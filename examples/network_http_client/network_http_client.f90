!> @brief HTTP Client Example
!> @details Demonstrates HTTP client functionality for making web requests
!> @author ForGE Contributors
!> @license GPL-3.0-or-later

program network_http_client_example
    use forge
    use forge_stub_backend
    implicit none

    type(forge_application) :: app
    type(forge_window_t) :: window
    type(forge_http_client) :: http_client
    type(forge_http_request) :: request
    type(forge_http_response) :: response
    type(forge_label) :: status_label, response_label
    type(forge_button) :: get_button, post_button, put_button, delete_button
    type(forge_entry) :: url_entry
    type(forge_text_view) :: response_view
    type(forge_status) :: status
    type(forge_stub_backend_t), target :: stub_backend

    print '(A)', "=== HTTP Client Example ==="
    print '(A)', ""
    print '(A)', "This example demonstrates:"
    print '(A)', "  - HTTP client creation and configuration"
    print '(A)', "  - Making GET, POST, PUT, DELETE requests"
    print '(A)', "  - Handling HTTP responses"
    print '(A)', "  - Setting request headers and timeouts"
    print '(A)', ""
    print '(A)', "NOTE: Using stub backend - HTTP requests won't actually execute"
    print '(A)', ""

    ! Initialize stub backend
    call stub_backend%init(status)
    if (status%is_error()) then
        print '(A)', "ERROR: Failed to initialize stub backend"
        call status%print()
        stop 1
    end if

    ! Create window
    block
        type(forge_window_builder) :: builder
        call builder%set_title("HTTP Client Example")
        call builder%set_size(700, 500)
        call builder%set_backend(stub_backend)
        window = builder%build(status)
    end block

    if (status%is_error()) then
        print '(A)', "ERROR: Failed to initialize window"
        call status%print()
        call stub_backend%shutdown()
        stop 1
    end if

    ! Create HTTP client
    print '(A)', "Creating HTTP client..."
    call http_client%set_timeout(30)  ! 30 second timeout
    call http_client%set_user_agent("ForGE HTTP Client Example/1.0")
    call http_client%set_follow_redirects(.true.)

    ! Create UI elements
    print '(A)', "Creating HTTP client interface..."

    ! URL entry
    call url_entry%set_placeholder_text("Enter URL (e.g., https://httpbin.org/get)")
    call url_entry%set_text("https://httpbin.org/get")
    call url_entry%set_name("url_entry")

    ! Request buttons
    call get_button%set_label("GET")
    call get_button%set_name("get_button")
    call get_button%on_click(on_get_clicked)

    call post_button%set_label("POST")
    call post_button%set_name("post_button")
    call post_button%on_click(on_post_clicked)

    call put_button%set_label("PUT")
    call put_button%set_name("put_button")
    call put_button%on_click(on_put_clicked)

    call delete_button%set_label("DELETE")
    call delete_button%set_name("delete_button")
    call delete_button%on_click(on_delete_clicked)

    ! Response display
    call response_view%set_name("response_view")
    call response_view%set_editable(.false.)
    call response_view%set_text("HTTP Response will appear here...")

    ! Status label
    call status_label%set_name("status_label")
    call status_label%set_text("Ready to make HTTP requests")

    ! Show window
    print '(A)', ""
    print '(A)', "Showing HTTP client interface..."
    call window%show()

    ! Run event loop (stub version just returns)
    call stub_backend%run()

    ! Cleanup
    print '(A)', ""
    print '(A)', "Cleaning up..."
    call window%close()
    call stub_backend%shutdown()

    print '(A)', ""
    print '(A)', "=== Example Complete ==="

contains

    !> @brief Make HTTP GET request
    subroutine make_get_request()
        character(len=:), allocatable :: url

        url = trim(url_entry%get_text())
        if (len_trim(url) == 0) then
            call status_label%set_text("Error: Please enter a URL")
            return
        end if

        print '(A,A)', "  Making GET request to: ", url

        ! Create GET request
        call request%set_url(url)
        call request%set_method("GET")
        call request%set_header("Accept", "application/json")

        ! Execute request (simulated)
        call simulate_http_request("GET", url)
    end subroutine make_get_request

    !> @brief Make HTTP POST request
    subroutine make_post_request()
        character(len=:), allocatable :: url, post_data

        url = trim(url_entry%get_text())
        if (len_trim(url) == 0) then
            call status_label%set_text("Error: Please enter a URL")
            return
        end if

        print '(A,A)', "  Making POST request to: ", url

        ! Create POST request with JSON data
        post_data = '{"name": "ForGE", "version": "1.0", "language": "Fortran"}'
        call request%set_url(url)
        call request%set_method("POST")
        call request%set_header("Content-Type", "application/json")
        call request%set_body(post_data)

        ! Execute request (simulated)
        call simulate_http_request("POST", url)
    end subroutine make_post_request

    !> @brief Make HTTP PUT request
    subroutine make_put_request()
        character(len=:), allocatable :: url, put_data

        url = trim(url_entry%get_text())
        if (len_trim(url) == 0) then
            call status_label%set_text("Error: Please enter a URL")
            return
        end if

        print '(A,A)', "  Making PUT request to: ", url

        ! Create PUT request
        put_data = '{"status": "updated", "timestamp": "2024-01-01T12:00:00Z"}'
        call request%set_url(url)
        call request%set_method("PUT")
        call request%set_header("Content-Type", "application/json")
        call request%set_body(put_data)

        ! Execute request (simulated)
        call simulate_http_request("PUT", url)
    end subroutine make_put_request

    !> @brief Make HTTP DELETE request
    subroutine make_delete_request()
        character(len=:), allocatable :: url

        url = trim(url_entry%get_text())
        if (len_trim(url) == 0) then
            call status_label%set_text("Error: Please enter a URL")
            return
        end if

        print '(A,A)', "  Making DELETE request to: ", url

        ! Create DELETE request
        call request%set_url(url)
        call request%set_method("DELETE")

        ! Execute request (simulated)
        call simulate_http_request("DELETE", url)
    end subroutine make_delete_request

    !> @brief Simulate HTTP request and response
    subroutine simulate_http_request(method, url)
        character(len=*), intent(in) :: method, url
        character(len=500) :: response_text

        ! Simulate network delay
        print '(A)', "  [Simulating network request...]"

        ! Create simulated response based on method
        select case (method)
        case ("GET")
            write(response_text, '(A)') &
                "HTTP/1.1 200 OK\n" // &
                "Content-Type: application/json\n" // &
                "Content-Length: 45\n\n" // &
                '{"message": "Hello from GET request!", "method": "GET"}'
        case ("POST")
            write(response_text, '(A)') &
                "HTTP/1.1 201 Created\n" // &
                "Content-Type: application/json\n" // &
                "Content-Length: 52\n\n" // &
                '{"message": "Resource created via POST", "method": "POST"}'
        case ("PUT")
            write(response_text, '(A)') &
                "HTTP/1.1 200 OK\n" // &
                "Content-Type: application/json\n" // &
                "Content-Length: 48\n\n" // &
                '{"message": "Resource updated via PUT", "method": "PUT"}'
        case ("DELETE")
            write(response_text, '(A)') &
                "HTTP/1.1 204 No Content\n" // &
                "Content-Length: 0\n\n"
        case default
            write(response_text, '(A)') &
                "HTTP/1.1 405 Method Not Allowed\n" // &
                "Content-Type: text/plain\n\n" // &
                "Method not supported"
        end select

        ! Display response
        call response_view%set_text(trim(response_text))

        ! Update status
        write(status_text, '(A,A,A)') method, " request completed for ", url
        call status_label%set_text(trim(status_text))

        print '(A,A,A)', "  → ", method, " request completed"
    end subroutine simulate_http_request

    !> @brief Handler for GET button click
    subroutine on_get_clicked(event)
        type(forge_event), intent(in) :: event
        call make_get_request()
    end subroutine on_get_clicked

    !> @brief Handler for POST button click
    subroutine on_post_clicked(event)
        type(forge_event), intent(in) :: event
        call make_post_request()
    end subroutine on_post_clicked

    !> @brief Handler for PUT button click
    subroutine on_put_clicked(event)
        type(forge_event), intent(in) :: event
        call make_put_request()
    end subroutine on_put_clicked

    !> @brief Handler for DELETE button click
    subroutine on_delete_clicked(event)
        type(forge_event), intent(in) :: event
        call make_delete_request()
    end subroutine on_delete_clicked

end program network_http_client_example