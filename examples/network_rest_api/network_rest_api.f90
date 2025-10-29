!> @brief REST API Client Example
!> @details Demonstrates REST API client functionality with JSON handling
!> @author ForGE Contributors
!> @license GPL-3.0-or-later

program network_rest_api_example
    use forge
    use forge_stub_backend
    implicit none

    type(forge_application) :: app
    type(forge_window_t) :: window
    type(forge_http_client) :: http_client
    type(forge_json_document) :: json_doc
    type(forge_label) :: status_label, api_label
    type(forge_button) :: get_users_button, create_user_button, update_user_button, delete_user_button
    type(forge_entry) :: api_url_entry, user_id_entry
    type(forge_text_view) :: response_view, json_view
    type(forge_status) :: status
    type(forge_stub_backend_t), target :: stub_backend

    print '(A)', "=== REST API Client Example ==="
    print '(A)', ""
    print '(A)', "This example demonstrates:"
    print '(A)', "  - REST API client operations (CRUD)"
    print '(A)', "  - JSON request/response handling"
    print '(A)', "  - HTTP methods (GET, POST, PUT, DELETE)"
    print '(A)', "  - API endpoint management"
    print '(A)', ""
    print '(A)', "NOTE: Using stub backend - API calls won't actually execute"
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
        call builder%set_title("REST API Client Example")
        call builder%set_size(700, 600)
        call builder%set_backend(stub_backend)
        window = builder%build(status)
    end block

    if (status%is_error()) then
        print '(A)', "ERROR: Failed to initialize window"
        call status%print()
        call stub_backend%shutdown()
        stop 1
    end if

    ! Create HTTP client for REST API
    print '(A)', "Creating REST API client..."
    call http_client%set_timeout(30)
    call http_client%set_user_agent("ForGE REST API Client/1.0")
    call http_client%set_header("Accept", "application/json")
    call http_client%set_header("Content-Type", "application/json")

    ! Create UI elements
    print '(A)', "Creating REST API client interface..."

    ! API configuration
    call api_url_entry%set_placeholder_text("API base URL (e.g., https://jsonplaceholder.typicode.com)")
    call api_url_entry%set_text("https://jsonplaceholder.typicode.com")
    call api_url_entry%set_name("api_url_entry")

    call user_id_entry%set_placeholder_text("User ID for operations")
    call user_id_entry%set_text("1")
    call user_id_entry%set_name("user_id_entry")

    ! API operation buttons
    call get_users_button%set_label("GET Users")
    call get_users_button%set_name("get_users_button")
    call get_users_button%on_click(on_get_users_clicked)

    call create_user_button%set_label("POST Create User")
    call create_user_button%set_name("create_user_button")
    call create_user_button%on_click(on_create_user_clicked)

    call update_user_button%set_label("PUT Update User")
    call update_user_button%set_name("update_user_button")
    call update_user_button%on_click(on_update_user_clicked)

    call delete_user_button%set_label("DELETE User")
    call delete_user_button%set_name("delete_user_button")
    call delete_user_button%on_click(on_delete_user_clicked)

    ! Response displays
    call response_view%set_name("response_view")
    call response_view%set_editable(.false.)
    call response_view%set_text("HTTP response will appear here...\n")

    call json_view%set_name("json_view")
    call json_view%set_editable(.false.)
    call json_view%set_text("Parsed JSON will appear here...\n")

    ! Status labels
    call status_label%set_name("status_label")
    call status_label%set_text("REST API client ready - select an operation")

    call api_label%set_name("api_label")
    call api_label%set_text("API: JSONPlaceholder (Mock API)")

    ! Show window
    print '(A)', ""
    print '(A)', "Showing REST API client interface..."
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

    !> @brief Make GET request to retrieve users
    subroutine get_users()
        character(len=:), allocatable :: api_url, endpoint

        api_url = trim(api_url_entry%get_text())
        endpoint = trim(api_url) // "/users"

        print '(A,A)', "  Making GET request to: ", endpoint

        ! Create and execute GET request (simulated)
        call simulate_api_request("GET", endpoint, "")
    end subroutine get_users

    !> @brief Make POST request to create a user
    subroutine create_user()
        character(len=:), allocatable :: api_url, endpoint, json_data

        api_url = trim(api_url_entry%get_text())
        endpoint = trim(api_url) // "/users"

        ! Create JSON data for new user
        json_data = '{"name": "John Doe", "username": "johndoe", "email": "john@example.com", ' // &
                   '"address": {"street": "123 Main St", "city": "Anytown", "zipcode": "12345"}, ' // &
                   '"phone": "555-1234", "website": "johndoe.com"}'

        print '(A,A)', "  Making POST request to: ", endpoint

        ! Create and execute POST request (simulated)
        call simulate_api_request("POST", endpoint, json_data)
    end subroutine create_user

    !> @brief Make PUT request to update a user
    subroutine update_user()
        character(len=:), allocatable :: api_url, endpoint, json_data, user_id

        api_url = trim(api_url_entry%get_text())
        user_id = trim(user_id_entry%get_text())
        endpoint = trim(api_url) // "/users/" // trim(user_id)

        ! Create JSON data for user update
        json_data = '{"name": "John Doe Updated", "email": "john.updated@example.com"}'

        print '(A,A)', "  Making PUT request to: ", endpoint

        ! Create and execute PUT request (simulated)
        call simulate_api_request("PUT", endpoint, json_data)
    end subroutine update_user

    !> @brief Make DELETE request to remove a user
    subroutine delete_user()
        character(len=:), allocatable :: api_url, endpoint, user_id

        api_url = trim(api_url_entry%get_text())
        user_id = trim(user_id_entry%get_text())
        endpoint = trim(api_url) // "/users/" // trim(user_id)

        print '(A,A)', "  Making DELETE request to: ", endpoint

        ! Create and execute DELETE request (simulated)
        call simulate_api_request("DELETE", endpoint, "")
    end subroutine delete_user

    !> @brief Simulate REST API request and response
    subroutine simulate_api_request(method, endpoint, request_data)
        character(len=*), intent(in) :: method, endpoint, request_data
        character(len=1000) :: response_text, json_response

        ! Simulate network delay
        print '(A)', "  [Simulating API request...]"

        ! Create simulated response based on method and endpoint
        select case (method)
        case ("GET")
            if (index(endpoint, "/users") > 0) then
                json_response = '[{"id": 1, "name": "John Doe", "email": "john@example.com"}, ' // &
                               '{"id": 2, "name": "Jane Smith", "email": "jane@example.com"}]'
                response_text = "HTTP/1.1 200 OK\nContent-Type: application/json\n\n" // trim(json_response)
            end if
        case ("POST")
            json_response = '{"id": 101, "name": "John Doe", "username": "johndoe", "email": "john@example.com"}'
            response_text = "HTTP/1.1 201 Created\nContent-Type: application/json\n\n" // trim(json_response)
        case ("PUT")
            json_response = '{"id": 1, "name": "John Doe Updated", "email": "john.updated@example.com"}'
            response_text = "HTTP/1.1 200 OK\nContent-Type: application/json\n\n" // trim(json_response)
        case ("DELETE")
            response_text = "HTTP/1.1 204 No Content\n\n"
        case default
            response_text = "HTTP/1.1 405 Method Not Allowed\n\n"
        end select

        ! Display raw response
        call response_view%set_text(trim(response_text))

        ! Parse and display JSON (simplified)
        if (len_trim(json_response) > 0) then
            call json_view%set_text("Parsed JSON:\n" // trim(json_response) // "\n\nFormatted display would show here.")
        else
            call json_view%set_text("No JSON content in response.")
        end if

        ! Update status
        write(status_text, '(A,A,A)') method, " request completed for ", trim(endpoint)
        call status_label%set_text(trim(status_text))

        print '(A,A,A)', "  → ", method, " request completed"
    end subroutine simulate_api_request

    !> @brief Handler for GET users button click
    subroutine on_get_users_clicked(event)
        type(forge_event), intent(in) :: event
        call get_users()
    end subroutine on_get_users_clicked

    !> @brief Handler for create user button click
    subroutine on_create_user_clicked(event)
        type(forge_event), intent(in) :: event
        call create_user()
    end subroutine on_create_user_clicked

    !> @brief Handler for update user button click
    subroutine on_update_user_clicked(event)
        type(forge_event), intent(in) :: event
        call update_user()
    end subroutine on_update_user_clicked

    !> @brief Handler for delete user button click
    subroutine on_delete_user_clicked(event)
        type(forge_event), intent(in) :: event
        call delete_user()
    end subroutine on_delete_user_clicked

end program network_rest_api_example