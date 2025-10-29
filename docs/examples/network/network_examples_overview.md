# Networking Examples

This section demonstrates ForGE Qt's networking capabilities, including HTTP, TCP, SSL/TLS, and WebSocket communication.

## HTTP Client

### Basic HTTP Operations

**File:** `examples/network_http_client/network_http_client.f90`

Demonstrates:
- HTTP GET, POST, PUT, DELETE requests
- Request headers and authentication
- Response handling and parsing
- Error handling and timeouts

```fortran
type(forge_http_client) :: client
type(forge_http_request) :: request
type(forge_http_response) :: response

call request%set_url("https://api.example.com/data")
call request%set_method("GET")
call client%execute(request, response)
```

## TCP Networking

### TCP Server

**File:** `examples/network_tcp_server/network_tcp_server.f90`

Demonstrates:
- TCP server creation and binding
- Accepting client connections
- Multi-client handling
- Data reception and transmission

### TCP Client

**File:** `examples/network_tcp_client/network_tcp_client.f90`

Demonstrates:
- TCP client connections
- Data sending and receiving
- Connection state management
- Error handling

## Secure Networking

### SSL/TLS Client

**File:** `examples/network_ssl_client/network_ssl_client.f90`

Demonstrates:
- SSL/TLS connection establishment
- Certificate verification
- Encrypted communication
- Secure protocol negotiation

```fortran
type(forge_ssl_socket) :: ssl_socket
call ssl_socket%set_verify_mode(SSL_VERIFY_PEER + SSL_VERIFY_HOST)
call ssl_socket%connect(host, port)
```

## Real-Time Communication

### WebSocket Client

**File:** `examples/network_websocket/network_websocket.f90`

Demonstrates:
- WebSocket connection establishment
- Bidirectional messaging
- Text and binary message handling
- Connection lifecycle management

## REST API Integration

### REST API Client

**File:** `examples/network_rest_api/network_rest_api.f90`

Demonstrates:
- RESTful API interaction
- JSON request/response handling
- CRUD operations
- API authentication

## Network Architecture

ForGE Qt's networking components:

1. **High-Level APIs:** HTTP, WebSocket clients
2. **Transport Layer:** TCP/UDP sockets
3. **Security Layer:** SSL/TLS encryption
4. **Protocol Support:** HTTP, WebSocket, custom protocols

### Connection Management

```fortran
type(forge_tcp_socket) :: socket
call socket%connect("localhost", 8080)
call socket%write("Hello, server!")
call socket%read(data)
```

### Asynchronous Operations

All network operations support asynchronous patterns:

```fortran
call socket%on_connected(connection_handler)
call socket%on_data_received(data_handler)
call socket%on_error(error_handler)
```

## Security Features

- **SSL/TLS Support:** Encrypted connections
- **Certificate Validation:** Peer and host verification
- **Protocol Negotiation:** Secure handshake
- **Authentication:** Basic, Digest, OAuth support

## Performance Considerations

1. **Connection Pooling:** Reuse connections
2. **Asynchronous I/O:** Non-blocking operations
3. **Buffering:** Efficient data handling
4. **Timeout Management:** Prevent hanging connections

## Error Handling

Network operations can fail due to:
- Connection timeouts
- Network unavailability
- SSL certificate issues
- Protocol errors

```fortran
call socket%on_error(error_handler)

subroutine error_handler(error)
    select case (error%type)
    case (NETWORK_TIMEOUT)
        ! Handle timeout
    case (SSL_HANDSHAKE_FAILED)
        ! Handle SSL error
    end select
end subroutine
```

## Running Network Examples

```bash
# Build and run network examples
cmake --build build --target network_http_client
./build/examples/network_http_client/network_http_client

# Test TCP communication
cmake --build build --target network_tcp_server
./build/examples/network_tcp_server/network_tcp_server &
./build/examples/network_tcp_client/network_tcp_client
```

## Network Protocols

| Protocol | Examples | Features |
|----------|----------|----------|
| **HTTP** | HTTP Client, REST API | Request/response, REST |
| **TCP** | TCP Server, TCP Client | Reliable streaming |
| **SSL/TLS** | SSL Client | Encrypted connections |
| **WebSocket** | WebSocket Client | Real-time messaging |

## Best Practices

1. **Error Handling:** Always handle network errors
2. **Timeouts:** Set appropriate timeouts
3. **Security:** Use SSL/TLS for sensitive data
4. **Resource Management:** Clean up connections
5. **Threading:** Network I/O is thread-safe

These examples provide comprehensive coverage of ForGE Qt's networking capabilities for building connected applications.