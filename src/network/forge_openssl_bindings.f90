!> @brief OpenSSL C bindings for SSL/TLS functionality
!> @details C interfaces to OpenSSL library functions
!> @author ForGE Contributors
!> @date 2025
!> @license GPL-3.0-or-later

module forge_openssl_bindings
    use iso_c_binding
    implicit none
    private

    ! OpenSSL types
    public :: SSL_CTX, SSL, X509, EVP_PKEY, BIO
    public :: SSL_METHOD

    ! SSL/TLS methods
    public :: TLS_client_method, TLS_server_method, TLS_method

    ! SSL context functions
    public :: SSL_CTX_new, SSL_CTX_free, SSL_CTX_set_verify
    public :: SSL_CTX_load_verify_locations, SSL_CTX_set_cipher_list
    public :: SSL_CTX_set_min_proto_version, SSL_CTX_set_max_proto_version

    ! SSL functions
    public :: SSL_new, SSL_free, SSL_set_fd, SSL_connect, SSL_accept
    public :: SSL_read, SSL_write, SSL_shutdown, SSL_get_verify_result
    public :: SSL_get_peer_certificate, SSL_set_connect_state, SSL_set_accept_state

    ! Certificate functions
    public :: X509_free, X509_get_subject_name, X509_get_issuer_name
    public :: X509_get_notBefore, X509_get_notAfter, X509_verify_cert_error_string
    public :: X509_STORE_CTX_get_error, X509_STORE_CTX_get_current_cert

    ! Private key functions
    public :: EVP_PKEY_free, EVP_PKEY_get_id

    ! BIO functions
    public :: BIO_new_mem_buf, BIO_free, BIO_read, BIO_write

    ! Error handling
    public :: ERR_get_error, ERR_error_string

    ! Constants
    integer(c_int), parameter, public :: SSL_VERIFY_NONE = 0
    integer(c_int), parameter, public :: SSL_VERIFY_PEER = 1
    integer(c_int), parameter, public :: SSL_VERIFY_FAIL_IF_NO_PEER_CERT = 2
    integer(c_int), parameter, public :: SSL_VERIFY_CLIENT_ONCE = 4

    integer(c_int), parameter, public :: TLS1_2_VERSION = 771
    integer(c_int), parameter, public :: TLS1_3_VERSION = 772

    ! Opaque types
    type, bind(c) :: SSL_CTX
        private
        type(c_ptr) :: ptr = c_null_ptr
    end type SSL_CTX

    type, bind(c) :: SSL
        private
        type(c_ptr) :: ptr = c_null_ptr
    end type SSL

    type, bind(c) :: X509
        private
        type(c_ptr) :: ptr = c_null_ptr
    end type X509

    type, bind(c) :: EVP_PKEY
        private
        type(c_ptr) :: ptr = c_null_ptr
    end type EVP_PKEY

    type, bind(c) :: BIO
        private
        type(c_ptr) :: ptr = c_null_ptr
    end type BIO

    type, bind(c) :: SSL_METHOD
        private
        type(c_ptr) :: ptr = c_null_ptr
    end type SSL_METHOD

    ! C interface declarations

    interface
        ! SSL/TLS methods
        function TLS_client_method() bind(c, name="TLS_client_method")
            import :: SSL_METHOD
            type(SSL_METHOD) :: TLS_client_method
        end function

        function TLS_server_method() bind(c, name="TLS_server_method")
            import :: SSL_METHOD
            type(SSL_METHOD) :: TLS_server_method
        end function

        function TLS_method() bind(c, name="TLS_method")
            import :: SSL_METHOD
            type(SSL_METHOD) :: TLS_method
        end function

        ! SSL context functions
        function SSL_CTX_new(method) bind(c, name="SSL_CTX_new")
            import :: SSL_CTX, SSL_METHOD
            type(SSL_METHOD), value :: method
            type(SSL_CTX) :: SSL_CTX_new
        end function

        subroutine SSL_CTX_free(ctx) bind(c, name="SSL_CTX_free")
            import :: SSL_CTX
            type(SSL_CTX), value :: ctx
        end subroutine

        function SSL_CTX_set_verify(ctx, mode, callback) bind(c, name="SSL_CTX_set_verify")
            import :: SSL_CTX, c_int, c_funptr
            type(SSL_CTX), value :: ctx
            integer(c_int), value :: mode
            type(c_funptr), value :: callback
            integer(c_int) :: SSL_CTX_set_verify
        end function

        function SSL_CTX_load_verify_locations(ctx, CAfile, CApath) bind(c, name="SSL_CTX_load_verify_locations")
            import :: SSL_CTX, c_char
            type(SSL_CTX), value :: ctx
            character(kind=c_char), dimension(*) :: CAfile
            character(kind=c_char), dimension(*) :: CApath
            integer(c_int) :: SSL_CTX_load_verify_locations
        end function

        function SSL_CTX_set_cipher_list(ctx, str) bind(c, name="SSL_CTX_set_cipher_list")
            import :: SSL_CTX, c_char
            type(SSL_CTX), value :: ctx
            character(kind=c_char), dimension(*) :: str
            integer(c_int) :: SSL_CTX_set_cipher_list
        end function

        function SSL_CTX_set_min_proto_version(ctx, version) bind(c, name="SSL_CTX_set_min_proto_version")
            import :: SSL_CTX, c_int
            type(SSL_CTX), value :: ctx
            integer(c_int), value :: version
            integer(c_int) :: SSL_CTX_set_min_proto_version
        end function

        function SSL_CTX_set_max_proto_version(ctx, version) bind(c, name="SSL_CTX_set_max_proto_version")
            import :: SSL_CTX, c_int
            type(SSL_CTX), value :: ctx
            integer(c_int), value :: version
            integer(c_int) :: SSL_CTX_set_max_proto_version
        end function

        ! SSL functions
        function SSL_new(ctx) bind(c, name="SSL_new")
            import :: SSL, SSL_CTX
            type(SSL_CTX), value :: ctx
            type(SSL) :: SSL_new
        end function

        subroutine SSL_free(ssl) bind(c, name="SSL_free")
            import :: SSL
            type(SSL), value :: ssl
        end subroutine

        function SSL_set_fd(ssl, fd) bind(c, name="SSL_set_fd")
            import :: SSL, c_int
            type(SSL), value :: ssl
            integer(c_int), value :: fd
            integer(c_int) :: SSL_set_fd
        end function

        function SSL_connect(ssl) bind(c, name="SSL_connect")
            import :: SSL
            type(SSL), value :: ssl
            integer(c_int) :: SSL_connect
        end function

        function SSL_accept(ssl) bind(c, name="SSL_accept")
            import :: SSL
            type(SSL), value :: ssl
            integer(c_int) :: SSL_accept
        end function

        function SSL_read(ssl, buf, num) bind(c, name="SSL_read")
            import :: SSL, c_ptr, c_int
            type(SSL), value :: ssl
            type(c_ptr), value :: buf
            integer(c_int), value :: num
            integer(c_int) :: SSL_read
        end function

        function SSL_write(ssl, buf, num) bind(c, name="SSL_write")
            import :: SSL, c_ptr, c_int
            type(SSL), value :: ssl
            type(c_ptr), value :: buf
            integer(c_int), value :: num
            integer(c_int) :: SSL_write
        end function

        function SSL_shutdown(ssl) bind(c, name="SSL_shutdown")
            import :: SSL
            type(SSL), value :: ssl
            integer(c_int) :: SSL_shutdown
        end function

        function SSL_get_verify_result(ssl) bind(c, name="SSL_get_verify_result")
            import :: SSL
            type(SSL), value :: ssl
            integer(c_long) :: SSL_get_verify_result
        end function

        function SSL_get_peer_certificate(ssl) bind(c, name="SSL_get_peer_certificate")
            import :: SSL, X509
            type(SSL), value :: ssl
            type(X509) :: SSL_get_peer_certificate
        end function

        subroutine SSL_set_connect_state(ssl) bind(c, name="SSL_set_connect_state")
            import :: SSL
            type(SSL), value :: ssl
        end subroutine

        subroutine SSL_set_accept_state(ssl) bind(c, name="SSL_set_accept_state")
            import :: SSL
            type(SSL), value :: ssl
        end subroutine

        ! Certificate functions
        subroutine X509_free(cert) bind(c, name="X509_free")
            import :: X509
            type(X509), value :: cert
        end subroutine

        function X509_get_subject_name(cert) bind(c, name="X509_get_subject_name")
            import :: X509, c_ptr
            type(X509), value :: cert
            type(c_ptr) :: X509_get_subject_name
        end function

        function X509_get_issuer_name(cert) bind(c, name="X509_get_issuer_name")
            import :: X509, c_ptr
            type(X509), value :: cert
            type(c_ptr) :: X509_get_issuer_name
        end function

        function X509_get_notBefore(cert) bind(c, name="X509_get_notBefore")
            import :: X509, c_ptr
            type(X509), value :: cert
            type(c_ptr) :: X509_get_notBefore
        end function

        function X509_get_notAfter(cert) bind(c, name="X509_get_notAfter")
            import :: X509, c_ptr
            type(X509), value :: cert
            type(c_ptr) :: X509_get_notAfter
        end function

        function X509_verify_cert_error_string(error) bind(c, name="X509_verify_cert_error_string")
            import :: c_long, c_ptr
            integer(c_long), value :: error
            type(c_ptr) :: X509_verify_cert_error_string
        end function

        function X509_STORE_CTX_get_error(ctx) bind(c, name="X509_STORE_CTX_get_error")
            import :: c_ptr, c_int
            type(c_ptr), value :: ctx
            integer(c_int) :: X509_STORE_CTX_get_error
        end function

        function X509_STORE_CTX_get_current_cert(ctx) bind(c, name="X509_STORE_CTX_get_current_cert")
            import :: c_ptr, X509
            type(c_ptr), value :: ctx
            type(X509) :: X509_STORE_CTX_get_current_cert
        end function

        ! Private key functions
        subroutine EVP_PKEY_free(pkey) bind(c, name="EVP_PKEY_free")
            import :: EVP_PKEY
            type(EVP_PKEY), value :: pkey
        end subroutine

        function EVP_PKEY_get_id(pkey) bind(c, name="EVP_PKEY_get_id")
            import :: EVP_PKEY, c_int
            type(EVP_PKEY), value :: pkey
            integer(c_int) :: EVP_PKEY_get_id
        end function

        ! BIO functions
        function BIO_new_mem_buf(buf, len) bind(c, name="BIO_new_mem_buf")
            import :: BIO, c_ptr, c_int
            type(c_ptr), value :: buf
            integer(c_int), value :: len
            type(BIO) :: BIO_new_mem_buf
        end function

        function BIO_read(bio, buf, len) bind(c, name="BIO_read")
            import :: BIO, c_ptr, c_int
            type(BIO), value :: bio
            type(c_ptr), value :: buf
            integer(c_int), value :: len
            integer(c_int) :: BIO_read
        end function

        function BIO_write(bio, buf, len) bind(c, name="BIO_write")
            import :: BIO, c_ptr, c_int
            type(BIO), value :: bio
            type(c_ptr), value :: buf
            integer(c_int), value :: len
            integer(c_int) :: BIO_write
        end function

        subroutine BIO_free(bio) bind(c, name="BIO_free")
            import :: BIO
            type(BIO), value :: bio
        end subroutine

        ! Error handling
        function ERR_get_error() bind(c, name="ERR_get_error")
            import :: c_long
            integer(c_long) :: ERR_get_error
        end function

        function ERR_error_string(error, buf) bind(c, name="ERR_error_string")
            import :: c_long, c_char
            integer(c_long), value :: error
            character(kind=c_char), dimension(*) :: buf
            type(c_ptr) :: ERR_error_string
        end function
    end interface

contains

    ! Helper functions for Fortran usage

    function c_to_f_string(c_str) result(f_str)
        type(c_ptr), intent(in) :: c_str
        character(len=:), allocatable :: f_str
        character(len=1024), pointer :: f_ptr
        integer :: i

        if (.not. c_associated(c_str)) then
            f_str = ""
            return
        end if

        call c_f_pointer(c_str, f_ptr)
        i = 1
        do while (f_ptr(i:i) /= achar(0) .and. i < 1024)
            i = i + 1
        end do
        f_str = f_ptr(1:i-1)
    end function c_to_f_string

end module forge_openssl_bindings