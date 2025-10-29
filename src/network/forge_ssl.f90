!> @brief SSL/TLS support for Qt-style networking
!> @details QSslCertificate, QSslKey, QSslConfiguration, and QSslSocket classes
!> @author ForGE Contributors
!> @date 2025
!> @license GPL-3.0-or-later

module forge_ssl
    use iso_c_binding
    use forge_types
    use forge_errors
    use forge_signals
    use forge_socket
    use forge_openssl_bindings
    implicit none
    private

    public :: QSslCertificate, QSslKey, QSslConfiguration, QSslSocket
    public :: QSslError, QSslCertificateExtension
    public :: QSsl, QSslCertificateFormat, QSslKeyAlgorithm, QSslEncodingFormat
    public :: QSslProtocol, QSslKeyType

    ! SSL/TLS enumerations
    integer, parameter :: QSsl_UnknownProtocol = -1
    integer, parameter :: QSsl_TlsV1_0 = 0
    integer, parameter :: QSsl_TlsV1_1 = 1
    integer, parameter :: QSsl_TlsV1_2 = 2
    integer, parameter :: QSsl_TlsV1_3 = 3
    integer, parameter :: QSsl_DtlsV1_0 = 4
    integer, parameter :: QSsl_DtlsV1_2 = 5
    integer, parameter :: QSsl_AnyProtocol = 6
    integer, parameter :: QSsl_SecureProtocols = 7

    integer, parameter :: QSslCertificateFormat_Pem = 0
    integer, parameter :: QSslCertificateFormat_Der = 1

    integer, parameter :: QSslKeyAlgorithm_Rsa = 0
    integer, parameter :: QSslKeyAlgorithm_Dsa = 1
    integer, parameter :: QSslKeyAlgorithm_Ec = 2
    integer, parameter :: QSslKeyAlgorithm_Dh = 3

    integer, parameter :: QSslEncodingFormat_Pem = 0
    integer, parameter :: QSslEncodingFormat_Der = 1
    integer, parameter :: QSslEncodingFormat_Pkcs8 = 2

    integer, parameter :: QSslKeyType_PrivateKey = 0
    integer, parameter :: QSslKeyType_PublicKey = 1

    !> @brief SSL/TLS certificate with validation
    type :: QSslCertificate
        private
        type(X509) :: cert_handle = X509(c_null_ptr)
        logical :: is_null = .true.
        character(len=:), allocatable :: subject_name
        character(len=:), allocatable :: issuer_name
        character(len=:), allocatable :: serial_number
        integer :: version = 0
        character(len=:), allocatable :: not_before
        character(len=:), allocatable :: not_after
        type(QByteArray) :: der_data
        logical :: is_self_signed = .false.
    contains
        procedure :: load => sslcertificate_load
        procedure :: load_from_data => sslcertificate_load_from_data
        procedure :: is_valid => sslcertificate_is_valid
        procedure :: is_blacklisted => sslcertificate_is_blacklisted
        procedure :: expiry_date => sslcertificate_expiry_date
        procedure :: effective_date => sslcertificate_effective_date
        procedure :: get_subject_info => sslcertificate_subject_info
        procedure :: get_issuer_info => sslcertificate_issuer_info
        procedure :: get_public_key => sslcertificate_public_key
        procedure :: to_der => sslcertificate_to_der
        procedure :: to_pem => sslcertificate_to_pem
        procedure :: verify => sslcertificate_verify
        procedure :: clear => sslcertificate_clear
        procedure :: is_null_cert => sslcertificate_is_null
    end type QSslCertificate

    !> @brief Private key management
    type :: QSslKey
        private
        type(EVP_PKEY) :: key_handle = EVP_PKEY(c_null_ptr)
        logical :: is_null = .true.
        integer :: algorithm = QSslKeyAlgorithm_Rsa
        integer :: type = QSslKeyType_PrivateKey
        integer :: length = 0
    contains
        procedure :: load => sslkey_load
        procedure :: load_from_data => sslkey_load_from_data
        procedure :: is_null_key => sslkey_is_null
        procedure :: algorithm_type => sslkey_algorithm
        procedure :: key_type => sslkey_type
        procedure :: length => sslkey_length
        procedure :: to_der => sslkey_to_der
        procedure :: to_pem => sslkey_to_pem
        procedure :: clear => sslkey_clear
    end type QSslKey

    !> @brief SSL/TLS configuration
    type :: QSslConfiguration
        private
        logical :: is_null = .true.
        integer :: protocol = QSsl_TlsV1_2
        integer :: peer_verify_mode = SSL_VERIFY_PEER
        type(QSslCertificate), allocatable :: local_certificate
        type(QSslKey), allocatable :: private_key
        type(QSslCertificate), allocatable :: ca_certificates(:)
        integer :: ca_count = 0
        character(len=:), allocatable :: ciphers
        character(len=:), allocatable :: session_ticket
        logical :: session_tickets_enabled = .true.
        integer :: session_ticket_lifetime_hint = 0
        logical :: dtls_cookie_verification_enabled = .false.
        logical :: ephemeral_key_enabled = .true.
    contains
        procedure :: set_protocol => sslconfig_set_protocol
        procedure :: protocol => sslconfig_protocol
        procedure :: set_peer_verify_mode => sslconfig_set_peer_verify_mode
        procedure :: peer_verify_mode => sslconfig_peer_verify_mode
        procedure :: set_local_certificate => sslconfig_set_local_certificate
        procedure :: local_certificate => sslconfig_local_certificate
        procedure :: set_private_key => sslconfig_set_private_key
        procedure :: private_key => sslconfig_private_key
        procedure :: set_ca_certificates => sslconfig_set_ca_certificates
        procedure :: ca_certificates => sslconfig_ca_certificates
        procedure :: add_ca_certificate => sslconfig_add_ca_certificate
        procedure :: set_ciphers => sslconfig_set_ciphers
        procedure :: ciphers => sslconfig_ciphers
        procedure :: set_session_ticket => sslconfig_set_session_ticket
        procedure :: session_ticket => sslconfig_session_ticket
        procedure :: set_dtls_cookie_verification_enabled => sslconfig_set_dtls_cookie_verification_enabled
        procedure :: dtls_cookie_verification_enabled => sslconfig_dtls_cookie_verification_enabled
        procedure :: is_null_config => sslconfig_is_null
        procedure :: default_configuration => sslconfig_default_configuration
        procedure :: set_default_configuration => sslconfig_set_default_configuration
    end type QSslConfiguration

    !> @brief SSL/TLS-enabled TCP socket
    type, extends(QTcpSocket) :: QSslSocket
        private
        type(SSL_CTX) :: ssl_ctx = SSL_CTX(c_null_ptr)
        type(SSL) :: ssl_handle = SSL(c_null_ptr)
        type(QSslConfiguration) :: ssl_config
        logical :: ssl_initialized = .false.
        logical :: handshake_completed = .false.
        integer :: ssl_error_code = 0
        character(len=256) :: ssl_error_string = ""
        type(QSslCertificate) :: peer_certificate
        type(QSslCertificate), allocatable :: peer_certificate_chain(:)
        integer :: chain_count = 0
        type(signal_void) :: encrypted
        type(signal_void) :: ssl_errors
        type(signal_int) :: peer_verify_error
        type(signal_void) :: mode_changed
    contains
        procedure :: connect_to_host_encrypted => sslsocket_connect_encrypted
        procedure :: start_server_encryption => sslsocket_start_server_encryption
        procedure :: start_client_encryption => sslsocket_start_client_encryption
        procedure :: set_ssl_configuration => sslsocket_set_ssl_configuration
        procedure :: ssl_configuration => sslsocket_ssl_configuration
        procedure :: write_data => sslsocket_write_data
        procedure :: read_data => sslsocket_read_data
        procedure :: bytes_available => sslsocket_bytes_available
        procedure :: flush => sslsocket_flush
        procedure :: close => sslsocket_close
        procedure :: abort => sslsocket_abort
        procedure :: peer_certificate => sslsocket_peer_certificate
        procedure :: peer_certificate_chain => sslsocket_peer_certificate_chain
        procedure :: ssl_errors => sslsocket_ssl_errors
        procedure :: is_encrypted => sslsocket_is_encrypted
        procedure :: protocol => sslsocket_protocol
        procedure :: cipher => sslsocket_cipher
        procedure :: session_cipher => sslsocket_session_cipher
        procedure :: session_protocol => sslsocket_session_protocol
        procedure :: ignore_ssl_errors => sslsocket_ignore_ssl_errors
        procedure :: init_ssl => sslsocket_init_ssl
        procedure :: cleanup_ssl => sslsocket_cleanup_ssl
    end type QSslSocket

    !> @brief SSL error information
    type :: QSslError
        private
        integer :: error_code = 0
        character(len=:), allocatable :: error_string
        type(QSslCertificate) :: certificate
    contains
        procedure :: error => sslerror_error
        procedure :: error_string => sslerror_error_string
        procedure :: certificate => sslerror_certificate
    end type QSslError

    !> @brief Certificate extension
    type :: QSslCertificateExtension
        private
        character(len=:), allocatable :: name
        type(QVariant) :: value
        logical :: is_critical = .false.
        logical :: is_supported = .true.
    contains
        procedure :: name => sslcertificateextension_name
        procedure :: value => sslcertificateextension_value
        procedure :: is_critical => sslcertificateextension_is_critical
        procedure :: is_supported => sslcertificateextension_is_supported
    end type QSslCertificateExtension

contains

    ! ========== QSslCertificate Implementation ==========

    subroutine sslcertificate_load(this, file_path, format)
        class(QSslCertificate), intent(inout) :: this
        character(len=*), intent(in) :: file_path
        integer, intent(in), optional :: format
        integer :: fmt
        type(BIO) :: bio
        type(c_ptr) :: file_ptr

        fmt = QSslCertificateFormat_Pem
        if (present(format)) fmt = format

        ! Open file
        file_ptr = c_fopen(trim(file_path) // c_null_char, "rb" // c_null_char)
        if (.not. c_associated(file_ptr)) return

        ! Create BIO from file
        bio = BIO_new_mem_buf(file_ptr, -1)
        if (.not. c_associated(bio%ptr)) then
            call c_fclose(file_ptr)
            return
        end if

        ! Load certificate
        if (fmt == QSslCertificateFormat_Pem) then
            this%cert_handle = PEM_read_bio_X509(bio, c_null_ptr, c_null_funptr, c_null_ptr)
        else
            this%cert_handle = d2i_X509_bio(bio, c_null_ptr)
        end if

        if (c_associated(this%cert_handle%ptr)) then
            this%is_null = .false.
            call this%parse_certificate()
        end if

        call BIO_free(bio)
        call c_fclose(file_ptr)
    end subroutine sslcertificate_load

    subroutine sslcertificate_load_from_data(this, data, format)
        class(QSslCertificate), intent(inout) :: this
        type(QByteArray), intent(in) :: data
        integer, intent(in), optional :: format
        integer :: fmt
        type(BIO) :: bio

        fmt = QSslCertificateFormat_Pem
        if (present(format)) fmt = format

        ! Create BIO from data
        bio = BIO_new_mem_buf(c_loc(data%data()), data%size())
        if (.not. c_associated(bio%ptr)) return

        ! Load certificate
        if (fmt == QSslCertificateFormat_Pem) then
            this%cert_handle = PEM_read_bio_X509(bio, c_null_ptr, c_null_funptr, c_null_ptr)
        else
            this%cert_handle = d2i_X509_bio(bio, c_null_ptr)
        end if

        if (c_associated(this%cert_handle%ptr)) then
            this%is_null = .false.
            call this%parse_certificate()
        end if

        call BIO_free(bio)
    end subroutine sslcertificate_load_from_data

    subroutine sslcertificate_parse_certificate(this)
        class(QSslCertificate), intent(inout) :: this
        type(c_ptr) :: subject_name, issuer_name, not_before, not_after
        integer :: serial

        if (.not. c_associated(this%cert_handle%ptr)) return

        ! Get subject name
        subject_name = X509_get_subject_name(this%cert_handle)
        if (c_associated(subject_name)) then
            this%subject_name = c_to_f_string(X509_NAME_oneline(subject_name, c_null_ptr, 0))
        end if

        ! Get issuer name
        issuer_name = X509_get_issuer_name(this%cert_handle)
        if (c_associated(issuer_name)) then
            this%issuer_name = c_to_f_string(X509_NAME_oneline(issuer_name, c_null_ptr, 0))
        end if

        ! Get validity dates
        not_before = X509_get_notBefore(this%cert_handle)
        if (c_associated(not_before)) then
            this%not_before = c_to_f_string(ASN1_TIME_to_string(not_before))
        end if

        not_after = X509_get_notAfter(this%cert_handle)
        if (c_associated(not_after)) then
            this%not_after = c_to_f_string(ASN1_TIME_to_string(not_after))
        end if

        ! Get version
        this%version = X509_get_version(this%cert_handle) + 1

        ! Check if self-signed
        this%is_self_signed = (this%subject_name == this%issuer_name)
    end subroutine sslcertificate_parse_certificate

    function sslcertificate_is_valid(this) result(valid)
        class(QSslCertificate), intent(in) :: this
        logical :: valid
        integer(c_long) :: verify_result

        valid = .false.
        if (this%is_null) return

        ! Basic validity check using OpenSSL
        verify_result = X509_verify_cert_error_string(X509_get_verify_result(this%cert_handle))
        valid = (verify_result == 0)
    end function sslcertificate_is_valid

    function sslcertificate_is_blacklisted(this) result(blacklisted)
        class(QSslCertificate), intent(in) :: this
        logical :: blacklisted
        ! In a real implementation, this would check against a blacklist
        blacklisted = .false.
    end function sslcertificate_is_blacklisted

    function sslcertificate_expiry_date(this) result(date)
        class(QSslCertificate), intent(in) :: this
        character(len=:), allocatable :: date
        date = this%not_after
    end function sslcertificate_expiry_date

    function sslcertificate_effective_date(this) result(date)
        class(QSslCertificate), intent(in) :: this
        character(len=:), allocatable :: date
        date = this%not_before
    end function sslcertificate_effective_date

    function sslcertificate_subject_info(this, attribute) result(info)
        class(QSslCertificate), intent(in) :: this
        character(len=*), intent(in) :: attribute
        character(len=:), allocatable :: info
        ! Simplified - in full implementation would parse X509_NAME
        if (attribute == "CN") then
            info = this%subject_name
        else
            info = ""
        end if
    end function sslcertificate_subject_info

    function sslcertificate_issuer_info(this, attribute) result(info)
        class(QSslCertificate), intent(in) :: this
        character(len=*), intent(in) :: attribute
        character(len=:), allocatable :: info
        ! Simplified - in full implementation would parse X509_NAME
        if (attribute == "CN") then
            info = this%issuer_name
        else
            info = ""
        end if
    end function sslcertificate_issuer_info

    function sslcertificate_public_key(this) result(key)
        class(QSslCertificate), intent(in) :: this
        type(QSslKey) :: key
        type(EVP_PKEY) :: pub_key

        if (this%is_null) return

        pub_key = X509_get_pubkey(this%cert_handle)
        if (c_associated(pub_key%ptr)) then
            key%key_handle = pub_key
            key%is_null = .false.
            key%type = QSslKeyType_PublicKey
            key%algorithm = EVP_PKEY_get_id(pub_key)
        end if
    end function sslcertificate_public_key

    function sslcertificate_to_der(this) result(data)
        class(QSslCertificate), intent(in) :: this
        type(QByteArray) :: data
        integer :: len
        type(c_ptr) :: buf

        if (this%is_null) return

        len = i2d_X509(this%cert_handle, c_null_ptr)
        if (len > 0) then
            buf = allocate_c_buffer(len)
            call i2d_X509(this%cert_handle, buf)
            call data%set_data(buf, len)
            call deallocate_c_buffer(buf)
        end if
    end function sslcertificate_to_der

    function sslcertificate_to_pem(this) result(pem)
        class(QSslCertificate), intent(in) :: this
        character(len=:), allocatable :: pem
        type(BIO) :: bio
        type(c_ptr) :: mem_buf
        integer :: len

        if (this%is_null) then
            pem = ""
            return
        end if

        bio = BIO_new(BIO_s_mem())
        if (.not. c_associated(bio%ptr)) return

        call PEM_write_bio_X509(bio, this%cert_handle)
        len = BIO_get_mem_data(bio, mem_buf)
        pem = c_to_f_string(mem_buf)

        call BIO_free(bio)
    end function sslcertificate_to_pem

    function sslcertificate_verify(this, key) result(valid)
        class(QSslCertificate), intent(in) :: this
        type(QSslKey), intent(in) :: key
        logical :: valid

        valid = .false.
        if (this%is_null .or. key%is_null) return

        ! Verify certificate signature with public key
        valid = (X509_verify(this%cert_handle, key%key_handle) == 1)
    end function sslcertificate_verify

    subroutine sslcertificate_clear(this)
        class(QSslCertificate), intent(inout) :: this
        if (c_associated(this%cert_handle%ptr)) then
            call X509_free(this%cert_handle)
            this%cert_handle%ptr = c_null_ptr
        end if
        this%is_null = .true.
        if (allocated(this%subject_name)) deallocate(this%subject_name)
        if (allocated(this%issuer_name)) deallocate(this%issuer_name)
        if (allocated(this%serial_number)) deallocate(this%serial_number)
        if (allocated(this%not_before)) deallocate(this%not_before)
        if (allocated(this%not_after)) deallocate(this%not_after)
    end subroutine sslcertificate_clear

    function sslcertificate_is_null(this) result(null)
        class(QSslCertificate), intent(in) :: this
        logical :: null
        null = this%is_null
    end function sslcertificate_is_null

    ! ========== QSslKey Implementation ==========

    subroutine sslkey_load(this, file_path, algorithm, format, type, pass_phrase)
        class(QSslKey), intent(inout) :: this
        character(len=*), intent(in) :: file_path
        integer, intent(in), optional :: algorithm, format, type
        character(len=*), intent(in), optional :: pass_phrase
        integer :: alg, fmt, typ
        type(BIO) :: bio
        type(c_ptr) :: file_ptr

        alg = QSslKeyAlgorithm_Rsa
        if (present(algorithm)) alg = algorithm
        fmt = QSslEncodingFormat_Pem
        if (present(format)) fmt = format
        typ = QSslKeyType_PrivateKey
        if (present(type)) typ = type

        ! Open file
        file_ptr = c_fopen(trim(file_path) // c_null_char, "rb" // c_null_char)
        if (.not. c_associated(file_ptr)) return

        ! Create BIO from file
        bio = BIO_new_mem_buf(file_ptr, -1)
        if (.not. c_associated(bio%ptr)) then
            call c_fclose(file_ptr)
            return
        end if

        ! Load key
        if (typ == QSslKeyType_PrivateKey) then
            if (fmt == QSslEncodingFormat_Pem) then
                this%key_handle = PEM_read_bio_PrivateKey(bio, c_null_ptr, c_null_funptr, c_null_ptr)
            else
                this%key_handle = d2i_PrivateKey_bio(bio, c_null_ptr)
            end if
        else
            if (fmt == QSslEncodingFormat_Pem) then
                this%key_handle = PEM_read_bio_PUBKEY(bio, c_null_ptr, c_null_funptr, c_null_ptr)
            else
                this%key_handle = d2i_PUBKEY_bio(bio, c_null_ptr)
            end if
        end if

        if (c_associated(this%key_handle%ptr)) then
            this%is_null = .false.
            this%algorithm = EVP_PKEY_get_id(this%key_handle)
            this%type = typ
            ! Get key length (simplified)
            this%length = 2048  ! Would need EVP_PKEY_get_size or similar
        end if

        call BIO_free(bio)
        call c_fclose(file_ptr)
    end subroutine sslkey_load

    subroutine sslkey_load_from_data(this, data, algorithm, format, type, pass_phrase)
        class(QSslKey), intent(inout) :: this
        type(QByteArray), intent(in) :: data
        integer, intent(in), optional :: algorithm, format, type
        character(len=*), intent(in), optional :: pass_phrase
        integer :: alg, fmt, typ
        type(BIO) :: bio

        alg = QSslKeyAlgorithm_Rsa
        if (present(algorithm)) alg = algorithm
        fmt = QSslEncodingFormat_Pem
        if (present(format)) fmt = format
        typ = QSslKeyType_PrivateKey
        if (present(type)) typ = type

        ! Create BIO from data
        bio = BIO_new_mem_buf(c_loc(data%data()), data%size())
        if (.not. c_associated(bio%ptr)) return

        ! Load key
        if (typ == QSslKeyType_PrivateKey) then
            if (fmt == QSslEncodingFormat_Pem) then
                this%key_handle = PEM_read_bio_PrivateKey(bio, c_null_ptr, c_null_funptr, c_null_ptr)
            else
                this%key_handle = d2i_PrivateKey_bio(bio, c_null_ptr)
            end if
        else
            if (fmt == QSslEncodingFormat_Pem) then
                this%key_handle = PEM_read_bio_PUBKEY(bio, c_null_ptr, c_null_funptr, c_null_ptr)
            else
                this%key_handle = d2i_PUBKEY_bio(bio, c_null_ptr)
            end if
        end if

        if (c_associated(this%key_handle%ptr)) then
            this%is_null = .false.
            this%algorithm = EVP_PKEY_get_id(this%key_handle)
            this%type = typ
            this%length = 2048  ! Would need proper length extraction
        end if

        call BIO_free(bio)
    end subroutine sslkey_load_from_data

    function sslkey_is_null(this) result(null)
        class(QSslKey), intent(in) :: this
        logical :: null
        null = this%is_null
    end function sslkey_is_null

    function sslkey_algorithm(this) result(alg)
        class(QSslKey), intent(in) :: this
        integer :: alg
        alg = this%algorithm
    end function sslkey_algorithm

    function sslkey_type(this) result(typ)
        class(QSslKey), intent(in) :: this
        integer :: typ
        typ = this%type
    end function sslkey_type

    function sslkey_length(this) result(len)
        class(QSslKey), intent(in) :: this
        integer :: len
        len = this%length
    end function sslkey_length

    function sslkey_to_der(this) result(data)
        class(QSslKey), intent(in) :: this
        type(QByteArray) :: data
        integer :: len
        type(c_ptr) :: buf

        if (this%is_null) return

        if (this%type == QSslKeyType_PrivateKey) then
            len = i2d_PrivateKey(this%key_handle, c_null_ptr)
            if (len > 0) then
                buf = allocate_c_buffer(len)
                call i2d_PrivateKey(this%key_handle, buf)
                call data%set_data(buf, len)
                call deallocate_c_buffer(buf)
            end if
        else
            len = i2d_PublicKey(this%key_handle, c_null_ptr)
            if (len > 0) then
                buf = allocate_c_buffer(len)
                call i2d_PublicKey(this%key_handle, buf)
                call data%set_data(buf, len)
                call deallocate_c_buffer(buf)
            end if
        end if
    end function sslkey_to_der

    function sslkey_to_pem(this) result(pem)
        class(QSslKey), intent(in) :: this
        character(len=:), allocatable :: pem
        type(BIO) :: bio
        type(c_ptr) :: mem_buf
        integer :: len

        if (this%is_null) then
            pem = ""
            return
        end if

        bio = BIO_new(BIO_s_mem())
        if (.not. c_associated(bio%ptr)) return

        if (this%type == QSslKeyType_PrivateKey) then
            call PEM_write_bio_PrivateKey(bio, this%key_handle, c_null_ptr, c_null_ptr, 0, c_null_funptr, c_null_ptr)
        else
            call PEM_write_bio_PUBKEY(bio, this%key_handle)
        end if

        len = BIO_get_mem_data(bio, mem_buf)
        pem = c_to_f_string(mem_buf)

        call BIO_free(bio)
    end function sslkey_to_pem

    subroutine sslkey_clear(this)
        class(QSslKey), intent(inout) :: this
        if (c_associated(this%key_handle%ptr)) then
            call EVP_PKEY_free(this%key_handle)
            this%key_handle%ptr = c_null_ptr
        end if
        this%is_null = .true.
    end subroutine sslkey_clear

    ! ========== QSslConfiguration Implementation ==========

    subroutine sslconfig_set_protocol(this, protocol)
        class(QSslConfiguration), intent(inout) :: this
        integer, intent(in) :: protocol
        this%protocol = protocol
        this%is_null = .false.
    end subroutine sslconfig_set_protocol

    function sslconfig_protocol(this) result(protocol)
        class(QSslConfiguration), intent(in) :: this
        integer :: protocol
        protocol = this%protocol
    end function sslconfig_protocol

    subroutine sslconfig_set_peer_verify_mode(this, mode)
        class(QSslConfiguration), intent(inout) :: this
        integer, intent(in) :: mode
        this%peer_verify_mode = mode
        this%is_null = .false.
    end subroutine sslconfig_set_peer_verify_mode

    function sslconfig_peer_verify_mode(this) result(mode)
        class(QSslConfiguration), intent(in) :: this
        integer :: mode
        mode = this%peer_verify_mode
    end function sslconfig_peer_verify_mode

    subroutine sslconfig_set_local_certificate(this, certificate)
        class(QSslConfiguration), intent(inout) :: this
        type(QSslCertificate), intent(in) :: certificate
        if (.not. allocated(this%local_certificate)) allocate(this%local_certificate)
        this%local_certificate = certificate
        this%is_null = .false.
    end subroutine sslconfig_set_local_certificate

    function sslconfig_local_certificate(this) result(certificate)
        class(QSslConfiguration), intent(in) :: this
        type(QSslCertificate) :: certificate
        if (allocated(this%local_certificate)) then
            certificate = this%local_certificate
        end if
    end function sslconfig_local_certificate

    subroutine sslconfig_set_private_key(this, key)
        class(QSslConfiguration), intent(inout) :: this
        type(QSslKey), intent(in) :: key
        if (.not. allocated(this%private_key)) allocate(this%private_key)
        this%private_key = key
        this%is_null = .false.
    end subroutine sslconfig_set_private_key

    function sslconfig_private_key(this) result(key)
        class(QSslConfiguration), intent(in) :: this
        type(QSslKey) :: key
        if (allocated(this%private_key)) then
            key = this%private_key
        end if
    end function sslconfig_private_key

    subroutine sslconfig_set_ca_certificates(this, certificates)
        class(QSslConfiguration), intent(inout) :: this
        type(QSslCertificate), intent(in) :: certificates(:)
        integer :: i

        if (allocated(this%ca_certificates)) deallocate(this%ca_certificates)
        allocate(this%ca_certificates(size(certificates)))
        do i = 1, size(certificates)
            this%ca_certificates(i) = certificates(i)
        end do
        this%ca_count = size(certificates)
        this%is_null = .false.
    end subroutine sslconfig_set_ca_certificates

    function sslconfig_ca_certificates(this) result(certificates)
        class(QSslConfiguration), intent(in) :: this
        type(QSslCertificate), allocatable :: certificates(:)
        if (allocated(this%ca_certificates)) then
            allocate(certificates(this%ca_count))
            certificates = this%ca_certificates(1:this%ca_count)
        end if
    end function sslconfig_ca_certificates

    subroutine sslconfig_add_ca_certificate(this, certificate)
        class(QSslConfiguration), intent(inout) :: this
        type(QSslCertificate), intent(in) :: certificate
        type(QSslCertificate), allocatable :: temp(:)

        if (.not. allocated(this%ca_certificates)) then
            allocate(this%ca_certificates(10))
        else if (this%ca_count >= size(this%ca_certificates)) then
            allocate(temp(size(this%ca_certificates) * 2))
            temp(1:this%ca_count) = this%ca_certificates(1:this%ca_count)
            call move_alloc(temp, this%ca_certificates)
        end if

        this%ca_count = this%ca_count + 1
        this%ca_certificates(this%ca_count) = certificate
        this%is_null = .false.
    end subroutine sslconfig_add_ca_certificate

    subroutine sslconfig_set_ciphers(this, ciphers)
        class(QSslConfiguration), intent(inout) :: this
        character(len=*), intent(in) :: ciphers
        this%ciphers = ciphers
        this%is_null = .false.
    end subroutine sslconfig_set_ciphers

    function sslconfig_ciphers(this) result(ciphers)
        class(QSslConfiguration), intent(in) :: this
        character(len=:), allocatable :: ciphers
        ciphers = this%ciphers
    end function sslconfig_ciphers

    subroutine sslconfig_set_session_ticket(this, ticket)
        class(QSslConfiguration), intent(inout) :: this
        character(len=*), intent(in) :: ticket
        this%session_ticket = ticket
        this%is_null = .false.
    end subroutine sslconfig_set_session_ticket

    function sslconfig_session_ticket(this) result(ticket)
        class(QSslConfiguration), intent(in) :: this
        character(len=:), allocatable :: ticket
        ticket = this%session_ticket
    end function sslconfig_session_ticket

    subroutine sslconfig_set_dtls_cookie_verification_enabled(this, enabled)
        class(QSslConfiguration), intent(inout) :: this
        logical, intent(in) :: enabled
        this%dtls_cookie_verification_enabled = enabled
        this%is_null = .false.
    end subroutine sslconfig_set_dtls_cookie_verification_enabled

    function sslconfig_dtls_cookie_verification_enabled(this) result(enabled)
        class(QSslConfiguration), intent(in) :: this
        logical :: enabled
        enabled = this%dtls_cookie_verification_enabled
    end function sslconfig_dtls_cookie_verification_enabled

    function sslconfig_is_null(this) result(null)
        class(QSslConfiguration), intent(in) :: this
        logical :: null
        null = this%is_null
    end function sslconfig_is_null

    function sslconfig_default_configuration() result(config)
        type(QSslConfiguration) :: config
        ! Return default SSL configuration
        config%protocol = QSsl_TlsV1_2
        config%peer_verify_mode = SSL_VERIFY_PEER
        config%ciphers = "HIGH:!aNULL:!eNULL:!EXPORT:!DES:!RC4:!MD5:!PSK:!SRP:!CAMELLIA"
        config%is_null = .false.
    end function sslconfig_default_configuration

    subroutine sslconfig_set_default_configuration(config)
        type(QSslConfiguration), intent(in) :: config
        ! In a real implementation, this would set the global default
    end subroutine sslconfig_set_default_configuration

    ! ========== QSslSocket Implementation ==========

    subroutine sslsocket_connect_encrypted(this, host, port, mode, verification_mode)
        class(QSslSocket), intent(inout) :: this
        character(len=*), intent(in) :: host
        integer, intent(in) :: port
        integer, intent(in), optional :: mode, verification_mode
        integer :: vmode

        vmode = SSL_VERIFY_PEER
        if (present(verification_mode)) vmode = verification_mode

        ! Set up SSL configuration
        if (this%ssl_config%is_null_config()) then
            this%ssl_config = QSslConfiguration_default_configuration()
        end if
        call this%ssl_config%set_peer_verify_mode(vmode)

        ! Connect underlying TCP socket
        call this%QTcpSocket%connect_to_host(host, port)

        ! Initialize SSL
        call this%init_ssl()

        ! Start client encryption
        call this%start_client_encryption()
    end subroutine sslsocket_connect_encrypted

    subroutine sslsocket_start_server_encryption(this)
        class(QSslSocket), intent(inout) :: this
        integer :: ret

        if (.not. c_associated(this%ssl_handle%ptr)) return

        call SSL_set_accept_state(this%ssl_handle)

        ! Perform SSL handshake
        ret = SSL_accept(this%ssl_handle)
        if (ret == 1) then
            this%handshake_completed = .true.
            call this%encrypted%emit()
        else
            this%ssl_error_code = SSL_get_error(this%ssl_handle, ret)
            call this%ssl_errors%emit()
        end if
    end subroutine sslsocket_start_server_encryption

    subroutine sslsocket_start_client_encryption(this)
        class(QSslSocket), intent(inout) :: this
        integer :: ret

        if (.not. c_associated(this%ssl_handle%ptr)) return

        call SSL_set_connect_state(this%ssl_handle)

        ! Perform SSL handshake
        ret = SSL_connect(this%ssl_handle)
        if (ret == 1) then
            this%handshake_completed = .true.
            call this%encrypted%emit()
            ! Get peer certificate
            this%peer_certificate%cert_handle = SSL_get_peer_certificate(this%ssl_handle)
            if (c_associated(this%peer_certificate%cert_handle%ptr)) then
                this%peer_certificate%is_null = .false.
            end if
        else
            this%ssl_error_code = SSL_get_error(this%ssl_handle, ret)
            call this%ssl_errors%emit()
        end if
    end subroutine sslsocket_start_client_encryption

    subroutine sslsocket_set_ssl_configuration(this, config)
        class(QSslSocket), intent(inout) :: this
        type(QSslConfiguration), intent(in) :: config
        this%ssl_config = config
    end subroutine sslsocket_set_ssl_configuration

    function sslsocket_ssl_configuration(this) result(config)
        class(QSslSocket), intent(in) :: this
        type(QSslConfiguration) :: config
        config = this%ssl_config
    end function sslsocket_ssl_configuration

    function sslsocket_write_data(this, data) result(bytes_written)
        class(QSslSocket), intent(inout) :: this
        character(len=*), intent(in) :: data
        integer :: bytes_written

        if (.not. this%handshake_completed) then
            bytes_written = -1
            return
        end if

        bytes_written = SSL_write(this%ssl_handle, c_loc(data), len(data))
        if (bytes_written > 0) then
            call this%bytes_written%emit(bytes_written)
        end if
    end function sslsocket_write_data

    function sslsocket_read_data(this, max_size) result(data)
        class(QSslSocket), intent(inout) :: this
        integer, intent(in) :: max_size
        character(len=:), allocatable :: data
        character(len=max_size) :: buffer
        integer :: bytes_read

        if (.not. this%handshake_completed) then
            allocate(character(len=0) :: data)
            return
        end if

        bytes_read = SSL_read(this%ssl_handle, c_loc(buffer), max_size)
        if (bytes_read > 0) then
            data = buffer(1:bytes_read)
            call this%ready_read%emit()
        else
            allocate(character(len=0) :: data)
        end if
    end function sslsocket_read_data

    function sslsocket_bytes_available(this) result(bytes)
        class(QSslSocket), intent(in) :: this
        integer :: bytes
        ! SSL_pending returns the number of bytes available in SSL buffer
        if (this%handshake_completed) then
            bytes = SSL_pending(this%ssl_handle)
        else
            bytes = 0
        end if
    end function sslsocket_bytes_available

    subroutine sslsocket_flush(this)
        class(QSslSocket), intent(inout) :: this
        ! SSL doesn't need explicit flushing
    end subroutine sslsocket_flush

    subroutine sslsocket_close(this)
        class(QSslSocket), intent(inout) :: this
        if (c_associated(this%ssl_handle%ptr)) then
            call SSL_shutdown(this%ssl_handle)
        end if
        call this%QTcpSocket%close()
        call this%cleanup_ssl()
    end subroutine sslsocket_close

    subroutine sslsocket_abort(this)
        class(QSslSocket), intent(inout) :: this
        call this%cleanup_ssl()
        call this%QTcpSocket%close()
    end subroutine sslsocket_abort

    function sslsocket_peer_certificate(this) result(cert)
        class(QSslSocket), intent(in) :: this
        type(QSslCertificate) :: cert
        cert = this%peer_certificate
    end function sslsocket_peer_certificate

    function sslsocket_peer_certificate_chain(this) result(chain)
        class(QSslSocket), intent(in) :: this
        type(QSslCertificate), allocatable :: chain(:)
        if (allocated(this%peer_certificate_chain)) then
            allocate(chain(this%chain_count))
            chain = this%peer_certificate_chain(1:this%chain_count)
        end if
    end function sslsocket_peer_certificate_chain

    function sslsocket_ssl_errors(this) result(errors)
        class(QSslSocket), intent(in) :: this
        type(QSslError), allocatable :: errors(:)
        ! In full implementation, would collect all SSL errors
        allocate(errors(1))
        errors(1)%error_code = this%ssl_error_code
        errors(1)%error_string = this%ssl_error_string
    end function sslsocket_ssl_errors

    function sslsocket_is_encrypted(this) result(encrypted)
        class(QSslSocket), intent(in) :: this
        logical :: encrypted
        encrypted = this%handshake_completed
    end function sslsocket_is_encrypted

    function sslsocket_protocol(this) result(protocol)
        class(QSslSocket), intent(in) :: this
        character(len=:), allocatable :: protocol
        ! Would need SSL_get_version
        protocol = "TLSv1.2"
    end function sslsocket_protocol

    function sslsocket_cipher(this) result(cipher)
        class(QSslSocket), intent(in) :: this
        character(len=:), allocatable :: cipher
        ! Would need SSL_get_cipher
        cipher = "ECDHE-RSA-AES256-GCM-SHA384"
    end function sslsocket_cipher

    function sslsocket_session_cipher(this) result(cipher)
        class(QSslSocket), intent(in) :: this
        character(len=:), allocatable :: cipher
        cipher = this%cipher()
    end function sslsocket_session_cipher

    function sslsocket_session_protocol(this) result(protocol)
        class(QSslSocket), intent(in) :: this
        character(len=:), allocatable :: protocol
        protocol = this%protocol()
    end function sslsocket_session_protocol

    subroutine sslsocket_ignore_ssl_errors(this, errors)
        class(QSslSocket), intent(inout) :: this
        type(QSslError), intent(in), optional :: errors(:)
        ! In full implementation, would ignore specific errors
    end subroutine sslsocket_ignore_ssl_errors

    subroutine sslsocket_init_ssl(this)
        class(QSslSocket), intent(inout) :: this
        type(SSL_METHOD) :: method

        if (this%ssl_initialized) return

        ! Create SSL context
        method = TLS_client_method()
        this%ssl_ctx = SSL_CTX_new(method)
        if (.not. c_associated(this%ssl_ctx%ptr)) return

        ! Configure SSL context
        call SSL_CTX_set_verify(this%ssl_ctx, this%ssl_config%peer_verify_mode(), c_null_funptr)

        ! Load CA certificates if specified
        if (allocated(this%ssl_config%ca_certificates)) then
            ! Would load CA certificates
        end if

        ! Set cipher list
        if (allocated(this%ssl_config%ciphers)) then
            call SSL_CTX_set_cipher_list(this%ssl_ctx, trim(this%ssl_config%ciphers) // c_null_char)
        end if

        ! Set protocol version
        select case (this%ssl_config%protocol)
        case (QSsl_TlsV1_2)
            call SSL_CTX_set_min_proto_version(this%ssl_ctx, TLS1_2_VERSION)
            call SSL_CTX_set_max_proto_version(this%ssl_ctx, TLS1_2_VERSION)
        case (QSsl_TlsV1_3)
            call SSL_CTX_set_min_proto_version(this%ssl_ctx, TLS1_3_VERSION)
            call SSL_CTX_set_max_proto_version(this%ssl_ctx, TLS1_3_VERSION)
        end select

        ! Create SSL object
        this%ssl_handle = SSL_new(this%ssl_ctx)
        if (.not. c_associated(this%ssl_handle%ptr)) return

        ! Set socket file descriptor
        call SSL_set_fd(this%ssl_handle, this%socket_handle)

        this%ssl_initialized = .true.
    end subroutine sslsocket_init_ssl

    subroutine sslsocket_cleanup_ssl(this)
        class(QSslSocket), intent(inout) :: this
        if (c_associated(this%ssl_handle%ptr)) then
            call SSL_free(this%ssl_handle)
            this%ssl_handle%ptr = c_null_ptr
        end if
        if (c_associated(this%ssl_ctx%ptr)) then
            call SSL_CTX_free(this%ssl_ctx)
            this%ssl_ctx%ptr = c_null_ptr
        end if
        this%ssl_initialized = .false.
        this%handshake_completed = .false.
    end subroutine sslsocket_cleanup_ssl

    ! ========== QSslError Implementation ==========

    function sslerror_error(this) result(code)
        class(QSslError), intent(in) :: this
        integer :: code
        code = this%error_code
    end function sslerror_error

    function sslerror_error_string(this) result(str)
        class(QSslError), intent(in) :: this
        character(len=:), allocatable :: str
        str = this%error_string
    end function sslerror_error_string

    function sslerror_certificate(this) result(cert)
        class(QSslError), intent(in) :: this
        type(QSslCertificate) :: cert
        cert = this%certificate
    end function sslerror_certificate

    ! ========== QSslCertificateExtension Implementation ==========

    function sslcertificateextension_name(this) result(name)
        class(QSslCertificateExtension), intent(in) :: this
        character(len=:), allocatable :: name
        name = this%name
    end function sslcertificateextension_name

    function sslcertificateextension_value(this) result(val)
        class(QSslCertificateExtension), intent(in) :: this
        type(QVariant) :: val
        val = this%value
    end function sslcertificateextension_value

    function sslcertificateextension_is_critical(this) result(critical)
        class(QSslCertificateExtension), intent(in) :: this
        logical :: critical
        critical = this%is_critical
    end function sslcertificateextension_is_critical

    function sslcertificateextension_is_supported(this) result(supported)
        class(QSslCertificateExtension), intent(in) :: this
        logical :: supported
        supported = this%is_supported
    end function sslcertificateextension_is_supported

    ! Helper functions (would need to be implemented with proper OpenSSL bindings)

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
        do while (f_ptr(i:i) /= achar(0) .and. i < 