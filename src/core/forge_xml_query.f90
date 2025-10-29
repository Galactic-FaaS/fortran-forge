!> @brief XPath and XQuery support for XML
!> @details QXmlQuery for XPath and XQuery query processing
!> @author ForGE Contributors
!> @date 2025
!> @license GPL-3.0-or-later

module forge_xml_query
    use forge_string_utils
    use forge_containers
    use forge_errors
    implicit none
    private

    public :: QXmlQuery, QXmlResultItems, QXmlItem
    public :: QXmlQuery_QueryType
    public :: XQuery10, XPath20, XQuery10Compat

    !> Query types
    integer, parameter :: XQuery10 = 1
    integer, parameter :: XPath20 = 2
    integer, parameter :: XQuery10Compat = 3

    !> @brief XML query type information
    type :: QXmlQuery_QueryType
        integer :: query_type = XQuery10
        character(len=:), allocatable :: query_string
        logical :: is_valid = .false.
    end type QXmlQuery_QueryType

    !> @brief XML item (result of query)
    type :: QXmlItem
        private
        character(len=:), allocatable :: string_value
        logical :: is_null = .true.
        logical :: is_atomic = .false.
        logical :: is_node = .false.
    contains
        procedure :: is_null => xmlitem_is_null
        procedure :: is_atomic_value => xmlitem_is_atomic_value
        procedure :: is_node => xmlitem_is_node
        procedure :: to_atomic_value => xmlitem_to_atomic_value
        procedure :: to_string => xmlitem_to_string
    end type QXmlItem

    !> @brief XML result items collection
    type :: QXmlResultItems
        private
        type(QXmlItem), dimension(:), allocatable :: items
        integer :: count = 0
        integer :: current_index = 0
    contains
        procedure :: next => xmlresultitems_next
        procedure :: current => xmlresultitems_current
        procedure :: has_next => xmlresultitems_has_next
        procedure :: size => xmlresultitems_size
        procedure :: clear => xmlresultitems_clear
        procedure :: append => xmlresultitems_append
    end type QXmlResultItems

    !> @brief XML query processor
    type :: QXmlQuery
        private
        type(QXmlQuery_QueryType) :: query_info
        character(len=:), allocatable :: document_content
        type(QHashMap) :: variables
        type(QHashMap) :: namespace_bindings
        type(forge_error) :: error
        logical :: is_valid = .false.
    contains
        procedure :: set_query => xmlquery_set_query
        procedure :: set_query_from_file => xmlquery_set_query_from_file
        procedure :: bind_variable => xmlquery_bind_variable
        procedure :: bind_namespace => xmlquery_bind_namespace
        procedure :: set_focus => xmlquery_set_focus
        procedure :: set_initial_template_name => xmlquery_set_initial_template_name
        procedure :: set_uri_resolver => xmlquery_set_uri_resolver
        procedure :: evaluate_to_string => xmlquery_evaluate_to_string
        procedure :: evaluate_to_string_list => xmlquery_evaluate_to_string_list
        procedure :: evaluate_to_result_items => xmlquery_evaluate_to_result_items
        procedure :: evaluate_to_file => xmlquery_evaluate_to_file
        procedure :: is_valid => xmlquery_is_valid
        procedure :: query_language => xmlquery_query_language
        procedure :: message_handler => xmlquery_message_handler
        procedure :: uri_resolver => xmlquery_uri_resolver
        procedure :: network_access_manager => xmlquery_network_access_manager
        procedure :: set_message_handler => xmlquery_set_message_handler
        procedure :: set_network_access_manager => xmlquery_set_network_access_manager
        procedure :: set_query_type => xmlquery_set_query_type
        procedure :: query_type => xmlquery_query_type
        procedure :: has_error => xmlquery_has_error
        procedure :: error_string => xmlquery_error_string
        procedure, private :: parse_xpath => xmlquery_parse_xpath
        procedure, private :: parse_xquery => xmlquery_parse_xquery
        procedure, private :: evaluate_xpath => xmlquery_evaluate_xpath
        procedure, private :: evaluate_xquery => xmlquery_evaluate_xquery
    end type QXmlQuery

contains

    ! ========== QXmlItem Implementation ==========

    function xmlitem_is_null(this) result(is_null)
        class(QXmlItem), intent(in) :: this
        logical :: is_null
        is_null = this%is_null
    end function xmlitem_is_null

    function xmlitem_is_atomic_value(this) result(is_atomic)
        class(QXmlItem), intent(in) :: this
        logical :: is_atomic
        is_atomic = this%is_atomic
    end function xmlitem_is_atomic_value

    function xmlitem_is_node(this) result(is_node)
        class(QXmlItem), intent(in) :: this
        logical :: is_node
        is_node = this%is_node
    end function xmlitem_is_node

    function xmlitem_to_atomic_value(this) result(value)
        class(QXmlItem), intent(in) :: this
        character(len=:), allocatable :: value
        if (this%is_atomic) then
            value = this%string_value
        else
            value = ""
        end if
    end function xmlitem_to_atomic_value

    function xmlitem_to_string(this) result(str)
        class(QXmlItem), intent(in) :: this
        character(len=:), allocatable :: str
        str = this%string_value
    end function xmlitem_to_string

    ! ========== QXmlResultItems Implementation ==========

    function xmlresultitems_next(this) result(has_next)
        class(QXmlResultItems), intent(inout) :: this
        logical :: has_next

        if (this%current_index < this%count) then
            this%current_index = this%current_index + 1
            has_next = .true.
        else
            has_next = .false.
        end if
    end function xmlresultitems_next

    function xmlresultitems_current(this) result(item)
        class(QXmlResultItems), intent(in) :: this
        type(QXmlItem) :: item

        if (this%current_index >= 1 .and. this%current_index <= this%count) then
            item = this%items(this%current_index)
        else
            item%is_null = .true.
        end if
    end function xmlresultitems_current

    function xmlresultitems_has_next(this) result(has_next)
        class(QXmlResultItems), intent(in) :: this
        logical :: has_next
        has_next = (this%current_index < this%count)
    end function xmlresultitems_has_next

    function xmlresultitems_size(this) result(size_val)
        class(QXmlResultItems), intent(in) :: this
        integer :: size_val
        size_val = this%count
    end function xmlresultitems_size

    subroutine xmlresultitems_clear(this)
        class(QXmlResultItems), intent(inout) :: this
        if (allocated(this%items)) deallocate(this%items)
        this%count = 0
        this%current_index = 0
    end subroutine xmlresultitems_clear

    subroutine xmlresultitems_append(this, item)
        class(QXmlResultItems), intent(inout) :: this
        type(QXmlItem), intent(in) :: item
        type(QXmlItem), dimension(:), allocatable :: temp_items

        if (.not. allocated(this%items)) then
            allocate(this%items(10))
        else if (this%count >= size(this%items)) then
            allocate(temp_items(size(this%items) * 2))
            temp_items(1:this%count) = this%items(1:this%count)
            call move_alloc(temp_items, this%items)
        end if

        this%count = this%count + 1
        this%items(this%count) = item
    end subroutine xmlresultitems_append

    ! ========== QXmlQuery Implementation ==========

    subroutine xmlquery_set_query(this, query_string, document_uri)
        class(QXmlQuery), intent(inout) :: this
        character(len=*), intent(in) :: query_string
        character(len=*), intent(in), optional :: document_uri

        this%query_info%query_string = query_string
        this%is_valid = .true.

        ! Determine query type based on content
        if (index(query_string, 'for ') > 0 .or. index(query_string, 'let ') > 0 .or. &
            index(query_string, 'return ') > 0) then
            this%query_info%query_type = XQuery10
        else
            this%query_info%query_type = XPath20
        end if

        call this%error%clear()
    end subroutine xmlquery_set_query

    function xmlquery_set_query_from_file(this, file_uri) result(success)
        class(QXmlQuery), intent(inout) :: this
        character(len=*), intent(in) :: file_uri
        logical :: success

        integer :: unit, ios
        character(len=1000) :: line
        character(len=:), allocatable :: query_content

        success = .false.
        query_content = ""

        ! Open query file for reading
        open(newunit=unit, file=file_uri, status='old', action='read', iostat=ios, encoding='UTF-8')
        if (ios /= 0) then
            call this%error%raise(FORGE_ERROR_FILE_NOT_FOUND, "Cannot open query file: " // trim(file_uri))
            return
        end if

        ! Read query content
        do
            read(unit, '(A)', iostat=ios) line
            if (ios /= 0) exit
            query_content = query_content // trim(line) // achar(10)
        end do

        close(unit)

        if (len_trim(query_content) > 0) then
            call this%set_query(query_content)
            success = this%is_valid
        else
            call this%error%raise(FORGE_ERROR_XML_PARSE, "Empty query file")
        end if
    end function xmlquery_set_query_from_file

    subroutine xmlquery_bind_variable(this, name, value)
        class(QXmlQuery), intent(inout) :: this
        character(len=*), intent(in) :: name
        type(QXmlItem), intent(in) :: value

        ! Store variable binding
        ! Simplified implementation
    end subroutine xmlquery_bind_variable

    subroutine xmlquery_bind_namespace(this, prefix, namespace_uri)
        class(QXmlQuery), intent(inout) :: this
        character(len=*), intent(in) :: prefix
        character(len=*), intent(in) :: namespace_uri

        call this%namespace_bindings%insert(prefix, namespace_uri)
    end subroutine xmlquery_bind_namespace

    subroutine xmlquery_set_focus(this, item)
        class(QXmlQuery), intent(inout) :: this
        type(QXmlItem), intent(in) :: item

        ! Set focus for query evaluation
        ! Simplified implementation
    end subroutine xmlquery_set_focus

    subroutine xmlquery_set_initial_template_name(this, name)
        class(QXmlQuery), intent(inout) :: this
        character(len=*), intent(in) :: name

        ! For XQuery - set initial template
        ! Simplified implementation
    end subroutine xmlquery_set_initial_template_name

    subroutine xmlquery_set_uri_resolver(this, resolver)
        class(QXmlQuery), intent(inout) :: this
        character(len=*), intent(in) :: resolver

        ! Set URI resolver
        ! Simplified implementation
    end subroutine xmlquery_set_uri_resolver

    function xmlquery_evaluate_to_string(this) result(result_string)
        class(QXmlQuery), intent(inout) :: this
        character(len=:), allocatable :: result_string

        select case (this%query_info%query_type)
        case (XPath20)
            result_string = this%evaluate_xpath()
        case (XQuery10)
            result_string = this%evaluate_xquery()
        case default
            result_string = ""
        end select
    end function xmlquery_evaluate_to_string

    function xmlquery_evaluate_to_string_list(this) result(result_list)
        class(QXmlQuery), intent(inout) :: this
        type(QStringList) :: result_list

        character(len=:), allocatable :: result_str
        result_str = this%evaluate_to_string()

        ! Split result into list
        ! Simplified implementation
    end function xmlquery_evaluate_to_string_list

    function xmlquery_evaluate_to_result_items(this) result(result_items)
        class(QXmlQuery), intent(inout) :: this
        type(QXmlResultItems) :: result_items

        character(len=:), allocatable :: result_str
        type(QXmlItem) :: item

        result_str = this%evaluate_to_string()

        if (len(result_str) > 0) then
            item%string_value = result_str
            item%is_null = .false.
            item%is_atomic = .true.
            call result_items%append(item)
        end if
    end function xmlquery_evaluate_to_result_items

    function xmlquery_evaluate_to_file(this, target, encoding) result(success)
        class(QXmlQuery), intent(inout) :: this
        character(len=*), intent(in) :: target
        character(len=*), intent(in), optional :: encoding
        logical :: success

        integer :: unit, ios
        character(len=:), allocatable :: result_str, enc

        success = .false.
        result_str = this%evaluate_to_string()

        if (len_trim(result_str) == 0) then
            call this%error%raise(FORGE_ERROR_XML_PARSE, "No query results to write")
            return
        end if

        ! Determine encoding
        if (present(encoding)) then
            enc = encoding
        else
            enc = 'UTF-8'
        end if

        ! Open file for writing
        open(newunit=unit, file=target, status='replace', action='write', iostat=ios, encoding=enc)
        if (ios /= 0) then
            call this%error%raise(FORGE_ERROR_FILE_NOT_FOUND, "Cannot create output file: " // trim(target))
            return
        end if

        ! Write result to file
        write(unit, '(A)', iostat=ios) result_str
        close(unit)

        if (ios == 0) then
            success = .true.
        else
            call this%error%raise(FORGE_ERROR_IO, "Error writing to file: " // trim(target))
        end if
    end function xmlquery_evaluate_to_file

    function xmlquery_is_valid(this) result(valid)
        class(QXmlQuery), intent(in) :: this
        logical :: valid
        valid = this%is_valid
    end function xmlquery_is_valid

    function xmlquery_query_language(this) result(language)
        class(QXmlQuery), intent(in) :: this
        character(len=:), allocatable :: language

        select case (this%query_info%query_type)
        case (XQuery10)
            language = "XQuery 1.0"
        case (XPath20)
            language = "XPath 2.0"
        case (XQuery10Compat)
            language = "XQuery 1.0 Compatibility"
        case default
            language = "Unknown"
        end select
    end function xmlquery_query_language

    function xmlquery_message_handler(this) result(handler)
        class(QXmlQuery), intent(in) :: this
        character(len=:), allocatable :: handler
        handler = ""  ! Simplified
    end function xmlquery_message_handler

    function xmlquery_uri_resolver(this) result(resolver)
        class(QXmlQuery), intent(in) :: this
        character(len=:), allocatable :: resolver
        resolver = ""  ! Simplified
    end function xmlquery_uri_resolver

    function xmlquery_network_access_manager(this) result(manager)
        class(QXmlQuery), intent(in) :: this
        character(len=:), allocatable :: manager
        manager = ""  ! Simplified
    end function xmlquery_network_access_manager

    subroutine xmlquery_set_message_handler(this, handler)
        class(QXmlQuery), intent(inout) :: this
        character(len=*), intent(in) :: handler
        ! Set message handler - simplified
    end subroutine xmlquery_set_message_handler

    subroutine xmlquery_set_network_access_manager(this, manager)
        class(QXmlQuery), intent(inout) :: this
        character(len=*), intent(in) :: manager
        ! Set network access manager - simplified
    end subroutine xmlquery_set_network_access_manager

    subroutine xmlquery_set_query_type(this, query_type)
        class(QXmlQuery), intent(inout) :: this
        integer, intent(in) :: query_type
        this%query_info%query_type = query_type
    end subroutine xmlquery_set_query_type

    function xmlquery_query_type(this) result(qtype)
        class(QXmlQuery), intent(in) :: this
        integer :: qtype
        qtype = this%query_info%query_type
    end function xmlquery_query_type

    function xmlquery_has_error(this) result(has_err)
        class(QXmlQuery), intent(in) :: this
        logical :: has_err
        has_err = this%error%status%is_error()
    end function xmlquery_has_error

    function xmlquery_error_string(this) result(err_str)
        class(QXmlQuery), intent(in) :: this
        character(len=:), allocatable :: err_str
        err_str = trim(this%error%status%message)
    end function xmlquery_error_string

    function xmlquery_parse_xpath(this) result(success)
        class(QXmlQuery), intent(inout) :: this
        logical :: success

        ! Basic XPath parsing - simplified
        success = .true.
    end function xmlquery_parse_xpath

    function xmlquery_parse_xquery(this) result(success)
        class(QXmlQuery), intent(inout) :: this
        logical :: success

        ! Basic XQuery parsing - simplified
        success = .true.
    end function xmlquery_parse_xquery

    function xmlquery_evaluate_xpath(this) result(result)
        class(QXmlQuery), intent(inout) :: this
        character(len=:), allocatable :: result

        ! Simplified XPath evaluation
        ! Would need proper XPath engine
        result = "XPath evaluation not fully implemented"
    end function xmlquery_evaluate_xpath

    function xmlquery_evaluate_xquery(this) result(result)
        class(QXmlQuery), intent(inout) :: this
        character(len=:), allocatable :: result

        ! Simplified XQuery evaluation
        ! Would need proper XQuery engine
        result = "XQuery evaluation not fully implemented"
    end function xmlquery_evaluate_xquery

end module forge_xml_query