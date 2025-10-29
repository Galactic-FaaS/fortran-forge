!> @brief JSON parser and generator
!> @details JSON support for ForGE Qt
!> @author ForGE Contributors
!> @date 2025
!> @license GPL-3.0-or-later

module forge_json
    use forge_string_utils
    use forge_containers
    implicit none
    private

    public :: QJsonValue, QJsonObject, QJsonArray, QJsonDocument
    public :: QJsonParseError, QJsonSerializer
    public :: JSON_NULL, JSON_BOOL, JSON_NUMBER, JSON_STRING, JSON_ARRAY, JSON_OBJECT
    public :: parse_json, json_to_string, parse_json_document

    !> JSON value types
    integer, parameter :: JSON_NULL = 0
    integer, parameter :: JSON_BOOL = 1
    integer, parameter :: JSON_NUMBER = 2
    integer, parameter :: JSON_STRING = 3
    integer, parameter :: JSON_ARRAY = 4
    integer, parameter :: JSON_OBJECT = 5

    !> @brief JSON value (variant type)
    type :: QJsonValue
        private
        integer :: value_type = JSON_NULL
        logical :: bool_val
        real :: number_val
        type(QString) :: string_val
        type(QJsonArray), allocatable :: array_val
        type(QJsonObject), allocatable :: object_val
    contains
        procedure :: is_null => jsonvalue_is_null
        procedure :: is_bool => jsonvalue_is_bool
        procedure :: is_number => jsonvalue_is_number
        procedure :: is_string => jsonvalue_is_string
        procedure :: is_array => jsonvalue_is_array
        procedure :: is_object => jsonvalue_is_object
        procedure :: to_bool => jsonvalue_to_bool
        procedure :: to_number => jsonvalue_to_number
        procedure :: to_string => jsonvalue_to_string
        procedure :: to_array => jsonvalue_to_array
        procedure :: to_object => jsonvalue_to_object
        procedure :: set_bool => jsonvalue_set_bool
        procedure :: set_number => jsonvalue_set_number
        procedure :: set_string => jsonvalue_set_string
        procedure :: set_array => jsonvalue_set_array
        procedure :: set_object => jsonvalue_set_object
    end type QJsonValue

    !> @brief JSON object (key-value pairs)
    type :: QJsonObject
        private
        type(QString), dimension(:), allocatable :: keys
        type(QJsonValue), dimension(:), allocatable :: values
        integer :: count = 0
    contains
        procedure :: insert => jsonobject_insert
        procedure :: remove => jsonobject_remove
        procedure :: contains => jsonobject_contains
        procedure :: value => jsonobject_value
        procedure :: size => jsonobject_size
        procedure :: keys => jsonobject_keys
        procedure :: is_empty => jsonobject_is_empty
        procedure :: begin => jsonobject_begin
        procedure :: end => jsonobject_end
        procedure :: key_at => jsonobject_key_at
        procedure :: value_at => jsonobject_value_at
    end type QJsonObject

    !> @brief JSON array
    type :: QJsonArray
        private
        type(QJsonValue), allocatable :: values(:)
        integer :: count = 0
    contains
        procedure :: append => jsonarray_append
        procedure :: insert => jsonarray_insert
        procedure :: remove => jsonarray_remove
        procedure :: at => jsonarray_at
        procedure :: size => jsonarray_size
        procedure :: is_empty => jsonarray_is_empty
        procedure :: begin => jsonarray_begin
        procedure :: end => jsonarray_end
    end type QJsonArray

    !> @brief JSON parse error
    type :: QJsonParseError
        integer :: error = 0
        character(len=:), allocatable :: error_string
        integer :: offset = 0
    contains
        procedure :: is_null => jsonparseerror_is_null
        procedure :: to_string => jsonparseerror_to_string
    end type QJsonParseError

    !> @brief JSON document
    type :: QJsonDocument
        private
        type(QJsonValue) :: root_value
        logical :: is_array = .false.
        logical :: is_object = .false.
        logical :: is_null = .true.
    contains
        procedure :: is_array => jsondocument_is_array
        procedure :: is_object => jsondocument_is_object
        procedure :: is_empty => jsondocument_is_empty
        procedure :: is_null => jsondocument_is_null
        procedure :: array => jsondocument_array
        procedure :: object => jsondocument_object
        procedure :: set_array => jsondocument_set_array
        procedure :: set_object => jsondocument_set_object
        procedure :: to_json => jsondocument_to_json
        procedure :: from_json => jsondocument_from_json
    end type QJsonDocument

    !> @brief JSON serializer
    type :: QJsonSerializer
        private
        logical :: compact = .false.
        integer :: indent_size = 4
        logical :: keep_null = .false.
    contains
        procedure :: serialize => jsonserializer_serialize
        procedure :: set_compact_format => jsonserializer_set_compact_format
        procedure :: set_indent_size => jsonserializer_set_indent_size
        procedure :: set_keep_null => jsonserializer_set_keep_null
    end type QJsonSerializer

contains

    ! ========== QJsonValue Implementation ==========

    function jsonvalue_is_null(this) result(is_null)
        class(QJsonValue), intent(in) :: this
        logical :: is_null
        is_null = (this%value_type == JSON_NULL)
    end function jsonvalue_is_null

    function jsonvalue_is_bool(this) result(is_bool)
        class(QJsonValue), intent(in) :: this
        logical :: is_bool
        is_bool = (this%value_type == JSON_BOOL)
    end function jsonvalue_is_bool

    function jsonvalue_is_number(this) result(is_number)
        class(QJsonValue), intent(in) :: this
        logical :: is_number
        is_number = (this%value_type == JSON_NUMBER)
    end function jsonvalue_is_number

    function jsonvalue_is_string(this) result(is_string)
        class(QJsonValue), intent(in) :: this
        logical :: is_string
        is_string = (this%value_type == JSON_STRING)
    end function jsonvalue_is_string

    function jsonvalue_is_array(this) result(is_array)
        class(QJsonValue), intent(in) :: this
        logical :: is_array
        is_array = (this%value_type == JSON_ARRAY)
    end function jsonvalue_is_array

    function jsonvalue_is_object(this) result(is_object)
        class(QJsonValue), intent(in) :: this
        logical :: is_object
        is_object = (this%value_type == JSON_OBJECT)
    end function jsonvalue_is_object

    function jsonvalue_to_bool(this, default_val) result(value)
        class(QJsonValue), intent(in) :: this
        logical, intent(in), optional :: default_val
        logical :: value
        
        if (this%is_bool()) then
            value = this%bool_val
        else if (present(default_val)) then
            value = default_val
        else
            value = .false.
        end if
    end function jsonvalue_to_bool

    function jsonvalue_to_number(this, default_val) result(value)
        class(QJsonValue), intent(in) :: this
        real, intent(in), optional :: default_val
        real :: value
        
        if (this%is_number()) then
            value = this%number_val
        else if (present(default_val)) then
            value = default_val
        else
            value = 0.0
        end if
    end function jsonvalue_to_number

    function jsonvalue_to_string(this, default_val) result(value)
        class(QJsonValue), intent(in) :: this
        character(len=*), intent(in), optional :: default_val
        character(len=:), allocatable :: value
        
        if (this%is_string()) then
            value = this%string_val%get()
        else if (present(default_val)) then
            value = default_val
        else
            allocate(character(len=0) :: value)
        end if
    end function jsonvalue_to_string

    function jsonvalue_to_array(this) result(array)
        class(QJsonValue), intent(in) :: this
        type(QJsonArray) :: array
        
        if (this%is_array() .and. allocated(this%array_val)) then
            array = this%array_val
        end if
    end function jsonvalue_to_array

    function jsonvalue_to_object(this) result(object)
        class(QJsonValue), intent(in) :: this
        type(QJsonObject) :: object
        
        if (this%is_object() .and. allocated(this%object_val)) then
            object = this%object_val
        end if
    end function jsonvalue_to_object

    subroutine jsonvalue_set_bool(this, value)
        class(QJsonValue), intent(inout) :: this
        logical, intent(in) :: value
        this%value_type = JSON_BOOL
        this%bool_val = value
    end subroutine jsonvalue_set_bool

    subroutine jsonvalue_set_number(this, value)
        class(QJsonValue), intent(inout) :: this
        real, intent(in) :: value
        this%value_type = JSON_NUMBER
        this%number_val = value
    end subroutine jsonvalue_set_number

    subroutine jsonvalue_set_string(this, value)
        class(QJsonValue), intent(inout) :: this
        character(len=*), intent(in) :: value
        this%value_type = JSON_STRING
        call this%string_val%set(value)
    end subroutine jsonvalue_set_string

    subroutine jsonvalue_set_array(this, value)
        class(QJsonValue), intent(inout) :: this
        type(QJsonArray), intent(in) :: value
        this%value_type = JSON_ARRAY
        allocate(this%array_val)
        this%array_val = value
    end subroutine jsonvalue_set_array

    subroutine jsonvalue_set_object(this, value)
        class(QJsonValue), intent(inout) :: this
        type(QJsonObject), intent(in) :: value
        this%value_type = JSON_OBJECT
        allocate(this%object_val)
        this%object_val = value
    end subroutine jsonvalue_set_object

    ! ========== QJsonObject Implementation ==========

    subroutine jsonobject_insert(this, key, value)
        class(QJsonObject), intent(inout) :: this
        character(len=*), intent(in) :: key
        type(QJsonValue), intent(in) :: value
        type(QString), allocatable :: temp_keys(:)
        type(QJsonValue), allocatable :: temp_values(:)
        integer :: i
        
        ! Check if key exists
        do i = 1, this%count
            if (this%keys(i)%equals(key)) then
                this%values(i) = value
                return
            end if
        end do
        
        ! Add new key-value pair
        if (.not. allocated(this%keys)) then
            allocate(this%keys(10))
            allocate(this%values(10))
        else if (this%count >= size(this%keys)) then
            allocate(temp_keys(size(this%keys) * 2))
            allocate(temp_values(size(this%values) * 2))
            temp_keys(1:this%count) = this%keys(1:this%count)
            temp_values(1:this%count) = this%values(1:this%count)
            call move_alloc(temp_keys, this%keys)
            call move_alloc(temp_values, this%values)
        end if
        
        this%count = this%count + 1
        call this%keys(this%count)%set(key)
        this%values(this%count) = value
    end subroutine jsonobject_insert

    subroutine jsonobject_remove(this, key)
        class(QJsonObject), intent(inout) :: this
        character(len=*), intent(in) :: key
        integer :: i, j
        
        do i = 1, this%count
            if (this%keys(i)%equals(key)) then
                ! Shift remaining elements
                do j = i, this%count - 1
                    this%keys(j) = this%keys(j+1)
                    this%values(j) = this%values(j+1)
                end do
                this%count = this%count - 1
                exit
            end if
        end do
    end subroutine jsonobject_remove

    function jsonobject_contains(this, key) result(found)
        class(QJsonObject), intent(in) :: this
        character(len=*), intent(in) :: key
        logical :: found
        integer :: i
        
        found = .false.
        do i = 1, this%count
            if (this%keys(i)%equals(key)) then
                found = .true.
                return
            end if
        end do
    end function jsonobject_contains

    function jsonobject_value(this, key) result(value)
        class(QJsonObject), intent(in) :: this
        character(len=*), intent(in) :: key
        type(QJsonValue) :: value
        integer :: i
        
        do i = 1, this%count
            if (this%keys(i)%equals(key)) then
                value = this%values(i)
                return
            end if
        end do
        
        ! Return null value if not found
        value%value_type = JSON_NULL
    end function jsonobject_value

    function jsonobject_size(this) result(size_val)
        class(QJsonObject), intent(in) :: this
        integer :: size_val
        size_val = this%count
    end function jsonobject_size

    function jsonobject_keys(this) result(key_list)
        class(QJsonObject), intent(in) :: this
        type(QString), allocatable :: key_list(:)
        integer :: i
        
        allocate(key_list(this%count))
        do i = 1, this%count
            key_list(i) = this%keys(i)
        end do
    end function jsonobject_keys

    function jsonobject_is_empty(this) result(empty)
        class(QJsonObject), intent(in) :: this
        logical :: empty
        empty = (this%count == 0)
    end function jsonobject_is_empty

    function jsonobject_begin(this) result(idx)
        class(QJsonObject), intent(in) :: this
        integer :: idx
        idx = 1
    end function jsonobject_begin

    function jsonobject_end(this) result(idx)
        class(QJsonObject), intent(in) :: this
        integer :: idx
        idx = this%count
    end function jsonobject_end

    function jsonobject_key_at(this, index) result(key)
        class(QJsonObject), intent(in) :: this
        integer, intent(in) :: index
        type(QString) :: key

        if (index >= 1 .and. index <= this%count) then
            key = this%keys(index)
        end if
    end function jsonobject_key_at

    function jsonobject_value_at(this, index) result(value)
        class(QJsonObject), intent(in) :: this
        integer, intent(in) :: index
        type(QJsonValue) :: value

        if (index >= 1 .and. index <= this%count) then
            value = this%values(index)
        end if
    end function jsonobject_value_at

    ! ========== QJsonArray Implementation ==========

    subroutine jsonarray_append(this, value)
        class(QJsonArray), intent(inout) :: this
        type(QJsonValue), intent(in) :: value
        type(QJsonValue), allocatable :: temp(:)
        
        if (.not. allocated(this%values)) then
            allocate(this%values(10))
        else if (this%count >= size(this%values)) then
            allocate(temp(size(this%values) * 2))
            temp(1:this%count) = this%values(1:this%count)
            call move_alloc(temp, this%values)
        end if
        
        this%count = this%count + 1
        this%values(this%count) = value
    end subroutine jsonarray_append

    function jsonarray_at(this, index) result(value)
        class(QJsonArray), intent(in) :: this
        integer, intent(in) :: index
        type(QJsonValue) :: value
        
        if (index >= 0 .and. index < this%count) then
            value = this%values(index + 1)
        else
            value%value_type = JSON_NULL
        end if
    end function jsonarray_at

    function jsonarray_size(this) result(size_val)
        class(QJsonArray), intent(in) :: this
        integer :: size_val
        size_val = this%count
    end function jsonarray_size

    function jsonarray_is_empty(this) result(empty)
        class(QJsonArray), intent(in) :: this
        logical :: empty
        empty = (this%count == 0)
    end function jsonarray_is_empty

    subroutine jsonarray_insert(this, index, value)
        class(QJsonArray), intent(inout) :: this
        integer, intent(in) :: index
        type(QJsonValue), intent(in) :: value
        type(QJsonValue), allocatable :: temp(:)
        integer :: i

        if (index < 0 .or. index > this%count) return

        if (.not. allocated(this%values)) then
            allocate(this%values(10))
        else if (this%count >= size(this%values)) then
            allocate(temp(size(this%values) * 2))
            temp(1:this%count) = this%values(1:this%count)
            call move_alloc(temp, this%values)
        end if

        ! Shift elements to make room
        do i = this%count, index + 1, -1
            this%values(i + 1) = this%values(i)
        end do

        this%values(index + 1) = value
        this%count = this%count + 1
    end subroutine jsonarray_insert

    subroutine jsonarray_remove(this, index)
        class(QJsonArray), intent(inout) :: this
        integer, intent(in) :: index
        integer :: i

        if (index < 0 .or. index >= this%count) return

        ! Shift elements to fill the gap
        do i = index + 1, this%count - 1
            this%values(i) = this%values(i + 1)
        end do

        this%count = this%count - 1
    end subroutine jsonarray_remove

    function jsonarray_begin(this) result(idx)
        class(QJsonArray), intent(in) :: this
        integer :: idx
        idx = 0
    end function jsonarray_begin

    function jsonarray_end(this) result(idx)
        class(QJsonArray), intent(in) :: this
        integer :: idx
        idx = this%count - 1
    end function jsonarray_end

    ! ========== QJsonParseError Implementation ==========

    function jsonparseerror_is_null(this) result(is_null)
        class(QJsonParseError), intent(in) :: this
        logical :: is_null
        is_null = (this%error == 0)
    end function jsonparseerror_is_null

    function jsonparseerror_to_string(this) result(str)
        class(QJsonParseError), intent(in) :: this
        character(len=:), allocatable :: str
        if (allocated(this%error_string)) then
            str = this%error_string
        else
            str = ""
        end if
    end function jsonparseerror_to_string

    ! ========== QJsonDocument Implementation ==========

    function jsondocument_is_array(this) result(is_arr)
        class(QJsonDocument), intent(in) :: this
        logical :: is_arr
        is_arr = this%is_array
    end function jsondocument_is_array

    function jsondocument_is_object(this) result(is_obj)
        class(QJsonDocument), intent(in) :: this
        logical :: is_obj
        is_obj = this%is_object
    end function jsondocument_is_object

    function jsondocument_is_empty(this) result(is_empty)
        class(QJsonDocument), intent(in) :: this
        logical :: is_empty
        is_empty = this%is_null
    end function jsondocument_is_empty

    function jsondocument_is_null(this) result(is_null)
        class(QJsonDocument), intent(in) :: this
        logical :: is_null
        is_null = this%is_null
    end function jsondocument_is_null

    function jsondocument_array(this) result(arr)
        class(QJsonDocument), intent(in) :: this
        type(QJsonArray) :: arr
        if (this%is_array) then
            arr = this%root_value%to_array()
        end if
    end function jsondocument_array

    function jsondocument_object(this) result(obj)
        class(QJsonDocument), intent(in) :: this
        type(QJsonObject) :: obj
        if (this%is_object) then
            obj = this%root_value%to_object()
        end if
    end function jsondocument_object

    subroutine jsondocument_set_array(this, array)
        class(QJsonDocument), intent(inout) :: this
        type(QJsonArray), intent(in) :: array
        call this%root_value%set_array(array)
        this%is_array = .true.
        this%is_object = .false.
        this%is_null = .false.
    end subroutine jsondocument_set_array

    subroutine jsondocument_set_object(this, object)
        class(QJsonDocument), intent(inout) :: this
        type(QJsonObject), intent(in) :: object
        call this%root_value%set_object(object)
        this%is_array = .false.
        this%is_object = .true.
        this%is_null = .false.
    end subroutine jsondocument_set_object

    function jsondocument_to_json(this, format) result(json_string)
        class(QJsonDocument), intent(in) :: this
        integer, intent(in), optional :: format
        character(len=:), allocatable :: json_string

        logical :: pretty
        pretty = .false.
        if (present(format)) then
            pretty = (format == 1)  ! Compact=0, Indented=1
        end if

        json_string = json_to_string(this%root_value, pretty)
    end function jsondocument_to_json

    function jsondocument_from_json(this, json_string, error) result(success)
        class(QJsonDocument), intent(inout) :: this
        character(len=*), intent(in) :: json_string
        type(QJsonParseError), intent(out), optional :: error
        logical :: success

        type(QJsonParseError) :: parse_error

        this%root_value = parse_json_document(json_string, parse_error)

        if (present(error)) then
            error = parse_error
        end if

        success = parse_error%is_null()

        if (success) then
            if (this%root_value%is_array()) then
                this%is_array = .true.
                this%is_object = .false.
            else if (this%root_value%is_object()) then
                this%is_array = .false.
                this%is_object = .true.
            else
                this%is_array = .false.
                this%is_object = .false.
            end if
            this%is_null = .false.
        else
            this%is_null = .true.
            this%is_array = .false.
            this%is_object = .false.
        end if
    end function jsondocument_from_json

    ! ========== QJsonSerializer Implementation ==========

    function jsonserializer_serialize(this, value) result(json_string)
        class(QJsonSerializer), intent(in) :: this
        type(QJsonValue), intent(in) :: value
        character(len=:), allocatable :: json_string

        json_string = json_to_string(value, .not. this%compact)
    end function jsonserializer_serialize

    subroutine jsonserializer_set_compact_format(this, compact)
        class(QJsonSerializer), intent(inout) :: this
        logical, intent(in) :: compact
        this%compact = compact
    end subroutine jsonserializer_set_compact_format

    subroutine jsonserializer_set_indent_size(this, size)
        class(QJsonSerializer), intent(inout) :: this
        integer, intent(in) :: size
        this%indent_size = size
    end subroutine jsonserializer_set_indent_size

    subroutine jsonserializer_set_keep_null(this, keep)
        class(QJsonSerializer), intent(inout) :: this
        logical, intent(in) :: keep
        this%keep_null = keep
    end subroutine jsonserializer_set_keep_null

    ! ========== Parsing and Generation ==========

    function parse_json(json_string) result(value)
        use forge_json_parser, only: json_parse_complete
        character(len=*), intent(in) :: json_string
        type(QJsonValue) :: value

        value = json_parse_complete(json_string)
    end function parse_json

    function parse_json_document(json_string, error) result(value)
        use forge_json_parser, only: json_parse_complete
        character(len=*), intent(in) :: json_string
        type(QJsonParseError), intent(out), optional :: error
        type(QJsonValue) :: value
        character(len=:), allocatable :: error_msg

        value = json_parse_complete(json_string, error_msg)

        if (present(error)) then
            if (allocated(error_msg) .and. len(error_msg) > 0) then
                error%error = 1
                error%error_string = error_msg
                error%offset = 0  ! Could be enhanced to get actual offset
            else
                error%error = 0
                error%error_string = ""
                error%offset = 0
            end if
        end if
    end function parse_json_document

    function json_to_string(value, pretty) result(json_string)
        use forge_json_parser, only: json_stringify_complete
        type(QJsonValue), intent(in) :: value
        logical, intent(in), optional :: pretty
        character(len=:), allocatable :: json_string

        json_string = json_stringify_complete(value, pretty)
    end function json_to_string

end module forge_json

