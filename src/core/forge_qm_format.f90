!> @brief QM file format implementation for compiled Qt translation files
!> @details Binary format parsing and generation of .qm translation files
!> @author ForGE Contributors
!> @date 2025
!> @license GPL-3.0-or-later

module forge_qm_format
    use iso_c_binding
    use iso_fortran_env, only: int32, int64, real32, real64
    use forge_string_utils, only: QString
    use forge_translator, only: QTranslator, QTranslatorMessage
    implicit none
    private

    public :: QMReader, QMWriter

    ! QM file magic number
    integer, parameter :: QM_MAGIC = z'3F3F3F00'  ! "???\0"

    ! QM file sections
    enum, bind(c)
        enumerator :: QM_Section_Hash = 0
        enumerator :: QM_Section_Messages = 1
        enumerator :: QM_Section_Context = 2
        enumerator :: QM_Section_Contexts = 3
    end enum

    !> @brief QM file header structure
    type :: QMHeader
        integer(int32) :: magic = QM_MAGIC
        integer(int32) :: version = 1
        integer(int32) :: num_messages = 0
        integer(int32) :: num_contexts = 0
        integer(int32) :: offset_hash = 0
        integer(int32) :: offset_messages = 0
        integer(int32) :: offset_contexts = 0
    end type QMHeader

    !> @brief QM file reader
    type :: QMReader
        private
        character(len=:), allocatable :: data_
        integer :: position_ = 1
        type(QMHeader) :: header_
    contains
        ! Reading methods
        procedure :: read => qmreader_read
        procedure :: read_file => qmreader_read_file
        procedure :: read_data => qmreader_read_data
        procedure :: parse_header => qmreader_parse_header
        procedure :: parse_messages => qmreader_parse_messages
        procedure :: parse_contexts => qmreader_parse_contexts

        ! Helper methods
        procedure, private :: read_int32 => qmreader_read_int32
        procedure, private :: read_string => qmreader_read_string
        procedure, private :: read_bytes => qmreader_read_bytes
    end type QMReader

    !> @brief QM file writer
    type :: QMWriter
        private
        character(len=:), allocatable :: data_
        integer :: position_ = 1
        type(QMHeader) :: header_
    contains
        ! Writing methods
        procedure :: write => qmwriter_write
        procedure :: write_file => qmwriter_write_file
        procedure :: write_data => qmwriter_write_data
        procedure :: build_header => qmwriter_build_header
        procedure :: build_messages => qmwriter_build_messages
        procedure :: build_contexts => qmwriter_build_contexts
        procedure :: build_hash => qmwriter_build_hash

        ! Helper methods
        procedure, private :: write_int32 => qmwriter_write_int32
        procedure, private :: write_string => qmwriter_write_string
        procedure, private :: write_bytes => qmwriter_write_bytes
        procedure, private :: ensure_capacity => qmwriter_ensure_capacity
    end type QMWriter

contains

    ! ========== QMReader Implementation ==========

    !> @brief Read QM file into translator
    function qmreader_read(this, translator, filename) result(success)
        class(QMReader), intent(inout) :: this
        type(QTranslator), intent(inout) :: translator
        character(len=*), intent(in) :: filename
        logical :: success

        success = this%read_file(filename)
        if (success) then
            success = this%read_data(translator)
        end if
    end function qmreader_read

    !> @brief Read QM file from disk
    function qmreader_read_file(this, filename) result(success)
        class(QMReader), intent(inout) :: this
        character(len=*), intent(in) :: filename
        logical :: success
        integer :: unit, iostat, file_size

        open(newunit=unit, file=filename, status='old', action='read', &
             iostat=iostat, form='unformatted', access='stream')
        if (iostat /= 0) then
            success = .false.
            return
        end if

        inquire(unit, size=file_size)
        if (file_size > 0) then
            allocate(character(len=file_size) :: this%data_)
            read(unit, iostat=iostat) this%data_
            success = (iostat == 0)
        else
            success = .false.
        end if

        close(unit)
        this%position_ = 1
    end function qmreader_read_file

    !> @brief Parse QM data into translator
    function qmreader_read_data(this, translator) result(success)
        class(QMReader), intent(inout) :: this
        type(QTranslator), intent(inout) :: translator
        logical :: success

        success = this%parse_header()
        if (.not. success) return

        success = this%parse_contexts(translator)
        if (.not. success) return

        success = this%parse_messages(translator)
    end function qmreader_read_data

    !> @brief Parse QM header
    function qmreader_parse_header(this) result(success)
        class(QMReader), intent(inout) :: this
        logical :: success

        if (len(this%data_) < 28) then  ! Size of QMHeader
            success = .false.
            return
        end if

        this%header_%magic = this%read_int32()
        if (this%header_%magic /= QM_MAGIC) then
            success = .false.
            return
        end if

        this%header_%version = this%read_int32()
        this%header_%num_messages = this%read_int32()
        this%header_%num_contexts = this%read_int32()
        this%header_%offset_hash = this%read_int32()
        this%header_%offset_messages = this%read_int32()
        this%header_%offset_contexts = this%read_int32()

        success = .true.
    end function qmreader_parse_header

    !> @brief Parse contexts section
    function qmreader_parse_contexts(this, translator) result(success)
        class(QMReader), intent(inout) :: this
        type(QTranslator), intent(inout) :: translator
        logical :: success
        integer :: i, context_len
        character(len=:), allocatable :: context_name

        success = .true.
        if (this%header_%offset_contexts == 0) return

        this%position_ = this%header_%offset_contexts + 1

        do i = 1, this%header_%num_contexts
            context_len = this%read_int32()
            context_name = this%read_string(context_len)
            ! Store context for later use
        end do
    end function qmreader_parse_contexts

    !> @brief Parse messages section
    function qmreader_parse_messages(this, translator) result(success)
        class(QMReader), intent(inout) :: this
        type(QTranslator), intent(inout) :: translator
        logical :: success
        integer :: i, source_len, translation_len, context_idx
        character(len=:), allocatable :: source_text, translation_text
        type(QTranslatorMessage) :: message

        success = .true.
        if (this%header_%offset_messages == 0) return

        this%position_ = this%header_%offset_messages + 1

        do i = 1, this%header_%num_messages
            context_idx = this%read_int32()
            source_len = this%read_int32()
            source_text = this%read_string(source_len)
            translation_len = this%read_int32()
            translation_text = this%read_string(translation_len)

            call message%set_source_text(source_text)
            call message%set_translation(translation_text)
            ! Set context from context_idx

            call translator%insert(message)
        end do
    end function qmreader_parse_messages

    !> @brief Read 32-bit integer
    function qmreader_read_int32(this) result(value)
        class(QMReader), intent(inout) :: this
        integer(int32) :: value

        if (this%position_ + 3 <= len(this%data_)) then
            value = transfer(this%data_(this%position_:this%position_+3), value)
            this%position_ = this%position_ + 4
        else
            value = 0
        end if
    end function qmreader_read_int32

    !> @brief Read string of given length
    function qmreader_read_string(this, length) result(str)
        class(QMReader), intent(inout) :: this
        integer, intent(in) :: length
        character(len=:), allocatable :: str

        if (length > 0 .and. this%position_ + length - 1 <= len(this%data_)) then
            str = this%data_(this%position_:this%position_+length-1)
            this%position_ = this%position_ + length
        else
            str = ''
        end if
    end function qmreader_read_string

    !> @brief Read bytes
    function qmreader_read_bytes(this, count) result(bytes)
        class(QMReader), intent(inout) :: this
        integer, intent(in) :: count
        character(len=:), allocatable :: bytes

        bytes = this%read_string(count)
    end function qmreader_read_bytes

    ! ========== QMWriter Implementation ==========

    !> @brief Write translator to QM file
    function qmwriter_write(this, translator, filename) result(success)
        class(QMWriter), intent(inout) :: this
        type(QTranslator), intent(in) :: translator
        character(len=*), intent(in) :: filename
        logical :: success
        character(len=:), allocatable :: data

        data = this%write_data(translator)
        success = this%write_file(data, filename)
    end function qmwriter_write

    !> @brief Write QM data to file
    function qmwriter_write_file(this, data, filename) result(success)
        class(QMWriter), intent(in) :: this
        character(len=*), intent(in) :: data
        character(len=*), intent(in) :: filename
        logical :: success
        integer :: unit, iostat

        open(newunit=unit, file=filename, status='replace', action='write', &
             iostat=iostat, form='unformatted', access='stream')
        if (iostat /= 0) then
            success = .false.
            return
        end if

        write(unit, iostat=iostat) data
        success = (iostat == 0)
        close(unit)
    end function qmwriter_write_file

    !> @brief Generate QM data from translator
    function qmwriter_write_data(this, translator) result(data)
        class(QMWriter), intent(inout) :: this
        type(QTranslator), intent(in) :: translator
        character(len=:), allocatable :: data

        ! Initialize
        this%position_ = 1
        this%header_%num_messages = translator%count()
        this%header_%num_contexts = 1  ! Simplified

        ! Build sections
        call this%build_header()
        call this%build_contexts(translator)
        call this%build_messages(translator)
        call this%build_hash(translator)

        ! Update header offsets
        this%header_%offset_hash = 28  ! After header
        this%header_%offset_messages = this%header_%offset_hash + 100  ! Simplified
        this%header_%offset_contexts = this%header_%offset_messages + 200  ! Simplified

        ! Write final data
        data = this%data_
    end function qmwriter_write_data

    !> @brief Build QM header
    subroutine qmwriter_build_header(this)
        class(QMWriter), intent(inout) :: this

        call this%ensure_capacity(28)
        call this%write_int32(this%header_%magic)
        call this%write_int32(this%header_%version)
        call this%write_int32(this%header_%num_messages)
        call this%write_int32(this%header_%num_contexts)
        call this%write_int32(this%header_%offset_hash)
        call this%write_int32(this%header_%offset_messages)
        call this%write_int32(this%header_%offset_contexts)
    end subroutine qmwriter_build_header

    !> @brief Build contexts section
    subroutine qmwriter_build_contexts(this, translator)
        class(QMWriter), intent(inout) :: this
        type(QTranslator), intent(in) :: translator

        ! Simplified - write one default context
        call this%write_int32(11)  ! Length of "MainWindow"
        call this%write_string("MainWindow")
    end subroutine qmwriter_build_contexts

    !> @brief Build messages section
    subroutine qmwriter_build_messages(this, translator)
        class(QMWriter), intent(inout) :: this
        type(QTranslator), intent(in) :: translator
        integer :: i
        character(len=:), allocatable :: source, translation

        ! Simplified - would iterate through all messages
        do i = 1, translator%count()
            ! Write context index (0 for default)
            call this%write_int32(0)

            ! Write source text
            source = "Hello"  ! Would get from message
            call this%write_int32(len(source))
            call this%write_string(source)

            ! Write translation
            translation = "Hola"  ! Would get from message
            call this%write_int32(len(translation))
            call this%write_string(translation)
        end do
    end subroutine qmwriter_build_messages

    !> @brief Build hash section
    subroutine qmwriter_build_hash(this, translator)
        class(QMWriter), intent(inout) :: this
        type(QTranslator), intent(in) :: translator

        ! Simplified hash table
        call this%write_int32(0)  ! Hash size
    end subroutine qmwriter_build_hash

    !> @brief Write 32-bit integer
    subroutine qmwriter_write_int32(this, value)
        class(QMWriter), intent(inout) :: this
        integer(int32), intent(in) :: value

        call this%ensure_capacity(4)
        this%data_(this%position_:this%position_+3) = transfer(value, this%data_(this%position_:this%position_+3))
        this%position_ = this%position_ + 4
    end subroutine qmwriter_write_int32

    !> @brief Write string
    subroutine qmwriter_write_string(this, str)
        class(QMWriter), intent(inout) :: this
        character(len=*), intent(in) :: str

        call this%ensure_capacity(len(str))
        this%data_(this%position_:this%position_+len(str)-1) = str
        this%position_ = this%position_ + len(str)
    end subroutine qmwriter_write_string

    !> @brief Write bytes
    subroutine qmwriter_write_bytes(this, bytes)
        class(QMWriter), intent(inout) :: this
        character(len=*), intent(in) :: bytes

        call this%write_string(bytes)
    end subroutine qmwriter_write_bytes

    !> @brief Ensure data buffer has enough capacity
    subroutine qmwriter_ensure_capacity(this, additional_bytes)
        class(QMWriter), intent(inout) :: this
        integer, intent(in) :: additional_bytes
        character(len=:), allocatable :: temp_data
        integer :: new_size

        if (.not. allocated(this%data_)) then
            allocate(character(len=1024) :: this%data_)
            return
        end if

        if (this%position_ + additional_bytes - 1 > len(this%data_)) then
            new_size = max(len(this%data_) * 2, this%position_ + additional_bytes)
            allocate(character(len=new_size) :: temp_data)
            temp_data(1:len(this%data_)) = this%data_
            call move_alloc(temp_data, this%data_)
        end if
    end subroutine qmwriter_ensure_capacity

end module forge_qm_format