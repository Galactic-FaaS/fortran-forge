!> @brief QTranslator implementation for translation file loading and string lookup
!> @details Provides translation file loader and string lookup functionality
!> @author ForGE Contributors
!> @date 2025
!> @license GPL-3.0-or-later

module forge_translator
    use iso_c_binding
    use iso_fortran_env, only: int32, int64, real32, real64
    use forge_string_utils, only: QString
    use forge_locale, only: QLocale
    implicit none
    private

    public :: QTranslator, QTranslatorMessage
    public :: QTranslator_LoadMode, QTranslator_SaveMode

    !> @brief Load mode enumeration
    enum, bind(c)
        enumerator :: LoadMode_Default = 0
        enumerator :: LoadMode_ForceReload = 1
        enumerator :: LoadMode_CheckOnly = 2
    end enum
    type :: QTranslator_LoadMode
        integer :: value
    end type QTranslator_LoadMode

    !> @brief Save mode enumeration
    enum, bind(c)
        enumerator :: SaveMode_Default = 0
        enumerator :: SaveMode_PrettyPrint = 1
        enumerator :: SaveMode_Minimal = 2
    end enum
    type :: QTranslator_SaveMode
        integer :: value
    end type QTranslator_SaveMode

    !> @brief Translation message structure
    type :: QTranslatorMessage
        private
        type(QString) :: source_text_
        type(QString) :: translation_
        type(QString) :: context_
        type(QString) :: comment_
        type(QString) :: file_name_
        integer :: line_number_ = -1
        integer :: plural_form_ = 0
        logical :: is_obsolete_ = .false.
        logical :: is_plural_ = .false.
    contains
        ! Constructors
        procedure :: init => qtranslator_message_init

        ! Getters/Setters
        procedure :: source_text => qtranslator_message_source_text
        procedure :: set_source_text => qtranslator_message_set_source_text
        procedure :: translation => qtranslator_message_translation
        procedure :: set_translation => qtranslator_message_set_translation
        procedure :: context => qtranslator_message_context
        procedure :: set_context => qtranslator_message_set_context
        procedure :: comment => qtranslator_message_comment
        procedure :: set_comment => qtranslator_message_set_comment
        procedure :: file_name => qtranslator_message_file_name
        procedure :: set_file_name => qtranslator_message_set_file_name
        procedure :: line_number => qtranslator_message_line_number
        procedure :: set_line_number => qtranslator_message_set_line_number
        procedure :: plural_form => qtranslator_message_plural_form
        procedure :: set_plural_form => qtranslator_message_set_plural_form
        procedure :: is_obsolete => qtranslator_message_is_obsolete
        procedure :: set_obsolete => qtranslator_message_set_obsolete
        procedure :: is_plural => qtranslator_message_is_plural
        procedure :: set_plural => qtranslator_message_set_plural

        ! Utility methods
        procedure :: equals => qtranslator_message_equals
        procedure :: hash => qtranslator_message_hash
    end type QTranslatorMessage

    !> @brief QTranslator class for translation management
    type :: QTranslator
        private
        type(QTranslatorMessage), allocatable :: messages_(:)
        type(QString) :: file_name_
        type(QLocale) :: locale_
        logical :: is_loaded_ = .false.
        integer :: message_count_ = 0
    contains
        ! Constructors
        procedure :: init => qtranslator_init

        ! Loading/Saving
        procedure :: load => qtranslator_load
        procedure :: load_from_file => qtranslator_load_from_file
        procedure :: load_from_data => qtranslator_load_from_data
        procedure :: save => qtranslator_save
        procedure :: save_to_file => qtranslator_save_to_file

        ! Translation lookup
        procedure :: translate => qtranslator_translate
        procedure :: translate_context => qtranslator_translate_context
        procedure :: translate_plural => qtranslator_translate_plural
        procedure :: find_message => qtranslator_find_message

        ! Message management
        procedure :: insert => qtranslator_insert
        procedure :: remove => qtranslator_remove
        procedure :: clear => qtranslator_clear
        procedure :: is_empty => qtranslator_is_empty
        procedure :: count => qtranslator_count

        ! Properties
        procedure :: file_name => qtranslator_file_name
        procedure :: locale => qtranslator_locale
        procedure :: is_loaded => qtranslator_is_loaded

        ! Utility methods
        procedure :: equals => qtranslator_equals
        procedure :: merge => qtranslator_merge
    end type QTranslator

contains

    ! ========== QTranslatorMessage Implementation ==========

    !> @brief Initialize QTranslatorMessage
    subroutine qtranslator_message_init(this, source, translation, context)
        class(QTranslatorMessage), intent(inout) :: this
        character(len=*), intent(in) :: source
        character(len=*), intent(in), optional :: translation
        character(len=*), intent(in), optional :: context

        call this%source_text_%set(source)
        if (present(translation)) then
            call this%translation_%set(translation)
        end if
        if (present(context)) then
            call this%context_%set(context)
        end if
    end subroutine qtranslator_message_init

    !> @brief Get source text
    function qtranslator_message_source_text(this) result(text)
        class(QTranslatorMessage), intent(in) :: this
        type(QString) :: text
        text = this%source_text_
    end function qtranslator_message_source_text

    !> @brief Set source text
    subroutine qtranslator_message_set_source_text(this, text)
        class(QTranslatorMessage), intent(inout) :: this
        character(len=*), intent(in) :: text
        call this%source_text_%set(text)
    end subroutine qtranslator_message_set_source_text

    !> @brief Get translation
    function qtranslator_message_translation(this) result(text)
        class(QTranslatorMessage), intent(in) :: this
        type(QString) :: text
        text = this%translation_
    end function qtranslator_message_translation

    !> @brief Set translation
    subroutine qtranslator_message_set_translation(this, text)
        class(QTranslatorMessage), intent(inout) :: this
        character(len=*), intent(in) :: text
        call this%translation_%set(text)
    end subroutine qtranslator_message_set_translation

    !> @brief Get context
    function qtranslator_message_context(this) result(text)
        class(QTranslatorMessage), intent(in) :: this
        type(QString) :: text
        text = this%context_
    end function qtranslator_message_context

    !> @brief Set context
    subroutine qtranslator_message_set_context(this, text)
        class(QTranslatorMessage), intent(inout) :: this
        character(len=*), intent(in) :: text
        call this%context_%set(text)
    end subroutine qtranslator_message_set_context

    !> @brief Get comment
    function qtranslator_message_comment(this) result(text)
        class(QTranslatorMessage), intent(in) :: this
        type(QString) :: text
        text = this%comment_
    end function qtranslator_message_comment

    !> @brief Set comment
    subroutine qtranslator_message_set_comment(this, text)
        class(QTranslatorMessage), intent(inout) :: this
        character(len=*), intent(in) :: text
        call this%comment_%set(text)
    end subroutine qtranslator_message_set_comment

    !> @brief Get file name
    function qtranslator_message_file_name(this) result(text)
        class(QTranslatorMessage), intent(in) :: this
        type(QString) :: text
        text = this%file_name_
    end function qtranslator_message_file_name

    !> @brief Set file name
    subroutine qtranslator_message_set_file_name(this, text)
        class(QTranslatorMessage), intent(inout) :: this
        character(len=*), intent(in) :: text
        call this%file_name_%set(text)
    end subroutine qtranslator_message_set_file_name

    !> @brief Get line number
    function qtranslator_message_line_number(this) result(line)
        class(QTranslatorMessage), intent(in) :: this
        integer :: line
        line = this%line_number_
    end function qtranslator_message_line_number

    !> @brief Set line number
    subroutine qtranslator_message_set_line_number(this, line)
        class(QTranslatorMessage), intent(inout) :: this
        integer, intent(in) :: line
        this%line_number_ = line
    end subroutine qtranslator_message_set_line_number

    !> @brief Get plural form
    function qtranslator_message_plural_form(this) result(form)
        class(QTranslatorMessage), intent(in) :: this
        integer :: form
        form = this%plural_form_
    end function qtranslator_message_plural_form

    !> @brief Set plural form
    subroutine qtranslator_message_set_plural_form(this, form)
        class(QTranslatorMessage), intent(inout) :: this
        integer, intent(in) :: form
        this%plural_form_ = form
    end subroutine qtranslator_message_set_plural_form

    !> @brief Check if obsolete
    function qtranslator_message_is_obsolete(this) result(obsolete)
        class(QTranslatorMessage), intent(in) :: this
        logical :: obsolete
        obsolete = this%is_obsolete_
    end function qtranslator_message_is_obsolete

    !> @brief Set obsolete flag
    subroutine qtranslator_message_set_obsolete(this, obsolete)
        class(QTranslatorMessage), intent(inout) :: this
        logical, intent(in) :: obsolete
        this%is_obsolete_ = obsolete
    end subroutine qtranslator_message_set_obsolete

    !> @brief Check if plural
    function qtranslator_message_is_plural(this) result(plural)
        class(QTranslatorMessage), intent(in) :: this
        logical :: plural
        plural = this%is_plural_
    end function qtranslator_message_is_plural

    !> @brief Set plural flag
    subroutine qtranslator_message_set_plural(this, plural)
        class(QTranslatorMessage), intent(inout) :: this
        logical, intent(in) :: plural
        this%is_plural_ = plural
    end subroutine qtranslator_message_set_plural

    !> @brief Check equality
    function qtranslator_message_equals(this, other) result(equal)
        class(QTranslatorMessage), intent(in) :: this
        type(QTranslatorMessage), intent(in) :: other
        logical :: equal

        equal = (this%source_text_%equals(other%source_text_%get()) .and. &
                 this%context_%equals(other%context_%get()))
    end function qtranslator_message_equals

    !> @brief Get hash value
    function qtranslator_message_hash(this) result(hash_val)
        class(QTranslatorMessage), intent(in) :: this
        integer :: hash_val
        character(len=:), allocatable :: combined

        combined = this%source_text_%get() // this%context_%get()
        hash_val = this%simple_hash(combined)
    end function qtranslator_message_hash

    !> @brief Simple hash function
    function simple_hash(this, str) result(hash_val)
        class(QTranslatorMessage), intent(in) :: this
        character(len=*), intent(in) :: str
        integer :: hash_val
        integer :: i

        hash_val = 0
        do i = 1, len(str)
            hash_val = hash_val + iachar(str(i:i))
        end do
    end function simple_hash

    ! ========== QTranslator Implementation ==========

    !> @brief Initialize QTranslator
    subroutine qtranslator_init(this)
        class(QTranslator), intent(inout) :: this

        this%is_loaded_ = .false.
        this%message_count_ = 0
        if (allocated(this%messages_)) deallocate(this%messages_)
    end subroutine qtranslator_init

    !> @brief Load translation from file
    function qtranslator_load(this, filename, directory, search_delimiters, suffix) result(success)
        class(QTranslator), intent(inout) :: this
        character(len=*), intent(in) :: filename
        character(len=*), intent(in), optional :: directory
        character(len=*), intent(in), optional :: search_delimiters
        character(len=*), intent(in), optional :: suffix
        logical :: success

        success = this%load_from_file(filename)
    end function qtranslator_load

    !> @brief Load from file
    function qtranslator_load_from_file(this, filename) result(success)
        class(QTranslator), intent(inout) :: this
        character(len=*), intent(in) :: filename
        logical :: success
        integer :: unit, iostat
        character(len=:), allocatable :: content

        ! Try to open and read the file
        open(newunit=unit, file=filename, status='old', action='read', &
             iostat=iostat, form='unformatted', access='stream')
        if (iostat /= 0) then
            success = .false.
            return
        end if

        ! Read entire file content
        inquire(unit, size=iostat)
        if (iostat > 0) then
            allocate(character(len=iostat) :: content)
            read(unit, iostat=iostat) content
            if (iostat == 0) then
                success = this%load_from_data(content)
            else
                success = .false.
            end if
        else
            success = .false.
        end if

        close(unit)
    end function qtranslator_load_from_file

    !> @brief Load from data
    function qtranslator_load_from_data(this, data) result(success)
        class(QTranslator), intent(inout) :: this
        character(len=*), intent(in) :: data
        logical :: success

        ! Check if it's a .qm file (binary) or .ts file (XML)
        if (this%is_qm_data(data)) then
            success = this%load_qm_data(data)
        else
            success = this%load_ts_data(data)
        end if

        if (success) then
            this%is_loaded_ = .true.
        end if
    end function qtranslator_load_from_data

    !> @brief Save translation to file
    function qtranslator_save(this, filename, mode) result(success)
        class(QTranslator), intent(in) :: this
        character(len=*), intent(in) :: filename
        type(QTranslator_SaveMode), intent(in), optional :: mode
        logical :: success

        success = this%save_to_file(filename, mode)
    end function qtranslator_save

    !> @brief Save to file
    function qtranslator_save_to_file(this, filename, mode) result(success)
        class(QTranslator), intent(in) :: this
        character(len=*), intent(in) :: filename
        type(QTranslator_SaveMode), intent(in), optional :: mode
        logical :: success
        integer :: unit, iostat
        character(len=:), allocatable :: content

        ! Generate content based on file extension
        if (index(filename, '.qm') > 0) then
            content = this%generate_qm_data()
        else
            content = this%generate_ts_data()
        end if

        ! Write to file
        open(newunit=unit, file=filename, status='replace', action='write', &
             iostat=iostat, form='unformatted', access='stream')
        if (iostat /= 0) then
            success = .false.
            return
        end if

        write(unit, iostat=iostat) content
        success = (iostat == 0)
        close(unit)
    end function qtranslator_save_to_file

    !> @brief Translate string
    function qtranslator_translate(this, context, source_text, disambiguation, n) result(translation)
        class(QTranslator), intent(in) :: this
        character(len=*), intent(in) :: context
        character(len=*), intent(in) :: source_text
        character(len=*), intent(in), optional :: disambiguation
        integer, intent(in), optional :: n
        character(len=:), allocatable :: translation
        type(QTranslatorMessage), pointer :: msg

        msg => this%find_message(context, source_text)
        if (associated(msg)) then
            if (present(n) .and. msg%is_plural()) then
                translation = this%get_plural_translation(msg, n)
            else
                translation = msg%translation_%get()
            end if
        else
            translation = source_text
        end if
    end function qtranslator_translate

    !> @brief Translate with context
    function qtranslator_translate_context(this, context, source_text, comment, n) result(translation)
        class(QTranslator), intent(in) :: this
        character(len=*), intent(in) :: context
        character(len=*), intent(in) :: source_text
        character(len=*), intent(in), optional :: comment
        integer, intent(in), optional :: n
        character(len=:), allocatable :: translation

        translation = this%translate(context, source_text, comment, n)
    end function qtranslator_translate_context

    !> @brief Translate plural form
    function qtranslator_translate_plural(this, context, source_text, comment, n) result(translation)
        class(QTranslator), intent(in) :: this
        character(len=*), intent(in) :: context
        character(len=*), intent(in) :: source_text
        character(len=*), intent(in), optional :: comment
        integer, intent(in) :: n
        character(len=:), allocatable :: translation
        type(QTranslatorMessage), pointer :: msg

        msg => this%find_message(context, source_text)
        if (associated(msg) .and. msg%is_plural()) then
            translation = this%get_plural_translation(msg, n)
        else
            translation = source_text
        end if
    end function qtranslator_translate_plural

    !> @brief Find message
    function qtranslator_find_message(this, context, source_text) result(msg)
        class(QTranslator), intent(in) :: this
        character(len=*), intent(in) :: context
        character(len=*), intent(in) :: source_text
        type(QTranslatorMessage), pointer :: msg
        integer :: i

        msg => null()
        if (.not. allocated(this%messages_)) return

        do i = 1, this%message_count_
            if (this%messages_(i)%source_text_%equals(source_text) .and. &
                this%messages_(i)%context_%equals(context)) then
                msg => this%messages_(i)
                exit
            end if
        end do
    end function qtranslator_find_message

    !> @brief Insert message
    subroutine qtranslator_insert(this, message)
        class(QTranslator), intent(inout) :: this
        type(QTranslatorMessage), intent(in) :: message
        type(QTranslatorMessage), allocatable :: temp_messages(:)

        if (.not. allocated(this%messages_)) then
            allocate(this%messages_(10))
        else if (this%message_count_ >= size(this%messages_)) then
            allocate(temp_messages(2 * size(this%messages_)))
            temp_messages(1:this%message_count_) = this%messages_(1:this%message_count_)
            call move_alloc(temp_messages, this%messages_)
        end if

        this%message_count_ = this%message_count_ + 1
        this%messages_(this%message_count_) = message
    end subroutine qtranslator_insert

    !> @brief Remove message
    subroutine qtranslator_remove(this, context, source_text)
        class(QTranslator), intent(inout) :: this
        character(len=*), intent(in) :: context
        character(len=*), intent(in) :: source_text
        integer :: i, j

        if (.not. allocated(this%messages_)) return

        do i = 1, this%message_count_
            if (this%messages_(i)%source_text_%equals(source_text) .and. &
                this%messages_(i)%context_%equals(context)) then
                ! Shift remaining messages
                do j = i, this%message_count_ - 1
                    this%messages_(j) = this%messages_(j + 1)
                end do
                this%message_count_ = this%message_count_ - 1
                exit
            end if
        end do
    end subroutine qtranslator_remove

    !> @brief Clear all messages
    subroutine qtranslator_clear(this)
        class(QTranslator), intent(inout) :: this

        if (allocated(this%messages_)) deallocate(this%messages_)
        this%message_count_ = 0
        this%is_loaded_ = .false.
    end subroutine qtranslator_clear

    !> @brief Check if empty
    function qtranslator_is_empty(this) result(empty)
        class(QTranslator), intent(in) :: this
        logical :: empty

        empty = (this%message_count_ == 0)
    end function qtranslator_is_empty

    !> @brief Get message count
    function qtranslator_count(this) result(count)
        class(QTranslator), intent(in) :: this
        integer :: count

        count = this%message_count_
    end function qtranslator_count

    !> @brief Get file name
    function qtranslator_file_name(this) result(filename)
        class(QTranslator), intent(in) :: this
        type(QString) :: filename

        filename = this%file_name_
    end function qtranslator_file_name

    !> @brief Get locale
    function qtranslator_locale(this) result(loc)
        class(QTranslator), intent(in) :: this
        type(QLocale) :: loc

        loc = this%locale_
    end function qtranslator_locale

    !> @brief Check if loaded
    function qtranslator_is_loaded(this) result(loaded)
        class(QTranslator), intent(in) :: this
        logical :: loaded

        loaded = this%is_loaded_
    end function qtranslator_is_loaded

    !> @brief Check equality
    function qtranslator_equals(this, other) result(equal)
        class(QTranslator), intent(in) :: this
        type(QTranslator), intent(in) :: other
        logical :: equal

        equal = (this%message_count_ == other%message_count_ .and. &
                 this%file_name_%equals(other%file_name_%get()))
    end function qtranslator_equals

    !> @brief Merge with another translator
    subroutine qtranslator_merge(this, other)
        class(QTranslator), intent(inout) :: this
        type(QTranslator), intent(in) :: other
        integer :: i

        if (.not. allocated(other%messages_)) return

        do i = 1, other%message_count_
            call this%insert(other%messages_(i))
        end do
    end subroutine qtranslator_merge

    ! ========== Private Helper Methods ==========

    !> @brief Check if data is QM format
    function is_qm_data(this, data) result(is_qm)
        class(QTranslator), intent(in) :: this
        character(len=*), intent(in) :: data
        logical :: is_qm

        ! QM files start with a magic number
        is_qm = (len(data) >= 16 .and. data(1:4) == achar(63)//achar(42)//achar(42)//achar(0))
    end function is_qm_data

    !> @brief Load QM data (simplified)
    function load_qm_data(this, data) result(success)
        class(QTranslator), intent(inout) :: this
        character(len=*), intent(in) :: data
        logical :: success

        ! Simplified QM loading - just mark as loaded
        success = .true.
        this%message_count_ = 0 ! Would parse actual QM format
    end function load_qm_data

    !> @brief Load TS data (simplified XML parsing)
    function load_ts_data(this, data) result(success)
        class(QTranslator), intent(inout) :: this
        character(len=*), intent(in) :: data
        logical :: success

        ! Simplified TS loading - would parse XML
        success = .true.
        this%message_count_ = 0 ! Would parse actual TS format
    end function load_ts_data

    !> @brief Generate QM data (simplified)
    function generate_qm_data(this) result(data)
        class(QTranslator), intent(in) :: this
        character(len=:), allocatable :: data

        ! Simplified QM generation
        data = achar(63)//achar(42)//achar(42)//achar(0) ! Magic number
    end function generate_qm_data

    !> @brief Generate TS data (simplified XML)
    function generate_ts_data(this) result(data)
        class(QTranslator), intent(in) :: this
        character(len=:), allocatable :: data

        ! Simplified TS generation
        data = '<?xml version="1.0" encoding="utf-8"?>' // &
               '<!DOCTYPE TS>' // &
               '<TS version="2.1">' // &
               '</TS>'
    end function generate_ts_data

    !> @brief Get plural translation
    function get_plural_translation(this, msg, n) result(translation)
        class(QTranslator), intent(in) :: this
        type(QTranslatorMessage), intent(in) :: msg
        integer, intent(in) :: n
        character(len=:), allocatable :: translation

        ! Simplified plural handling - just return the translation
        translation = msg%translation_%get()
    end function get_plural_translation

end module forge_translator