!> @brief Internationalization (i18n) framework for ForGE
!> @details Main i18n module providing tr() function and translation management
!> @author ForGE Contributors
!> @date 2025
!> @license GPL-3.0-or-later

module forge_i18n
    use iso_c_binding
    use iso_fortran_env, only: int32, int64, real32, real64
    use forge_string_utils, only: QString
    use forge_translator, only: QTranslator
    use forge_locale, only: QLocale
    implicit none
    private

    public :: tr, tr_context, tr_plural, install_translator
    public :: remove_translator, current_translator, set_locale
    public :: I18nContext, I18nManager

    !> @brief Context information for translations
    type :: I18nContext
        character(len=:), allocatable :: context_name
        character(len=:), allocatable :: file_name
        integer :: line_number = -1
    end type I18nContext

    !> @brief Global i18n manager
    type :: I18nManager
        private
        type(QTranslator), allocatable :: translators(:)
        type(QLocale) :: current_locale
        integer :: num_translators = 0
        logical :: initialized = .false.
    contains
        procedure :: init => i18n_manager_init
        procedure :: install => i18n_manager_install
        procedure :: remove => i18n_manager_remove
        procedure :: translate => i18n_manager_translate
        procedure :: translate_plural => i18n_manager_translate_plural
        procedure :: set_locale => i18n_manager_set_locale
        procedure :: get_locale => i18n_manager_get_locale
        procedure :: find_translator => i18n_manager_find_translator
    end type I18nManager

    ! Global i18n manager instance
    type(I18nManager), save, target :: global_i18n_manager

contains

    ! ========== Translation Functions ==========

    !> @brief Translate string (Qt-style tr function)
    function tr(source_text, context, disambiguation, n) result(translation)
        character(len=*), intent(in) :: source_text
        character(len=*), intent(in), optional :: context
        character(len=*), intent(in), optional :: disambiguation
        integer, intent(in), optional :: n
        character(len=:), allocatable :: translation

        if (.not. global_i18n_manager%initialized) then
            call global_i18n_manager%init()
        end if

        if (present(n)) then
            translation = global_i18n_manager%translate_plural(source_text, context, disambiguation, n)
        else
            translation = global_i18n_manager%translate(source_text, context, disambiguation)
        end if
    end function tr

    !> @brief Translate with explicit context
    function tr_context(context, source_text, disambiguation, n) result(translation)
        character(len=*), intent(in) :: context
        character(len=*), intent(in) :: source_text
        character(len=*), intent(in), optional :: disambiguation
        integer, intent(in), optional :: n
        character(len=:), allocatable :: translation

        translation = tr(source_text, context, disambiguation, n)
    end function tr_context

    !> @brief Translate plural form
    function tr_plural(source_text, context, disambiguation, n) result(translation)
        character(len=*), intent(in) :: source_text
        character(len=*), intent(in), optional :: context
        character(len=*), intent(in), optional :: disambiguation
        integer, intent(in) :: n
        character(len=:), allocatable :: translation

        translation = tr(source_text, context, disambiguation, n)
    end function tr_plural

    !> @brief Install translator
    function install_translator(translator) result(success)
        type(QTranslator), intent(in) :: translator
        logical :: success

        if (.not. global_i18n_manager%initialized) then
            call global_i18n_manager%init()
        end if

        success = global_i18n_manager%install(translator)
    end function install_translator

    !> @brief Remove translator
    function remove_translator(translator) result(success)
        type(QTranslator), intent(in) :: translator
        logical :: success

        success = global_i18n_manager%remove(translator)
    end function remove_translator

    !> @brief Get current translator
    function current_translator() result(translator)
        type(QTranslator) :: translator

        ! Return first installed translator (simplified)
        if (global_i18n_manager%num_translators > 0) then
            translator = global_i18n_manager%translators(1)
        end if
    end function current_translator

    !> @brief Set current locale
    subroutine set_locale(locale)
        type(QLocale), intent(in) :: locale

        if (.not. global_i18n_manager%initialized) then
            call global_i18n_manager%init()
        end if

        call global_i18n_manager%set_locale(locale)
    end subroutine set_locale

    ! ========== I18nManager Implementation ==========

    !> @brief Initialize i18n manager
    subroutine i18n_manager_init(this)
        class(I18nManager), intent(inout) :: this

        this%num_translators = 0
        this%initialized = .true.
        call this%current_locale%init()
    end subroutine i18n_manager_init

    !> @brief Install translator
    function i18n_manager_install(this, translator) result(success)
        class(I18nManager), intent(inout) :: this
        type(QTranslator), intent(in) :: translator
        logical :: success
        type(QTranslator), allocatable :: temp_translators(:)

        if (.not. allocated(this%translators)) then
            allocate(this%translators(4))
        else if (this%num_translators >= size(this%translators)) then
            allocate(temp_translators(2 * size(this%translators)))
            temp_translators(1:this%num_translators) = this%translators(1:this%num_translators)
            call move_alloc(temp_translators, this%translators)
        end if

        this%num_translators = this%num_translators + 1
        this%translators(this%num_translators) = translator
        success = .true.
    end function i18n_manager_install

    !> @brief Remove translator
    function i18n_manager_remove(this, translator) result(success)
        class(I18nManager), intent(inout) :: this
        type(QTranslator), intent(in) :: translator
        logical :: success
        integer :: i, j

        success = .false.
        if (.not. allocated(this%translators)) return

        do i = 1, this%num_translators
            if (this%translators(i)%equals(translator)) then
                ! Shift remaining translators
                do j = i, this%num_translators - 1
                    this%translators(j) = this%translators(j + 1)
                end do
                this%num_translators = this%num_translators - 1
                success = .true.
                exit
            end if
        end do
    end function i18n_manager_remove

    !> @brief Translate string
    function i18n_manager_translate(this, source_text, context, disambiguation) result(translation)
        class(I18nManager), intent(in) :: this
        character(len=*), intent(in) :: source_text
        character(len=*), intent(in), optional :: context
        character(len=*), intent(in), optional :: disambiguation
        character(len=:), allocatable :: translation
        integer :: i
        character(len=:), allocatable :: ctx

        ctx = "default"
        if (present(context)) ctx = context

        translation = source_text  ! Default to source text

        ! Try each translator
        do i = 1, this%num_translators
            translation = this%translators(i)%translate(ctx, source_text, disambiguation)
            if (translation /= source_text) exit  ! Found translation
        end do
    end function i18n_manager_translate

    !> @brief Translate plural form
    function i18n_manager_translate_plural(this, source_text, context, disambiguation, n) result(translation)
        class(I18nManager), intent(in) :: this
        character(len=*), intent(in) :: source_text
        character(len=*), intent(in), optional :: context
        character(len=*), intent(in), optional :: disambiguation
        integer, intent(in) :: n
        character(len=:), allocatable :: translation
        integer :: i
        character(len=:), allocatable :: ctx

        ctx = "default"
        if (present(context)) ctx = context

        translation = source_text  ! Default to source text

        ! Try each translator
        do i = 1, this%num_translators
            translation = this%translators(i)%translate_plural(ctx, source_text, disambiguation, n)
            if (translation /= source_text) exit  ! Found translation
        end do
    end function i18n_manager_translate_plural

    !> @brief Set current locale
    subroutine i18n_manager_set_locale(this, locale)
        class(I18nManager), intent(inout) :: this
        type(QLocale), intent(in) :: locale

        this%current_locale = locale
    end subroutine i18n_manager_set_locale

    !> @brief Get current locale
    function i18n_manager_get_locale(this) result(locale)
        class(I18nManager), intent(in) :: this
        type(QLocale) :: locale

        locale = this%current_locale
    end function i18n_manager_get_locale

    !> @brief Find translator for current locale
    function i18n_manager_find_translator(this) result(translator)
        class(I18nManager), intent(in) :: this
        type(QTranslator) :: translator
        integer :: i

        ! Return translator that matches current locale
        do i = 1, this%num_translators
            if (this%translators(i)%locale()%equals(this%current_locale)) then
                translator = this%translators(i)
                return
            end if
        end do

        ! Return first translator as fallback
        if (this%num_translators > 0) then
            translator = this%translators(1)
        end if
    end function i18n_manager_find_translator

end module forge_i18n