!> @brief Integration module for i18n with existing ForGE string system
!> @details Provides seamless integration between i18n framework and existing string handling
!> @author ForGE Contributors
!> @date 2025
!> @license GPL-3.0-or-later

module forge_i18n_integration
    use iso_c_binding
    use iso_fortran_env, only: int32, int64, real32, real64
    use forge_string_utils, only: QString
    use forge_i18n, only: tr, tr_context, tr_plural, install_translator, set_locale
    use forge_translator, only: QTranslator
    use forge_locale, only: QLocale
    implicit none
    private

    public :: init_i18n_system, load_translation_file, switch_language
    public :: get_current_language, set_application_locale
    public :: QString_tr, QString_tr_context, QString_tr_plural

contains

    !> @brief Initialize the i18n system
    subroutine init_i18n_system()
        ! Initialize global i18n manager
        ! This would be called at application startup
    end subroutine init_i18n_system

    !> @brief Load translation file
    function load_translation_file(filename) result(success)
        character(len=*), intent(in) :: filename
        logical :: success
        type(QTranslator) :: translator

        success = translator%load(filename)
        if (success) then
            success = install_translator(translator)
        end if
    end function load_translation_file

    !> @brief Switch application language at runtime
    function switch_language(language_code) result(success)
        character(len=*), intent(in) :: language_code
        logical :: success
        type(QLocale) :: new_locale

        call new_locale%init_from_name(language_code)
        call set_locale(new_locale)
        success = .true.
    end function switch_language

    !> @brief Get current language code
    function get_current_language() result(lang_code)
        character(len=:), allocatable :: lang_code
        type(QLocale) :: current_locale

        ! Get from global i18n manager
        lang_code = "en"  ! Default
    end function get_current_language

    !> @brief Set application locale
    subroutine set_application_locale(locale)
        type(QLocale), intent(in) :: locale

        call set_locale(locale)
    end subroutine set_application_locale

    !> @brief QString extension for translation
    function QString_tr(this, context, disambiguation, n) result(translated)
        class(QString), intent(in) :: this
        character(len=*), intent(in), optional :: context
        character(len=*), intent(in), optional :: disambiguation
        integer, intent(in), optional :: n
        type(QString) :: translated
        character(len=:), allocatable :: result_str

        if (present(n)) then
            result_str = tr(this%get(), context, disambiguation, n)
        else
            result_str = tr(this%get(), context, disambiguation)
        end if

        call translated%set(result_str)
    end function QString_tr

    !> @brief QString extension for context-based translation
    function QString_tr_context(this, context, disambiguation, n) result(translated)
        class(QString), intent(in) :: this
        character(len=*), intent(in) :: context
        character(len=*), intent(in), optional :: disambiguation
        integer, intent(in), optional :: n
        type(QString) :: translated

        translated = this%tr(context, disambiguation, n)
    end function QString_tr_context

    !> @brief QString extension for plural translation
    function QString_tr_plural(this, context, disambiguation, n) result(translated)
        class(QString), intent(in) :: this
        character(len=*), intent(in), optional :: context
        character(len=*), intent(in), optional :: disambiguation
        integer, intent(in) :: n
        type(QString) :: translated

        translated = this%tr(context, disambiguation, n)
    end function QString_tr_plural

end module forge_i18n_integration