!> @brief Test suite for internationalization (i18n) framework
!> @details Unit tests for all i18n components
!> @author ForGE Contributors
!> @date 2025
!> @license GPL-3.0-or-later

program test_i18n
    use iso_c_binding
    use iso_fortran_env, only: int32, int64, real32, real64
    use forge_i18n, only: tr, tr_context, tr_plural, install_translator, set_locale
    use forge_translator, only: QTranslator, QTranslatorMessage
    use forge_locale, only: QLocale
    use forge_string_utils, only: QString
    use forge_plural, only: get_plural_form
    implicit none

    integer :: passed = 0, failed = 0

    call test_qlocale()
    call test_qtranslator()
    call test_plural_forms()
    call test_translation_functions()
    call test_qstring_integration()

    write(*,*) ""
    write(*,*) "Test Results: ", passed, " passed, ", failed, " failed"

contains

    !> @brief Test QLocale functionality
    subroutine test_qlocale()
        type(QLocale) :: locale_en, locale_fr, locale_de
        character(len=:), allocatable :: str

        write(*,*) "Testing QLocale..."

        ! Test initialization
        call locale_en%init_from_name("en_US")
        call locale_fr%init_from_name("fr_FR")
        call locale_de%init_from_name("de_DE")

        ! Test locale properties
        if (locale_en%name() == "en_US") then
            call test_pass("QLocale name")
        else
            call test_fail("QLocale name")
        end if

        ! Test number formatting
        str = locale_en%to_string_int(1234)
        if (str == "1234") then  ! No grouping for simplicity
            call test_pass("QLocale number formatting")
        else
            call test_fail("QLocale number formatting: " // str)
        end if

        ! Test currency formatting
        str = locale_en%to_currency_string_int(100)
        if (index(str, "$") > 0) then
            call test_pass("QLocale currency formatting")
        else
            call test_fail("QLocale currency formatting: " // str)
        end if

        ! Test date formatting
        str = locale_en%to_string_date(2025, 1, 15)
        if (len(str) > 0) then
            call test_pass("QLocale date formatting")
        else
            call test_fail("QLocale date formatting")
        end if
    end subroutine test_qlocale

    !> @brief Test QTranslator functionality
    subroutine test_qtranslator()
        type(QTranslator) :: translator
        type(QTranslatorMessage) :: message
        character(len=:), allocatable :: translation

        write(*,*) "Testing QTranslator..."

        ! Test message creation
        call message%set_source_text("Hello")
        call message%set_translation("Hola")
        call message%set_context("MainWindow")

        ! Test translator operations
        call translator%insert(message)

        if (translator%count() == 1) then
            call test_pass("QTranslator insert")
        else
            call test_fail("QTranslator insert")
        end if

        ! Test translation lookup
        translation = translator%translate("MainWindow", "Hello")
        if (translation == "Hola") then
            call test_pass("QTranslator translate")
        else
            call test_fail("QTranslator translate: " // translation)
        end if

        ! Test message removal
        call translator%remove("MainWindow", "Hello")
        if (translator%count() == 0) then
            call test_pass("QTranslator remove")
        else
            call test_fail("QTranslator remove")
        end if
    end subroutine test_qtranslator

    !> @brief Test plural forms
    subroutine test_plural_forms()
        type(QLocale) :: locale_en, locale_ru
        integer :: form

        write(*,*) "Testing plural forms..."

        call locale_en%init_from_name("en")
        call locale_ru%init_from_name("ru")

        ! Test English plurals
        form = get_plural_form(1, locale_en)
        if (form == 0) then  ! Plural_One
            call test_pass("English singular")
        else
            call test_fail("English singular")
        end if

        form = get_plural_form(2, locale_en)
        if (form == 3) then  ! Plural_Other
            call test_pass("English plural")
        else
            call test_fail("English plural")
        end if

        ! Test Russian plurals
        form = get_plural_form(1, locale_ru)
        if (form == 0) then  ! Plural_One
            call test_pass("Russian singular")
        else
            call test_fail("Russian singular")
        end if

        form = get_plural_form(2, locale_ru)
        if (form == 1) then  ! Plural_Few
            call test_pass("Russian paucal")
        else
            call test_fail("Russian paucal")
        end if
    end subroutine test_plural_forms

    !> @brief Test translation functions
    subroutine test_translation_functions()
        type(QTranslator) :: translator
        type(QTranslatorMessage) :: message
        character(len=:), allocatable :: result
        logical :: success

        write(*,*) "Testing translation functions..."

        ! Set up translator with test data
        call message%set_source_text("File")
        call message%set_translation("Archivo")
        call message%set_context("MainWindow")
        call translator%insert(message)

        success = install_translator(translator)
        if (success) then
            call test_pass("install_translator")
        else
            call test_fail("install_translator")
        end if

        ! Test tr function
        result = tr("File", "MainWindow")
        if (result == "Archivo") then
            call test_pass("tr function")
        else
            call test_fail("tr function: " // result)
        end if

        ! Test untranslated string
        result = tr("Unknown", "MainWindow")
        if (result == "Unknown") then
            call test_pass("tr untranslated")
        else
            call test_fail("tr untranslated: " // result)
        end if
    end subroutine test_translation_functions

    !> @brief Test QString integration
    subroutine test_qstring_integration()
        type(QString) :: qstr
        character(len=:), allocatable :: result

        write(*,*) "Testing QString integration..."

        call qstr%set("Test String")

        ! Test basic QString operations
        if (qstr%length() == 11) then
            call test_pass("QString length")
        else
            call test_fail("QString length")
        end if

        result = qstr%get()
        if (result == "Test String") then
            call test_pass("QString get")
        else
            call test_fail("QString get: " // result)
        end if

        ! Test QString modification
        call qstr%append(" Appended")
        if (qstr%length() == 20) then
            call test_pass("QString append")
        else
            call test_fail("QString append")
        end if
    end subroutine test_qstring_integration

    !> @brief Mark test as passed
    subroutine test_pass(test_name)
        character(len=*), intent(in) :: test_name
        write(*,*) "  ✓ ", test_name
        passed = passed + 1
    end subroutine test_pass

    !> @brief Mark test as failed
    subroutine test_fail(test_name)
        character(len=*), intent(in) :: test_name
        write(*,*) "  ✗ ", test_name
        failed = failed + 1
    end subroutine test_fail

end program test_i18n