!> @brief Plural forms support for internationalization
!> @details Handles different plural rules for various languages
!> @author ForGE Contributors
!> @date 2025
!> @license GPL-3.0-or-later

module forge_plural
    use iso_c_binding
    use iso_fortran_env, only: int32, int64, real32, real64
    use forge_locale, only: QLocale, QLocale_Language
    implicit none
    private

    public :: PluralRule, get_plural_form, evaluate_plural_rule

    !> @brief Plural rule types
    enum, bind(c)
        enumerator :: Plural_One = 0     ! n == 1
        enumerator :: Plural_Few = 1     ! n in 2..4
        enumerator :: Plural_Many = 2    ! n >= 5 or n == 0
        enumerator :: Plural_Other = 3   ! fallback
    end enum
    type :: PluralRule
        integer :: rule_type
        integer :: min_val = 0
        integer :: max_val = -1  ! -1 means no upper limit
    end type PluralRule

contains

    !> @brief Get plural form for a number in given locale
    function get_plural_form(n, locale) result(form)
        integer, intent(in) :: n
        type(QLocale), intent(in) :: locale
        integer :: form

        select case (locale%language()%value)
        case (QLocale_Language%English)
            form = get_english_plural(n)
        case (QLocale_Language%French)
            form = get_french_plural(n)
        case (QLocale_Language%German)
            form = get_german_plural(n)
        case (QLocale_Language%Russian)
            form = get_russian_plural(n)
        case (QLocale_Language%Arabic)
            form = get_arabic_plural(n)
        case (QLocale_Language%Chinese)
            form = get_chinese_plural(n)
        case (QLocale_Language%Japanese)
            form = get_japanese_plural(n)
        case default
            form = get_english_plural(n)  ! Default to English
        end select
    end function get_plural_form

    !> @brief Evaluate plural rule
    function evaluate_plural_rule(n, rule) result(matches)
        integer, intent(in) :: n
        type(PluralRule), intent(in) :: rule
        logical :: matches

        select case (rule%rule_type)
        case (Plural_One)
            matches = (n == 1)
        case (Plural_Few)
            matches = (n >= rule%min_val .and. (rule%max_val == -1 .or. n <= rule%max_val))
        case (Plural_Many)
            matches = (n >= rule%min_val)
        case default
            matches = .true.  ! Other matches everything
        end select
    end function evaluate_plural_rule

    ! ========== Language-Specific Plural Rules ==========

    !> @brief English plural rules (simple: singular/plural)
    function get_english_plural(n) result(form)
        integer, intent(in) :: n
        integer :: form

        if (n == 1) then
            form = Plural_One
        else
            form = Plural_Other
        end if
    end function get_english_plural

    !> @brief French plural rules (same as English for most cases)
    function get_french_plural(n) result(form)
        integer, intent(in) :: n
        integer :: form

        if (n == 1) then
            form = Plural_One
        else
            form = Plural_Other
        end if
    end function get_french_plural

    !> @brief German plural rules (same as English)
    function get_german_plural(n) result(form)
        integer, intent(in) :: n
        integer :: form

        if (n == 1) then
            form = Plural_One
        else
            form = Plural_Other
        end if
    end function get_german_plural

    !> @brief Russian plural rules (complex: 1, 2-4, 5+)
    function get_russian_plural(n) result(form)
        integer, intent(in) :: n
        integer :: form

        if (n % 10 == 1 .and. n % 100 /= 11) then
            form = Plural_One
        else if (n % 10 >= 2 .and. n % 10 <= 4 .and. (n % 100 < 10 .or. n % 100 >= 20)) then
            form = Plural_Few
        else
            form = Plural_Many
        end if
    end function get_russian_plural

    !> @brief Arabic plural rules (very complex)
    function get_arabic_plural(n) result(form)
        integer, intent(in) :: n
        integer :: form

        if (n == 0) then
            form = Plural_Other
        else if (n == 1) then
            form = Plural_One
        else if (n == 2) then
            form = Plural_Few
        else if (n % 100 >= 3 .and. n % 100 <= 10) then
            form = Plural_Few
        else if (n % 100 >= 11 .and. n % 100 <= 99) then
            form = Plural_Many
        else
            form = Plural_Other
        end if
    end function get_arabic_plural

    !> @brief Chinese plural rules (no plural forms)
    function get_chinese_plural(n) result(form)
        integer, intent(in) :: n
        integer :: form

        form = Plural_Other  ! Chinese doesn't have plural forms
    end function get_chinese_plural

    !> @brief Japanese plural rules (no plural forms)
    function get_japanese_plural(n) result(form)
        integer, intent(in) :: n
        integer :: form

        form = Plural_Other  ! Japanese doesn't have plural forms
    end function get_japanese_plural

end module forge_plural