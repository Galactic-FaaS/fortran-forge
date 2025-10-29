!> @brief Internationalization (i18n) demo for ForGE
!> @details Demonstrates Qt-style internationalization features
!> @author ForGE Contributors
!> @date 2025
!> @license GPL-3.0-or-later

program i18n_demo
    use iso_c_binding
    use iso_fortran_env, only: int32, int64, real32, real64
    use forge
    use forge_i18n, only: tr, tr_context, tr_plural, install_translator, set_locale
    use forge_translator, only: QTranslator
    use forge_locale, only: QLocale
    use forge_string_utils, only: QString
    implicit none

    type(QApplication) :: app
    type(QMainWindow) :: window
    type(QWidget) :: central_widget
    type(QVBoxLayout) :: layout
    type(QLabel) :: title_label, info_label, plural_label
    type(QPushButton) :: lang_en_btn, lang_fr_btn, lang_de_btn
    type(QComboBox) :: language_combo
    type(QTranslator) :: translator_en, translator_fr, translator_de
    type(QLocale) :: locale_en, locale_fr, locale_de
    character(len=:), allocatable :: translated_text
    integer :: item_count = 5

    ! Initialize application
    app = qapplication_new(command_argument_count(), get_command_line_args())

    ! Create main window
    window = qmainwindow_new(c_null_ptr)
    call qwidget_set_window_title(window%ptr, tr("Internationalization Demo"))

    ! Create central widget and layout
    central_widget = qwidget_new(c_null_ptr)
    layout = qvboxlayout_new()

    ! Create title label
    title_label = qlabel_new(tr("Welcome to ForGE i18n Demo"))
    call qvboxlayout_add_widget(layout%ptr, title_label%ptr)

    ! Create info label
    info_label = qlabel_new(tr("This demonstrates Qt-style internationalization"))
    call qvboxlayout_add_widget(layout%ptr, info_label%ptr)

    ! Create plural demonstration
    plural_label = qlabel_new(tr_plural("You have %1 item", "You have %1 items", item_count))
    call qvboxlayout_add_widget(layout%ptr, plural_label%ptr)

    ! Create language selection combo box
    language_combo = qcombobox_new()
    call qcombobox_add_item(language_combo%ptr, tr("English"))
    call qcombobox_add_item(language_combo%ptr, tr("Français"))
    call qcombobox_add_item(language_combo%ptr, tr("Deutsch"))
    call qvboxlayout_add_widget(layout%ptr, language_combo%ptr)

    ! Create language switch buttons
    lang_en_btn = qpushbutton_new(tr("Switch to English"))
    lang_fr_btn = qpushbutton_new(tr("Switch to French"))
    lang_de_btn = qpushbutton_new(tr("Switch to German"))

    call qvboxlayout_add_widget(layout%ptr, lang_en_btn%ptr)
    call qvboxlayout_add_widget(layout%ptr, lang_fr_btn%ptr)
    call qvboxlayout_add_widget(layout%ptr, lang_de_btn%ptr)

    ! Set central widget
    call qwidget_set_layout(central_widget%ptr, layout%ptr)
    call qmainwindow_set_central_widget(window%ptr, central_widget%ptr)

    ! Load translation files
    call load_translations()

    ! Connect signals
    call connect_signals()

    ! Show window
    call qwidget_show(window%ptr)

    ! Run application
    call qapplication_exec()

contains

    !> @brief Load translation files
    subroutine load_translations()
        logical :: success

        ! Load English translations (default)
        success = translator_en%load("translations/demo_en.qm")
        if (success) then
            success = install_translator(translator_en)
        end if

        ! Load French translations
        success = translator_fr%load("translations/demo_fr.qm")
        if (success) then
            success = install_translator(translator_fr)
        end if

        ! Load German translations
        success = translator_de%load("translations/demo_de.qm")
        if (success) then
            success = install_translator(translator_de)
        end if
    end subroutine load_translations

    !> @brief Connect UI signals
    subroutine connect_signals()
        ! Connect language buttons
        call qobject_connect(lang_en_btn%ptr, "clicked()"//c_null_char, switch_to_english)
        call qobject_connect(lang_fr_btn%ptr, "clicked()"//c_null_char, switch_to_french)
        call qobject_connect(lang_de_btn%ptr, "clicked()"//c_null_char, switch_to_german)

        ! Connect combo box
        call qobject_connect(language_combo%ptr, "currentIndexChanged(int)"//c_null_char, combo_changed)
    end subroutine connect_signals

    !> @brief Switch to English
    subroutine switch_to_english()
        call locale_en%init_from_name("en_US")
        call set_locale(locale_en)
        call update_ui_texts()
    end subroutine switch_to_english

    !> @brief Switch to French
    subroutine switch_to_french()
        call locale_fr%init_from_name("fr_FR")
        call set_locale(locale_fr)
        call update_ui_texts()
    end subroutine switch_to_french

    !> @brief Switch to German
    subroutine switch_to_german()
        call locale_de%init_from_name("de_DE")
        call set_locale(locale_de)
        call update_ui_texts()
    end subroutine switch_to_german

    !> @brief Handle combo box changes
    subroutine combo_changed(index)
        integer, intent(in) :: index

        select case (index)
        case (0)
            call switch_to_english()
        case (1)
            call switch_to_french()
        case (2)
            call switch_to_german()
        end select
    end subroutine combo_changed

    !> @brief Update all UI text elements
    subroutine update_ui_texts()
        call qwidget_set_window_title(window%ptr, tr("Internationalization Demo"))
        call qlabel_set_text(title_label%ptr, tr("Welcome to ForGE i18n Demo"))
        call qlabel_set_text(info_label%ptr, tr("This demonstrates Qt-style internationalization"))
        call qlabel_set_text(plural_label%ptr, tr_plural("You have %1 item", "You have %1 items", item_count))

        ! Update combo box items
        call qcombobox_set_item_text(language_combo%ptr, 0, tr("English"))
        call qcombobox_set_item_text(language_combo%ptr, 1, tr("Français"))
        call qcombobox_set_item_text(language_combo%ptr, 2, tr("Deutsch"))

        ! Update buttons
        call qpushbutton_set_text(lang_en_btn%ptr, tr("Switch to English"))
        call qpushbutton_set_text(lang_fr_btn%ptr, tr("Switch to French"))
        call qpushbutton_set_text(lang_de_btn%ptr, tr("Switch to German"))
    end subroutine update_ui_texts

    !> @brief Get command line arguments (simplified)
    function get_command_line_args() result(args)
        type(c_ptr), dimension(:), pointer :: args
        allocate(args(1))
        args(1) = c_null_ptr
    end function get_command_line_args

end program i18n_demo