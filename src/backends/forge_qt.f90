!> @brief Qt backend implementation
!> @details Implements ForGE backend using Qt6 GUI library
!> @author ForGE Contributors
!> @date 2025
!> @license GPL-3.0-or-later

module forge_qt_backend
    use iso_c_binding
    use forge_backend
    use forge_types
    use forge_errors
    use forge_qt_bindings
    use iso_fortran_env, only: output_unit
    implicit none
    private

    public :: forge_qt_backend_t

    !> @brief Qt backend implementation
    type, extends(forge_backend_base) :: forge_qt_backend_t
        private
        type(qapplication) :: app
        integer :: window_count = 0
        integer :: widget_count = 0
        ! Multimedia components
        type(qaudioformat) :: audio_format
        type(qaudioinput) :: audio_input
        type(qaudiooutput) :: audio_output
        type(qvideoformat) :: video_format
        type(qcamera) :: camera
        type(qcameraviewfinder) :: camera_viewfinder
        type(qcameraimagecapture) :: camera_capture
        type(qmediaplayer) :: media_player
        type(qmediarecorder) :: media_recorder
        type(qaudiorecorder) :: audio_recorder
        type(qvideorecorder) :: video_recorder
    contains
        procedure :: init => qt_init
        procedure :: shutdown => qt_shutdown
        procedure :: run => qt_run
        procedure :: process_events => qt_process_events
        procedure :: create_window => qt_create_window
        procedure :: destroy_window => qt_destroy_window
        procedure :: show_window => qt_show_window
        procedure :: hide_window => qt_hide_window
        procedure :: create_widget => qt_create_widget
        procedure :: destroy_widget => qt_destroy_widget
        procedure :: get_name => qt_get_name
        ! Multimedia procedures
        procedure :: init_audio => qt_init_audio
        procedure :: init_video => qt_init_video
        procedure :: init_camera => qt_init_camera
        procedure :: init_media_player => qt_init_media_player
        procedure :: init_media_recorder => qt_init_media_recorder
        procedure :: play_audio => qt_play_audio
        procedure :: play_video => qt_play_video
        procedure :: record_audio => qt_record_audio
        procedure :: record_video => qt_record_video
        procedure :: capture_image => qt_capture_image
    end type forge_qt_backend_t

contains

    !> @brief Initialize Qt backend
    subroutine qt_init(this, status)
        class(forge_qt_backend_t), intent(inout) :: this
        type(forge_status), intent(out) :: status

        ! Create QApplication
        this%app%ptr = qapplication_new(0, c_null_ptr)
        if (.not. c_associated(this%app%ptr)) then
            call status%set(FORGE_ERROR_BACKEND, "Failed to create Qt application")
            return
        end if

        this%initialized = .true.
        this%backend_type%id = BACKEND_QT
        call status%clear()

        write(output_unit, '(A)') "[QT] Initialized Qt backend"
    end subroutine qt_init

    !> @brief Shutdown Qt backend
    subroutine qt_shutdown(this)
        class(forge_qt_backend_t), intent(inout) :: this

        if (c_associated(this%app%ptr)) then
            call qapplication_quit()
            this%app%ptr = c_null_ptr
        end if

        this%initialized = .false.
        write(output_unit, '(A)') "[QT] Shutdown Qt backend"
    end subroutine qt_shutdown

    !> @brief Run main event loop
    subroutine qt_run(this)
        class(forge_qt_backend_t), intent(inout) :: this
        integer(c_int) :: exit_code

        if (.not. this%initialized) return

        write(output_unit, '(A)') "[QT] Starting Qt main loop"
        exit_code = qapplication_exec()
        write(output_unit, '(A,I0)') "[QT] Exited Qt main loop with code ", exit_code
    end subroutine qt_run

    !> @brief Process pending events (non-blocking)
    subroutine qt_process_events(this)
        class(forge_qt_backend_t), intent(inout) :: this

        if (.not. this%initialized) return

        call qcoreapplication_process_events()
    end subroutine qt_process_events

    !> @brief Create a window
    subroutine qt_create_window(this, handle, title, width, height, status)
        class(forge_qt_backend_t), intent(inout) :: this
        type(forge_window_handle), intent(out) :: handle
        character(len=*), intent(in) :: title
        integer(c_int), intent(in) :: width, height
        type(forge_status), intent(out) :: status
        type(qmainwindow) :: qt_win

        if (.not. this%initialized) then
            call status%set(FORGE_ERROR_BACKEND, "Backend not initialized")
            return
        end if

        ! Create QMainWindow
        qt_win%ptr = qmainwindow_new(c_null_ptr)
        if (.not. c_associated(qt_win%ptr)) then
            call status%set(FORGE_ERROR_BACKEND, "Failed to create Qt main window")
            return
        end if

        ! Set window properties
        call qwidget_set_window_title(qt_win%ptr, trim(title) // c_null_char)
        call qwidget_resize(qt_win%ptr, width, height)

        ! Store handle
        this%window_count = this%window_count + 1
        handle%window_id = this%window_count
        handle%ptr = qt_win%ptr

        call status%clear()
        write(output_unit, '(A,I0,A,A,A,I0,A,I0)') &
            "[QT] Created window #", handle%window_id, &
            ' "', trim(title), '" (', width, 'x', height, ')'
    end subroutine qt_create_window

    !> @brief Destroy a window
    subroutine qt_destroy_window(this, handle)
        class(forge_qt_backend_t), intent(inout) :: this
        type(forge_window_handle), intent(in) :: handle
        type(qwidget) :: qt_win

        qt_win%ptr = handle%ptr
        if (c_associated(qt_win%ptr)) then
            call qobject_delete(qt_win%ptr)
        end if

        write(output_unit, '(A,I0)') "[QT] Destroyed window #", handle%window_id
    end subroutine qt_destroy_window

    !> @brief Show a window
    subroutine qt_show_window(this, handle)
        class(forge_qt_backend_t), intent(inout) :: this
        type(forge_window_handle), intent(in) :: handle
        type(qwidget) :: qt_win

        qt_win%ptr = handle%ptr
        if (c_associated(qt_win%ptr)) then
            call qwidget_show(qt_win%ptr)
        end if

        write(output_unit, '(A,I0)') "[QT] Showing window #", handle%window_id
    end subroutine qt_show_window

    !> @brief Hide a window
    subroutine qt_hide_window(this, handle)
        class(forge_qt_backend_t), intent(inout) :: this
        type(forge_window_handle), intent(in) :: handle
        type(qwidget) :: qt_win

        qt_win%ptr = handle%ptr
        if (c_associated(qt_win%ptr)) then
            call qwidget_hide(qt_win%ptr)
        end if

        write(output_unit, '(A,I0)') "[QT] Hiding window #", handle%window_id
    end subroutine qt_hide_window

    !> @brief Create a widget
    subroutine qt_create_widget(this, handle, widget_type, parent, status)
        class(forge_qt_backend_t), intent(inout) :: this
        type(forge_widget_handle), intent(out) :: handle
        character(len=*), intent(in) :: widget_type
        type(forge_window_handle), intent(in) :: parent
        type(forge_status), intent(out) :: status
        type(qwidget) :: qt_wid, parent_win

        if (.not. this%initialized) then
            call status%set(FORGE_ERROR_BACKEND, "Backend not initialized")
            return
        end if

        parent_win%ptr = parent%ptr

        ! Create widget based on type
        select case (trim(widget_type))
        case ("button")
            qt_wid%ptr = qpushbutton_new("Button" // c_null_char, parent_win%ptr)
        case ("label")
            qt_wid%ptr = qlabel_new("Label" // c_null_char, parent_win%ptr)
        case ("entry")
            qt_wid%ptr = qlineedit_new(parent_win%ptr)
        case ("text")
            qt_wid%ptr = qtextedit_new(parent_win%ptr)
        case ("frame")
            qt_wid%ptr = qwidget_new(parent_win%ptr)  ! Use QWidget as frame
        case ("videowidget")
            qt_wid%ptr = qvideowidget_new(parent_win%ptr)
        case ("graphicsvideoitem")
            qt_wid%ptr = qgraphicsvideoitem_new(parent_win%ptr)
        case ("cameraviewfinder")
            qt_wid%ptr = qcameraviewfinder_new(parent_win%ptr)
        case default
            qt_wid%ptr = qlabel_new("Widget" // c_null_char, parent_win%ptr)  ! Default fallback
        end select

        if (.not. c_associated(qt_wid%ptr)) then
            call status%set(FORGE_ERROR_BACKEND, "Failed to create Qt widget")
            return
        end if

        ! Store handle
        this%widget_count = this%widget_count + 1
        handle%widget_id = this%widget_count
        handle%ptr = qt_wid%ptr

        call status%clear()
        write(output_unit, '(A,I0,A,A,A,I0)') &
            "[QT] Created widget #", handle%widget_id, &
            ' type="', trim(widget_type), '" in window #', parent%window_id
    end subroutine qt_create_widget

    !> @brief Destroy a widget
    subroutine qt_destroy_widget(this, handle)
        class(forge_qt_backend_t), intent(inout) :: this
        type(forge_widget_handle), intent(in) :: handle
        type(qwidget) :: qt_wid

        qt_wid%ptr = handle%ptr
        if (c_associated(qt_wid%ptr)) then
            call qobject_delete(qt_wid%ptr)
        end if

        write(output_unit, '(A,I0)') "[QT] Destroyed widget #", handle%widget_id
    end subroutine qt_destroy_widget

    !> @brief Get backend name
    function qt_get_name(this) result(name)
        class(forge_qt_backend_t), intent(in) :: this
        character(len=:), allocatable :: name

        name = "Qt Backend"
    end function qt_get_name

    !> @brief Initialize audio components
    subroutine qt_init_audio(this, status)
        class(forge_qt_backend_t), intent(inout) :: this
        type(forge_status), intent(out) :: status

        if (.not. this%initialized) then
            call status%set(FORGE_ERROR_BACKEND, "Backend not initialized")
            return
        end if

        ! Initialize audio format
        this%audio_format%ptr = qaudioformat_new()
        if (.not. c_associated(this%audio_format%ptr)) then
            call status%set(FORGE_ERROR_BACKEND, "Failed to create audio format")
            return
        end if

        ! Set default audio format (44.1kHz, 16-bit, stereo)
        call qaudioformat_set_sample_rate(this%audio_format%ptr, 44100)
        call qaudioformat_set_channel_count(this%audio_format%ptr, 2)
        call qaudioformat_set_sample_size(this%audio_format%ptr, 16)
        call qaudioformat_set_codec(this%audio_format%ptr, "audio/pcm" // c_null_char)

        ! Initialize audio input
        this%audio_input%ptr = qaudioinput_new(this%audio_format%ptr, c_null_ptr)
        if (.not. c_associated(this%audio_input%ptr)) then
            call status%set(FORGE_ERROR_BACKEND, "Failed to create audio input")
            return
        end if

        ! Initialize audio output
        this%audio_output%ptr = qaudiooutput_new(this%audio_format%ptr, c_null_ptr)
        if (.not. c_associated(this%audio_output%ptr)) then
            call status%set(FORGE_ERROR_BACKEND, "Failed to create audio output")
            return
        end if

        call status%clear()
        write(output_unit, '(A)') "[QT] Initialized audio components"
    end subroutine qt_init_audio

    !> @brief Initialize video components
    subroutine qt_init_video(this, status)
        class(forge_qt_backend_t), intent(inout) :: this
        type(forge_status), intent(out) :: status

        if (.not. this%initialized) then
            call status%set(FORGE_ERROR_BACKEND, "Backend not initialized")
            return
        end if

        ! Initialize video format
        this%video_format%ptr = qvideoformat_new()
        if (.not. c_associated(this%video_format%ptr)) then
            call status%set(FORGE_ERROR_BACKEND, "Failed to create video format")
            return
        end if

        ! Set default video format (640x480, 30fps)
        call qvideoformat_set_resolution(this%video_format%ptr, 640, 480)
        call qvideoformat_set_frame_rate(this%video_format%ptr, 30.0_c_double)
        call qvideoformat_set_pixel_format(this%video_format%ptr, 1)  ! RGB32

        call status%clear()
        write(output_unit, '(A)') "[QT] Initialized video components"
    end subroutine qt_init_video

    !> @brief Initialize camera components
    subroutine qt_init_camera(this, status)
        class(forge_qt_backend_t), intent(inout) :: this
        type(forge_status), intent(out) :: status

        if (.not. this%initialized) then
            call status%set(FORGE_ERROR_BACKEND, "Backend not initialized")
            return
        end if

        ! Initialize camera
        this%camera%ptr = qcamera_new(c_null_ptr)
        if (.not. c_associated(this%camera%ptr)) then
            call status%set(FORGE_ERROR_BACKEND, "Failed to create camera")
            return
        end if

        ! Initialize camera viewfinder
        this%camera_viewfinder%ptr = qcameraviewfinder_new(c_null_ptr)
        if (.not. c_associated(this%camera_viewfinder%ptr)) then
            call status%set(FORGE_ERROR_BACKEND, "Failed to create camera viewfinder")
            return
        end if

        ! Initialize camera image capture
        this%camera_capture%ptr = qcameraimagecapture_new(this%camera%ptr, c_null_ptr)
        if (.not. c_associated(this%camera_capture%ptr)) then
            call status%set(FORGE_ERROR_BACKEND, "Failed to create camera image capture")
            return
        end if

        ! Connect camera to viewfinder
        call qcamera_set_viewfinder(this%camera%ptr, this%camera_viewfinder%ptr)
        call qcameraviewfinder_set_camera(this%camera_viewfinder%ptr, this%camera%ptr)
        call qcameraimagecapture_set_camera(this%camera_capture%ptr, this%camera%ptr)

        call status%clear()
        write(output_unit, '(A)') "[QT] Initialized camera components"
    end subroutine qt_init_camera

    !> @brief Initialize media player
    subroutine qt_init_media_player(this, status)
        class(forge_qt_backend_t), intent(inout) :: this
        type(forge_status), intent(out) :: status

        if (.not. this%initialized) then
            call status%set(FORGE_ERROR_BACKEND, "Backend not initialized")
            return
        end if

        ! Initialize media player
        this%media_player%ptr = qmediaplayer_new(c_null_ptr)
        if (.not. c_associated(this%media_player%ptr)) then
            call status%set(FORGE_ERROR_BACKEND, "Failed to create media player")
            return
        end if

        call status%clear()
        write(output_unit, '(A)') "[QT] Initialized media player"
    end subroutine qt_init_media_player

    !> @brief Initialize media recorder
    subroutine qt_init_media_recorder(this, status)
        class(forge_qt_backend_t), intent(inout) :: this
        type(forge_status), intent(out) :: status

        if (.not. this%initialized) then
            call status%set(FORGE_ERROR_BACKEND, "Backend not initialized")
            return
        end if

        ! Initialize media recorder
        this%media_recorder%ptr = qmediarecorder_new(this%media_player%ptr, c_null_ptr)
        if (.not. c_associated(this%media_recorder%ptr)) then
            call status%set(FORGE_ERROR_BACKEND, "Failed to create media recorder")
            return
        end if

        ! Initialize audio recorder
        this%audio_recorder%ptr = qaudiorecorder_new(c_null_ptr)
        if (.not. c_associated(this%audio_recorder%ptr)) then
            call status%set(FORGE_ERROR_BACKEND, "Failed to create audio recorder")
            return
        end if

        ! Initialize video recorder
        this%video_recorder%ptr = qvideorecorder_new(c_null_ptr)
        if (.not. c_associated(this%video_recorder%ptr)) then
            call status%set(FORGE_ERROR_BACKEND, "Failed to create video recorder")
            return
        end if

        call status%clear()
        write(output_unit, '(A)') "[QT] Initialized media recorder components"
    end subroutine qt_init_media_recorder

    !> @brief Play audio
    subroutine qt_play_audio(this, filename, status)
        class(forge_qt_backend_t), intent(inout) :: this
        character(len=*), intent(in) :: filename
        type(forge_status), intent(out) :: status

        if (.not. c_associated(this%media_player%ptr)) then
            call status%set(FORGE_ERROR_BACKEND, "Media player not initialized")
            return
        end if

        ! Set media content
        call qmediaplayer_set_media(this%media_player%ptr, qmediacontent_new(trim(filename) // c_null_char))

        ! Start playback
        call qmediaplayer_play(this%media_player%ptr)

        call status%clear()
        write(output_unit, '(A,A)') "[QT] Playing audio: ", trim(filename)
    end subroutine qt_play_audio

    !> @brief Play video
    subroutine qt_play_video(this, filename, video_widget, status)
        class(forge_qt_backend_t), intent(inout) :: this
        character(len=*), intent(in) :: filename
        type(forge_widget_handle), intent(in) :: video_widget
        type(forge_status), intent(out) :: status

        if (.not. c_associated(this%media_player%ptr)) then
            call status%set(FORGE_ERROR_BACKEND, "Media player not initialized")
            return
        end if

        ! Set media content
        call qmediaplayer_set_media(this%media_player%ptr, qmediacontent_new(trim(filename) // c_null_char))

        ! Set video output to the widget
        call qmediaplayer_set_video_output(this%media_player%ptr, video_widget%ptr)

        ! Start playback
        call qmediaplayer_play(this%media_player%ptr)

        call status%clear()
        write(output_unit, '(A,A)') "[QT] Playing video: ", trim(filename)
    end subroutine qt_play_video

    !> @brief Record audio
    subroutine qt_record_audio(this, filename, status)
        class(forge_qt_backend_t), intent(inout) :: this
        character(len=*), intent(in) :: filename
        type(forge_status), intent(out) :: status

        if (.not. c_associated(this%media_recorder%ptr)) then
            call status%set(FORGE_ERROR_BACKEND, "Media recorder not initialized")
            return
        end if

        ! Set output location
        call qmediarecorder_set_output_location(this%media_recorder%ptr, trim(filename) // c_null_char)

        ! Set audio input
        call qmediarecorder_set_audio_input(this%media_recorder%ptr, this%audio_input%ptr)

        ! Start recording
        call qmediarecorder_record(this%media_recorder%ptr)

        call status%clear()
        write(output_unit, '(A,A)') "[QT] Recording audio to: ", trim(filename)
    end subroutine qt_record_audio

    !> @brief Record video
    subroutine qt_record_video(this, filename, status)
        class(forge_qt_backend_t), intent(inout) :: this
        character(len=*), intent(in) :: filename
        type(forge_status), intent(out) :: status

        if (.not. c_associated(this%media_recorder%ptr)) then
            call status%set(FORGE_ERROR_BACKEND, "Media recorder not initialized")
            return
        end if

        ! Set output location
        call qmediarecorder_set_output_location(this%media_recorder%ptr, trim(filename) // c_null_char)

        ! Set audio and video inputs
        call qmediarecorder_set_audio_input(this%media_recorder%ptr, this%audio_input%ptr)
        call qmediarecorder_set_video_input(this%media_recorder%ptr, this%camera%ptr)

        ! Start recording
        call qmediarecorder_record(this%media_recorder%ptr)

        call status%clear()
        write(output_unit, '(A,A)') "[QT] Recording video to: ", trim(filename)
    end subroutine qt_record_video

    !> @brief Capture image from camera
    subroutine qt_capture_image(this, filename, status)
        class(forge_qt_backend_t), intent(inout) :: this
        character(len=*), intent(in) :: filename
        type(forge_status), intent(out) :: status

        if (.not. c_associated(this%camera_capture%ptr)) then
            call status%set(FORGE_ERROR_BACKEND, "Camera capture not initialized")
            return
        end if

        ! Start camera if not already started
        call qcamera_start(this%camera%ptr)

        ! Capture image
        call qcameraimagecapture_capture(this%camera_capture%ptr, trim(filename) // c_null_char)

        call status%clear()
        write(output_unit, '(A,A)') "[QT] Capturing image to: ", trim(filename)
    end subroutine qt_capture_image

end module forge_qt_backend