! ForGE Multimedia Platform Support
! Cross-platform multimedia functionality
!
! This module provides platform-specific implementations for:
! - Audio device enumeration and access
! - Video device enumeration and access
! - Codec support detection
! - Hardware acceleration detection

module forge_multimedia_platform
  use iso_c_binding
  use forge_types
  implicit none

  ! Platform detection - simplified for Fortran compatibility
  integer, parameter :: PLATFORM_WINDOWS = 0  ! Will be set by build system
  integer, parameter :: PLATFORM_LINUX = 0    ! Will be set by build system
  integer, parameter :: PLATFORM_MACOS = 0   ! Will be set by build system

  ! Audio device info structure
  type :: platform_audio_device
    character(len=:), allocatable :: name
    character(len=:), allocatable :: id
    logical :: is_default = .false.
    logical :: is_input = .false.
    logical :: is_output = .false.
    integer(c_int) :: channels = 0
    integer(c_int) :: sample_rates(10) = 0
    integer(c_int) :: formats(5) = 0  ! Bit flags for supported formats
  end type platform_audio_device

  ! Video device info structure
  type :: platform_video_device
    character(len=:), allocatable :: name
    character(len=:), allocatable :: id
    logical :: is_default = .false.
    integer(c_int) :: width = 0
    integer(c_int) :: height = 0
    integer(c_int) :: frame_rates(10) = 0
    integer(c_int) :: pixel_formats(5) = 0  ! Bit flags for supported formats
  end type platform_video_device

contains

  ! ============================================================================
  ! Platform Audio Device Enumeration
  ! ============================================================================

  subroutine enumerate_audio_devices(devices)
    type(platform_audio_device), dimension(:), allocatable, intent(out) :: devices
    integer :: num_devices = 0

    ! Use fallback for all platforms (simplified)
    allocate(devices(2))

    ! Default output device
    devices(1)%name = "Default Audio Device"
    devices(1)%id = "default_output"
    devices(1)%is_default = .true.
    devices(1)%is_output = .true.
    devices(1)%channels = 2
    devices(1)%sample_rates(1:4) = [44100, 48000, 88200, 96000]
    devices(1)%formats(1:2) = [1, 2]  ! PCM, IEEE Float

    ! Default input device
    devices(2)%name = "Default Microphone"
    devices(2)%id = "default_input"
    devices(2)%is_input = .true.
    devices(2)%channels = 1
    devices(2)%sample_rates(1:3) = [16000, 44100, 48000]
    devices(2)%formats(1) = 1  ! PCM
  end subroutine enumerate_audio_devices

  ! ============================================================================
  ! Platform Video Device Enumeration
  ! ============================================================================

  subroutine enumerate_video_devices(devices)
    type(platform_video_device), dimension(:), allocatable, intent(out) :: devices

    ! Use fallback for all platforms (simplified)
    allocate(devices(1))
    devices(1)%name = "Default Camera"
    devices(1)%id = "default_camera"
    devices(1)%is_default = .true.
    devices(1)%width = 1280
    devices(1)%height = 720
    devices(1)%frame_rates(1:3) = [15, 30, 60]
    devices(1)%pixel_formats(1:2) = [1, 2]  ! RGB32, YUV420
  end subroutine enumerate_video_devices

  ! ============================================================================
  ! Codec Support Detection
  ! ============================================================================

  subroutine get_supported_audio_codecs(codecs)
    character(len=:), dimension(:), allocatable, intent(out) :: codecs

    allocate(character(len=20) :: codecs(4))
    codecs(1) = "audio/pcm"      ! Always supported
    codecs(2) = "audio/mpeg"     ! MP3
    codecs(3) = "audio/flac"     ! FLAC
    codecs(4) = "audio/aac"      ! AAC
  end subroutine get_supported_audio_codecs

  subroutine get_supported_video_codecs(codecs)
    character(len=:), dimension(:), allocatable, intent(out) :: codecs

    allocate(character(len=20) :: codecs(4))
    codecs(1) = "video/raw"      ! Raw video
    codecs(2) = "video/h264"     ! H.264
    codecs(3) = "video/mpeg4"    ! MPEG-4
    codecs(4) = "video/av1"      ! AV1
  end subroutine get_supported_video_codecs

  ! ============================================================================
  ! Hardware Acceleration Detection
  ! ============================================================================

  function has_hardware_acceleration() result(has_hw_accel)
    logical :: has_hw_accel
    has_hw_accel = .true.  ! Assume available for now
  end function has_hardware_acceleration

  function get_accelerated_codecs() result(codecs)
    character(len=:), dimension(:), allocatable :: codecs

    if (has_hardware_acceleration()) then
      allocate(character(len=20) :: codecs(3))
      codecs(1) = "video/h264"
      codecs(2) = "video/mpeg4"
      codecs(3) = "video/av1"
    else
      allocate(character(len=20) :: codecs(0))
    end if
  end function get_accelerated_codecs

end module forge_multimedia_platform