# Windows Development Guide

This guide covers Windows-specific development with ForGE, including platform features, Win32 integration, and Windows development best practices.

## Windows-Specific Features

### Win32 API Integration

ForGE provides seamless Win32 API integration for Windows applications:

```fortran
use forge_platform_windows
use iso_c_binding

type(forge_windows_platform) :: platform
type(platform_window_handle) :: window_handle
type(forge_status) :: status

! Initialize Windows platform
call platform%init(status)

! Create window with Win32 handle access
call platform%create_window(window_handle, "My App", 800, 600, status)

! Access raw Win32 handle
hwnd = window_handle%native_handle

! Use Win32 APIs directly
call SetWindowText(hwnd, "New Title"C)
```

### Windows Message Handling

```fortran
! Custom Windows message processing
subroutine handle_windows_message(hwnd, msg, wparam, lparam) bind(C)
    use iso_c_binding
    type(c_ptr), value :: hwnd
    integer(c_int), value :: msg, wparam, lparam

    select case (msg)
    case (WM_CLOSE)
        ! Handle window close
        call cleanup_resources()
    case (WM_SIZE)
        ! Handle window resize
        width = LOWORD(lparam)
        height = HIWORD(lparam)
        call resize_ui(width, height)
    case (WM_KEYDOWN)
        ! Handle key press
        key_code = wparam
        call handle_key_press(key_code)
    end select
end subroutine handle_windows_message
```

### Windows Resources

#### Icons and Cursors

```fortran
! Load Windows resources
hicon = LoadIcon(GetModuleHandle(NULL), IDI_APPLICATION)
hcursor = LoadCursor(NULL, IDC_ARROW)

! Set window icon
call SendMessage(hwnd, WM_SETICON, ICON_BIG, hicon)
```

#### Menus

```fortran
! Create Windows menu
hmenu = CreateMenu()
hfile_menu = CreatePopupMenu()

! Add menu items
call AppendMenu(hfile_menu, MF_STRING, IDM_OPEN, "Open"C)
call AppendMenu(hfile_menu, MF_STRING, IDM_EXIT, "Exit"C)
call AppendMenu(hmenu, MF_POPUP, hfile_menu, "File"C)

! Set window menu
call SetMenu(hwnd, hmenu)
```

### Windows Services

```fortran
! Windows service integration (advanced)
program windows_service
    use forge_windows_service

    type(windows_service) :: service

    call service%init("MyForGEApp")
    call service%set_service_main(service_main)
    call service%start()

contains

    subroutine service_main()
        ! Service main function
        call initialize_application()
        call run_service_loop()
    end subroutine service_main

end program windows_service
```

## Windows Build Configuration

### Visual Studio Integration

```xml
<!-- ForGE.props - Visual Studio property sheet -->
<?xml version="1.0" encoding="utf-8"?>
<Project ToolsVersion="4.0" xmlns="http://schemas.microsoft.com/developer/msproj/2003">
  <ImportGroup Label="PropertySheets" />
  <PropertyGroup Label="UserMacros">
    <ForGEInclude>$(SolutionDir)..\fortran-forge\src</ForGEInclude>
    <ForGELib>$(SolutionDir)..\fortran-forge\build\lib</ForGELib>
  </PropertyGroup>
  <ItemDefinitionGroup>
    <ClCompile>
      <AdditionalIncludeDirectories>$(ForGEInclude);%(AdditionalIncludeDirectories)</AdditionalIncludeDirectories>
    </ClCompile>
    <Link>
      <AdditionalLibraryDirectories>$(ForGELib);%(AdditionalLibraryDirectories)</AdditionalLibraryDirectories>
      <AdditionalDependencies>forge.lib;gdi32.lib;user32.lib;kernel32.lib;%(AdditionalDependencies)</AdditionalDependencies>
    </Link>
  </ItemDefinitionGroup>
</Project>
```

### CMake Windows Configuration

```cmake
# Windows-specific CMake configuration
if(WIN32)
    # Windows-specific libraries
    set(WINDOWS_LIBRARIES
        gdi32
        user32
        kernel32
        shell32
        ole32
        uuid
    )

    # Windows executable properties
    set_target_properties(my_app PROPERTIES
        WIN32_EXECUTABLE TRUE  # No console window
        LINK_FLAGS "/SUBSYSTEM:WINDOWS /ENTRY:mainCRTStartup"
    )

    target_link_libraries(my_app ForGE::forge ${WINDOWS_LIBRARIES})
endif()
```

### MinGW/Cygwin Builds

```bash
# MinGW build
gfortran -I/path/to/forge/include \
         -L/path/to/forge/lib \
         -Wl,--subsystem,windows \  # No console
         main.f90 -lforge -o my_app.exe

# With console for debugging
gfortran -I/path/to/forge/include \
         -L/path/to/forge/lib \
         main.f90 -lforge -o my_app_debug.exe
```

## Windows UI Guidelines

### Windows Design Language

```fortran
! Windows 11 style application
program windows11_app
    use forge
    use forge_windows_theme

    type(forge_application) :: app
    type(forge_window_t) :: window
    type(windows_theme) :: theme

    ! Apply Windows 11 theme
    call theme%apply_windows11_theme()

    ! Create Mica/Acrylic effect window
    call window%set_background_effect(BACKGROUND_EFFECT_MICA)
    call window%enable_rounded_corners(.true.)

    ! Use Windows 11 controls
    call button%set_style(STYLE_WINDOWS11_BUTTON)
end program windows11_app
```

### High DPI Support

```fortran
! High DPI awareness
program high_dpi_app
    use forge_platform_windows

    ! Enable DPI awareness before creating windows
    call set_process_dpi_aware()

    ! Handle DPI changes
    call window%on_dpi_changed(handle_dpi_change)

contains

    subroutine handle_dpi_change(new_dpi)
        integer, intent(in) :: new_dpi

        ! Scale UI elements
        scale_factor = new_dpi / 96.0  ! 96 DPI = 100%
        call scale_ui_elements(scale_factor)
    end subroutine handle_dpi_change

end program high_dpi_app
```

### Windows Accessibility

```fortran
! Windows accessibility support
program accessible_app
    use forge_accessibility

    type(accessible_widget) :: accessible_button

    ! Set accessibility properties
    call accessible_button%set_name("Submit Button")
    call accessible_button%set_description("Submits the form data")
    call accessible_button%set_role(ROLE_BUTTON)

    ! Screen reader support
    call accessible_button%set_accelerator("Alt+S")
    call accessible_button%set_default_action("Press")

end program accessible_app
```

## Windows System Integration

### File Associations

```fortran
! Register file associations
program file_association_app
    use forge_windows_registry

    type(registry_key) :: classes_key

    ! Register .myext extension
    call classes_key%create(".myext")
    call classes_key%set_value("", "MyApp.Document")
    call classes_key%set_value("Content Type", "application/myapp")

    ! Register application
    call classes_key%create("MyApp.Document\shell\open\command")
    call classes_key%set_value("", '"C:\Program Files\MyApp\myapp.exe" "%1"')

end program file_association_app
```

### Windows Notifications

```fortran
! Windows toast notifications
program notification_app
    use forge_windows_notifications

    type(toast_notification) :: notification

    call notification%set_title("Task Complete")
    call notification%set_message("Your long-running task has finished")
    call notification%set_icon("app_icon.png")
    call notification%show()

end program notification_app
```

### Taskbar Integration

```fortran
! Windows taskbar features
program taskbar_app
    use forge_windows_taskbar

    type(taskbar_progress) :: progress

    ! Show progress in taskbar
    call progress%set_range(0, 100)
    call progress%set_value(50)
    call progress%set_state(TASKBAR_PROGRESS_NORMAL)

    ! Taskbar jump list
    call add_taskbar_jump_item("Open Recent", "recent.txt")
    call add_taskbar_jump_item("New Document", "")

end program taskbar_app
```

## Windows Performance Optimization

### Windows-Specific Optimizations

```fortran
! Windows performance tips
program optimized_windows_app
    use forge_platform_windows

    ! Disable Windows ghosting (prevents UI hanging)
    call disable_process_windows_ghosting()

    ! Use Windows message queue efficiently
    call optimize_message_queue()

    ! Memory-mapped files for large data
    call use_memory_mapped_files(large_file)

    ! Windows-specific threading
    call use_windows_thread_pool()

end program optimized_windows_app
```

### Windows Graphics

```fortran
! Direct2D/DirectWrite integration (planned)
program direct2d_app
    use forge_direct2d

    type(direct2d_context) :: d2d

    call d2d%init()
    call d2d%begin_draw()

    ! High-performance 2D graphics
    call d2d%draw_rectangle(rect, brush)
    call d2d%draw_text("Hello Direct2D", font, brush)

    call d2d%end_draw()

end program direct2d_app
```

## Windows Deployment

### Windows Installer

```xml
<!-- WiX installer configuration -->
<?xml version="1.0" encoding="UTF-8"?>
<Wix xmlns="http://schemas.microsoft.com/wix/2006/wi">
  <Product Id="*" Name="My ForGE App" Version="1.0.0.0">
    <Package InstallerVersion="200" Compressed="yes" />

    <Directory Id="TARGETDIR" Name="SourceDir">
      <Directory Id="ProgramFilesFolder">
        <Directory Id="MyAppDir" Name="My ForGE App">
          <Component Id="MainExecutable" Guid="*">
            <File Id="MyAppExe" Source="my_app.exe" KeyPath="yes" />
          </Component>
          <Component Id="ForGELib" Guid="*">
            <File Id="ForGEDLL" Source="forge.dll" />
          </Component>
        </Directory>
      </Directory>
    </Directory>

    <Feature Id="MainFeature" Title="Main Application">
      <ComponentRef Id="MainExecutable" />
      <ComponentRef Id="ForGELib" />
    </Feature>
  </Product>
</Wix>
```

### Portable Windows Apps

```fortran
! Portable application setup
program portable_app
    use forge_portable

    ! Detect portable mode
    if (is_portable_mode()) then
        ! Use relative paths
        config_path = "./config/settings.ini"
        data_path = "./data/"
    else
        ! Use standard Windows paths
        config_path = get_appdata_path() // "/MyApp/settings.ini"
        data_path = get_appdata_path() // "/MyApp/data/"
    end if

end program portable_app
```

### Windows Store Apps

```xml
<!-- Package.appxmanifest for Microsoft Store -->
<?xml version="1.0" encoding="utf-8"?>
<Package
  xmlns="http://schemas.microsoft.com/appx/manifest/foundation/windows10"
  xmlns:mp="http://schemas.microsoft.com/appx/2014/phone/manifest"
  xmlns:uap="http://schemas.microsoft.com/appx/manifest/uap/windows10">

  <Identity Name="MyForGEApp" Version="1.0.0.0" Publisher="CN=YourName" />
  <mp:PhoneIdentity PhoneProductId="12345678-1234-1234-1234-123456789012" PhonePublisherId="00000000-0000-0000-0000-000000000000"/>

  <Properties>
    <DisplayName>My ForGE App</DisplayName>
    <PublisherDisplayName>Your Name</PublisherDisplayName>
    <Logo>Assets\StoreLogo.png</Logo>
  </Properties>

  <Dependencies>
    <TargetDeviceFamily Name="Windows.Universal" MinVersion="10.0.0.0" MaxVersionTested="10.0.0.0" />
  </Dependencies>

  <Resources>
    <Resource Language="en-us"/>
  </Resources>

  <Applications>
    <Application Id="App" Executable="my_app.exe" EntryPoint="Windows.FullTrustApplication">
      <uap:VisualElements DisplayName="My ForGE App" Description="A ForGE application"
        BackgroundColor="transparent" Square150x150Logo="Assets\Square150x150Logo.png"
        Square44x44Logo="Assets\Square44x44Logo.png">
        <uap:DefaultTile Wide310x150Logo="Assets\Wide310x150Logo.png" />
        <uap:SplashScreen Image="Assets\SplashScreen.png" />
      </uap:VisualElements>
    </Application>
  </Applications>

  <Capabilities>
    <rescap:Capability Name="runFullTrust" />
  </Capabilities>
</Package>
```

## Windows Development Best Practices

### Windows Conventions

1. **Follow Windows Design Guidelines**
   - Use proper window borders and decorations
   - Implement standard keyboard shortcuts (Ctrl+C, Ctrl+V, etc.)
   - Support high DPI displays

2. **Windows Integration**
   - Register file associations
   - Add context menu items
   - Support Windows notifications

3. **Performance**
   - Use Windows memory management
   - Leverage Windows threading APIs
   - Optimize for Windows graphics

4. **Security**
   - Use Windows security features
   - Implement proper UAC handling
   - Follow Windows security best practices

### Windows-Specific Error Handling

```fortran
! Windows error handling
program robust_windows_app
    use forge_platform_windows

    call do_windows_operation()

contains

    subroutine do_windows_operation()
        integer :: result

        result = some_windows_api_call()

        if (result == 0) then  ! Windows API error
            error_code = GetLastError()
            call handle_windows_error(error_code)
        end if
    end subroutine do_windows_operation

    subroutine handle_windows_error(error_code)
        integer, intent(in) :: error_code

        select case (error_code)
        case (ERROR_FILE_NOT_FOUND)
            call show_error_dialog("File not found")
        case (ERROR_ACCESS_DENIED)
            call show_error_dialog("Access denied")
        case default
            call show_error_dialog("Windows error: " // int_to_string(error_code))
        end select
    end subroutine handle_windows_error

end program robust_windows_app
```

## Next Steps

- Read the [Linux guide](linux_guide.md) for cross-platform development
- Learn about [macOS development](macos_guide.md)
- Explore [deployment strategies](deployment_guide.md)
- Study [performance optimization](performance_guide.md) for Windows