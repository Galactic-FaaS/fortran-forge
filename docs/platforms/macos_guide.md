# macOS Development Guide

This guide covers macOS-specific development with ForGE, including Cocoa integration, AppKit usage, and macOS development best practices.

## macOS-Specific Features

### Cocoa/AppKit Integration

ForGE provides seamless Cocoa integration for native macOS applications:

```fortran
use forge_platform_cocoa
use iso_c_binding

type(forge_cocoa_platform) :: platform
type(platform_window_handle) :: window_handle
type(forge_status) :: status

! Initialize Cocoa platform
call platform%init(status)

! Create NSWindow
call platform%create_window(window_handle, "My App", 800, 600, status)

! Access Cocoa objects
nswindow = window_handle%cocoa_window
nsview = window_handle%cocoa_view

! Use Cocoa APIs directly
call objc_msgSend(nswindow, "setTitle:", "New Title")
call objc_msgSend(nsview, "setNeedsDisplay:", YES)
```

### Objective-C Runtime Integration

```fortran
! Direct Objective-C messaging
program objc_integration
    use forge_objc_runtime

    type(objc_object) :: app, window, button
    type(objc_selector) :: sel_alloc, sel_init, sel_addSubview

    ! Get Objective-C classes
    nsapplication_class = objc_getClass("NSApplication")
    nswindow_class = objc_getClass("NSWindow")
    nsbutton_class = objc_getClass("NSButton")

    ! Create application
    app = objc_msgSend(objc_msgSend(nsapplication_class, "alloc"), "init")
    call objc_msgSend(app, "run")

end program objc_integration
```

### macOS Event Handling

```fortran
! Cocoa event processing
subroutine handle_cocoa_event(event)
    use cocoa_events
    type(nsevent), intent(in) :: event

    event_type = objc_msgSend(event, "type")

    select case (event_type)
    case (NSLeftMouseDown)
        ! Handle mouse down
        location = objc_msgSend(event, "locationInWindow")
        call handle_mouse_down(location)
    case (NSKeyDown)
        ! Handle key press
        key_code = objc_msgSend(event, "keyCode")
        call handle_key_press(key_code)
    case (NSApplicationDefined)
        ! Custom event
        call handle_custom_event(event)
    end select
end subroutine handle_cocoa_event
```

## macOS Build Configuration

### Xcode Integration

```xml
<!-- ForGE.xcconfig - Xcode configuration -->
FORGE_INCLUDE_PATH = $(SRCROOT)/../fortran-forge/src
FORGE_LIBRARY_PATH = $(SRCROOT)/../fortran-forge/build/lib

GCC_PREPROCESSOR_DEFINITIONS = $(inherited) FORGE_MACOS=1
HEADER_SEARCH_PATHS = $(FORGE_INCLUDE_PATH)
LIBRARY_SEARCH_PATHS = $(FORGE_LIBRARY_PATH)
OTHER_LDFLAGS = -lforge -framework Cocoa -framework AppKit -framework CoreFoundation
```

### CMake macOS Configuration

```cmake
# macOS-specific CMake configuration
if(APPLE)
    # macOS frameworks
    set(MACOS_FRAMEWORKS
        Cocoa
        AppKit
        CoreFoundation
        Foundation
        QuartzCore
    )

    # macOS bundle properties
    set_target_properties(my_app PROPERTIES
        MACOSX_BUNDLE TRUE
        MACOSX_BUNDLE_GUI_IDENTIFIER "com.yourcompany.myapp"
        MACOSX_BUNDLE_BUNDLE_NAME "My ForGE App"
        MACOSX_BUNDLE_BUNDLE_VERSION "1.0.0"
        MACOSX_BUNDLE_SHORT_VERSION_STRING "1.0"
        MACOSX_BUNDLE_INFO_PLIST "${CMAKE_SOURCE_DIR}/Info.plist.in"
    )

    # Link frameworks
    target_link_libraries(my_app ForGE::forge ${MACOS_FRAMEWORKS})
endif()
```

### Homebrew Integration

```bash
# Install dependencies via Homebrew
brew install gcc
brew install cmake
brew install pkg-config

# Build ForGE
git clone https://github.com/your-org/fortran-forge.git
cd fortran-forge
mkdir build && cd build
cmake .. -DCMAKE_BUILD_TYPE=Release
make -j$(sysctl -n hw.ncpu)
sudo make install
```

## macOS UI Guidelines

### macOS Design Language

```fortran
! macOS Big Sur/Monterey style application
program macos_modern_app
    use forge
    use forge_macos_theme

    type(forge_application) :: app
    type(forge_window_t) :: window
    type(macos_theme) :: theme

    ! Apply macOS theme
    call theme%apply_macos_theme()

    ! Create window with macOS styling
    call window%set_titlebar_appears_transparent(.true.)
    call window%enable_full_size_content_view(.true.)
    call window%set_title_visibility(TITLE_VISIBILITY_HIDDEN)

    ! Use macOS-style controls
    call button%set_bezel_style(BEZEL_STYLE_ROUNDED)

end program macos_modern_app
```

### macOS Human Interface Guidelines

```fortran
! Following macOS HIG
program hig_compliant_app
    use forge_macos_hig

    ! Proper window sizing
    call window%set_content_min_size(400, 300)
    call window%set_content_max_size(1200, 800)

    ! Standard toolbar
    call window%add_toolbar(standard_toolbar)

    ! Sheet dialogs for modal operations
    call window%begin_sheet(modal_dialog, completion_handler)

    ! Proper menu bar
    call setup_macos_menus()

end program hig_compliant_app
```

### Dark Mode Support

```fortran
! macOS dark mode integration
program dark_mode_app
    use forge_macos_appearance

    type(appearance_observer) :: observer

    ! Monitor appearance changes
    call observer%set_appearance_changed_callback(appearance_changed)

    ! Get current appearance
    if (is_dark_mode()) then
        call apply_dark_theme()
    else
        call apply_light_theme()
    end if

contains

    subroutine appearance_changed()
        if (is_dark_mode()) then
            call apply_dark_theme()
        else
            call apply_light_theme()
        end if
    end subroutine appearance_changed

end program dark_mode_app
```

## macOS System Integration

### macOS Notifications

```fortran
! macOS notification center
program notification_app
    use forge_macos_notifications

    type(nsusernotification) :: notification

    call notification%set_title("Task Complete")
    call notification%set_subtitle("Processing finished")
    call notification%set_informative_text("Your long-running task has completed successfully")
    call notification%set_sound_name("Ping")
    call notification%deliver()

end program notification_app
```

### Keychain Integration

```fortran
! macOS keychain services
program keychain_app
    use forge_macos_keychain

    type(keychain_item) :: item

    ! Store password
    call item%set_service("MyApp")
    call item%set_account("user@example.com")
    call item%set_password("secret_password")
    call item%add_to_keychain()

    ! Retrieve password
    password = item%password_from_keychain("MyApp", "user@example.com")

end program keychain_app
```

### Spotlight Integration

```fortran
! macOS spotlight integration
program spotlight_app
    use forge_macos_spotlight

    type(spotlight_importer) :: importer

    ! Register document types
    call importer%register_document_type(".myext", "MyApp Document")
    call importer%set_document_icon(".myext", "document_icon.icns")

    ! Index document content
    call importer%index_document(file_path, title, content)

end program spotlight_app
```

### macOS Services

```fortran
! macOS services integration
program services_app
    use forge_macos_services

    ! Register service
    call register_service("MyApp Service", "Process Text", service_handler)

contains

    subroutine service_handler(pboard, user_data, error)
        type(nspasteboard), intent(in) :: pboard
        type(nsstring), intent(in) :: user_data
        type(nserror), intent(out) :: error

        ! Process text from pasteboard
        text = pboard%string_for_type(NSPasteboardTypeString)
        processed_text = process_text(text)

        ! Put result back on pasteboard
        call pboard%set_string(processed_text, NSPasteboardTypeString)
    end subroutine service_handler

end program services_app
```

## macOS Performance Optimization

### macOS-Specific Optimizations

```fortran
! macOS performance optimizations
program optimized_macos_app
    use forge_platform_cocoa

    ! Use Grand Central Dispatch
    call use_gcd_for_concurrency()

    ! Metal graphics acceleration
    call enable_metal_acceleration()

    ! macOS memory management
    call use_arc_optimization()

    ! App Nap prevention
    call prevent_app_nap_for_task()

end program optimized_macos_app
```

### Cocoa Performance

```fortran
! Cocoa performance tips
program cocoa_performance_app
    use forge_platform_cocoa

    ! Layer-backed views
    call enable_layer_backing()

    ! Asynchronous drawing
    call use_async_drawing()

    ! Efficient scrolling
    call optimize_scroll_performance()

end program cocoa_performance_app
```

## macOS Deployment

### macOS App Bundles

```cmake
# CMake app bundle configuration
set(MACOSX_BUNDLE_ICON_FILE "AppIcon.icns")
set(MACOSX_BUNDLE_GUI_IDENTIFIER "com.yourcompany.${PROJECT_NAME}")
set(MACOSX_BUNDLE_BUNDLE_NAME "${PROJECT_NAME}")
set(MACOSX_BUNDLE_BUNDLE_VERSION "${PROJECT_VERSION}")
set(MACOSX_BUNDLE_SHORT_VERSION_STRING "${PROJECT_VERSION}")
set(MACOSX_BUNDLE_COPYRIGHT "Copyright © 2025 Your Company")

# Copy icon
set_source_files_properties("${CMAKE_SOURCE_DIR}/resources/AppIcon.icns"
    PROPERTIES MACOSX_PACKAGE_LOCATION "Resources")

# Copy resources
set_source_files_properties("${CMAKE_SOURCE_DIR}/resources/data.txt"
    PROPERTIES MACOSX_PACKAGE_LOCATION "Resources")
```

### Info.plist Configuration

```xml
<!-- Info.plist -->
<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0">
<dict>
    <key>CFBundleDevelopmentRegion</key>
    <string>en</string>
    <key>CFBundleExecutable</key>
    <string>my_app</string>
    <key>CFBundleIconFile</key>
    <string>AppIcon.icns</string>
    <key>CFBundleIdentifier</key>
    <string>com.yourcompany.myapp</string>
    <key>CFBundleInfoDictionaryVersion</key>
    <string>6.0</string>
    <key>CFBundleName</key>
    <string>My ForGE App</string>
    <key>CFBundlePackageType</key>
    <string>APPL</string>
    <key>CFBundleShortVersionString</key>
    <string>1.0</string>
    <key>CFBundleVersion</key>
    <string>1</string>
    <key>LSMinimumSystemVersion</key>
    <string>10.12</string>
    <key>NSHumanReadableCopyright</key>
    <string>Copyright © 2025 Your Company. All rights reserved.</string>
    <key>NSMainNibFile</key>
    <string>MainMenu</string>
    <key>NSPrincipalClass</key>
    <string>NSApplication</string>
</dict>
</plist>
```

### Code Signing and Notarization

```bash
# Sign application
codesign --deep --force --verify --verbose --sign "Developer ID Application: Your Name (TEAMID)" MyApp.app

# Create notarization request
xcrun altool --notarize-app --primary-bundle-id "com.yourcompany.myapp" --username "your@email.com" --password "@keychain:AC_PASSWORD" --file MyApp.app

# Check notarization status
xcrun altool --notarization-info REQUEST_UUID --username "your@email.com" --password "@keychain:AC_PASSWORD"

# Staple notarization ticket
xcrun stapler staple MyApp.app
```

### macOS DMG Creation

```bash
# Create DMG
create-dmg \
    --volname "My ForGE App" \
    --volicon "AppIcon.icns" \
    --window-pos 200 120 \
    --window-size 800 400 \
    --icon-size 100 \
    --icon "MyApp.app" 200 190 \
    --hide-extension "MyApp.app" \
    --app-drop-link 600 185 \
    "MyApp.dmg" \
    "MyApp.app"
```

### Mac App Store

```xml
<!-- App Store Info.plist additions -->
<key>LSApplicationCategoryType</key>
<string>public.app-category.utilities</string>
<key>LSMinimumSystemVersion</key>
<string>10.12</string>
<key>CFBundleSupportedPlatforms</key>
<array>
    <string>MacOSX</string>
</array>
<key>com.apple.security.app-sandbox</key>
<true/>
<key>com.apple.security.files.user-selected.read-write</key>
<true/>
```

## macOS Development Best Practices

### macOS Conventions

1. **Follow macOS Human Interface Guidelines**
   - Use proper window chrome and controls
   - Implement standard keyboard shortcuts (Cmd+C, Cmd+V, etc.)
   - Support full screen mode and window restoration

2. **macOS Integration**
   - Provide proper app bundle structure
   - Support macOS notifications and services
   - Integrate with system preferences

3. **Performance**
   - Use Grand Central Dispatch for concurrency
   - Leverage Metal for graphics acceleration
   - Optimize memory usage with ARC

4. **Security**
   - Use macOS sandboxing
   - Implement proper code signing
   - Follow macOS security best practices

### macOS-Specific Error Handling

```fortran
! macOS error handling
program robust_macos_app
    use forge_platform_cocoa

    call do_macos_operation()

contains

    subroutine do_macos_operation()
        type(nserror) :: error

        call some_cocoa_operation(error)

        if (error%code /= 0) then
            call handle_macos_error(error)
        end if
    end subroutine do_macos_operation

    subroutine handle_macos_error(error)
        type(nserror), intent(in) :: error

        domain = error%domain()
        code = error%code()
        description = error%localized_description()

        select case (domain)
        case (NSCocoaErrorDomain)
            select case (code)
            case (NSFileNoSuchFileError)
                call show_error_dialog("File not found: " // description)
            case (NSFileWriteNoPermissionError)
                call show_error_dialog("Permission denied: " // description)
            case default
                call show_error_dialog("Cocoa error: " // description)
            end select
        case default
            call show_error_dialog("Error: " // description)
        end select
    end subroutine handle_macos_error

end program robust_macos_app
```

### macOS File System Integration

```fortran
! macOS file system integration
program macos_fs_app
    use forge_macos_filesystem

    ! Standard macOS directories
    app_support_dir = get_application_support_directory()
    documents_dir = get_documents_directory()
    downloads_dir = get_downloads_directory()

    ! Create app support directory
    call create_directory_recursive(app_support_dir // "/MyApp")

    ! Use NSFileManager for file operations
    call move_file_to_trash(file_path)

    ! File coordination
    call coordinate_reading_file(file_path, reading_completion)
    call coordinate_writing_file(file_path, writing_completion)

end program macos_fs_app
```

### Sandbox Considerations

```fortran
! macOS sandbox compliance
program sandboxed_app
    use forge_macos_sandbox

    ! Request user-selected file access
    call request_file_access(file_url, completion_handler)

    ! Use security-scoped bookmarks
    bookmark_data = create_security_scoped_bookmark(file_url)
    resolved_url = resolve_security_scoped_bookmark(bookmark_data)

    ! Network access (requires entitlement)
    call perform_network_request(url)

end program sandboxed_app
```

## Next Steps

- Read the [Windows guide](windows_guide.md) for Windows development
- Learn about [Linux development](linux_guide.md)
- Explore [deployment strategies](deployment_guide.md)
- Study [performance optimization](performance_guide.md) for macOS