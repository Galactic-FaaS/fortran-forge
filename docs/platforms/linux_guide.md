# Linux Development Guide

This guide covers Linux-specific development with ForGE, including X11 integration, Wayland support, and Linux development best practices.

## Linux-Specific Features

### X11 Integration

ForGE provides comprehensive X11 support for traditional Linux desktop environments:

```fortran
use forge_platform_x11
use iso_c_binding

type(forge_x11_platform) :: platform
type(platform_window_handle) :: window_handle
type(forge_status) :: status

! Initialize X11 platform
call platform%init(status)

! Create X11 window
call platform%create_window(window_handle, "My App", 800, 600, status)

! Access X11 display and window
display = window_handle%x11_display
window = window_handle%x11_window

! Use X11 APIs directly
call XStoreName(display, window, "New Title"C)
call XFlush(display)
```

### X11 Event Handling

```fortran
! X11 event processing
subroutine handle_x11_event(display, event)
    use x11_events
    type(c_ptr), intent(in) :: display
    type(xevent), intent(in) :: event

    select case (event%type)
    case (Expose)
        ! Handle expose event (redraw)
        call redraw_window()
    case (ConfigureNotify)
        ! Handle window resize
        width = event%xconfigure%width
        height = event%xconfigure%height
        call resize_ui(width, height)
    case (KeyPress)
        ! Handle key press
        keysym = XLookupKeysym(event%xkey, 0)
        call handle_key_press(keysym)
    case (ButtonPress)
        ! Handle mouse click
        x = event%xbutton%x
        y = event%xbutton%y
        call handle_mouse_click(x, y)
    end select
end subroutine handle_x11_event
```

### Wayland Support

```fortran
! Wayland integration (planned)
program wayland_app
    use forge_platform_wayland

    type(forge_wayland_platform) :: platform
    type(wl_surface) :: surface

    call platform%init()
    surface = platform%create_surface()

    ! Wayland-specific rendering
    call platform%render_to_surface(surface, render_callback)

end program wayland_app
```

## Linux Build Configuration

### GCC/GFortran Configuration

```bash
# Optimized Linux build
gfortran -O3 -march=native -flto \
         -I/usr/local/include/forge \
         -L/usr/local/lib \
         main.f90 -lforge \
         -lX11 -lcairo -lpthread \
         -o my_app

# Debug build
gfortran -g -O0 -fcheck=all -fbacktrace \
         -I/usr/local/include/forge \
         -L/usr/local/lib \
         main.f90 -lforge \
         -o my_app_debug
```

### CMake Linux Configuration

```cmake
# Linux-specific CMake configuration
if(UNIX AND NOT APPLE)
    # Linux-specific libraries
    find_package(X11 REQUIRED)
    find_package(PkgConfig REQUIRED)

    pkg_check_modules(CAIRO REQUIRED cairo)
    pkg_check_modules(PANGO REQUIRED pango)

    set(LINUX_LIBRARIES
        ${X11_LIBRARIES}
        ${CAIRO_LIBRARIES}
        ${PANGO_LIBRARIES}
        pthread
        dl
        m
    )

    # Include directories
    include_directories(
        ${X11_INCLUDE_DIR}
        ${CAIRO_INCLUDE_DIRS}
        ${PANGO_INCLUDE_DIRS}
    )

    target_link_libraries(my_app ForGE::forge ${LINUX_LIBRARIES})
endif()
```

### Package Management

#### Debian/Ubuntu

```bash
# Install build dependencies
sudo apt-get update
sudo apt-get install gfortran cmake pkg-config \
                     libx11-dev libcairo2-dev libpango1.0-dev \
                     libgtk-3-dev libglib2.0-dev

# Build ForGE
git clone https://github.com/your-org/fortran-forge.git
cd fortran-forge
mkdir build && cd build
cmake ..
make -j$(nproc)
sudo make install
```

#### Red Hat/Fedora

```bash
# Install build dependencies
sudo dnf install gcc-gfortran cmake pkgconfig \
                libX11-devel cairo-devel pango-devel \
                gtk3-devel glib2-devel

# Build process same as above
```

#### Arch Linux

```bash
# Install build dependencies
sudo pacman -S gcc-fortran cmake pkgconf \
             libx11 cairo pango \
             gtk3 glib2

# Build process same as above
```

## Linux UI Guidelines

### GTK Integration

```fortran
! GTK theme integration
program gtk_integrated_app
    use forge_gtk_integration

    type(gtk_theme) :: theme

    ! Load GTK theme
    call theme%load_from_system()

    ! Apply GTK colors and fonts
    call apply_gtk_theme_to_widgets(theme)

    ! GTK file dialogs
    call show_gtk_file_chooser(callback)

end program gtk_integrated_app
```

### GNOME Integration

```fortran
! GNOME desktop integration
program gnome_app
    use forge_gnome_integration

    ! GNOME notifications
    call show_gnome_notification("Title", "Message", "dialog-information")

    ! GNOME settings integration
    theme_preference = get_gnome_theme_preference()
    font_size = get_gnome_font_size()

    ! GNOME keyring integration
    call store_password_in_keyring("myapp", "username", "password")

end program gnome_app
```

### KDE Integration

```fortran
! KDE desktop integration
program kde_app
    use forge_kde_integration

    ! KDE notifications
    call show_kde_notification("Title", "Message")

    ! KDE color scheme
    colors = get_kde_color_scheme()

    ! KDE file dialogs
    call show_kde_file_dialog(callback)

end program kde_app
```

## Linux System Integration

### D-Bus Integration

```fortran
! D-Bus IPC
program dbus_app
    use forge_dbus

    type(dbus_connection) :: connection
    type(dbus_message) :: message

    ! Connect to session bus
    call connection%connect_to_session_bus()

    ! Send message
    call message%create_method_call("org.freedesktop.Notifications", "/org/freedesktop/Notifications", "org.freedesktop.Notifications", "Notify")
    call message%append_string("My App")
    call message%append_uint32(0)
    call message%append_string("")
    call message%append_string("Notification Title")
    call message%append_string("Notification Body")

    call connection%send_message(message)

end program dbus_app
```

### systemd Integration

```fortran
! systemd service integration
program systemd_service
    use forge_systemd

    type(systemd_service) :: service

    call service%notify_ready()
    call service%notify_status("Processing data...")

    ! Watchdog
    do while (.true.)
        call service%notify_watchdog()
        call do_work()
    end do

end program systemd_service
```

### Linux Notifications

```fortran
! Linux desktop notifications
program notification_app
    use forge_linux_notifications

    type(notification) :: notif

    call notif%set_summary("Task Complete")
    call notif%set_body("Your long-running task has finished")
    call notif%set_icon("dialog-information")
    call notif%set_urgency(NOTIFICATION_URGENCY_NORMAL)
    call notif%add_action("view", "View Results", action_callback)

    call notif%show()

end program notification_app
```

## Linux Performance Optimization

### Linux-Specific Optimizations

```fortran
! Linux performance optimizations
program optimized_linux_app
    use forge_platform_linux

    ! Use Linux-specific allocators
    call use_tcmalloc_allocator()

    ! Optimize for Linux I/O
    call use_linux_aio()

    ! Linux-specific threading
    call use_linux_futexes()

    ! Memory management
    call use_linux_huge_pages()

end program optimized_linux_app
```

### X11 Performance

```fortran
! X11 performance tips
program x11_performance_app
    use forge_platform_x11

    ! Double buffering
    call enable_x11_double_buffering()

    ! X11 extensions
    call use_x11_shm_extension()  ! Shared memory
    call use_x11_composite_extension()

    ! Efficient rendering
    call use_x11_damage_extension()  ! Only redraw damaged areas

end program x11_performance_app
```

## Linux Deployment

### AppImage Creation

```bash
# Create AppImage
# 1. Build statically if possible
gfortran -static -O3 main.f90 -lforge -o my_app

# 2. Create AppDir structure
mkdir -p MyApp.AppDir/usr/bin
mkdir -p MyApp.AppDir/usr/lib
mkdir -p MyApp.AppDir/usr/share/applications
mkdir -p MyApp.AppDir/usr/share/icons/hicolor/256x256/apps

# Copy files
cp my_app MyApp.AppDir/usr/bin/
cp *.so MyApp.AppDir/usr/lib/  # If not static
cp my_app.desktop MyApp.AppDir/usr/share/applications/
cp my_app.png MyApp.AppDir/usr/share/icons/hicolor/256x256/apps/

# Create AppRun
cat > MyApp.AppDir/AppRun << 'EOF'
#!/bin/bash
HERE="$(dirname "$(readlink -f "${0}")")"
export LD_LIBRARY_PATH="${HERE}/usr/lib:${LD_LIBRARY_PATH}"
exec "${HERE}/usr/bin/my_app" "$@"
EOF
chmod +x MyApp.AppDir/AppRun

# Download appimagetool
wget https://github.com/AppImage/AppImageKit/releases/download/continuous/appimagetool-x86_64.AppImage
chmod +x appimagetool-x86_64.AppImage

# Create AppImage
./appimagetool-x86_64.AppImage MyApp.AppDir MyApp.AppImage
```

### Snap Packages

```yaml
# snapcraft.yaml
name: my-forge-app
version: '1.0.0'
summary: My ForGE Application
description: A ForGE-based application

grade: stable
confinement: strict

apps:
  my-app:
    command: my_app
    plugs: [home, network, x11]

parts:
  my-app:
    plugin: nil
    source: .
    build-packages: [gfortran, libforge-dev, libx11-dev, libcairo2-dev]
    override-build: |
      gfortran -O3 main.f90 -lforge -lX11 -lcairo -o my_app
      install -D my_app $SNAPCRAFT_PART_INSTALL/my_app
```

### Flatpak

```ini
# my_app.flatpak
[Application]
name=com.yourcompany.MyApp
runtime=org.freedesktop.Platform/x86_64/21.08
sdk=org.freedesktop.Sdk/x86_64/21.08

[Build]
buildsystem=simple
build-commands=
    gfortran -O3 main.f90 $(pkg-config --libs forge) -o my_app

[Modules]
# Dependencies would be listed here
```

### Linux Package Managers

#### Debian Package

```cmake
# CMake Debian package
set(CPACK_GENERATOR "DEB")
set(CPACK_DEBIAN_PACKAGE_NAME "my-forge-app")
set(CPACK_DEBIAN_PACKAGE_VERSION "1.0.0")
set(CPACK_DEBIAN_PACKAGE_MAINTAINER "Your Name <your.email@example.com>")
set(CPACK_DEBIAN_PACKAGE_DESCRIPTION "My ForGE Application")
set(CPACK_DEBIAN_PACKAGE_SECTION "utils")
set(CPACK_DEBIAN_PACKAGE_DEPENDS "libforge1 (>= 1.0.0), libc6 (>= 2.27)")

include(CPack)
```

#### RPM Package

```cmake
# CMake RPM package
set(CPACK_GENERATOR "RPM")
set(CPACK_RPM_PACKAGE_NAME "my-forge-app")
set(CPACK_RPM_PACKAGE_VERSION "1.0.0")
set(CPACK_RPM_PACKAGE_SUMMARY "My ForGE Application")
set(CPACK_RPM_PACKAGE_DESCRIPTION "A ForGE-based application")
set(CPACK_RPM_PACKAGE_MAINTAINER "Your Name <your.email@example.com>")
set(CPACK_RPM_PACKAGE_REQUIRES "forge >= 1.0.0, glibc >= 2.27")

include(CPack)
```

## Linux Development Best Practices

### Linux Conventions

1. **Follow Linux/Unix Conventions**
   - Use standard directory structure (/usr/bin, /usr/share, etc.)
   - Support command-line options (--help, --version)
   - Handle signals properly (SIGTERM, SIGINT)

2. **Desktop Integration**
   - Provide .desktop files
   - Support freedesktop.org standards
   - Integrate with system themes

3. **Security**
   - Use proper file permissions
   - Avoid running as root unnecessarily
   - Follow principle of least privilege

4. **Performance**
   - Use Linux-specific optimizations
   - Leverage kernel features (epoll, inotify)
   - Optimize memory usage

### Linux-Specific Error Handling

```fortran
! Linux error handling
program robust_linux_app
    use forge_platform_linux

    call do_linux_operation()

contains

    subroutine do_linux_operation()
        integer :: result

        result = some_linux_api_call()

        if (result == -1) then  ! Linux system call error
            error_code = errno()
            call handle_linux_error(error_code)
        end if
    end subroutine do_linux_operation

    subroutine handle_linux_error(error_code)
        integer, intent(in) :: error_code

        select case (error_code)
        case (ENOENT)
            call show_error_dialog("File not found")
        case (EACCES)
            call show_error_dialog("Permission denied")
        case (ENOMEM)
            call show_error_dialog("Out of memory")
        case default
            call show_error_dialog("System error: " // strerror(error_code))
        end select
    end subroutine handle_linux_error

end program robust_linux_app
```

### Signal Handling

```fortran
! Linux signal handling
program signal_handling_app
    use iso_c_binding
    use forge_signals

    ! Signal handlers
    call signal(SIGINT, interrupt_handler)
    call signal(SIGTERM, terminate_handler)
    call signal(SIGHUP, reload_config_handler)

contains

    subroutine interrupt_handler(signum) bind(C)
        integer(c_int), value :: signum
        print *, "Received SIGINT, shutting down gracefully..."
        call cleanup_and_exit()
    end subroutine interrupt_handler

    subroutine terminate_handler(signum) bind(C)
        integer(c_int), value :: signum
        print *, "Received SIGTERM, shutting down..."
        call cleanup_and_exit()
    end subroutine terminate_handler

    subroutine reload_config_handler(signum) bind(C)
        integer(c_int), value :: signum
        print *, "Received SIGHUP, reloading configuration..."
        call reload_configuration()
    end subroutine reload_config_handler

end program signal_handling_app
```

### Linux File System Integration

```fortran
! Linux file system integration
program linux_fs_app
    use forge_linux_filesystem

    ! XDG Base Directory support
    config_dir = get_xdg_config_home() // "/myapp/"
    data_dir = get_xdg_data_home() // "/myapp/"
    cache_dir = get_xdg_cache_home() // "/myapp/"

    ! Create directories if needed
    call create_directory_recursive(config_dir)

    ! Monitor file changes
    call watch_file_changes(config_file, config_changed_callback)

    ! Use inotify for file monitoring
    call setup_inotify_watch(directory, file_created_callback)

end program linux_fs_app
```

## Next Steps

- Read the [Windows guide](windows_guide.md) for Windows development
- Learn about [macOS development](macos_guide.md)
- Explore [deployment strategies](deployment_guide.md)
- Study [performance optimization](performance_guide.md) for Linux