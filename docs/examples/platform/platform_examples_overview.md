# Platform-Specific Examples

This section demonstrates platform-specific functionality in ForGE Qt, including Windows registry access, Linux system information, and cross-platform compatibility.

## Windows-Specific Features

### Registry Access

**File:** `examples/platform_windows_registry/platform_windows_registry.f90`

Demonstrates:
- Windows registry key access
- Reading and writing registry values
- Registry key enumeration
- Different registry hives (HKLM, HKCU, etc.)

```fortran
! Access Windows registry
call registry%open_key("HKEY_CURRENT_USER\\Software\\MyApp")
call registry%read_value("Version", version)
call registry%write_value("Setting", value)
```

## Linux-Specific Features

### System Information

**File:** `examples/platform_linux_system_info/platform_linux_system_info.f90`

Demonstrates:
- Reading Linux system information
- CPU usage and load monitoring
- Memory and swap statistics
- Disk usage information
- Process monitoring
- Network interface details

```fortran
! Get Linux system information
call sysinfo%get_cpu_usage(cpu_percent)
call sysinfo%get_memory_info(total, used, free)
call sysinfo%get_process_list(processes)
```

## Cross-Platform Compatibility

ForGE Qt provides abstractions for platform differences:

### File System Operations
```fortran
! Cross-platform path handling
type(forge_file_path) :: path
call path%set_path("/home/user/file.txt")  ! Works on all platforms
call path%to_native_path(native_path)      ! Converts to platform format
```

### System Integration
- **Process Management:** Cross-platform process spawning
- **Environment Variables:** Platform-independent access
- **System Directories:** Standardized directory locations
- **Network Interfaces:** Unified network interface enumeration

## Platform Detection

ForGE Qt provides compile-time and runtime platform detection:

```fortran
! Compile-time detection
#ifdef Q_OS_WIN
    ! Windows-specific code
#endif

#ifdef Q_OS_LINUX
    ! Linux-specific code
#endif

! Runtime detection
if (forge_platform%os_name() == "Windows") then
    ! Windows-specific runtime code
end if
```

## Platform-Specific APIs

### Windows APIs
- Registry access
- Windows services
- COM integration
- Windows-specific file operations

### Linux APIs
- System information (/proc filesystem)
- Package management
- SystemD services
- Linux-specific networking

### macOS APIs
- Keychain access
- Notification center
- macOS-specific UI integration

### Mobile APIs
- Touch gestures
- Sensor access
- Mobile-specific UI patterns
- Platform app stores

## Running Platform Examples

```bash
# Windows registry example (Windows only)
cmake --build build --target platform_windows_registry
./build/examples/platform_windows_registry/platform_windows_registry

# Linux system info example (Linux only)
cmake --build build --target platform_linux_system_info
./build/examples/platform_linux_system_info/platform_linux_system_info
```

## Platform Categories

| Platform | Examples | Features |
|----------|----------|----------|
| **Windows** | Registry | System integration |
| **Linux** | System Info | System monitoring |
| **macOS** | Notifications | User interaction |
| **Mobile** | Touch Gestures | Input handling |
| **Cross-Platform** | File Paths | Compatibility |

## Best Practices

1. **Conditional Compilation:** Use platform-specific code only when necessary
2. **Abstraction Layers:** Wrap platform differences in common interfaces
3. **Graceful Degradation:** Handle missing platform features
4. **Documentation:** Clearly document platform requirements
5. **Testing:** Test on all supported platforms

## Platform Support Matrix

| Feature | Windows | Linux | macOS | Mobile |
|---------|---------|-------|-------|--------|
| Registry | ✓ | ✗ | Keychain | ✗ |
| System Info | WMI | /proc | sysctl | Platform APIs |
| Notifications | ✓ | ✓ | ✓ | ✓ |
| Services | ✓ | systemd | launchd | ✗ |
| Touch | ✗ | ✗ | Trackpad | ✓ |

These examples demonstrate how to leverage platform-specific capabilities while maintaining cross-platform compatibility in ForGE Qt applications.