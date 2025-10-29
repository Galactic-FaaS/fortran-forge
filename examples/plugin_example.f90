!> @brief Example demonstrating the Qt-style plugin system in ForGE
!> @details Shows how to create, load, and use plugins with the QPluginLoader
!> @author ForGE Contributors
!> @date 2025
!> @license GPL-3.0-or-later

program plugin_example
    use iso_c_binding
    use forge_types
    use forge_plugin
    implicit none

    ! Plugin loader and metadata
    type(QPluginLoader) :: loader
    type(QPluginMetaData) :: meta
    type(QPluginMetaData), dimension(:), pointer :: static_plugins

    ! Plugin instance
    class(QFactoryInterface), pointer :: plugin_instance => null()

    ! Test objects
    class(forge_qobject), pointer :: test_object => null()

    ! Error handling
    type(QPluginError) :: error_info

    ! Local variables
    character(len=:), allocatable :: plugin_file
    character(len=:), allocatable :: iid
    character(len=:), allocatable :: class_name
    character(len=:), allocatable :: version
    logical :: success
    integer :: i, static_count

    write(*,*) "ForGE Plugin System Example"
    write(*,*) "============================"

    ! Initialize the loader
    call loader%init()

    ! Example 1: Static plugin enumeration
    write(*,*) ""
    write(*,*) "1. Static Plugin Enumeration:"
    write(*,*) "-----------------------------"

    static_plugins => QPluginLoader_staticPlugins()
    static_count = QPluginLoader_staticPluginCount()

    write(*,*) "Number of static plugins registered:", static_count

    do i = 1, static_count
        meta = static_plugins(i)
        iid = meta%get_iid()
        class_name = meta%get_class_name()
        version = meta%get_version()

        write(*,*) "  Plugin", i, ":"
        write(*,*) "    IID:", iid
        write(*,*) "    Class:", class_name
        write(*,*) "    Version:", version
        write(*,*) "    Static:", meta%is_static_plugin()
    end do

    ! Example 2: Dynamic plugin loading (simulated)
    write(*,*) ""
    write(*,*) "2. Dynamic Plugin Loading:"
    write(*,*) "--------------------------"

    ! Note: In a real scenario, you would specify an actual plugin file path
    plugin_file = "example_plugin.dll"  ! This file doesn't exist, for demonstration

    call loader%set_file_name(plugin_file)

    ! Set loading hints
    call loader%set_load_hints(QPluginLoader_ResolveAllSymbolsHint)

    write(*,*) "Attempting to load plugin:", plugin_file
    success = loader%load()

    if (success) then
        write(*,*) "Plugin loaded successfully!"

        ! Get plugin metadata
        meta = loader%metaData()
        iid = meta%get_iid()
        class_name = meta%get_class_name()
        version = meta%get_version()

        write(*,*) "Plugin metadata:"
        write(*,*) "  IID:", iid
        write(*,*) "  Class:", class_name
        write(*,*) "  Version:", version
        write(*,*) "  Vendor:", meta%get_vendor()
        write(*,*) "  Description:", meta%get_description()

        ! Get plugin instance
        plugin_instance => loader%instance()
        if (associated(plugin_instance)) then
            write(*,*) "Plugin instance obtained successfully"

            ! Get available keys from the factory
            ! Note: This would require implementing the keys() method in concrete plugins
            write(*,*) "Plugin factory available"
        end if

        ! Test unloading
        write(*,*) "Unloading plugin..."
        success = loader%unload()
        if (success) then
            write(*,*) "Plugin unloaded successfully"
        else
            write(*,*) "Failed to unload plugin:", loader%error_string()
        end if

    else
        write(*,*) "Failed to load plugin:", loader%error_string()

        ! Demonstrate error handling
        call error_info%set_error(PluginError_FileNotFound, &
                                "Plugin file not found: " // plugin_file, &
                                plugin_file, "")
        write(*,*) "Error details:"
        write(*,*) "  Code:", error_info%get_error_code()
        write(*,*) "  Message:", error_info%get_error_message()
        write(*,*) "  File:", error_info%get_plugin_file()
    end if

    ! Example 3: Plugin discovery
    write(*,*) ""
    write(*,*) "3. Plugin Discovery:"
    write(*,*) "--------------------"

    ! This would normally scan directories for plugins
    write(*,*) "Plugin discovery would scan the following paths:"
    write(*,*) "  ./plugins"
    write(*,*) "  User-specific plugin directories"
    write(*,*) "  System plugin directories"

    ! Example 4: Version compatibility checking
    write(*,*) ""
    write(*,*) "4. Version Compatibility:"
    write(*,*) "-------------------------"

    ! Test version compatibility
    success = check_version_compatibility("1.2.3", "1.3.0")
    write(*,*) "Version 1.2.3 compatible with 1.3.0:", success

    success = check_version_compatibility("1.2.3", "2.0.0")
    write(*,*) "Version 1.2.3 compatible with 2.0.0:", success

    success = check_version_compatibility("1.2.3", "1.1.0")
    write(*,*) "Version 1.2.3 compatible with 1.1.0:", success

    ! Example 5: Plugin metadata manipulation
    write(*,*) ""
    write(*,*) "5. Plugin Metadata Example:"
    write(*,*) "---------------------------"

    call meta%set_iid("org.forge.example.PluginInterface/1.0")
    call meta%set_class_name("ExamplePlugin")
    call meta%set_version("1.0.0")
    call meta%set_vendor("ForGE Contributors")
    call meta%set_description("Example plugin for demonstration")
    call meta%set_major_version(1)
    call meta%set_minor_version(0)
    call meta%set_patch_version(0)

    ! Add some dependencies
    call meta%add_dependency("org.forge.core/1.0")
    call meta%add_dependency("org.forge.widgets/1.0")

    write(*,*) "Created plugin metadata:"
    write(*,*) "  IID:", meta%get_iid()
    write(*,*) "  Class:", meta%get_class_name()
    write(*,*) "  Version:", meta%version_string()
    write(*,*) "  Vendor:", meta%get_vendor()
    write(*,*) "  Description:", meta%get_description()
    write(*,*) "  Dependencies:", meta%dependency_count()

    ! Check if metadata is valid
    if (meta%is_valid()) then
        write(*,*) "Plugin metadata is valid"
    else
        write(*,*) "Plugin metadata is invalid"
    end if

    ! Example 6: Loading hints demonstration
    write(*,*) ""
    write(*,*) "6. Loading Hints:"
    write(*,*) "-----------------"

    type(QPluginLoader_LoadHints) :: hints

    call hints%set_flag(QPluginLoader_ResolveAllSymbolsHint)
    call hints%set_flag(QPluginLoader_PreventUnloadHint)

    write(*,*) "Loading hints set:"
    write(*,*) "  Resolve all symbols:", hints%test_flag(QPluginLoader_ResolveAllSymbolsHint)
    write(*,*) "  Export external symbols:", hints%test_flag(QPluginLoader_ExportExternalSymbolsHint)
    write(*,*) "  Load archive member:", hints%test_flag(QPluginLoader_LoadArchiveMemberHint)
    write(*,*) "  Prevent unload:", hints%test_flag(QPluginLoader_PreventUnloadHint)

    write(*,*) "Hints flags value:", hints%flags()

    ! Clean up
    call loader%cleanup()

    write(*,*) ""
    write(*,*) "Plugin system example completed successfully!"

end program plugin_example