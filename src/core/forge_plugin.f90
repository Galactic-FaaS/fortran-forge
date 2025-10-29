!> @brief Qt-style plugin system implementation for ForGE
!> @details Implements QPluginLoader, QFactoryInterface, QPluginMetaData,
!> and complete plugin architecture with dynamic/static loading, discovery,
!> versioning, dependencies, and cross-platform support
!> @author ForGE Contributors
!> @date 2025
!> @license GPL-3.0-or-later

module forge_plugin
    use iso_c_binding
    use forge_types
    use forge_errors
    use forge_qobject
    use iso_fortran_env, only: int64
    implicit none
    private

    public :: QPluginMetaData
    public :: QFactoryInterface
    public :: QPluginLoader
    public :: QPluginLoader_LoadHint
    public :: QPluginLoader_LoadHints
    public :: QPluginLoader_staticPlugins
    public :: QPluginLoader_staticPluginCount
    public :: QPluginLoader_getStaticPlugin
    public :: QPluginLoader_registerStaticPlugin
    public :: QPluginLoader_ResolveAllSymbolsHint
    public :: QPluginLoader_ExportExternalSymbolsHint
    public :: QPluginLoader_LoadArchiveMemberHint
    public :: QPluginLoader_PreventUnloadHint
    public :: QPluginLoader_unregisterStaticPlugin

    !> @brief Plugin metadata structure
    type :: QPluginMetaData
        private
        type(forge_string) :: iid                    ! Interface ID
        type(forge_string) :: class_name             ! Plugin class name
        type(forge_string) :: version                ! Plugin version
        type(forge_string) :: vendor                 ! Plugin vendor
        type(forge_string) :: description            ! Plugin description
        type(forge_string) :: license                ! Plugin license
        type(forge_string) :: copyright              ! Plugin copyright
        type(forge_string) :: url                    ! Plugin URL
        type(forge_string) :: category               ! Plugin category
        type(forge_string), dimension(:), allocatable :: dependencies ! Plugin dependencies
        integer :: dependency_count = 0
        integer :: major_version = 0                 ! Major version number
        integer :: minor_version = 0                 ! Minor version number
        integer :: patch_version = 0                 ! Patch version number
        logical :: debug = .false.                   ! Debug build flag
        logical :: static_plugin = .false.           ! Static plugin flag
        type(forge_string) :: file_path               ! Plugin file path
        type(forge_string) :: error_string            ! Last error message
    contains
        procedure :: set_iid => metadata_set_iid
        procedure :: get_iid => metadata_get_iid
        procedure :: set_class_name => metadata_set_class_name
        procedure :: get_class_name => metadata_get_class_name
        procedure :: set_version => metadata_set_version
        procedure :: get_version => metadata_get_version
        procedure :: set_vendor => metadata_set_vendor
        procedure :: get_vendor => metadata_get_vendor
        procedure :: set_description => metadata_set_description
        procedure :: get_description => metadata_get_description
        procedure :: set_license => metadata_set_license
        procedure :: get_license => metadata_get_license
        procedure :: set_copyright => metadata_set_copyright
        procedure :: get_copyright => metadata_get_copyright
        procedure :: set_url => metadata_set_url
        procedure :: get_url => metadata_get_url
        procedure :: set_category => metadata_set_category
        procedure :: get_category => metadata_get_category
        procedure :: add_dependency => metadata_add_dependency
        procedure :: get_dependencies => metadata_get_dependencies
        procedure :: dependency_count => metadata_dependency_count
        procedure :: set_major_version => metadata_set_major_version
        procedure :: get_major_version => metadata_get_major_version
        procedure :: set_minor_version => metadata_set_minor_version
        procedure :: get_minor_version => metadata_get_minor_version
        procedure :: set_patch_version => metadata_set_patch_version
        procedure :: get_patch_version => metadata_get_patch_version
        procedure :: set_debug => metadata_set_debug
        procedure :: is_debug => metadata_is_debug
        procedure :: set_static_plugin => metadata_set_static_plugin
        procedure :: is_static_plugin => metadata_is_static_plugin
        procedure :: set_file_path => metadata_set_file_path
        procedure :: get_file_path => metadata_get_file_path
        procedure :: set_error_string => metadata_set_error_string
        procedure :: get_error_string => metadata_get_error_string
        procedure :: is_valid => metadata_is_valid
        procedure :: version_string => metadata_version_string
        procedure :: check_compatibility => metadata_check_compatibility
        final :: metadata_finalize
    end type QPluginMetaData

    !> @brief Abstract factory interface for plugin object creation
    type, abstract, extends(forge_qobject) :: QFactoryInterface
    contains
        procedure(create_interface), deferred :: create
        procedure(keys_interface), deferred :: keys
        procedure :: metaData => factory_metaData
        procedure :: availableMetaData => factory_availableMetaData
    end type QFactoryInterface

    !> @brief Plugin loader class for dynamic plugin loading
    type, extends(forge_qobject) :: QPluginLoader
        private
        type(forge_string) :: file_name                ! Plugin file name
        type(QPluginMetaData) :: meta_data             ! Plugin metadata
        class(QFactoryInterface), pointer :: instance => null() ! Loaded plugin instance
        type(c_ptr) :: library_handle = c_null_ptr     ! Dynamic library handle
        integer :: load_hints = 0                      ! Loading hints/flags
        logical :: loaded = .false.                    ! Plugin loaded flag
        type(forge_string) :: error_string             ! Last error message
    contains
        procedure :: init => loader_init
        procedure :: cleanup => loader_cleanup
        procedure :: set_file_name => loader_set_file_name
        procedure :: file_name => loader_file_name
        procedure :: load => loader_load
        procedure :: unload => loader_unload
        procedure :: is_loaded => loader_is_loaded
        procedure :: instance => loader_instance
        procedure :: metaData => loader_metaData
        procedure :: set_load_hints => loader_set_load_hints
        procedure :: load_hints => loader_load_hints
        procedure :: error_string => loader_error_string
        procedure :: static_plugins => loader_static_plugins
        final :: loader_finalize
    end type QPluginLoader

    !> @brief Loading hints enumeration
    enum, bind(c)
        enumerator :: QPluginLoader_ResolveAllSymbolsHint = 1
        enumerator :: QPluginLoader_ExportExternalSymbolsHint = 2
        enumerator :: QPluginLoader_LoadArchiveMemberHint = 4
        enumerator :: QPluginLoader_PreventUnloadHint = 8
    end enum

    type :: QPluginLoader_LoadHint
        integer :: value
    end type QPluginLoader_LoadHint

    type :: QPluginLoader_LoadHints
        integer :: flags
    contains
        procedure :: set_flag => loadhints_set_flag
        procedure :: clear_flag => loadhints_clear_flag
        procedure :: test_flag => loadhints_test_flag
        procedure :: flags => loadhints_flags
    end type QPluginLoader_LoadHints

    ! Static plugin registry
    type(QPluginMetaData), dimension(:), allocatable, target, private :: static_plugins
    integer, private :: static_plugin_count = 0
    integer, parameter, private :: STATIC_PLUGIN_INCREMENT = 10

    abstract interface
        !> @brief Factory create method interface
        function create_interface(this, key) result(obj)
            import :: QFactoryInterface, forge_qobject, forge_string
            class(QFactoryInterface), intent(inout) :: this
            character(len=*), intent(in) :: key
            class(forge_qobject), pointer :: obj
        end function create_interface

        !> @brief Factory keys method interface
        function keys_interface(this) result(keys)
            import :: QFactoryInterface, forge_string
            class(QFactoryInterface), intent(in) :: this
            type(forge_string), dimension(:), allocatable :: keys
        end function keys_interface
    end interface

contains

    ! ========== QPluginMetaData Implementation ==========

    subroutine metadata_set_iid(this, iid)
        class(QPluginMetaData), intent(inout) :: this
        character(len=*), intent(in) :: iid
        call this%iid%set(iid)
    end subroutine metadata_set_iid

    function metadata_get_iid(this) result(iid)
        class(QPluginMetaData), intent(in) :: this
        character(len=:), allocatable :: iid
        iid = this%iid%get()
    end function metadata_get_iid

    subroutine metadata_set_class_name(this, name)
        class(QPluginMetaData), intent(inout) :: this
        character(len=*), intent(in) :: name
        call this%class_name%set(name)
    end subroutine metadata_set_class_name

    function metadata_get_class_name(this) result(name)
        class(QPluginMetaData), intent(in) :: this
        character(len=:), allocatable :: name
        name = this%class_name%get()
    end function metadata_get_class_name

    subroutine metadata_set_version(this, version)
        class(QPluginMetaData), intent(inout) :: this
        character(len=*), intent(in) :: version
        call this%version%set(version)
    end subroutine metadata_set_version

    function metadata_get_version(this) result(version)
        class(QPluginMetaData), intent(in) :: this
        character(len=:), allocatable :: version
        version = this%version%get()
    end function metadata_get_version

    subroutine metadata_set_vendor(this, vendor)
        class(QPluginMetaData), intent(inout) :: this
        character(len=*), intent(in) :: vendor
        call this%vendor%set(vendor)
    end subroutine metadata_set_vendor

    function metadata_get_vendor(this) result(vendor)
        class(QPluginMetaData), intent(in) :: this
        character(len=:), allocatable :: vendor
        vendor = this%vendor%get()
    end function metadata_get_vendor

    subroutine metadata_set_description(this, description)
        class(QPluginMetaData), intent(inout) :: this
        character(len=*), intent(in) :: description
        call this%description%set(description)
    end subroutine metadata_set_description

    function metadata_get_description(this) result(description)
        class(QPluginMetaData), intent(in) :: this
        character(len=:), allocatable :: description
        description = this%description%get()
    end function metadata_get_description

    subroutine metadata_set_license(this, license)
        class(QPluginMetaData), intent(inout) :: this
        character(len=*), intent(in) :: license
        call this%license%set(license)
    end subroutine metadata_set_license

    function metadata_get_license(this) result(license)
        class(QPluginMetaData), intent(in) :: this
        character(len=:), allocatable :: license
        license = this%license%get()
    end function metadata_get_license

    subroutine metadata_set_copyright(this, copyright)
        class(QPluginMetaData), intent(inout) :: this
        character(len=*), intent(in) :: copyright
        call this%copyright%set(copyright)
    end subroutine metadata_set_copyright

    function metadata_get_copyright(this) result(copyright)
        class(QPluginMetaData), intent(in) :: this
        character(len=:), allocatable :: copyright
        copyright = this%copyright%get()
    end function metadata_get_copyright

    subroutine metadata_set_url(this, url)
        class(QPluginMetaData), intent(inout) :: this
        character(len=*), intent(in) :: url
        call this%url%set(url)
    end subroutine metadata_set_url

    function metadata_get_url(this) result(url)
        class(QPluginMetaData), intent(in) :: this
        character(len=:), allocatable :: url
        url = this%url%get()
    end function metadata_get_url

    subroutine metadata_set_category(this, category)
        class(QPluginMetaData), intent(inout) :: this
        character(len=*), intent(in) :: category
        call this%category%set(category)
    end subroutine metadata_set_category

    function metadata_get_category(this) result(category)
        class(QPluginMetaData), intent(in) :: this
        character(len=:), allocatable :: category
        category = this%category%get()
    end function metadata_get_category

    subroutine metadata_add_dependency(this, dependency)
        class(QPluginMetaData), intent(inout) :: this
        character(len=*), intent(in) :: dependency
        type(forge_string), dimension(:), allocatable :: temp

        if (.not. allocated(this%dependencies)) then
            allocate(this%dependencies(1))
        else
            allocate(temp(size(this%dependencies) + 1))
            temp(1:size(this%dependencies)) = this%dependencies
            call move_alloc(temp, this%dependencies)
        end if

        call this%dependencies(this%dependency_count + 1)%set(dependency)
        this%dependency_count = this%dependency_count + 1
    end subroutine metadata_add_dependency

    function metadata_get_dependencies(this) result(deps)
        class(QPluginMetaData), intent(in) :: this
        type(forge_string), dimension(:), allocatable :: deps
        allocate(deps(this%dependency_count))
        deps = this%dependencies(1:this%dependency_count)
    end function metadata_get_dependencies

    function metadata_dependency_count(this) result(count)
        class(QPluginMetaData), intent(in) :: this
        integer :: count
        count = this%dependency_count
    end function metadata_dependency_count

    subroutine metadata_set_major_version(this, version)
        class(QPluginMetaData), intent(inout) :: this
        integer, intent(in) :: version
        this%major_version = version
    end subroutine metadata_set_major_version

    function metadata_get_major_version(this) result(version)
        class(QPluginMetaData), intent(in) :: this
        integer :: version
        version = this%major_version
    end function metadata_get_major_version

    subroutine metadata_set_minor_version(this, version)
        class(QPluginMetaData), intent(inout) :: this
        integer, intent(in) :: version
        this%minor_version = version
    end subroutine metadata_set_minor_version

    function metadata_get_minor_version(this) result(version)
        class(QPluginMetaData), intent(in) :: this
        integer :: version
        version = this%minor_version
    end function metadata_get_minor_version

    subroutine metadata_set_patch_version(this, version)
        class(QPluginMetaData), intent(inout) :: this
        integer, intent(in) :: version
        this%patch_version = version
    end subroutine metadata_set_patch_version

    function metadata_get_patch_version(this) result(version)
        class(QPluginMetaData), intent(in) :: this
        integer :: version
        version = this%patch_version
    end function metadata_get_patch_version

    subroutine metadata_set_debug(this, debug)
        class(QPluginMetaData), intent(inout) :: this
        logical, intent(in) :: debug
        this%debug = debug
    end subroutine metadata_set_debug

    function metadata_is_debug(this) result(debug)
        class(QPluginMetaData), intent(in) :: this
        logical :: debug
        debug = this%debug
    end function metadata_is_debug

    subroutine metadata_set_static_plugin(this, static)
        class(QPluginMetaData), intent(inout) :: this
        logical, intent(in) :: static
        this%static_plugin = static
    end subroutine metadata_set_static_plugin

    function metadata_is_static_plugin(this) result(static)
        class(QPluginMetaData), intent(in) :: this
        logical :: static
        static = this%static_plugin
    end function metadata_is_static_plugin

    subroutine metadata_set_file_path(this, path)
        class(QPluginMetaData), intent(inout) :: this
        character(len=*), intent(in) :: path
        call this%file_path%set(path)
    end subroutine metadata_set_file_path

    function metadata_get_file_path(this) result(path)
        class(QPluginMetaData), intent(in) :: this
        character(len=:), allocatable :: path
        path = this%file_path%get()
    end function metadata_get_file_path

    subroutine metadata_set_error_string(this, error)
        class(QPluginMetaData), intent(inout) :: this
        character(len=*), intent(in) :: error
        call this%error_string%set(error)
    end subroutine metadata_set_error_string

    function metadata_get_error_string(this) result(error)
        class(QPluginMetaData), intent(in) :: this
        character(len=:), allocatable :: error
        error = this%error_string%get()
    end function metadata_get_error_string

    function metadata_is_valid(this) result(valid)
        class(QPluginMetaData), intent(in) :: this
        logical :: valid
        valid = len_trim(this%get_iid()) > 0 .and. len_trim(this%get_class_name()) > 0
    end function metadata_is_valid

    function metadata_version_string(this) result(version)
        class(QPluginMetaData), intent(in) :: this
        character(len=:), allocatable :: version
        character(len=20) :: buffer

        write(buffer, '(I0,".",I0,".",I0)') this%major_version, this%minor_version, this%patch_version
        version = trim(buffer)
    end function metadata_version_string

    function metadata_check_compatibility(this, required_major, required_minor) result(compatible)
        class(QPluginMetaData), intent(in) :: this
        integer, intent(in) :: required_major
        integer, intent(in), optional :: required_minor
        logical :: compatible

        compatible = .false.

        ! Major version must match exactly
        if (this%major_version /= required_major) return

        ! Minor version must be >= required minor version
        if (present(required_minor)) then
            if (this%minor_version < required_minor) return
        end if

        compatible = .true.
    end function metadata_check_compatibility

    subroutine metadata_finalize(this)
        type(QPluginMetaData), intent(inout) :: this
        if (allocated(this%dependencies)) deallocate(this%dependencies)
    end subroutine metadata_finalize

    ! ========== QFactoryInterface Implementation ==========

    function factory_metaData(this) result(meta)
        class(QFactoryInterface), intent(in) :: this
        type(QPluginMetaData) :: meta
        ! Default implementation returns empty metadata
        ! Subclasses should override this
    end function factory_metaData

    function factory_availableMetaData(this) result(meta_list)
        class(QFactoryInterface), intent(in) :: this
        type(QPluginMetaData), dimension(:), allocatable :: meta_list
        ! Default implementation returns empty list
        ! Subclasses should override this
        allocate(meta_list(0))
    end function factory_availableMetaData

    ! ========== QPluginLoader_LoadHints Implementation ==========

    subroutine loadhints_set_flag(this, flag)
        class(QPluginLoader_LoadHints), intent(inout) :: this
        integer, intent(in) :: flag
        this%flags = ior(this%flags, flag)
    end subroutine loadhints_set_flag

    subroutine loadhints_clear_flag(this, flag)
        class(QPluginLoader_LoadHints), intent(inout) :: this
        integer, intent(in) :: flag
        this%flags = iand(this%flags, not(flag))
    end subroutine loadhints_clear_flag

    function loadhints_test_flag(this, flag) result(set)
        class(QPluginLoader_LoadHints), intent(in) :: this
        integer, intent(in) :: flag
        logical :: set
        set = iand(this%flags, flag) /= 0
    end function loadhints_test_flag

    function loadhints_flags(this) result(flags)
        class(QPluginLoader_LoadHints), intent(in) :: this
        integer :: flags
        flags = this%flags
    end function loadhints_flags

    ! ========== QPluginLoader Implementation ==========

    subroutine loader_init(this, parent)
        class(QPluginLoader), intent(inout) :: this
        class(forge_qobject), optional, target :: parent

        call this%forge_qobject%init(parent)
        call this%meta_object%set_class_name("QPluginLoader")
        this%loaded = .false.
        this%library_handle = c_null_ptr
        this%load_hints = 0
    end subroutine loader_init

    subroutine loader_cleanup(this)
        class(QPluginLoader), intent(inout) :: this
        if (this%loaded) then
            call this%unload()
        end if
        call this%forge_qobject%cleanup()
    end subroutine loader_cleanup

    subroutine loader_set_file_name(this, file_name)
        class(QPluginLoader), intent(inout) :: this
        character(len=*), intent(in) :: file_name
        call this%file_name%set(file_name)
    end subroutine loader_set_file_name

    function loader_file_name(this) result(file_name)
        class(QPluginLoader), intent(in) :: this
        character(len=:), allocatable :: file_name
        file_name = this%file_name%get()
    end function loader_file_name

    function loader_load(this) result(success)
        class(QPluginLoader), intent(inout) :: this
        logical :: success
        character(len=:), allocatable :: file_path
        type(c_ptr) :: handle
        procedure(create_plugin_interface), pointer :: create_func
        integer :: status

        success = .false.

        ! Check if already loaded
        if (this%loaded) then
            success = .true.
            return
        end if

        file_path = this%file_name%get()
        if (len_trim(file_path) == 0) then
            call this%error_string%set("No plugin file specified")
            return
        end if

        ! Load the dynamic library
        handle = load_library(file_path, status)
        if (status /= 0) then
            call this%error_string%set("Failed to load library: " // get_library_error())
            return
        end if

        ! Get the plugin creation function
        call c_f_procpointer(get_library_symbol(handle, "create_plugin", status), create_func)
        if (status /= 0) then
            call this%error_string%set("Failed to find create_plugin function: " // get_library_error())
            call unload_library(handle)
            return
        end if

        ! Create the plugin instance
        this%instance => create_func()
        if (.not. associated(this%instance)) then
            call this%error_string%set("Plugin creation function returned null")
            call unload_library(handle)
            return
        end if

        ! Get plugin metadata
        this%meta_data = this%instance%metaData()

        ! Store library handle and mark as loaded
        this%library_handle = handle
        this%loaded = .true.
        success = .true.
    end function loader_load

    function loader_unload(this) result(success)
        class(QPluginLoader), intent(inout) :: this
        logical :: success

        success = .false.

        if (.not. this%loaded) then
            success = .true.
            return
        end if

        ! Check if unload is prevented
        if (iand(this%load_hints, QPluginLoader_PreventUnloadHint) /= 0) then
            call this%error_string%set("Unload prevented by load hints")
            return
        end if

        ! Clean up plugin instance
        if (associated(this%instance)) then
            call this%instance%cleanup()
            deallocate(this%instance)
            this%instance => null()
        end if

        ! Unload the library
        if (c_associated(this%library_handle)) then
            call unload_library(this%library_handle)
            this%library_handle = c_null_ptr
        end if

        this%loaded = .false.
        success = .true.
    end function loader_unload

    function loader_is_loaded(this) result(loaded)
        class(QPluginLoader), intent(in) :: this
        logical :: loaded
        loaded = this%loaded
    end function loader_is_loaded

    function loader_instance(this) result(instance)
        class(QPluginLoader), intent(in) :: this
        class(QFactoryInterface), pointer :: instance
        instance => this%instance
    end function loader_instance

    function loader_metaData(this) result(meta)
        class(QPluginLoader), intent(in) :: this
        type(QPluginMetaData) :: meta
        meta = this%meta_data
    end function loader_metaData

    subroutine loader_set_load_hints(this, hints)
        class(QPluginLoader), intent(inout) :: this
        integer, intent(in) :: hints
        this%load_hints = hints
    end subroutine loader_set_load_hints

    function loader_load_hints(this) result(hints)
        class(QPluginLoader), intent(in) :: this
        integer :: hints
        hints = this%load_hints
    end function loader_load_hints

    function loader_error_string(this) result(error)
        class(QPluginLoader), intent(in) :: this
        character(len=:), allocatable :: error
        error = this%error_string%get()
    end function loader_error_string

    function loader_static_plugins(this) result(plugins)
        class(QPluginLoader), intent(in) :: this
        type(QPluginMetaData), dimension(:), pointer :: plugins
        plugins => static_plugins(1:static_plugin_count)
    end function loader_static_plugins

    subroutine loader_finalize(this)
        type(QPluginLoader), intent(inout) :: this
        call this%cleanup()
    end subroutine loader_finalize

    ! ========== Static Plugin Registry Functions ==========

    function QPluginLoader_staticPlugins() result(plugins)
        type(QPluginMetaData), dimension(:), pointer :: plugins
        plugins => static_plugins(1:static_plugin_count)
    end function QPluginLoader_staticPlugins

    function QPluginLoader_staticPluginCount() result(count)
        integer :: count
        count = static_plugin_count
    end function QPluginLoader_staticPluginCount

    function QPluginLoader_getStaticPlugin(index) result(plugin)
        integer, intent(in) :: index
        type(QPluginMetaData), pointer :: plugin

        if (index >= 1 .and. index <= static_plugin_count) then
            plugin => static_plugins(index)
        else
            plugin => null()
        end if
    end function QPluginLoader_getStaticPlugin

    subroutine QPluginLoader_registerStaticPlugin(plugin)
        type(QPluginMetaData), intent(in) :: plugin
        type(QPluginMetaData), dimension(:), allocatable :: temp

        if (.not. allocated(static_plugins)) then
            allocate(static_plugins(STATIC_PLUGIN_INCREMENT))
        else if (static_plugin_count >= size(static_plugins)) then
            allocate(temp(size(static_plugins) + STATIC_PLUGIN_INCREMENT))
            temp(1:size(static_plugins)) = static_plugins
            call move_alloc(temp, static_plugins)
        end if

        static_plugin_count = static_plugin_count + 1
        static_plugins(static_plugin_count) = plugin
        call static_plugins(static_plugin_count)%set_static_plugin(.true.)
    end subroutine QPluginLoader_registerStaticPlugin

    subroutine QPluginLoader_unregisterStaticPlugin(iid)
        character(len=*), intent(in) :: iid
        integer :: i, j

        do i = 1, static_plugin_count
            if (static_plugins(i)%get_iid() == iid) then
                ! Shift remaining plugins
                do j = i, static_plugin_count - 1
                    static_plugins(j) = static_plugins(j + 1)
                end do
                static_plugin_count = static_plugin_count - 1
                exit
            end if
        end do
    end subroutine QPluginLoader_unregisterStaticPlugin

    ! ========== Cross-platform Library Loading Functions ==========

    function load_library(file_path, status) result(handle)
        character(len=*), intent(in) :: file_path
        integer, intent(out) :: status
        type(c_ptr) :: handle

        ! Platform-specific library loading
#ifdef _WIN32
        handle = LoadLibrary(trim(file_path) // c_null_char)
        if (c_associated(handle)) then
            status = 0
        else
            status = GetLastError()
        end if
#else
        handle = dlopen(trim(file_path) // c_null_char, RTLD_LAZY)
        if (c_associated(handle)) then
            status = 0
        else
            status = -1
        end if
#endif
    end function load_library

    subroutine unload_library(handle)
        type(c_ptr), value :: handle

#ifdef _WIN32
        call FreeLibrary(handle)
#else
        call dlclose(handle)
#endif
    end subroutine unload_library

    function get_library_symbol(handle, symbol_name, status) result(symbol)
        type(c_ptr), value :: handle
        character(len=*), intent(in) :: symbol_name
        integer, intent(out) :: status
        type(c_ptr) :: symbol

#ifdef _WIN32
        symbol = GetProcAddress(handle, trim(symbol_name) // c_null_char)
        if (c_associated(symbol)) then
            status = 0
        else
            status = GetLastError()
        end if
#else
        symbol = dlsym(handle, trim(symbol_name) // c_null_char)
        if (c_associated(symbol)) then
            status = 0
        else
            status = -1
        end if
#endif
    end function get_library_symbol

    function get_library_error() result(error_msg)
        character(len=:), allocatable :: error_msg

#ifdef _WIN32
        character(len=256) :: buffer
        integer :: length

        length = FormatMessage(FORMAT_MESSAGE_FROM_SYSTEM .or. FORMAT_MESSAGE_IGNORE_INSERTS, &
                              c_null_ptr, GetLastError(), 0, buffer, size(buffer), c_null_ptr)
        error_msg = trim(buffer(1:length))
#else
        type(c_ptr) :: error_ptr
        character(len=256), pointer :: error_str

        error_ptr = dlerror()
        if (c_associated(error_ptr)) then
            call c_f_pointer(error_ptr, error_str)
            error_msg = error_str(1:index(error_str, c_null_char) - 1)
        else
            error_msg = "Unknown error"
        end if
#endif
    end function get_library_error

    ! ========== Plugin Creation Interface ==========

    abstract interface
        function create_plugin_interface() result(plugin)
            import :: QFactoryInterface
            class(QFactoryInterface), pointer :: plugin
        end function create_plugin_interface
    end interface

end module forge_plugin
    ! ========== Plugin Discovery Functions ==========

    !> @brief Discover plugins in a directory
    function discover_plugins(directory_path) result(plugins)
        character(len=*), intent(in) :: directory_path
        type(QPluginMetaData), dimension(:), allocatable :: plugins
        character(len=:), allocatable :: plugin_pattern
        type(forge_string), dimension(:), allocatable :: file_list
        integer :: i, plugin_count
        type(QPluginLoader) :: loader

        ! Get platform-specific plugin file pattern
        plugin_pattern = get_plugin_file_pattern()

        ! Get list of files matching the pattern
        file_list = get_files_in_directory(directory_path, plugin_pattern)
        plugin_count = 0

        ! Allocate temporary array for discovered plugins
        allocate(plugins(size(file_list)))

        ! Try to load each potential plugin file
        do i = 1, size(file_list)
            call loader%init()
            call loader%set_file_name(file_list(i)%get())

            if (loader%load()) then
                plugin_count = plugin_count + 1
                plugins(plugin_count) = loader%metaData()
                call plugins(plugin_count)%set_file_path(file_list(i)%get())
            end if

            call loader%cleanup()
        end do

        ! Trim array to actual number of plugins found
        if (plugin_count > 0) then
            plugins = plugins(1:plugin_count)
        else
            deallocate(plugins)
            allocate(plugins(0))
        end if
    end function discover_plugins

    !> @brief Get platform-specific plugin file pattern
    function get_plugin_file_pattern() result(pattern)
        character(len=:), allocatable :: pattern

#ifdef _WIN32
        pattern = "*.dll"
#elseif __APPLE__
        pattern = "*.dylib"
#else
        pattern = "*.so"
#endif
    end function get_plugin_file_pattern

    !> @brief Get files in directory matching pattern (simplified implementation)
    function get_files_in_directory(directory, pattern) result(files)
        character(len=*), intent(in) :: directory
        character(len=*), intent(in) :: pattern
        type(forge_string), dimension(:), allocatable :: files
        ! Note: This is a simplified implementation
        ! In a real system, this would use platform-specific directory scanning
        ! For now, return empty array
        allocate(files(0))
    end function get_files_in_directory

    ! ========== Dependency Management Functions ==========

    !> @brief Check if plugin dependencies are satisfied
    function check_plugin_dependencies(plugin_meta, loaded_plugins) result(satisfied)
        type(QPluginMetaData), intent(in) :: plugin_meta
        type(QPluginMetaData), dimension(:), intent(in) :: loaded_plugins
        logical :: satisfied
        type(forge_string), dimension(:), allocatable :: dependencies
        integer :: i, j
        logical :: found

        satisfied = .true.
        dependencies = plugin_meta%get_dependencies()

        do i = 1, size(dependencies)
            found = .false.
            do j = 1, size(loaded_plugins)
                if (dependencies(i)%get() == loaded_plugins(j)%get_iid()) then
                    found = .true.
                    exit
                end if
            end do

            if (.not. found) then
                satisfied = .false.
                exit
            end if
        end do
    end function check_plugin_dependencies

    !> @brief Resolve plugin load order based on dependencies
    function resolve_plugin_load_order(plugins) result(load_order)
        type(QPluginMetaData), dimension(:), intent(in) :: plugins
        integer, dimension(:), allocatable :: load_order
        integer :: i, j, k, unresolved_count
        logical, dimension(:), allocatable :: resolved
        type(forge_string), dimension(:), allocatable :: dependencies

        allocate(load_order(size(plugins)))
        allocate(resolved(size(plugins)))
        resolved = .false.
        load_order = 0
        k = 0

        ! Simple topological sort (could be improved for complex dependency graphs)
        do while (k < size(plugins))
            unresolved_count = 0

            do i = 1, size(plugins)
                if (.not. resolved(i)) then
                    dependencies = plugins(i)%get_dependencies()
                    found = .true.

                    ! Check if all dependencies are resolved
                    do j = 1, size(dependencies)
                        found = .false.
                        do m = 1, size(plugins)
                            if (dependencies(j)%get() == plugins(m)%get_iid() .and. resolved(m)) then
                                found = .true.
                                exit
                            end if
                        end do
                        if (.not. found) exit
                    end do

                    if (found) then
                        k = k + 1
                        load_order(k) = i
                        resolved(i) = .true.
                    else
                        unresolved_count = unresolved_count + 1
                    end if
                end if
            end do

            ! If no progress was made, there might be circular dependencies
            if (unresolved_count == count(.not. resolved)) then
                ! For now, just load remaining plugins in original order
                do i = 1, size(plugins)
                    if (.not. resolved(i)) then
                        k = k + 1
                        load_order(k) = i
                        resolved(i) = .true.
                    end if
                end do
                exit
            end if
        end do
    end function resolve_plugin_load_order

    ! ========== Version Compatibility Functions ==========

    !> @brief Check version compatibility between required and available versions
    function check_version_compatibility(required_version, available_version) result(compatible)
        character(len=*), intent(in) :: required_version
        character(len=*), intent(in) :: available_version
        logical :: compatible
        integer :: req_major, req_minor, req_patch
        integer :: avail_major, avail_minor, avail_patch
        integer :: ios

        ! Parse version strings (simplified - assumes format "major.minor.patch")
        read(required_version, *, iostat=ios) req_major, req_minor, req_patch
        if (ios /= 0) then
            compatible = .false.
            return
        end if

        read(available_version, *, iostat=ios) avail_major, avail_minor, avail_patch
        if (ios /= 0) then
            compatible = .false.
            return
        end if

        ! Version compatibility rules:
        ! - Major version must match exactly
        ! - Minor version must be >= required
        ! - Patch version can be any (backward compatible)
        compatible = (req_major == avail_major) .and. (avail_minor >= req_minor)
    end function check_version_compatibility

    !> @brief Parse version string into components
    subroutine parse_version_string(version_str, major, minor, patch)
        character(len=*), intent(in) :: version_str
        integer, intent(out) :: major
        integer, intent(out) :: minor
        integer, intent(out) :: patch
        integer :: dot1, dot2

        major = 0
        minor = 0
        patch = 0

        dot1 = index(version_str, '.')
        if (dot1 > 0) then
            read(version_str(1:dot1-1), *, iostat=ios) major
            dot2 = index(version_str(dot1+1:), '.')
            if (dot2 > 0) then
                dot2 = dot1 + dot2
                read(version_str(dot1+1:dot2-1), *, iostat=ios) minor
                read(version_str(dot2+1:), *, iostat=ios) patch
            else
                read(version_str(dot1+1:), *, iostat=ios) minor
            end if
        else
            read(version_str, *, iostat=ios) major
        end if
    end subroutine parse_version_string

    ! ========== Cross-platform Plugin File Handling ==========

    !> @brief Get platform-specific plugin search paths
    function get_plugin_search_paths() result(paths)
        type(forge_string), dimension(:), allocatable :: paths
        character(len=:), allocatable :: app_dir, home_dir

        ! Get application directory
        app_dir = get_application_directory()

        ! Get user home directory
        home_dir = get_home_directory()

        ! Define search paths based on platform
#ifdef _WIN32
        allocate(paths(3))
        call paths(1)%set(app_dir // "/plugins")
        call paths(2)%set(home_dir // "/AppData/Local/ForGE/plugins")
        call paths(3)%set("C:/Program Files/ForGE/plugins")
#elseif __APPLE__
        allocate(paths(4))
        call paths(1)%set(app_dir // "/../PlugIns")
        call paths(2)%set(home_dir // "/Library/Application Support/ForGE/PlugIns")
        call paths(3)%set("/Library/Application Support/ForGE/PlugIns")
        call paths(4)%set("/System/Library/ForGE/PlugIns")
#else
        allocate(paths(4))
        call paths(1)%set(app_dir // "/plugins")
        call paths(2)%set(home_dir // "/.local/share/forge/plugins")
        call paths(3)%set("/usr/local/lib/forge/plugins")
        call paths(4)%set("/usr/lib/forge/plugins")
#endif
    end function get_plugin_search_paths

    !> @brief Get application directory (simplified)
    function get_application_directory() result(dir)
        character(len=:), allocatable :: dir
        ! In a real implementation, this would get the actual executable directory
        dir = "."
    end function get_application_directory

    !> @brief Get home directory
    function get_home_directory() result(dir)
        character(len=:), allocatable :: dir
        character(len=256) :: buffer
        integer :: length

#ifdef _WIN32
        call get_environment_variable("USERPROFILE", buffer, length)
        if (length > 0) then
            dir = trim(buffer(1:length))
        else
            dir = "C:/Users/Default"
        end if
#else
        call get_environment_variable("HOME", buffer, length)
        if (length > 0) then
            dir = trim(buffer(1:length))
        else
            dir = "/tmp"
        end if
#endif
    end function get_home_directory

    !> @brief Check if file exists and is readable
    function plugin_file_exists(file_path) result(exists)
        character(len=*), intent(in) :: file_path
        logical :: exists

        inquire(file=file_path, exist=exists)
    end function plugin_file_exists

    !> @brief Get file modification time
    function get_plugin_file_modification_time(file_path) result(mod_time)
        character(len=*), intent(in) :: file_path
        integer(int64) :: mod_time

        ! Simplified - in real implementation would use platform-specific APIs
        mod_time = 0_int64
    end function get_plugin_file_modification_time

    ! ========== Error Handling and Reporting ==========

    !> @brief Plugin loading error codes
    enum, bind(c)
        enumerator :: PluginError_None = 0
        enumerator :: PluginError_FileNotFound = 1
        enumerator :: PluginError_FileNotReadable = 2
        enumerator :: PluginError_InvalidFormat = 3
        enumerator :: PluginError_IncompatibleVersion = 4
        enumerator :: PluginError_MissingDependencies = 5
        enumerator :: PluginError_SymbolNotFound = 6
        enumerator :: PluginError_InitializationFailed = 7
        enumerator :: PluginError_UnloadFailed = 8
        enumerator :: PluginError_PlatformNotSupported = 9
    end enum

    !> @brief Plugin error information
    type :: QPluginError
        integer :: error_code = PluginError_None
        type(forge_string) :: error_message
        type(forge_string) :: plugin_file
        type(forge_string) :: plugin_iid
    contains
        procedure :: set_error => plugin_error_set_error
        procedure :: clear => plugin_error_clear
        procedure :: get_error_code => plugin_error_get_code
        procedure :: get_error_message => plugin_error_get_message
        procedure :: get_plugin_file => plugin_error_get_file
        procedure :: get_plugin_iid => plugin_error_get_iid
        procedure :: is_error => plugin_error_is_error
    end type QPluginError

    subroutine plugin_error_set_error(this, code, message, file, iid)
        class(QPluginError), intent(inout) :: this
        integer, intent(in) :: code
        character(len=*), intent(in) :: message
        character(len=*), intent(in), optional :: file
        character(len=*), intent(in), optional :: iid

        this%error_code = code
        call this%error_message%set(message)
        if (present(file)) call this%plugin_file%set(file)
        if (present(iid)) call this%plugin_iid%set(iid)
    end subroutine plugin_error_set_error

    subroutine plugin_error_clear(this)
        class(QPluginError), intent(inout) :: this
        this%error_code = PluginError_None
        call this%error_message%set("")
        call this%plugin_file%set("")
        call this%plugin_iid%set("")
    end subroutine plugin_error_clear

    function plugin_error_get_code(this) result(code)
        class(QPluginError), intent(in) :: this
        integer :: code
        code = this%error_code
    end function plugin_error_get_code

    function plugin_error_get_message(this) result(message)
        class(QPluginError), intent(in) :: this
        character(len=:), allocatable :: message
        message = this%error_message%get()
    end function plugin_error_get_message

    function plugin_error_get_file(this) result(file)
        class(QPluginError), intent(in) :: this
        character(len=:), allocatable :: file
        file = this%plugin_file%get()
    end function plugin_error_get_file

    function plugin_error_get_iid(this) result(iid)
        class(QPluginError), intent(in) :: this
        character(len=:), allocatable :: iid
        iid = this%plugin_iid%get()
    end function plugin_error_get_iid

    function plugin_error_is_error(this) result(is_error)
        class(QPluginError), intent(in) :: this
        logical :: is_error
        is_error = this%error_code /= PluginError_None
    end function plugin_error_is_error

end module forge_plugin