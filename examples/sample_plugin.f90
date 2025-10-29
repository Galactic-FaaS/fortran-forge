!> @brief Sample plugin implementation demonstrating the plugin interface
!> @details Shows how to create a concrete plugin that implements QFactoryInterface
!> @author ForGE Contributors
!> @date 2025
!> @license GPL-3.0-or-later

module sample_plugin
    use iso_c_binding
    use forge_types
    use forge_plugin
    implicit none

    ! Sample plugin factory
    type, extends(QFactoryInterface) :: SamplePluginFactory
        private
        type(QPluginMetaData) :: plugin_meta
    contains
        procedure :: create => sample_create
        procedure :: keys => sample_keys
        procedure :: metaData => sample_metaData
        procedure :: availableMetaData => sample_availableMetaData
    end type SamplePluginFactory

    ! Sample object that the plugin creates
    type, extends(forge_qobject) :: SampleObject
        private
        character(len=:), allocatable :: name
    contains
        procedure :: init => sample_object_init
        procedure :: get_name => sample_object_get_name
        procedure :: set_name => sample_object_set_name
    end type SampleObject

contains

    ! ========== SamplePluginFactory Implementation ==========

    function sample_create(this, key) result(obj)
        class(SamplePluginFactory), intent(inout) :: this
        character(len=*), intent(in) :: key
        class(forge_qobject), pointer :: obj

        type(SampleObject), pointer :: sample_obj

        ! Create a sample object based on the key
        if (key == "sample.object") then
            allocate(sample_obj)
            call sample_obj%init()
            call sample_obj%set_name("Created by Sample Plugin")
            obj => sample_obj
        else
            obj => null()
        end if
    end function sample_create

    function sample_keys(this) result(keys)
        class(SamplePluginFactory), intent(in) :: this
        type(forge_string), dimension(:), allocatable :: keys

        ! Return the keys this factory can create
        allocate(keys(1))
        call keys(1)%set("sample.object")
    end function sample_keys

    function sample_metaData(this) result(meta)
        class(SamplePluginFactory), intent(in) :: this
        type(QPluginMetaData) :: meta

        ! Return plugin metadata
        meta = this%plugin_meta
    end function sample_metaData

    function sample_availableMetaData(this) result(meta_list)
        class(SamplePluginFactory), intent(in) :: this
        type(QPluginMetaData), dimension(:), allocatable :: meta_list

        ! Return list of available metadata (just this plugin's metadata)
        allocate(meta_list(1))
        meta_list(1) = this%plugin_meta
    end function sample_availableMetaData

    ! ========== SampleObject Implementation ==========

    subroutine sample_object_init(this, parent)
        class(SampleObject), intent(inout) :: this
        class(forge_qobject), optional, target :: parent

        call this%forge_qobject%init(parent)
        call this%meta_object%set_class_name("SampleObject")
        this%name = "Sample Object"
    end subroutine sample_object_init

    function sample_object_get_name(this) result(name)
        class(SampleObject), intent(in) :: this
        character(len=:), allocatable :: name
        name = this%name
    end function sample_object_get_name

    subroutine sample_object_set_name(this, name)
        class(SampleObject), intent(inout) :: this
        character(len=*), intent(in) :: name
        this%name = name
    end subroutine sample_object_set_name

end module sample_plugin

!> @brief Plugin creation function that external code calls
function create_plugin() result(plugin)
    use sample_plugin
    class(QFactoryInterface), pointer :: plugin

    type(SamplePluginFactory), pointer :: factory
    type(QPluginMetaData) :: meta

    ! Create the factory
    allocate(factory)

    ! Initialize the factory
    call factory%init()

    ! Set up plugin metadata
    call meta%set_iid("org.forge.example.SamplePlugin/1.0")
    call meta%set_class_name("SamplePluginFactory")
    call meta%set_version("1.0.0")
    call meta%set_vendor("ForGE Contributors")
    call meta%set_description("Sample plugin demonstrating the plugin interface")
    call meta%set_major_version(1)
    call meta%set_minor_version(0)
    call meta%set_patch_version(0)
    call meta%set_category("Examples")

    ! Add dependencies (none for this simple example)
    ! call meta%add_dependency("org.forge.core/1.0")

    ! Store metadata in factory
    factory%plugin_meta = meta

    ! Return the factory as the plugin interface
    plugin => factory
end function create_plugin