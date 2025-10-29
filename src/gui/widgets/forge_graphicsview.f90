!> @brief QGraphicsView implementation
!> @details 2D graphics scene framework with items, transformations, and interactions
!> @author ForGE Contributors
!> @date 2025
!> @license GPL-3.0-or-later

module forge_graphicsview
    use iso_c_binding
    use forge_widgets
    use forge_types
    use forge_signals
    implicit none
    private

    public :: QGraphicsView, QGraphicsScene, QGraphicsItem, QGraphicsItemGroup

    !> @brief Graphics item types
    integer, parameter, public :: GraphicsItemType = 1
    integer, parameter, public :: GraphicsEllipseItemType = 2
    integer, parameter, public :: GraphicsLineItemType = 3
    integer, parameter, public :: GraphicsPathItemType = 4
    integer, parameter, public :: GraphicsPixmapItemType = 5
    integer, parameter, public :: GraphicsPolygonItemType = 6
    integer, parameter, public :: GraphicsRectItemType = 7
    integer, parameter, public :: GraphicsSimpleTextItemType = 8
    integer, parameter, public :: GraphicsTextItemType = 9
    integer, parameter, public :: GraphicsItemGroupType = 10

    !> @brief Graphics item flags
    integer, parameter, public :: ItemIsMovable = 1
    integer, parameter, public :: ItemIsSelectable = 2
    integer, parameter, public :: ItemIsFocusable = 4
    integer, parameter, public :: ItemClipsToShape = 8
    integer, parameter, public :: ItemClipsChildrenToShape = 16

    !> @brief Abstract base class for all graphics items
    type, abstract :: QGraphicsItem
        private
        type(forge_position) :: pos
        real(c_double) :: rotation = 0.0_c_double
        real(c_double) :: scale_x = 1.0_c_double
        real(c_double) :: scale_y = 1.0_c_double
        real(c_double) :: z_value = 0.0_c_double
        logical :: visible = .true.
        logical :: enabled = .true.
        logical :: selected = .false.
        integer :: flags = 0
        real(c_double) :: opacity = 1.0_c_double
        class(QGraphicsItem), pointer :: parent => null()
        type(QGraphicsItem), pointer :: first_child => null()
        type(QGraphicsItem), pointer :: next_sibling => null()
        integer :: child_count = 0
        class(QGraphicsScene), pointer :: scene => null()
    contains
        procedure :: set_pos => graphics_item_set_pos
        procedure :: pos => graphics_item_pos
        procedure :: set_rotation => graphics_item_set_rotation
        procedure :: rotation => graphics_item_rotation
        procedure :: set_scale => graphics_item_set_scale
        procedure :: scale => graphics_item_scale
        procedure :: set_z_value => graphics_item_set_z_value
        procedure :: z_value => graphics_item_z_value
        procedure :: set_visible => graphics_item_set_visible
        procedure :: is_visible => graphics_item_is_visible
        procedure :: set_enabled => graphics_item_set_enabled
        procedure :: is_enabled => graphics_item_is_enabled
        procedure :: set_selected => graphics_item_set_selected
        procedure :: is_selected => graphics_item_is_selected
        procedure :: set_flags => graphics_item_set_flags
        procedure :: flags => graphics_item_flags
        procedure :: set_opacity => graphics_item_set_opacity
        procedure :: opacity => graphics_item_opacity
        procedure :: set_parent_item => graphics_item_set_parent_item
        procedure :: parent_item => graphics_item_parent_item
        procedure :: scene => graphics_item_scene
        procedure :: add_to_group => graphics_item_add_to_group
        procedure :: remove_from_group => graphics_item_remove_from_group
        procedure :: move_by => graphics_item_move_by
        procedure :: ensure_visible => graphics_item_ensure_visible
        procedure :: update => graphics_item_update
        procedure(graphics_item_bounding_rect), deferred :: bounding_rect
        procedure(graphics_item_paint), deferred :: paint
        procedure :: type => graphics_item_type
        procedure :: collides_with_item => graphics_item_collides_with_item
        procedure :: collides_with_path => graphics_item_collides_with_path
        procedure :: contains => graphics_item_contains
        procedure :: is_obscured_by => graphics_item_is_obscured_by
        procedure :: map_to_item => graphics_item_map_to_item
        procedure :: map_to_scene => graphics_item_map_to_scene
        procedure :: map_from_item => graphics_item_map_from_item
        procedure :: map_from_scene => graphics_item_map_from_scene
        procedure :: advance => graphics_item_advance
        procedure :: set_transform_origin_point => graphics_item_set_transform_origin_point
        procedure :: transform_origin_point => graphics_item_transform_origin_point
        final :: graphics_item_finalize
    end type QGraphicsItem

    !> @brief Deferred procedures for QGraphicsItem
    abstract interface
        function graphics_item_bounding_rect(this) result(rect)
            import :: QGraphicsItem, forge_rect
            class(QGraphicsItem), intent(in) :: this
            type(forge_rect) :: rect
        end function graphics_item_bounding_rect

        subroutine graphics_item_paint(this, painter, option, widget)
            import :: QGraphicsItem
            class(QGraphicsItem), intent(in) :: this
            class(*), intent(in) :: painter
            class(*), intent(in), optional :: option
            class(*), intent(in), optional :: widget
        end subroutine graphics_item_paint
    end interface

    !> @brief Graphics scene containing items
    type :: QGraphicsScene
        private
        type(forge_rect) :: scene_rect
        type(forge_color) :: background_brush
        logical :: has_focus = .false.
        type(QGraphicsItem), pointer :: focus_item => null()
        type(QGraphicsItem), pointer :: root_item => null()
        integer :: item_count = 0
        type(signal_item) :: changed
        type(signal_item) :: scene_rect_changed
        type(signal_item) :: selection_changed
        type(signal_mouse) :: mouse_press
        type(signal_mouse) :: mouse_move
        type(signal_mouse) :: mouse_release
    contains
        procedure :: set_scene_rect => scene_set_scene_rect
        procedure :: scene_rect => scene_scene_rect
        procedure :: set_background_brush => scene_set_background_brush
        procedure :: background_brush => scene_background_brush
        procedure :: add_item => scene_add_item
        procedure :: remove_item => scene_remove_item
        procedure :: items => scene_items
        procedure :: selected_items => scene_selected_items
        procedure :: clear_selection => scene_clear_selection
        procedure :: set_focus_item => scene_set_focus_item
        procedure :: focus_item => scene_focus_item
        procedure :: has_focus => scene_has_focus
        procedure :: set_focus => scene_set_focus
        procedure :: clear_focus => scene_clear_focus
        procedure :: item_at => scene_item_at
        procedure :: items_bounding_rect => scene_items_bounding_rect
        procedure :: colliding_items => scene_colliding_items
        procedure :: advance => scene_advance
        procedure :: update => scene_update
        procedure :: invalidate => scene_invalidate
        procedure :: render => scene_render
        final :: scene_finalize
    end type QGraphicsScene

    !> @brief Graphics view widget for displaying scenes
    type, extends(forge_widget) :: QGraphicsView
        private
        class(QGraphicsScene), pointer :: scene => null()
        integer :: drag_mode = 0  ! 0=NoDrag, 1=ScrollHandDrag, 2=RubberBandDrag
        logical :: interactive = .true.
        integer :: render_hints = 0
        real(c_double) :: scale_factor = 1.0_c_double
        type(forge_position) :: center_pos
        integer :: alignment = 0  ! Qt::Alignment
        integer :: transformation_anchor = 0  ! 0=NoAnchor, 1=AnchorUnderMouse, 2=AnchorViewCenter
        integer :: resize_anchor = 0  ! 0=NoAnchor, 1=AnchorUnderMouse, 2=AnchorViewCenter
        type(signal_void) :: rubber_band_changed
    contains
        procedure :: set_scene => view_set_scene
        procedure :: scene => view_scene
        procedure :: set_drag_mode => view_set_drag_mode
        procedure :: drag_mode => view_drag_mode
        procedure :: set_interactive => view_set_interactive
        procedure :: is_interactive => view_is_interactive
        procedure :: set_render_hints => view_set_render_hints
        procedure :: render_hints => view_render_hints
        procedure :: center_on => view_center_on
        procedure :: ensure_visible => view_ensure_visible
        procedure :: fit_in_view => view_fit_in_view
        procedure :: scale => view_scale
        procedure :: rotate => view_rotate
        procedure :: shear => view_shear
        procedure :: translate => view_translate
        procedure :: reset_transform => view_reset_transform
        procedure :: set_transform => view_set_transform
        procedure :: transform => view_transform
        procedure :: map_to_scene => view_map_to_scene
        procedure :: map_from_scene => view_map_from_scene
        procedure :: item_at => view_item_at
        procedure :: items => view_items
        procedure :: set_alignment => view_set_alignment
        procedure :: alignment => view_alignment
        procedure :: set_transformation_anchor => view_set_transformation_anchor
        procedure :: transformation_anchor => view_transformation_anchor
        procedure :: set_resize_anchor => view_set_resize_anchor
        procedure :: resize_anchor => view_resize_anchor
        procedure :: set_optimization_flags => view_set_optimization_flags
        procedure :: optimization_flags => view_optimization_flags
        procedure :: set_rubber_band_selection_mode => view_set_rubber_band_selection_mode
        procedure :: rubber_band_selection_mode => view_rubber_band_selection_mode
        procedure :: set_rubber_band_rect => view_set_rubber_band_rect
        procedure :: rubber_band_rect => view_rubber_band_rect
        procedure :: render => view_render
        procedure :: draw_background => view_draw_background
        procedure :: draw_foreground => view_draw_foreground
        procedure :: draw_items => view_draw_items
        procedure :: invalidate_scene => view_invalidate_scene
        procedure :: update_scene => view_update_scene
        procedure :: update_scene_rect => view_update_scene_rect
    end type QGraphicsView

    !> @brief Graphics item group for managing collections of items
    type, extends(QGraphicsItem) :: QGraphicsItemGroup
        private
    contains
        procedure :: add_to_group => group_add_to_group
        procedure :: remove_from_group => group_remove_from_group
        procedure :: bounding_rect => group_bounding_rect
        procedure :: paint => group_paint
    end type QGraphicsItemGroup

    !> @brief Mouse event signal type
    type :: signal_mouse
        private
        integer :: connection_count = 0
        type(slot_mouse_proc), dimension(:), allocatable :: slots
        type(signal_connection) :: connections(100)
        logical :: blocked = .false.
    contains
        procedure :: connect => signal_mouse_connect
        procedure :: disconnect => signal_mouse_disconnect
        procedure :: emit => signal_mouse_emit
        procedure :: is_connected => signal_mouse_is_connected
    end type signal_mouse

    !> @brief Mouse slot procedure pointer
    type :: slot_mouse_proc
        procedure(slot_mouse), pointer, nopass :: proc => null()
    end type slot_mouse_proc

contains

    ! ========== QGraphicsItem Implementation ==========

    subroutine graphics_item_set_pos(this, pos)
        class(QGraphicsItem), intent(inout) :: this
        type(forge_position), intent(in) :: pos
        this%pos = pos
    end subroutine graphics_item_set_pos

    function graphics_item_pos(this) result(pos)
        class(QGraphicsItem), intent(in) :: this
        type(forge_position) :: pos
        pos = this%pos
    end function graphics_item_pos

    subroutine graphics_item_set_rotation(this, angle)
        class(QGraphicsItem), intent(inout) :: this
        real(c_double), intent(in) :: angle
        this%rotation = angle
    end subroutine graphics_item_set_rotation

    function graphics_item_rotation(this) result(angle)
        class(QGraphicsItem), intent(in) :: this
        real(c_double) :: angle
        angle = this%rotation
    end function graphics_item_rotation

    subroutine graphics_item_set_scale(this, scale)
        class(QGraphicsItem), intent(inout) :: this
        real(c_double), intent(in) :: scale
        this%scale_x = scale
        this%scale_y = scale
    end subroutine graphics_item_set_scale

    function graphics_item_scale(this) result(scale)
        class(QGraphicsItem), intent(in) :: this
        real(c_double) :: scale
        scale = this%scale_x  ! Assuming uniform scaling
    end function graphics_item_scale

    subroutine graphics_item_set_z_value(this, z)
        class(QGraphicsItem), intent(inout) :: this
        real(c_double), intent(in) :: z
        this%z_value = z
    end subroutine graphics_item_set_z_value

    function graphics_item_z_value(this) result(z)
        class(QGraphicsItem), intent(in) :: this
        real(c_double) :: z
        z = this%z_value
    end function graphics_item_z_value

    subroutine graphics_item_set_visible(this, visible)
        class(QGraphicsItem), intent(inout) :: this
        logical, intent(in) :: visible
        this%visible = visible
    end subroutine graphics_item_set_visible

    function graphics_item_is_visible(this) result(visible)
        class(QGraphicsItem), intent(in) :: this
        logical :: visible
        visible = this%visible
    end function graphics_item_is_visible

    subroutine graphics_item_set_enabled(this, enabled)
        class(QGraphicsItem), intent(inout) :: this
        logical, intent(in) :: enabled
        this%enabled = enabled
    end subroutine graphics_item_set_enabled

    function graphics_item_is_enabled(this) result(enabled)
        class(QGraphicsItem), intent(in) :: this
        logical :: enabled
        enabled = this%enabled
    end function graphics_item_is_enabled

    subroutine graphics_item_set_selected(this, selected)
        class(QGraphicsItem), intent(inout) :: this
        logical, intent(in) :: selected
        this%selected = selected
    end subroutine graphics_item_set_selected

    function graphics_item_is_selected(this) result(selected)
        class(QGraphicsItem), intent(in) :: this
        logical :: selected
        selected = this%selected
    end function graphics_item_is_selected

    subroutine graphics_item_set_flags(this, flags)
        class(QGraphicsItem), intent(inout) :: this
        integer, intent(in) :: flags
        this%flags = flags
    end subroutine graphics_item_set_flags

    function graphics_item_flags(this) result(flags)
        class(QGraphicsItem), intent(in) :: this
        integer :: flags
        flags = this%flags
    end function graphics_item_flags

    subroutine graphics_item_set_opacity(this, opacity)
        class(QGraphicsItem), intent(inout) :: this
        real(c_double), intent(in) :: opacity
        this%opacity = opacity
    end subroutine graphics_item_set_opacity

    function graphics_item_opacity(this) result(opacity)
        class(QGraphicsItem), intent(in) :: this
        real(c_double) :: opacity
        opacity = this%opacity
    end function graphics_item_opacity

    subroutine graphics_item_set_parent_item(this, parent)
        class(QGraphicsItem), intent(inout) :: this
        class(QGraphicsItem), pointer, intent(in) :: parent

        if (associated(this%parent)) then
            call this%parent%remove_child(this)
        end if

        this%parent => parent
        if (associated(parent)) then
            call parent%add_child(this)
        end if
    end subroutine graphics_item_set_parent_item

    function graphics_item_parent_item(this) result(parent)
        class(QGraphicsItem), intent(in) :: this
        class(QGraphicsItem), pointer :: parent
        parent => this%parent
    end function graphics_item_parent_item

    function graphics_item_scene(this) result(scene)
        class(QGraphicsItem), intent(in) :: this
        class(QGraphicsScene), pointer :: scene
        scene => this%scene
    end function graphics_item_scene

    subroutine graphics_item_add_to_group(this, group)
        class(QGraphicsItem), intent(inout) :: this
        class(QGraphicsItemGroup), pointer, intent(in) :: group
        ! Implementation would add to group
    end subroutine graphics_item_add_to_group

    subroutine graphics_item_remove_from_group(this, group)
        class(QGraphicsItem), intent(inout) :: this
        class(QGraphicsItemGroup), pointer, intent(in) :: group
        ! Implementation would remove from group
    end subroutine graphics_item_remove_from_group

    subroutine graphics_item_move_by(this, dx, dy)
        class(QGraphicsItem), intent(inout) :: this
        real(c_double), intent(in) :: dx, dy
        this%pos%x = this%pos%x + dx
        this%pos%y = this%pos%y + dy
    end subroutine graphics_item_move_by

    subroutine graphics_item_ensure_visible(this, xmargin, ymargin, animation)
        class(QGraphicsItem), intent(inout) :: this
        real(c_double), intent(in), optional :: xmargin, ymargin
        logical, intent(in), optional :: animation
        ! Would ensure item is visible in view
    end subroutine graphics_item_ensure_visible

    subroutine graphics_item_update(this, rect)
        class(QGraphicsItem), intent(inout) :: this
        type(forge_rect), intent(in), optional :: rect
        ! Would trigger repaint
    end subroutine graphics_item_update

    function graphics_item_type(this) result(type_val)
        class(QGraphicsItem), intent(in) :: this
        integer :: type_val
        type_val = GraphicsItemType
    end function graphics_item_type

    function graphics_item_collides_with_item(this, other, mode) result(collides)
        class(QGraphicsItem), intent(in) :: this
        class(QGraphicsItem), pointer, intent(in) :: other
        integer, intent(in), optional :: mode
        logical :: collides
        collides = .false.
        ! Would implement collision detection
    end function graphics_item_collides_with_item

    function graphics_item_collides_with_path(this, path, mode) result(collides)
        class(QGraphicsItem), intent(in) :: this
        class(*), intent(in) :: path
        integer, intent(in), optional :: mode
        logical :: collides
        collides = .false.
        ! Would implement path collision detection
    end function graphics_item_collides_with_path

    function graphics_item_contains(this, point) result(contains)
        class(QGraphicsItem), intent(in) :: this
        type(forge_position), intent(in) :: point
        logical :: contains
        contains = .false.
        ! Would implement point containment check
    end function graphics_item_contains

    function graphics_item_is_obscured_by(this, item) result(obscured)
        class(QGraphicsItem), intent(in) :: this
        class(QGraphicsItem), pointer, intent(in) :: item
        logical :: obscured
        obscured = .false.
        ! Would check if item is obscured by another
    end function graphics_item_is_obscured_by

    function graphics_item_map_to_item(this, item, path) result(mapped_path)
        class(QGraphicsItem), intent(in) :: this
        class(QGraphicsItem), pointer, intent(in) :: item
        class(*), intent(in) :: path
        class(*), allocatable :: mapped_path
        ! Would map path to item coordinates
    end function graphics_item_map_to_item

    function graphics_item_map_to_scene(this, path) result(mapped_path)
        class(QGraphicsItem), intent(in) :: this
        class(*), intent(in) :: path
        class(*), allocatable :: mapped_path
        ! Would map path to scene coordinates
    end function graphics_item_map_to_scene

    function graphics_item_map_from_item(this, item, path) result(mapped_path)
        class(QGraphicsItem), intent(in) :: this
        class(QGraphicsItem), pointer, intent(in) :: item
        class(*), intent(in) :: path
        class(*), allocatable :: mapped_path
        ! Would map path from item coordinates
    end function graphics_item_map_from_item

    function graphics_item_map_from_scene(this, path) result(mapped_path)
        class(QGraphicsItem), intent(in) :: this
        class(*), intent(in) :: path
        class(*), allocatable :: mapped_path
        ! Would map path from scene coordinates
    end function graphics_item_map_from_scene

    subroutine graphics_item_advance(this, phase)
        class(QGraphicsItem), intent(inout) :: this
        integer, intent(in) :: phase
        ! Would advance animation
    end subroutine graphics_item_advance

    subroutine graphics_item_set_transform_origin_point(this, origin)
        class(QGraphicsItem), intent(inout) :: this
        type(forge_position), intent(in) :: origin
        ! Would set transform origin
    end subroutine graphics_item_set_transform_origin_point

    function graphics_item_transform_origin_point(this) result(origin)
        class(QGraphicsItem), intent(in) :: this
        type(forge_position) :: origin
        ! Would return transform origin
    end function graphics_item_transform_origin_point

    subroutine graphics_item_add_child(this, child)
        class(QGraphicsItem), intent(inout) :: this
        type(QGraphicsItem), pointer, intent(in) :: child

        if (.not. associated(child)) return

        child%parent => this
        child%scene => this%scene

        if (.not. associated(this%first_child)) then
            this%first_child => child
        else
            ! Find last sibling
            block
                type(QGraphicsItem), pointer :: last_child
                last_child => this%first_child
                do while (associated(last_child%next_sibling))
                    last_child => last_child%next_sibling
                end do
                last_child%next_sibling => child
            end block
        end if

        this%child_count = this%child_count + 1
    end subroutine graphics_item_add_child

    subroutine graphics_item_remove_child(this, child)
        class(QGraphicsItem), intent(inout) :: this
        type(QGraphicsItem), pointer, intent(in) :: child

        if (.not. associated(child) .or. .not. associated(child%parent)) return
        if (child%parent .ne. this) return

        ! Remove from linked list
        if (associated(this%first_child, child)) then
            this%first_child => child%next_sibling
        else
            block
                type(QGraphicsItem), pointer :: prev_child
                prev_child => this%first_child
                do while (associated(prev_child%next_sibling))
                    if (associated(prev_child%next_sibling, child)) then
                        prev_child%next_sibling => child%next_sibling
                        exit
                    end if
                    prev_child => prev_child%next_sibling
                end do
            end block
        end if

        child%parent => null()
        child%scene => null()
        child%next_sibling => null()
        this%child_count = this%child_count - 1
    end subroutine graphics_item_remove_child

    subroutine graphics_item_finalize(this)
        type(QGraphicsItem), intent(inout) :: this
        ! Cleanup children would be implemented
    end subroutine graphics_item_finalize

    ! ========== QGraphicsScene Implementation ==========

    subroutine scene_set_scene_rect(this, rect)
        class(QGraphicsScene), intent(inout) :: this
        type(forge_rect), intent(in) :: rect
        this%scene_rect = rect
        call this%scene_rect_changed%emit(null())
    end subroutine scene_set_scene_rect

    function scene_scene_rect(this) result(rect)
        class(QGraphicsScene), intent(in) :: this
        type(forge_rect) :: rect
        rect = this%scene_rect
    end function scene_scene_rect

    subroutine scene_set_background_brush(this, brush)
        class(QGraphicsScene), intent(inout) :: this
        type(forge_color), intent(in) :: brush
        this%background_brush = brush
    end subroutine scene_set_background_brush

    function scene_background_brush(this) result(brush)
        class(QGraphicsScene), intent(in) :: this
        type(forge_color) :: brush
        brush = this%background_brush
    end function scene_background_brush

    subroutine scene_add_item(this, item)
        class(QGraphicsScene), intent(inout) :: this
        class(QGraphicsItem), pointer, intent(in) :: item

        if (.not. associated(item)) return

        item%scene => this
        this%item_count = this%item_count + 1

        if (.not. associated(this%root_item)) then
            allocate(this%root_item)
        end if

        call this%root_item%add_child(item)
    end subroutine scene_add_item

    subroutine scene_remove_item(this, item)
        class(QGraphicsScene), intent(inout) :: this
        class(QGraphicsItem), pointer, intent(in) :: item

        if (.not. associated(item)) return

        if (associated(this%root_item)) then
            call this%root_item%remove_child(item)
        end if

        item%scene => null()
        this%item_count = this%item_count - 1
    end subroutine scene_remove_item

    function scene_items(this, rect, mode, order, device_transform) result(items)
        class(QGraphicsScene), intent(in) :: this
        type(forge_rect), intent(in), optional :: rect
        integer, intent(in), optional :: mode, order
        class(*), intent(in), optional :: device_transform
        type(QGraphicsItem), dimension(:), pointer :: items
        items => null()
        ! Would return items in scene
    end function scene_items

    function scene_selected_items(this) result(items)
        class(QGraphicsScene), intent(in) :: this
        type(QGraphicsItem), dimension(:), pointer :: items
        items => null()
        ! Would return selected items
    end function scene_selected_items

    subroutine scene_clear_selection(this)
        class(QGraphicsScene), intent(inout) :: this
        ! Would clear selection of all items
    end subroutine scene_clear_selection

    subroutine scene_set_focus_item(this, item, focus_reason)
        class(QGraphicsScene), intent(inout) :: this
        class(QGraphicsItem), pointer, intent(in) :: item
        integer, intent(in), optional :: focus_reason
        this%focus_item => item
    end subroutine scene_set_focus_item

    function scene_focus_item(this) result(item)
        class(QGraphicsScene), intent(in) :: this
        class(QGraphicsItem), pointer :: item
        item => this%focus_item
    end function scene_focus_item

    function scene_has_focus(this) result(has_focus)
        class(QGraphicsScene), intent(in) :: this
        logical :: has_focus
        has_focus = this%has_focus
    end function scene_has_focus

    subroutine scene_set_focus(this, focus_reason)
        class(QGraphicsScene), intent(inout) :: this
        integer, intent(in), optional :: focus_reason
        this%has_focus = .true.
    end subroutine scene_set_focus

    subroutine scene_clear_focus(this)
        class(QGraphicsScene), intent(inout) :: this
        this%has_focus = .false.
        this%focus_item => null()
    end subroutine scene_clear_focus

    function scene_item_at(this, pos, device_transform) result(item)
        class(QGraphicsScene), intent(in) :: this
        type(forge_position), intent(in) :: pos
        class(*), intent(in), optional :: device_transform
        type(QGraphicsItem), pointer :: item
        item => null()
        ! Would find topmost item at position
    end function scene_item_at

    function scene_items_bounding_rect(this) result(rect)
        class(QGraphicsScene), intent(in) :: this
        type(forge_rect) :: rect
        ! Would return bounding rect of all items
    end function scene_items_bounding_rect

    function scene_colliding_items(this, item, mode) result(items)
        class(QGraphicsScene), intent(in) :: this
        class(QGraphicsItem), pointer, intent(in) :: item
        integer, intent(in), optional :: mode
        type(QGraphicsItem), dimension(:), pointer :: items
        items => null()
        ! Would return items colliding with given item
    end function scene_colliding_items

    subroutine scene_advance(this)
        class(QGraphicsScene), intent(inout) :: this
        ! Would advance all items
    end subroutine scene_advance

    subroutine scene_update(this, rect)
        class(QGraphicsScene), intent(inout) :: this
        type(forge_rect), intent(in), optional :: rect
        call this%changed%emit(null())
    end subroutine scene_update

    subroutine scene_invalidate(this, rect, layers)
        class(QGraphicsScene), intent(inout) :: this
        type(forge_rect), intent(in), optional :: rect
        integer, intent(in), optional :: layers
        ! Would invalidate scene area
    end subroutine scene_invalidate

    subroutine scene_render(this, painter, target, source, aspect_ratio_mode)
        class(QGraphicsScene), intent(in) :: this
        class(*), intent(in) :: painter
        type(forge_rect), intent(in), optional :: target, source
        integer, intent(in), optional :: aspect_ratio_mode
        ! Would render scene to painter
    end subroutine scene_render

    subroutine scene_finalize(this)
        type(QGraphicsScene), intent(inout) :: this
        ! Cleanup items would be implemented
    end subroutine scene_finalize

    ! ========== QGraphicsView Implementation ==========

    subroutine view_set_scene(this, scene)
        class(QGraphicsView), intent(inout) :: this
        class(QGraphicsScene), pointer, intent(in) :: scene
        this%scene => scene
    end subroutine view_set_scene

    function view_scene(this) result(scene)
        class(QGraphicsView), intent(in) :: this
        class(QGraphicsScene), pointer :: scene
        scene => this%scene
    end function view_scene

    subroutine view_set_drag_mode(this, mode)
        class(QGraphicsView), intent(inout) :: this
        integer, intent(in) :: mode
        this%drag_mode = mode
    end subroutine view_set_drag_mode

    function view_drag_mode(this) result(mode)
        class(QGraphicsView), intent(in) :: this
        integer :: mode
        mode = this%drag_mode
    end function view_drag_mode

    subroutine view_set_interactive(this, allowed)
        class(QGraphicsView), intent(inout) :: this
        logical, intent(in) :: allowed
        this%interactive = allowed
    end subroutine view_set_interactive

    function view_is_interactive(this) result(allowed)
        class(QGraphicsView), intent(in) :: this
        logical :: allowed
        allowed = this%interactive
    end function view_is_interactive

    subroutine view_set_render_hints(this, hints)
        class(QGraphicsView), intent(inout) :: this
        integer, intent(in) :: hints
        this%render_hints = hints
    end subroutine view_set_render_hints

    function view_render_hints(this) result(hints)
        class(QGraphicsView), intent(in) :: this
        integer :: hints
        hints = this%render_hints
    end function view_render_hints

    subroutine view_center_on(this, x, y)
        class(QGraphicsView), intent(inout) :: this
        real(c_double), intent(in) :: x, y
        this%center_pos%x = x
        this%center_pos%y = y
    end subroutine view_center_on

    subroutine view_ensure_visible(this, x, y, w, h, xmargin, ymargin)
        class(QGraphicsView), intent(inout) :: this
        real(c_double), intent(in) :: x, y, w, h
        integer, intent(in), optional :: xmargin, ymargin
        ! Would ensure rectangle is visible
    end subroutine view_ensure_visible

    subroutine view_fit_in_view(this, x, y, w, h, aspect_radio_mode)
        class(QGraphicsView), intent(inout) :: this
        real(c_double), intent(in) :: x, y, w, h
        integer, intent(in), optional :: aspect_radio_mode
        ! Would fit rectangle in view
    end subroutine view_fit_in_view

    subroutine view_scale(this, sx, sy)
        class(QGraphicsView), intent(inout) :: this
        real(c_double), intent(in) :: sx, sy
        this%scale_factor = this%scale_factor * sx
        ! Y scaling would be handled
    end subroutine view_scale

    subroutine view_rotate(this, angle)
        class(QGraphicsView), intent(inout) :: this
        real(c_double), intent(in) :: angle
        ! Would apply rotation
    end subroutine view_rotate

    subroutine view_shear(this, sh, sv)
        class(QGraphicsView), intent(inout) :: this
        real(c_double), intent(in) :: sh, sv
        ! Would apply shear
    end subroutine view_shear

    subroutine view_translate(this, dx, dy)
        class(QGraphicsView), intent(inout) :: this
        real(c_double), intent(in) :: dx, dy
        this%center_pos%x = this%center_pos%x + dx
        this%center_pos%y = this%center_pos%y + dy
    end subroutine view_translate

    subroutine view_reset_transform(this)
        class(QGraphicsView), intent(inout) :: this
        this%scale_factor = 1.0_c_double
        this%center_pos%x = 0.0_c_double
        this%center_pos%y = 0.0_c_double
    end subroutine view_reset_transform

    subroutine view_set_transform(this, matrix, combine)
        class(QGraphicsView), intent(inout) :: this
        class(*), intent(in) :: matrix
        logical, intent(in), optional :: combine
        ! Would set transformation matrix
    end subroutine view_set_transform

    function view_transform(this) result(matrix)
        class(QGraphicsView), intent(in) :: this
        class(*), allocatable :: matrix
        ! Would return transformation matrix
    end function view_transform

    function view_map_to_scene(this, point) result(scene_point)
        class(QGraphicsView), intent(in) :: this
        type(forge_position), intent(in) :: point
        type(forge_position) :: scene_point
        ! Would map view coordinates to scene coordinates
        scene_point = point
    end function view_map_to_scene

    function view_map_from_scene(this, point) result(view_point)
        class(QGraphicsView), intent(in) :: this
        type(forge_position), intent(in) :: point
        type(forge_position) :: view_point
        ! Would map scene coordinates to view coordinates
        view_point = point
    end function view_map_from_scene

    function view_item_at(this, pos) result(item)
        class(QGraphicsView), intent(in) :: this
        type(forge_position), intent(in) :: pos
        type(QGraphicsItem), pointer :: item
        item => null()
        if (associated(this%scene)) then
            item => this%scene%item_at(pos)
        end if
    end function view_item_at

    function view_items(this, rect, mode, order, device_transform) result(items)
        class(QGraphicsView), intent(in) :: this
        type(forge_rect), intent(in), optional :: rect
        integer, intent(in), optional :: mode, order
        class(*), intent(in), optional :: device_transform
        type(QGraphicsItem), dimension(:), pointer :: items
        items => null()
        if (associated(this%scene)) then
            items => this%scene%items(rect, mode, order, device_transform)
        end if
    end function view_items

    subroutine view_set_alignment(this, alignment)
        class(QGraphicsView), intent(inout) :: this
        integer, intent(in) :: alignment
        this%alignment = alignment
    end subroutine view_set_alignment

    function view_alignment(this) result(alignment)
        class(QGraphicsView), intent(in) :: this
        integer :: alignment
        alignment = this%alignment
    end function view_alignment

    subroutine view_set_transformation_anchor(this, anchor)
        class(QGraphicsView), intent(inout) :: this
        integer, intent(in) :: anchor
        this%transformation_anchor = anchor
    end subroutine view_set_transformation_anchor

    function view_transformation_anchor(this) result(anchor)
        class(QGraphicsView), intent(in) :: this
        integer :: anchor
        anchor = this%transformation_anchor
    end function view_transformation_anchor

    subroutine view_set_resize_anchor(this, anchor)
        class(QGraphicsView), intent(inout) :: this
        integer, intent(in) :: anchor
        this%resize_anchor = anchor
    end subroutine view_set_resize_anchor

    function view_resize_anchor(this) result(anchor)
        class(QGraphicsView), intent(in) :: this
        integer :: anchor
        anchor = this%resize_anchor
    end function view_resize_anchor

    subroutine view_set_optimization_flags(this, flags)
        class(QGraphicsView), intent(inout) :: this
        integer, intent(in) :: flags
        ! Would set optimization flags
    end subroutine view_set_optimization_flags

    function view_optimization_flags(this) result(flags)
        class(QGraphicsView), intent(in) :: this
        integer :: flags
        flags = 0
    end function view_optimization_flags

    subroutine view_set_rubber_band_selection_mode(this, mode)
        class(QGraphicsView), intent(inout) :: this
        integer, intent(in) :: mode
        ! Would set rubber band selection mode
    end subroutine view_set_rubber_band_selection_mode

    function view_rubber_band_selection_mode(this) result(mode)
        class(QGraphicsView), intent(in) :: this
        integer :: mode
        mode = 0
    end function view_rubber_band_selection_mode

    subroutine view_set_rubber_band_rect(this, rect)
        class(QGraphicsView), intent(inout) :: this
        type(forge_rect), intent(in) :: rect
        ! Would set rubber band rectangle
    end subroutine view_set_rubber_band_rect

    function view_rubber_band_rect(this) result(rect)
        class(QGraphicsView), intent(in) :: this
        type(forge_rect) :: rect
        ! Would return rubber band rectangle
    end function view_rubber_band_rect

    subroutine view_render(this, painter, target, source, aspect_ratio_mode)
        class(QGraphicsView), intent(in) :: this
        class(*), intent(in) :: painter
        type(forge_rect), intent(in), optional :: target, source
        integer, intent(in), optional :: aspect_ratio_mode
        if (associated(this%scene)) then
            call this%scene%render(painter, target, source, aspect_ratio_mode)
        end if
    end subroutine view_render

    subroutine view_draw_background(this, painter, rect)
        class(QGraphicsView), intent(in) :: this
        class(*), intent(in) :: painter
        type(forge_rect), intent(in) :: rect
        ! Would draw background
    end subroutine view_draw_background

    subroutine view_draw_foreground(this, painter, rect)
        class(QGraphicsView), intent(in) :: this
        class(*), intent(in) :: painter
        type(forge_rect), intent(in) :: rect
        ! Would draw foreground
    end subroutine view_draw_foreground

    subroutine view_draw_items(this, painter, num_items, items, options, widget)
        class(QGraphicsView), intent(in) :: this
        class(*), intent(in) :: painter
        integer, intent(in) :: num_items
        type(QGraphicsItem), dimension(:), intent(in) :: items
        class(*), dimension(:), intent(in) :: options
        class(*), intent(in), optional :: widget
        ! Would draw items
    end subroutine view_draw_items

    subroutine view_invalidate_scene(this, rect, layers)
        class(QGraphicsView), intent(inout) :: this
        type(forge_rect), intent(in), optional :: rect
        integer, intent(in), optional :: layers
        if (associated(this%scene)) then
            call this%scene%invalidate(rect, layers)
        end if
    end subroutine view_invalidate_scene

    subroutine view_update_scene(this, rects)
        class(QGraphicsView), intent(inout) :: this
        type(forge_rect), dimension(:), intent(in), optional :: rects
        if (associated(this%scene)) then
            call this%scene%update()
        end if
    end subroutine view_update_scene

    subroutine view_update_scene_rect(this, rect)
        class(QGraphicsView), intent(inout) :: this
        type(forge_rect), intent(in) :: rect
        if (associated(this%scene)) then
            call this%scene%set_scene_rect(rect)
        end if
    end subroutine view_update_scene_rect

    ! ========== QGraphicsItemGroup Implementation ==========

    subroutine group_add_to_group(this, item)
        class(QGraphicsItemGroup), intent(inout) :: this
        class(QGraphicsItem), pointer, intent(in) :: item
        call this%QGraphicsItem%add_child(item)
    end subroutine group_add_to_group

    subroutine group_remove_from_group(this, item)
        class(QGraphicsItemGroup), intent(inout) :: this
        class(QGraphicsItem), pointer, intent(in) :: item
        call this%QGraphicsItem%remove_child(item)
    end subroutine group_remove_from_group

    function group_bounding_rect(this) result(rect)
        class(QGraphicsItemGroup), intent(in) :: this
        type(forge_rect) :: rect
        ! Would calculate bounding rect of all children
    end function group_bounding_rect

    subroutine group_paint(this, painter, option, widget)
        class(QGraphicsItemGroup), intent(in) :: this
        class(*), intent(in) :: painter
        class(*), intent(in), optional :: option
        class(*), intent(in), optional :: widget
        ! Group doesn't paint itself, just contains items
    end subroutine group_paint

    ! ========== signal_mouse Implementation ==========

    subroutine signal_mouse_connect(this, slot)
        class(signal_mouse), intent(inout) :: this
        procedure(slot_mouse) :: slot

        if (this%connection_count < size(this%slots)) then
            this%connection_count = this%connection_count + 1
            allocate(this%slots(this%connection_count)%proc, source=slot)
        end if
    end subroutine signal_mouse_connect

    subroutine signal_mouse_disconnect(this, slot)
        class(signal_mouse), intent(inout) :: this
        procedure(slot_mouse) :: slot
        integer :: i

        do i = 1, this%connection_count
            if (associated(this%slots(i)%proc, slot)) then
                this%slots(i)%proc => null()
                exit
            end if
        end do
    end subroutine signal_mouse_disconnect

    subroutine signal_mouse_emit(this, event)
        class(signal_mouse), intent(inout) :: this
        class(*), intent(in) :: event
        integer :: i

        if (this%blocked) return

        do i = 1, this%connection_count
            if (associated(this%slots(i)%proc)) then
                call this%slots(i)%proc(event)
            end if
        end do
    end subroutine signal_mouse_emit

    function signal_mouse_is_connected(this) result(connected)
        class(signal_mouse), intent(in) :: this
        logical :: connected
        connected = this%connection_count > 0
    end function signal_mouse_is_connected

end module forge_graphicsview