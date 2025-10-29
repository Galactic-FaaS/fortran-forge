# Performance Guide

This guide covers performance optimization techniques for ForGE applications, including profiling, optimization strategies, and best practices for high-performance GUI applications.

## Performance Profiling

### Basic Profiling

```fortran
program profile_application
    use forge
    use iso_c_binding
    implicit none

    type(forge_application) :: app
    type(forge_window_t) :: window
    type(forge_status) :: status
    real(c_double) :: start_time, end_time
    integer :: frame_count = 0

    ! Initialize
    call cpu_time(start_time)
    call app%init(BACKEND_CUSTOM, status)
    window = app%create_window("Performance Test", 800, 600)
    call cpu_time(end_time)

    print *, "Initialization time:", end_time - start_time, "seconds"

    ! Profile main loop
    call cpu_time(start_time)
    call profile_main_loop(app, window)
    call cpu_time(end_time)

    print *, "Main loop time:", end_time - start_time, "seconds"
    print *, "Average frame time:", (end_time - start_time) / frame_count, "seconds"

end program profile_application

subroutine profile_main_loop(app, window)
    type(forge_application), intent(inout) :: app
    type(forge_window_t), intent(in) :: window

    integer :: i
    real(c_double) :: frame_start, frame_end

    do i = 1, 1000  ! Profile 1000 frames
        call cpu_time(frame_start)

        ! Simulate frame work
        call app%process_events()
        call update_game_logic()
        call render_frame(window)

        call cpu_time(frame_end)
        frame_count = frame_count + 1

        ! Log slow frames
        if (frame_end - frame_start > 0.0167) then  ! > 60 FPS
            print *, "Slow frame:", frame_end - frame_start, "seconds"
        end if
    end do

end subroutine profile_main_loop
```

### Memory Profiling

```fortran
module memory_profiler
    implicit none

    type :: memory_stats
        integer :: allocation_count = 0
        integer :: deallocation_count = 0
        integer(c_size_t) :: peak_memory = 0
        integer(c_size_t) :: current_memory = 0
    end type memory_stats

contains

    subroutine track_allocation(size)
        integer(c_size_t), intent(in) :: size

        ! Track memory allocation
        memory_stats%allocation_count = memory_stats%allocation_count + 1
        memory_stats%current_memory = memory_stats%current_memory + size

        if (memory_stats%current_memory > memory_stats%peak_memory) then
            memory_stats%peak_memory = memory_stats%current_memory
        end if
    end subroutine track_allocation

    subroutine track_deallocation(size)
        integer(c_size_t), intent(in) :: size

        memory_stats%deallocation_count = memory_stats%deallocation_count + 1
        memory_stats%current_memory = memory_stats%current_memory - size
    end subroutine track_deallocation

    subroutine print_memory_stats()
        print *, "Memory Statistics:"
        print *, "  Allocations:", memory_stats%allocation_count
        print *, "  Deallocations:", memory_stats%deallocation_count
        print *, "  Peak memory:", memory_stats%peak_memory, "bytes"
        print *, "  Current memory:", memory_stats%current_memory, "bytes"
        print *, "  Memory leaks:", memory_stats%current_memory > 0
    end subroutine print_memory_stats

end module memory_profiler
```

## Widget Optimization

### Widget Creation Optimization

```fortran
! Bad: Create widgets in tight loop
do i = 1, 1000
    type(forge_button) :: button  ! Inefficient - creates/destroys each iteration
    call button%set_label("Button " // int_to_string(i))
    ! ... use button ...
end do

! Good: Create widgets once, reuse
type(forge_button), allocatable :: buttons(:)
allocate(buttons(1000))

do i = 1, 1000
    call buttons(i)%set_label("Button " // int_to_string(i))
end do
```

### Lazy Widget Initialization

```fortran
type :: lazy_widget
    class(forge_widget), allocatable :: widget
    logical :: initialized = .false.
contains
    procedure :: get_widget => lazy_get_widget
end type lazy_widget

function lazy_get_widget(this) result(widget)
    class(lazy_widget), intent(inout) :: this
    class(forge_widget), pointer :: widget

    if (.not. this%initialized) then
        ! Create widget only when first accessed
        allocate(forge_button :: this%widget)
        call this%widget%set_label("Lazy Button")
        this%initialized = .true.
    end if

    widget => this%widget
end function lazy_get_widget
```

### Widget Pool

```fortran
module widget_pool
    use forge
    implicit none

    type :: button_pool
        type(forge_button), allocatable :: buttons(:)
        integer :: next_available = 1
    contains
        procedure :: get_button => pool_get_button
        procedure :: return_button => pool_return_button
    end type button_pool

contains

    function pool_get_button(this) result(button)
        class(button_pool), intent(inout) :: this
        type(forge_button), pointer :: button

        if (this%next_available > size(this%buttons)) then
            ! Expand pool
            call expand_pool(this)
        end if

        button => this%buttons(this%next_available)
        this%next_available = this%next_available + 1
    end function pool_get_button

    subroutine pool_return_button(this, button)
        class(button_pool), intent(inout) :: this
        type(forge_button), intent(in) :: button

        ! Return to pool for reuse
        this%next_available = this%next_available - 1
        this%buttons(this%next_available) = button
    end subroutine pool_return_button

end module widget_pool
```

## Layout Optimization

### Layout Caching

```fortran
type :: layout_cache
    integer :: cached_width = 0
    integer :: cached_height = 0
    logical :: valid = .false.
    ! Cached widget positions...
end type layout_cache

subroutine compute_layout_cached(layout, cache, width, height)
    type(forge_layout_base), intent(inout) :: layout
    type(layout_cache), intent(inout) :: cache
    integer, intent(in) :: width, height

    ! Only recompute if size changed
    if (.not. cache%valid .or. &
        cache%cached_width /= width .or. &
        cache%cached_height /= height) then

        call layout%set_parent_size(width, height)
        call layout%compute()

        cache%cached_width = width
        cache%cached_height = height
        cache%valid = .true.
    end if
end subroutine compute_layout_cached
```

### Batch Layout Updates

```fortran
subroutine batch_layout_updates(layout, updates)
    type(forge_layout_base), intent(inout) :: layout
    type(layout_update), intent(in) :: updates(:)

    integer :: i

    ! Disable layout recalculation
    call layout%set_auto_recalc(.false.)

    ! Apply all updates
    do i = 1, size(updates)
        select case (updates(i)%type)
        case (UPDATE_ADD_WIDGET)
            call layout%add_widget(updates(i)%widget)
        case (UPDATE_REMOVE_WIDGET)
            call layout%remove_widget(updates(i)%widget)
        case (UPDATE_RESIZE)
            call layout%set_parent_size(updates(i)%width, updates(i)%height)
        end select
    end do

    ! Re-enable and compute once
    call layout%set_auto_recalc(.true.)
    call layout%compute()
end subroutine batch_layout_updates
```

### Layout Simplification

```fortran
! Bad: Deep nesting
type(forge_box_layout) :: outer, middle, inner
call outer%add_widget(middle)
call middle%add_widget(inner)
call inner%add_widget(button)  ! 3 levels deep

! Good: Flatten when possible
type(forge_box_layout) :: layout
call layout%add_widget(button)  ! 1 level
```

## Event Handling Optimization

### Event Throttling

```fortran
module event_throttler
    implicit none

    type :: throttled_event
        real(c_double) :: last_fire = 0.0_c_double
        real(c_double) :: min_interval = 0.1_c_double  ! 100ms
        procedure(event_callback), pointer :: callback => null()
    contains
        procedure :: fire => throttled_fire
    end type throttled_event

contains

    subroutine throttled_fire(this, event)
        class(throttled_event), intent(inout) :: this
        type(forge_event), intent(in) :: event

        real(c_double) :: current_time

        call cpu_time(current_time)

        if (current_time - this%last_fire >= this%min_interval) then
            call this%callback(event)
            this%last_fire = current_time
        end if
    end subroutine throttled_fire

end module event_throttler

! Usage
type(throttled_event) :: throttled_resizer

call window%on_resize(throttled_resizer%fire)
! Resize events now throttled to 100ms intervals
```

### Event Debouncing

```fortran
type :: debounced_event
    integer :: timer_id = 0
    real(c_double) :: delay = 0.3_c_double  ! 300ms
    procedure(event_callback), pointer :: callback => null()
contains
    procedure :: fire => debounced_fire
    procedure :: execute => debounced_execute
end type debounced_event

subroutine debounced_fire(this, event)
    class(debounced_event), intent(inout) :: this
    type(forge_event), intent(in) :: event

    ! Cancel previous timer
    if (this%timer_id /= 0) then
        call cancel_timer(this%timer_id)
    end if

    ! Start new timer
    this%timer_id = start_timer(this%delay, this%execute)
end subroutine debounced_fire

subroutine debounced_execute(this)
    class(debounced_event), intent(inout) :: this

    call this%callback()  ! Execute actual callback
    this%timer_id = 0
end subroutine debounced_execute
```

### Event Prioritization

```fortran
subroutine prioritize_events(event_queue)
    type(event_queue), intent(inout) :: event_queue

    ! Process high-priority events first
    call process_events_by_priority(event_queue, PRIORITY_HIGH)
    call process_events_by_priority(event_queue, PRIORITY_NORMAL)
    call process_events_by_priority(event_queue, PRIORITY_LOW)
end subroutine prioritize_events
```

## Rendering Optimization

### Dirty Rectangle Rendering

```fortran
type :: dirty_region
    type(forge_rect) :: rect
    logical :: needs_redraw = .false.
end type dirty_region

type :: dirty_renderer
    type(dirty_region), allocatable :: regions(:)
contains
    procedure :: mark_dirty => renderer_mark_dirty
    procedure :: render => renderer_render
end type dirty_renderer

subroutine renderer_mark_dirty(this, rect)
    class(dirty_renderer), intent(inout) :: this
    type(forge_rect), intent(in) :: rect

    ! Add to dirty regions, merge overlapping
    call merge_dirty_region(this%regions, rect)
end subroutine renderer_mark_dirty

subroutine renderer_render(this)
    class(dirty_renderer), intent(inout) :: this

    integer :: i

    ! Only render dirty regions
    do i = 1, size(this%regions)
        if (this%regions(i)%needs_redraw) then
            call render_region(this%regions(i)%rect)
            this%regions(i)%needs_redraw = .false.
        end if
    end do
end subroutine renderer_render
```

### Back Buffer Rendering

```fortran
type :: double_buffered_renderer
    type(render_buffer) :: front_buffer, back_buffer
contains
    procedure :: render_to_back => double_render_to_back
    procedure :: swap_buffers => double_swap_buffers
end type double_buffered_renderer

subroutine double_render_to_back(this)
    class(double_buffered_renderer), intent(inout) :: this

    ! Render to back buffer
    call render_scene(this%back_buffer)
end subroutine double_render_to_back

subroutine double_swap_buffers(this)
    class(double_buffered_renderer), intent(inout) :: this

    ! Swap buffers
    call swap(this%front_buffer, this%back_buffer)

    ! Display front buffer
    call display_buffer(this%front_buffer)
end subroutine double_swap_buffers
```

## Memory Optimization

### String Optimization

```fortran
! Bad: Frequent string concatenation
character(len=:), allocatable :: result
do i = 1, 1000
    result = result // "item" // int_to_string(i)  ! Reallocates each time
end do

! Good: Pre-allocate or use string builder
type(string_builder) :: builder
call builder%init(10000)  ! Pre-allocate capacity

do i = 1, 1000
    call builder%append("item")
    call builder%append(int_to_string(i))
end do

result = builder%to_string()
```

### Object Pooling

```fortran
module object_pool
    implicit none

    type :: widget_pool
        class(forge_widget), allocatable :: pool(:)
        logical, allocatable :: in_use(:)
        integer :: pool_size = 100
    contains
        procedure :: get_object => pool_get_object
        procedure :: return_object => pool_return_object
    end type widget_pool

contains

    function pool_get_object(this) result(obj)
        class(widget_pool), intent(inout) :: this
        class(forge_widget), pointer :: obj

        integer :: i

        ! Find free object
        do i = 1, this%pool_size
            if (.not. this%in_use(i)) then
                this%in_use(i) = .true.
                obj => this%pool(i)
                return
            end if
        end do

        ! Pool exhausted - could expand or return null
        obj => null()
    end function pool_get_object

end module object_pool
```

## Backend-Specific Optimization

### Custom Backend Optimization

```fortran
! Custom backend specific optimizations
call backend%enable_double_buffering(.true.)
call backend%set_vsync(.false.)  ! For performance over smoothness
call backend%enable_hardware_acceleration(.true.)
```

### Qt Backend Optimization

```fortran
! Qt-specific optimizations (planned)
call qt_backend%set_attribute(Qt_AA_EnableHighDpiScaling, .true.)
call qt_backend%set_render_hint(Qt_Antialiasing, .true.)
```

## Profiling Tools

### Custom Profiler

```fortran
module custom_profiler
    implicit none

    type :: profile_sample
        character(len=:), allocatable :: name
        real(c_double) :: start_time
        real(c_double) :: end_time
        integer :: call_count = 0
    end type profile_sample

    type :: profiler
        type(profile_sample), allocatable :: samples(:)
        integer :: sample_count = 0
    contains
        procedure :: begin_sample => profiler_begin_sample
        procedure :: end_sample => profiler_end_sample
        procedure :: print_report => profiler_print_report
    end type profiler

contains

    subroutine profiler_begin_sample(this, name)
        class(profiler), intent(inout) :: this
        character(len=*), intent(in) :: name

        integer :: sample_index

        sample_index = find_or_create_sample(this, name)
        call cpu_time(this%samples(sample_index)%start_time)
    end subroutine profiler_begin_sample

    subroutine profiler_end_sample(this, name)
        class(profiler), intent(inout) :: this
        character(len=*), intent(in) :: name

        integer :: sample_index

        sample_index = find_sample(this, name)
        if (sample_index > 0) then
            call cpu_time(this%samples(sample_index)%end_time)
            this%samples(sample_index)%call_count = &
                this%samples(sample_index)%call_count + 1
        end if
    end subroutine profiler_end_sample

    subroutine profiler_print_report(this)
        class(profiler), intent(in) :: this

        integer :: i
        real(c_double) :: total_time, avg_time

        print *, "Performance Report:"
        print *, "=================="

        do i = 1, this%sample_count
            total_time = this%samples(i)%end_time - this%samples(i)%start_time
            avg_time = total_time / this%samples(i)%call_count

            print *, this%samples(i)%name, ":"
            print *, "  Total time:", total_time, "seconds"
            print *, "  Call count:", this%samples(i)%call_count
            print *, "  Avg time:", avg_time, "seconds"
        end do
    end subroutine profiler_print_report

end module custom_profiler

! Usage
type(profiler) :: prof

call prof%begin_sample("widget_creation")
! ... create widgets ...
call prof%end_sample("widget_creation")

call prof%begin_sample("layout_computation")
! ... compute layout ...
call prof%end_sample("layout_computation")

call prof%print_report()
```

## Performance Monitoring

### Runtime Performance Stats

```fortran
module performance_monitor
    implicit none

    type :: perf_stats
        real(c_double) :: fps = 0.0_c_double
        real(c_double) :: frame_time = 0.0_c_double
        integer(c_size_t) :: memory_usage = 0
        integer :: widget_count = 0
        integer :: layout_recalcs = 0
    end type perf_stats

contains

    subroutine update_performance_stats(stats)
        type(perf_stats), intent(inout) :: stats

        real(c_double) :: current_time
        static :: real(c_double) :: last_time = 0.0_c_double
        static :: integer :: frame_count = 0

        call cpu_time(current_time)

        if (last_time > 0.0_c_double) then
            stats%frame_time = current_time - last_time
            stats%fps = 1.0_c_double / stats%frame_time
        end if

        last_time = current_time
        frame_count = frame_count + 1

        ! Update other stats
        stats%memory_usage = get_memory_usage()
        stats%widget_count = get_widget_count()
    end subroutine update_performance_stats

    subroutine display_performance_overlay(window, stats)
        type(forge_window_t), intent(in) :: window
        type(perf_stats), intent(in) :: stats

        character(len=100) :: overlay_text

        write(overlay_text, '("FPS: ", F5.1, " Frame: ", F6.4, "ms Memory: ", I0, "KB")') &
              stats%fps, stats%frame_time * 1000.0_c_double, stats%memory_usage / 1024

        ! Render overlay text on window
        call render_overlay_text(window, overlay_text)
    end subroutine display_performance_overlay

end module performance_monitor
```

## Best Practices

### General Performance Tips

1. **Profile first:** Don't optimize without measuring
2. **Focus on bottlenecks:** 80/20 rule applies
3. **Test on target hardware:** Performance varies by platform
4. **Monitor memory usage:** Watch for leaks
5. **Use appropriate data structures:** Choose based on access patterns

### GUI-Specific Optimizations

1. **Minimize widget count:** Fewer widgets = better performance
2. **Use layout caching:** Avoid unnecessary recalculations
3. **Batch updates:** Group related changes
4. **Lazy initialization:** Create widgets only when needed
5. **Event throttling:** Don't react to every keystroke/mouse move

### Memory Management

1. **Avoid allocations in hot paths:** Pre-allocate when possible
2. **Use object pools:** For frequently created/destroyed objects
3. **Clean up resources:** Explicitly free unused objects
4. **Monitor garbage collection:** If applicable in backend

### Rendering Performance

1. **Use dirty rectangles:** Only redraw changed areas
2. **Double buffering:** Prevent flickering
3. **Hardware acceleration:** When available
4. **Texture atlasing:** Combine small images

## Next Steps

- Read the [backend guide](backend_guide.md) for backend-specific optimizations
- Explore the [examples](../../examples/) directory for performance examples
- Learn about [layout managers](layout_managers.md) for efficient UI organization
- Study [widget gallery](widget_gallery.md) for widget performance characteristics