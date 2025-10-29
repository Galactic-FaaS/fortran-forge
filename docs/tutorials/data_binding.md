# Data Binding Tutorial

This tutorial covers data binding patterns in ForGE applications, enabling automatic synchronization between data models and UI widgets.

## Overview

Data binding connects your application's data to UI widgets, automatically updating the display when data changes and vice versa. ForGE provides patterns and utilities for implementing data binding.

## Basic Data Binding

### Model-View Synchronization

```fortran
module person_model
    implicit none

    type :: person
        character(len=:), allocatable :: name
        integer :: age
        character(len=:), allocatable :: email
    contains
        procedure :: set_name => person_set_name
        procedure :: set_age => person_set_age
        procedure :: set_email => person_set_email
        procedure :: notify_changed => person_notify_changed
    end type person

    ! Observer pattern for data binding
    type :: data_observer
        procedure(observer_callback), pointer, nopass :: callback => null()
    end type data_observer

contains

    subroutine person_set_name(this, name)
        class(person), intent(inout) :: this
        character(len=*), intent(in) :: name

        this%name = name
        call this%notify_changed("name")
    end subroutine person_set_name

    subroutine person_set_age(this, age)
        class(person), intent(inout) :: this
        integer, intent(in) :: age

        this%age = age
        call this%notify_changed("age")
    end subroutine person_set_age

    subroutine person_set_email(this, email)
        class(person), intent(inout) :: this
        character(len=*), intent(in) :: email

        this%email = email
        call this%notify_changed("email")
    end subroutine person_set_email

    subroutine person_notify_changed(this, property)
        class(person), intent(in) :: this
        character(len=*), intent(in) :: property

        ! Notify observers (implementation depends on observer system)
        call notify_observers(this, property)
    end subroutine person_notify_changed

end module person_model
```

### Two-Way Binding

```fortran
program two_way_binding_demo
    use forge
    use person_model
    implicit none

    type(forge_application) :: app
    type(forge_window_t) :: window
    type(forge_entry) :: name_entry
    type(forge_spin_button) :: age_spin
    type(forge_entry) :: email_entry
    type(forge_form_layout) :: form
    type(forge_status) :: status

    type(person) :: current_person

    ! Initialize
    call app%init(BACKEND_CUSTOM, status)
    window = app%create_window("Data Binding Demo", 400, 250)

    ! Create form widgets
    call name_entry%set_placeholder("Enter name")
    call age_spin%set_range(0.0_c_double, 120.0_c_double)
    call email_spin%set_placeholder("Enter email")

    ! Set up two-way binding
    call bind_text_field(name_entry, current_person, "name")
    call bind_numeric_field(age_spin, current_person, "age")
    call bind_text_field(email_entry, current_person, "email")

    ! Layout
    call form%add_row(name_entry, name_entry)
    call form%add_row(age_spin, age_spin)
    call form%add_row(email_entry, email_entry)
    call form%set_parent_size(400, 250)
    call form%compute()

    ! Show window
    call window%show()
    call app%run()
    call app%shutdown()

contains

    subroutine bind_text_field(entry, model, property)
        type(forge_entry), intent(inout) :: entry
        type(person), intent(inout), target :: model
        character(len=*), intent(in) :: property

        ! Set initial value from model
        select case (property)
        case ("name")
            if (allocated(model%name)) call entry%set_text(model%name)
        case ("email")
            if (allocated(model%email)) call entry%set_text(model%email)
        end select

        ! Bind change handler
        call entry%on_change(create_text_updater(model, property))
    end subroutine bind_text_field

    subroutine bind_numeric_field(spin, model, property)
        type(forge_spin_button), intent(inout) :: spin
        type(person), intent(inout), target :: model
        character(len=*), intent(in) :: property

        ! Set initial value from model
        select case (property)
        case ("age")
            call spin%set_value(real(model%age, c_double))
        end select

        ! Bind change handler
        call spin%on_value_changed(create_numeric_updater(model, property))
    end subroutine bind_numeric_field

end program two_way_binding_demo
```

## Binding Utilities

### Generic Binding System

```fortran
module data_binding
    use forge
    implicit none

    type :: binding
        class(*), pointer :: model => null()
        character(len=:), allocatable :: property
        class(forge_widget), pointer :: widget => null()
        procedure(binder), pointer :: update_model => null()
        procedure(binder), pointer :: update_widget => null()
    contains
        procedure :: update_from_model => binding_update_from_model
        procedure :: update_from_widget => binding_update_from_widget
    end type binding

    abstract interface
        subroutine binder(binding_ptr)
            import :: binding
            type(binding), pointer :: binding_ptr
        end subroutine binder
    end interface

contains

    subroutine create_text_binding(widget, model, property, bindings)
        class(forge_widget), intent(in), target :: widget
        class(*), intent(in), target :: model
        character(len=*), intent(in) :: property
        type(binding), intent(inout), allocatable :: bindings(:)

        type(binding), allocatable :: temp(:)
        integer :: n

        ! Add to bindings array
        if (.not. allocated(bindings)) then
            allocate(bindings(1))
            n = 1
        else
            n = size(bindings) + 1
            allocate(temp(n))
            temp(1:n-1) = bindings
            deallocate(bindings)
            bindings = temp
        end if

        ! Create binding
        bindings(n)%model => model
        bindings(n)%property = property
        bindings(n)%widget => widget
        bindings(n)%update_model => update_text_model
        bindings(n)%update_widget => update_text_widget

        ! Set initial value
        call bindings(n)%update_from_model()
    end subroutine create_text_binding

    subroutine binding_update_from_model(this)
        class(binding), intent(in) :: this

        call this%update_widget(this)
    end subroutine binding_update_from_model

    subroutine binding_update_from_widget(this)
        class(binding), intent(in) :: this

        call this%update_model(this)
    end subroutine binding_update_from_widget

end module data_binding
```

### Property Binding

```fortran
subroutine bind_property(widget, model, property_name)
    class(forge_widget), intent(inout) :: widget
    class(*), intent(in) :: model
    character(len=*), intent(in) :: property_name

    ! Create property binding
    ! This would use reflection or property maps in a full implementation
    select type (model)
    type is (person)
        select case (property_name)
        case ("name")
            call bind_to_property(widget, model%name)
        case ("age")
            call bind_to_property(widget, model%age)
        case ("email")
            call bind_to_property(widget, model%email)
        end select
    end select
end subroutine bind_property
```

## Observable Collections

### List Binding

```fortran
module observable_list
    implicit none

    type :: observable_list
        class(*), allocatable :: items(:)
        type(list_observer), allocatable :: observers(:)
    contains
        procedure :: add_item => list_add_item
        procedure :: remove_item => list_remove_item
        procedure :: notify_observers => list_notify_observers
    end type observable_list

    type :: list_observer
        procedure(list_callback), pointer, nopass :: callback => null()
    end type list_observer

    abstract interface
        subroutine list_callback(change_type, index, item)
            integer, intent(in) :: change_type
            integer, intent(in) :: index
            class(*), intent(in) :: item
        end subroutine list_callback
    end interface

contains

    subroutine list_add_item(this, item)
        class(observable_list), intent(inout) :: this
        class(*), intent(in) :: item

        ! Add item to array
        ! ... implementation ...

        ! Notify observers
        call this%notify_observers(CHANGE_ADDED, size(this%items), item)
    end subroutine list_add_item

    subroutine list_remove_item(this, index)
        class(observable_list), intent(inout) :: this
        integer, intent(in) :: index

        class(*), allocatable :: removed_item

        ! Remove item
        ! ... implementation ...

        ! Notify observers
        call this%notify_observers(CHANGE_REMOVED, index, removed_item)
    end subroutine list_remove_item

end module observable_list
```

### Combo Box Binding

```fortran
subroutine bind_combo_to_list(combo, list)
    type(forge_combo_box), intent(inout) :: combo
    type(observable_list), intent(inout) :: list

    ! Clear combo and populate from list
    call combo%clear_items()
    ! ... populate items ...

    ! Set up list change handler
    call list%add_observer(create_combo_updater(combo))
end subroutine bind_combo_to_list

function create_combo_updater(combo) result(observer)
    type(forge_combo_box), intent(in), target :: combo
    type(list_observer) :: observer

    observer%callback => combo_list_changed
end function create_combo_updater

subroutine combo_list_changed(change_type, index, item)
    integer, intent(in) :: change_type
    integer, intent(in) :: index
    class(*), intent(in) :: item

    select case (change_type)
    case (CHANGE_ADDED)
        ! Add item to combo
        select type (item)
        type is (character(len=*))
            call combo%add_item(item)
        end select
    case (CHANGE_REMOVED)
        ! Remove item from combo
        call combo%remove_item(index)
    end select
end subroutine combo_list_changed
```

## Validation Binding

### Input Validation

```fortran
module validation_binding
    use forge
    implicit none

    type :: validation_rule
        procedure(validator_func), pointer, nopass :: validate => null()
        character(len=:), allocatable :: error_message
    end type validation_rule

    type :: validated_binding
        type(binding) :: base_binding
        type(validation_rule), allocatable :: rules(:)
        class(forge_widget), pointer :: error_display => null()
    contains
        procedure :: validate => validated_binding_validate
        procedure :: show_errors => validated_binding_show_errors
    end type validated_binding

    abstract interface
        function validator_func(value) result(valid)
            class(*), intent(in) :: value
            logical :: valid
        end function validator_func
    end interface

contains

    subroutine create_validated_text_binding(entry, model, property, rules, error_label)
        type(forge_entry), intent(inout) :: entry
        class(*), intent(in) :: model
        character(len=*), intent(in) :: property
        type(validation_rule), intent(in) :: rules(:)
        type(forge_label), intent(in), target :: error_label

        type(validated_binding) :: vbinding

        ! Create base binding
        call create_text_binding(entry, model, property, vbinding%base_binding)

        ! Add validation
        vbinding%rules = rules
        vbinding%error_display => error_label

        ! Set up validation on change
        call entry%on_change(create_validation_handler(vbinding))
    end subroutine create_validated_text_binding

    subroutine validated_binding_validate(this)
        class(validated_binding), intent(inout) :: this

        logical :: all_valid = .true.
        integer :: i

        ! Run all validation rules
        do i = 1, size(this%rules)
            if (.not. this%rules(i)%validate(get_current_value())) then
                all_valid = .false.
                exit
            end if
        end do

        if (.not. all_valid) then
            call this%show_errors()
        else
            call this%clear_errors()
        end if
    end subroutine validated_binding_validate

end module validation_binding
```

### Common Validators

```fortran
function validate_required(value) result(valid)
    class(*), intent(in) :: value
    logical :: valid

    select type (value)
    type is (character(len=*))
        valid = len_trim(value) > 0
    end select
end function validate_required

function validate_email(value) result(valid)
    class(*), intent(in) :: value
    logical :: valid

    select type (value)
    type is (character(len=*))
        ! Simple email validation
        integer :: at_pos, dot_pos
        at_pos = index(value, '@')
        dot_pos = index(value, '.', back=.true.)
        valid = at_pos > 1 .and. dot_pos > at_pos + 1
    end select
end function validate_email

function validate_range(value, min_val, max_val) result(valid)
    class(*), intent(in) :: value
    real(c_double), intent(in) :: min_val, max_val
    logical :: valid

    select type (value)
    type is (real(c_double))
        valid = value >= min_val .and. value <= max_val
    type is (integer)
        valid = value >= int(min_val) .and. value <= int(max_val)
    end select
end function validate_range
```

## Advanced Patterns

### Computed Properties

```fortran
type :: computed_property
    character(len=:), allocatable :: name
    procedure(compute_func), pointer :: compute => null()
    type(forge_widget), pointer :: display_widget => null()
    type(dependency), allocatable :: dependencies(:)
contains
    procedure :: update => computed_property_update
end type computed_property

subroutine create_computed_property(name, compute_func, dependencies, display)
    character(len=*), intent(in) :: name
    procedure(compute_func) :: compute_func
    character(len=*), intent(in) :: dependencies(:)
    type(forge_widget), intent(in), target :: display

    type(computed_property) :: prop

    prop%name = name
    prop%compute => compute_func
    prop%display_widget => display

    ! Set up dependencies
    ! ... implementation ...
end subroutine create_computed_property
```

### Master-Detail Binding

```fortran
type :: master_detail_binding
    class(*), pointer :: master_model => null()
    class(*), pointer :: detail_model => null()
    type(widget_binding), allocatable :: detail_bindings(:)
contains
    procedure :: update_detail => master_detail_update_detail
end type master_detail_binding

subroutine master_detail_update_detail(this)
    class(master_detail_binding), intent(inout) :: this

    ! Update detail model based on master selection
    ! Update all detail bindings
end subroutine master_detail_update_detail
```

## MVVM Pattern

### View Model

```fortran
module view_model
    use forge
    implicit none

    type :: person_view_model
        type(person) :: model
        type(forge_entry) :: name_field
        type(forge_spin_button) :: age_field
        type(forge_entry) :: email_field
        type(forge_button) :: save_button
        logical :: is_dirty = .false.
    contains
        procedure :: init => vm_init
        procedure :: save => vm_save
        procedure :: revert => vm_revert
        procedure :: update_dirty_state => vm_update_dirty_state
    end type person_view_model

contains

    subroutine vm_init(this)
        class(person_view_model), intent(inout) :: this

        ! Set up bindings
        call bind_text_field(this%name_field, this%model, "name")
        call bind_numeric_field(this%age_field, this%model, "age")
        call bind_text_field(this%email_field, this%model, "email")

        ! Set up change tracking
        call this%name_field%on_change(create_dirty_handler(this))
        call this%age_field%on_value_changed(create_dirty_handler(this))
        call this%email_field%on_change(create_dirty_handler(this))

        ! Set up save button
        call this%save_button%on_click(create_save_handler(this))
    end subroutine vm_init

    subroutine vm_save(this)
        class(person_view_model), intent(inout) :: this

        ! Save model to database/file
        call save_person(this%model)
        this%is_dirty = .false.
    end subroutine vm_save

    subroutine vm_revert(this)
        class(person_view_model), intent(inout) :: this

        ! Reload model from database
        call load_person(this%model)

        ! Update UI
        call this%name_field%set_text(this%model%name)
        call this%age_field%set_value(real(this%model%age, c_double))
        call this%email_field%set_text(this%model%email)

        this%is_dirty = .false.
    end subroutine vm_revert

end module view_model
```

## Performance Considerations

### Binding Updates

```fortran
! Batch binding updates
subroutine batch_update_bindings(bindings)
    type(binding), intent(inout) :: bindings(:)

    integer :: i

    ! Disable UI updates temporarily
    call disable_ui_updates()

    ! Update all bindings
    do i = 1, size(bindings)
        call bindings(i)%update_from_model()
    end do

    ! Re-enable and refresh
    call enable_ui_updates()
end subroutine batch_update_bindings
```

### Lazy Binding

```fortran
type :: lazy_binding
    type(binding) :: binding
    logical :: needs_update = .true.
contains
    procedure :: mark_dirty => lazy_binding_mark_dirty
    procedure :: update_if_needed => lazy_binding_update_if_needed
end type lazy_binding

subroutine lazy_binding_update_if_needed(this)
    class(lazy_binding), intent(inout) :: this

    if (this%needs_update) then
        call this%binding%update_from_model()
        this%needs_update = .false.
    end if
end subroutine lazy_binding_update_if_needed
```

## Testing Data Binding

### Unit Tests

```fortran
program test_data_binding
    use person_model
    implicit none

    type(person) :: test_person
    logical :: test_passed = .true.

    ! Test model updates
    call test_person%set_name("John Doe")
    test_passed = test_passed .and. (test_person%name == "John Doe")

    call test_person%set_age(30)
    test_passed = test_passed .and. (test_person%age == 30)

    ! Test binding updates (mock UI)
    ! ... binding tests ...

    if (test_passed) then
        print *, "All data binding tests passed"
    else
        print *, "Data binding tests failed"
    end if

end program test_data_binding
```

## Best Practices

### Binding Design

1. **Keep bindings simple:** One property per binding
2. **Use appropriate update frequency:** Not every keystroke needs to update
3. **Handle errors gracefully:** Validation failures shouldn't crash
4. **Provide user feedback:** Show loading states, validation errors

### Memory Management

1. **Clean up bindings:** Remove when widgets/models are destroyed
2. **Avoid circular references:** Use weak references when possible
3. **Pool observers:** Reuse observer objects

### Threading

1. **UI thread only:** All UI updates must happen on main thread
2. **Background processing:** Do expensive work off main thread
3. **Synchronization:** Use proper synchronization for shared data

## Next Steps

- Read the [API documentation](../api/) for widget and event details
- Explore the [examples](../../examples/) directory for binding examples
- Learn about [event handling](event_handling.md) for UI updates
- Study [layout managers](layout_managers.md) for UI organization