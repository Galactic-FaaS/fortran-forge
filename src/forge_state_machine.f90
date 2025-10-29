!> @brief Qt-style State Machine Framework for ForGE
!> @details Implements hierarchical state machines with event-driven and signal-driven transitions
!> @author ForGE Contributors
!> @date 2025
!> @license GPL-3.0-or-later

module forge_state_machine
    use iso_c_binding
    use forge_types
    use forge_errors
    use forge_signals
    use forge_qobject
    use iso_fortran_env, only: int64
    implicit none
    private

    public :: QAbstractTransition, QTransition, QSignalTransition, QEventTransition
    public :: QAbstractState, QState, QFinalState, QHistoryState, QStateMachine
    public :: QHistoryState_Type

    !> History state types
    enum, bind(c)
        enumerator :: QHistoryState_Shallow = 0
        enumerator :: QHistoryState_Deep = 1
    end enum

    !> @brief History state type enumeration
    type :: QHistoryState_Type
        integer :: value
    end type QHistoryState_Type

    !> @brief Abstract base class for all transitions
    type, extends(forge_qobject), abstract :: QAbstractTransition
        private
        class(QAbstractState), pointer :: source_state => null()
        class(QAbstractState), pointer :: target_state => null()
        logical :: active = .false.
    contains
        procedure :: set_source_state => qabstracttransition_set_source_state
        procedure :: sourceState => qabstracttransition_source_state
        procedure :: set_target_state => qabstracttransition_set_target_state
        procedure :: targetState => qabstracttransition_target_state
        procedure :: set_active => qabstracttransition_set_active
        procedure :: is_active => qabstracttransition_is_active
        procedure(qabstracttransition_event_test), deferred :: eventTest
        procedure(qabstracttransition_on_transition), deferred :: onTransition
        procedure :: add_animation => qabstracttransition_add_animation
        procedure :: remove_animation => qabstracttransition_remove_animation
        procedure :: animations => qabstracttransition_animations
    end type QAbstractTransition

    !> @brief Deferred procedures for QAbstractTransition
    abstract interface
        function qabstracttransition_event_test(this, event) result(test_passed)
            import :: QAbstractTransition, forge_event
            class(QAbstractTransition), intent(in) :: this
            class(forge_event), intent(in) :: event
            logical :: test_passed
        end function qabstracttransition_event_test

        subroutine qabstracttransition_on_transition(this, event)
            import :: QAbstractTransition, forge_event
            class(QAbstractTransition), intent(inout) :: this
            class(forge_event), intent(in) :: event
        end subroutine qabstracttransition_on_transition
    end interface

    !> @brief Basic transition triggered by events
    type, extends(QAbstractTransition) :: QTransition
        private
        integer :: event_type = 0
    contains
        procedure :: init => qtransition_init
        procedure :: set_event_type => qtransition_set_event_type
        procedure :: eventType => qtransition_event_type
        procedure :: eventTest => qtransition_event_test
        procedure :: onTransition => qtransition_on_transition
    end type QTransition

    !> @brief Transition triggered by Qt signals
    type, extends(QTransition) :: QSignalTransition
        private
        class(*), pointer :: sender => null()
        character(len=:), allocatable :: signal_name
        procedure(slot_void), pointer, nopass :: slot_proc => null()
    contains
        procedure :: init => qsignaltransition_init
        procedure :: set_sender => qsignaltransition_set_sender
        procedure :: sender => qsignaltransition_sender
        procedure :: set_signal => qsignaltransition_set_signal
        procedure :: signal => qsignaltransition_signal
        procedure :: eventTest => qsignaltransition_event_test
        procedure :: onTransition => qsignaltransition_on_transition
    end type QSignalTransition

    !> @brief Transition triggered by QEvents
    type, extends(QTransition) :: QEventTransition
        private
        class(forge_qobject), pointer :: event_source => null()
        integer :: event_type_filter = 0
    contains
        procedure :: init => qeventtransition_init
        procedure :: set_event_source => qeventtransition_set_event_source
        procedure :: eventSource => qeventtransition_event_source
        procedure :: set_event_type => qeventtransition_set_event_type
        procedure :: eventType => qeventtransition_event_type
        procedure :: eventTest => qeventtransition_event_test
        procedure :: onTransition => qeventtransition_on_transition
    end type QEventTransition

    !> @brief Abstract base class for all states
    type, extends(forge_qobject), abstract :: QAbstractState
        private
        class(QStateMachine), pointer :: machine => null()
        class(QAbstractState), pointer :: parent_state => null()
        logical :: active = .false.
        type(signal_void) :: entered_signal
        type(signal_void) :: exited_signal
    contains
        procedure :: set_machine => qabstractstate_set_machine
        procedure :: machine => qabstractstate_machine
        procedure :: set_parent => qabstractstate_set_parent
        procedure :: parentState => qabstractstate_parent_state
        procedure :: set_active => qabstractstate_set_active
        procedure :: is_active => qabstractstate_is_active
        procedure(qabstractstate_on_entry), deferred :: onEntry
        procedure(qabstractstate_on_exit), deferred :: onExit
        procedure :: entered => qabstractstate_entered_signal
        procedure :: exited => qabstractstate_exited_signal
        procedure :: add_transition => qabstractstate_add_transition
        procedure :: remove_transition => qabstractstate_remove_transition
        procedure :: transitions => qabstractstate_transitions
    end type QAbstractState

    !> @brief Deferred procedures for QAbstractState
    abstract interface
        subroutine qabstractstate_on_entry(this, event)
            import :: QAbstractState, forge_event
            class(QAbstractState), intent(inout) :: this
            class(forge_event), intent(in) :: event
        end subroutine qabstractstate_on_entry

        subroutine qabstractstate_on_exit(this, event)
            import :: QAbstractState, forge_event
            class(QAbstractState), intent(inout) :: this
            class(forge_event), intent(in) :: event
        end subroutine qabstractstate_on_exit
    end interface

    !> @brief Individual state with entry/exit actions and child states
    type, extends(QAbstractState) :: QState
        private
        integer :: initial_state_index = 0
        logical :: error_state = .false.
        type(QAbstractTransition), dimension(:), allocatable :: transitions_
        type(QAbstractState), dimension(:), allocatable :: child_states
        integer :: child_count = 0
        type(signal_void) :: finished_signal
        type(signal_void) :: properties_assigned_signal
        type(signal_void) :: child_mode_changed_signal
    contains
        procedure :: init => qstate_init
        procedure :: set_initial_state => qstate_set_initial_state
        procedure :: initialState => qstate_initial_state
        procedure :: set_error_state => qstate_set_error_state
        procedure :: is_error_state => qstate_is_error_state
        procedure :: add_substate => qstate_add_substate
        procedure :: remove_substate => qstate_remove_substate
        procedure :: substates => qstate_substates
        procedure :: onEntry => qstate_on_entry
        procedure :: onExit => qstate_on_exit
        procedure :: assign_property => qstate_assign_property
        procedure :: finished => qstate_finished_signal
        procedure :: propertiesAssigned => qstate_properties_assigned_signal
        procedure :: childModeChanged => qstate_child_mode_changed_signal
        procedure :: add_transition => qstate_add_transition
        procedure :: remove_transition => qstate_remove_transition
        procedure :: transitions => qstate_transitions
    end type QState

    !> @brief Terminal state that completes state machine execution
    type, extends(QState) :: QFinalState
    contains
        procedure :: init => qfinalstate_init
        procedure :: onEntry => qfinalstate_on_entry
        procedure :: onExit => qfinalstate_on_exit
    end type QFinalState

    !> @brief State that remembers previous substates
    type, extends(QState) :: QHistoryState
        private
        integer :: history_type = QHistoryState_Shallow
        class(QAbstractState), pointer :: default_state => null()
    contains
        procedure :: init => qhistorystate_init
        procedure :: set_history_type => qhistorystate_set_history_type
        procedure :: historyType => qhistorystate_history_type
        procedure :: set_default_state => qhistorystate_set_default_state
        procedure :: defaultState => qhistorystate_default_state
        procedure :: onEntry => qhistorystate_on_entry
        procedure :: onExit => qhistorystate_on_exit
    end type QHistoryState

    !> @brief Main state machine controller with hierarchical states
    type, extends(QState) :: QStateMachine
        private
        logical :: running = .false.
        logical :: processing_events = .false.
        class(QAbstractState), pointer :: current_state => null()
        class(QAbstractState), pointer :: initial_state => null()
        class(QAbstractState), pointer :: error_state => null()
        type(QAbstractState), dimension(:), allocatable :: state_history
        integer :: history_size = 0
        integer :: max_history_size = 100
        type(signal_void) :: started_signal
        type(signal_void) :: stopped_signal
        type(signal_void) :: finished_signal
        type(signal_qobject) :: running_changed_signal
        type(signal_qobject) :: configuration_changed_signal
    contains
        procedure :: init => qstatemachine_init
        procedure :: start => qstatemachine_start
        procedure :: stop => qstatemachine_stop
        procedure :: is_running => qstatemachine_is_running
        procedure :: set_running => qstatemachine_set_running
        procedure :: set_initial_state => qstatemachine_set_initial_state
        procedure :: initialState => qstatemachine_initial_state
        procedure :: set_error_state => qstatemachine_set_error_state
        procedure :: errorState => qstatemachine_error_state
        procedure :: currentState => qstatemachine_current_state
        procedure :: post_event => qstatemachine_post_event
        procedure :: post_delayed_event => qstatemachine_post_delayed_event
        procedure :: cancel_delayed_event => qstatemachine_cancel_delayed_event
        procedure :: clear_error => qstatemachine_clear_error
        procedure :: configuration => qstatemachine_configuration
        procedure :: started => qstatemachine_started_signal
        procedure :: stopped => qstatemachine_stopped_signal
        procedure :: finished => qstatemachine_finished_signal
        procedure :: runningChanged => qstatemachine_running_changed_signal
        procedure :: configurationChanged => qstatemachine_configuration_changed_signal
        procedure :: onEntry => qstatemachine_on_entry
        procedure :: onExit => qstatemachine_on_exit
        procedure :: process_event => qstatemachine_process_event
        procedure :: begin_select_transitions => qstatemachine_begin_select_transitions
        procedure :: end_select_transitions => qstatemachine_end_select_transitions
        procedure :: begin_microstep => qstatemachine_begin_microstep
        procedure :: end_microstep => qstatemachine_end_microstep
    end type QStateMachine

contains

    ! ========== QAbstractTransition Implementation ==========

    subroutine qabstracttransition_set_source_state(this, source)
        class(QAbstractTransition), intent(inout) :: this
        class(QAbstractState), target :: source
        this%source_state => source
    end subroutine qabstracttransition_set_source_state

    function qabstracttransition_source_state(this) result(source)
        class(QAbstractTransition), intent(in) :: this
        class(QAbstractState), pointer :: source
        source => this%source_state
    end function qabstracttransition_source_state

    subroutine qabstracttransition_set_target_state(this, target)
        class(QAbstractTransition), intent(inout) :: this
        class(QAbstractState), target :: target
        this%target_state => target
    end subroutine qabstracttransition_set_target_state

    function qabstracttransition_target_state(this) result(target)
        class(QAbstractTransition), intent(in) :: this
        class(QAbstractState), pointer :: target
        target => this%target_state
    end function qabstracttransition_target_state

    subroutine qabstracttransition_set_active(this, active)
        class(QAbstractTransition), intent(inout) :: this
        logical, intent(in) :: active
        this%active = active
    end subroutine qabstracttransition_set_active

    function qabstracttransition_is_active(this) result(active)
        class(QAbstractTransition), intent(in) :: this
        logical :: active
        active = this%active
    end function qabstracttransition_is_active

    subroutine qabstracttransition_add_animation(this, animation)
        class(QAbstractTransition), intent(inout) :: this
        class(*), intent(in) :: animation
        ! Placeholder for animation support
    end subroutine qabstracttransition_add_animation

    subroutine qabstracttransition_remove_animation(this, animation)
        class(QAbstractTransition), intent(inout) :: this
        class(*), intent(in) :: animation
        ! Placeholder for animation support
    end subroutine qabstracttransition_remove_animation

    function qabstracttransition_animations(this) result(anims)
        class(QAbstractTransition), intent(in) :: this
        type(*), dimension(:), allocatable :: anims
        ! Placeholder for animation support
        allocate(anims(0))
    end function qabstracttransition_animations

    ! ========== QTransition Implementation ==========

    subroutine qtransition_init(this, source_state, event_type)
        class(QTransition), intent(inout) :: this
        class(QAbstractState), optional, target :: source_state
        integer, optional, intent(in) :: event_type

        call this%forge_qobject%init()
        call this%meta_object%set_class_name("QTransition")

        if (present(source_state)) then
            call this%set_source_state(source_state)
        end if

        if (present(event_type)) then
            this%event_type = event_type
        end if
    end subroutine qtransition_init

    subroutine qtransition_set_event_type(this, event_type)
        class(QTransition), intent(inout) :: this
        integer, intent(in) :: event_type
        this%event_type = event_type
    end subroutine qtransition_set_event_type

    function qtransition_event_type(this) result(type_val)
        class(QTransition), intent(in) :: this
        integer :: type_val
        type_val = this%event_type
    end function qtransition_event_type

    function qtransition_event_test(this, event) result(test_passed)
        class(QTransition), intent(in) :: this
        class(forge_event), intent(in) :: event
        logical :: test_passed
        test_passed = (event%type() == this%event_type)
    end function qtransition_event_test

    subroutine qtransition_on_transition(this, event)
        class(QTransition), intent(inout) :: this
        class(forge_event), intent(in) :: event
        ! Default implementation: do nothing
        ! Subclasses can override for custom transition behavior
    end subroutine qtransition_on_transition

    ! ========== QSignalTransition Implementation ==========

    subroutine qsignaltransition_init(this, source_state, sender, signal)
        class(QSignalTransition), intent(inout) :: this
        class(QAbstractState), optional, target :: source_state
        class(*), optional, target :: sender
        character(len=*), optional, intent(in) :: signal

        call this%QTransition%init(source_state)
        call this%meta_object%set_class_name("QSignalTransition")

        if (present(sender)) then
            this%sender => sender
        end if

        if (present(signal)) then
            this%signal_name = signal
        end if
    end subroutine qsignaltransition_init

    subroutine qsignaltransition_set_sender(this, sender)
        class(QSignalTransition), intent(inout) :: this
        class(*), target :: sender
        this%sender => sender
    end subroutine qsignaltransition_set_sender

    function qsignaltransition_sender(this) result(sender)
        class(QSignalTransition), intent(in) :: this
        class(*), pointer :: sender
        sender => this%sender
    end function qsignaltransition_sender

    subroutine qsignaltransition_set_signal(this, signal)
        class(QSignalTransition), intent(inout) :: this
        character(len=*), intent(in) :: signal
        this%signal_name = signal
    end subroutine qsignaltransition_set_signal

    function qsignaltransition_signal(this) result(signal)
        class(QSignalTransition), intent(in) :: this
        character(len=:), allocatable :: signal
        if (allocated(this%signal_name)) then
            signal = this%signal_name
        else
            signal = ""
        end if
    end function qsignaltransition_signal

    function qsignaltransition_event_test(this, event) result(test_passed)
        class(QSignalTransition), intent(in) :: this
        class(forge_event), intent(in) :: event
        logical :: test_passed

        ! Check if event is a signal event and matches our signal
        test_passed = .false.
        ! Implementation would check signal event details
        ! For now, assume signal events are handled separately
    end function qsignaltransition_event_test

    subroutine qsignaltransition_on_transition(this, event)
        class(QSignalTransition), intent(inout) :: this
        class(forge_event), intent(in) :: event
        ! Signal transition specific behavior
        call this%QTransition%on_transition(event)
    end subroutine qsignaltransition_on_transition

    ! ========== QEventTransition Implementation ==========

    subroutine qeventtransition_init(this, source_state, object, event_type)
        class(QEventTransition), intent(inout) :: this
        class(QAbstractState), optional, target :: source_state
        class(forge_qobject), optional, target :: object
        integer, optional, intent(in) :: event_type

        call this%QTransition%init(source_state)
        call this%meta_object%set_class_name("QEventTransition")

        if (present(object)) then
            this%event_source => object
        end if

        if (present(event_type)) then
            this%event_type_filter = event_type
        end if
    end subroutine qeventtransition_init

    subroutine qeventtransition_set_event_source(this, object)
        class(QEventTransition), intent(inout) :: this
        class(forge_qobject), target :: object
        this%event_source => object
    end subroutine qeventtransition_set_event_source

    function qeventtransition_event_source(this) result(object)
        class(QEventTransition), intent(in) :: this
        class(forge_qobject), pointer :: object
        object => this%event_source
    end function qeventtransition_event_source

    subroutine qeventtransition_set_event_type(this, event_type)
        class(QEventTransition), intent(inout) :: this
        integer, intent(in) :: event_type
        this%event_type_filter = event_type
    end subroutine qeventtransition_set_event_type

    function qeventtransition_event_type(this) result(type_val)
        class(QEventTransition), intent(in) :: this
        integer :: type_val
        type_val = this%event_type_filter
    end function qeventtransition_event_type

    function qeventtransition_event_test(this, event) result(test_passed)
        class(QEventTransition), intent(in) :: this
        class(forge_event), intent(in) :: event
        logical :: test_passed

        test_passed = (event%type() == this%event_type_filter)
        if (test_passed .and. associated(this%event_source)) then
            ! Additional checks for event source could be added here
        end if
    end function qeventtransition_event_test

    subroutine qeventtransition_on_transition(this, event)
        class(QEventTransition), intent(inout) :: this
        class(forge_event), intent(in) :: event
        ! Event transition specific behavior
        call this%QTransition%on_transition(event)
    end subroutine qeventtransition_on_transition

    ! ========== QAbstractState Implementation ==========

    subroutine qabstractstate_set_machine(this, machine)
        class(QAbstractState), intent(inout) :: this
        class(QStateMachine), target :: machine
        this%machine => machine
    end subroutine qabstractstate_set_machine

    function qabstractstate_machine(this) result(machine)
        class(QAbstractState), intent(in) :: this
        class(QStateMachine), pointer :: machine
        machine => this%machine
    end function qabstractstate_machine

    subroutine qabstractstate_set_parent(this, parent)
        class(QAbstractState), intent(inout) :: this
        class(QAbstractState), target :: parent
        this%parent_state => parent
    end subroutine qabstractstate_set_parent

    function qabstractstate_parent_state(this) result(parent)
        class(QAbstractState), intent(in) :: this
        class(QAbstractState), pointer :: parent
        parent => this%parent_state
    end function qabstractstate_parent_state

    subroutine qabstractstate_set_active(this, active)
        class(QAbstractState), intent(inout) :: this
        logical, intent(in) :: active
        this%active = active
    end subroutine qabstractstate_set_active

    function qabstractstate_is_active(this) result(active)
        class(QAbstractState), intent(in) :: this
        logical :: active
        active = this%active
    end function qabstractstate_is_active

    function qabstractstate_entered_signal(this) result(sig)
        class(QAbstractState), intent(in) :: this
        type(signal_void) :: sig
        sig = this%entered_signal
    end function qabstractstate_entered_signal

    function qabstractstate_exited_signal(this) result(sig)
        class(QAbstractState), intent(in) :: this
        type(signal_void) :: sig
        sig = this%exited_signal
    end function qabstractstate_exited_signal

    subroutine qabstractstate_add_transition(this, transition)
        class(QAbstractState), intent(inout) :: this
        class(QAbstractTransition), intent(in) :: transition
        ! Default implementation - subclasses should override
    end subroutine qabstractstate_add_transition

    subroutine qabstractstate_remove_transition(this, transition)
        class(QAbstractState), intent(inout) :: this
        class(QAbstractTransition), intent(in) :: transition
        ! Default implementation - subclasses should override
    end subroutine qabstractstate_remove_transition

    function qabstractstate_transitions(this) result(transitions_list)
        class(QAbstractState), intent(in) :: this
        type(QAbstractTransition), dimension(:), allocatable :: transitions_list
        ! Default implementation - subclasses should override
        allocate(transitions_list(0))
    end function qabstractstate_transitions

    ! ========== QState Implementation ==========

    subroutine qstate_init(this, parent)
        class(QState), intent(inout) :: this
        class(QAbstractState), optional, target :: parent

        call this%forge_qobject%init()
        call this%meta_object%set_class_name("QState")

        if (present(parent)) then
            call this%set_parent(parent)
        end if

        allocate(this%transitions_(10))  ! Initial capacity
        allocate(this%child_states(10))  ! Initial capacity
    end subroutine qstate_init

    subroutine qstate_set_initial_state(this, state)
        class(QState), intent(inout) :: this
        class(QAbstractState), target :: state

        ! Find the index of the state in child_states
        integer :: i
        do i = 1, this%child_count
            if (associated(this%child_states(i)%state, state)) then
                this%initial_state_index = i
                exit
            end if
        end do
    end subroutine qstate_set_initial_state

    function qstate_initial_state(this) result(state)
        class(QState), intent(in) :: this
        class(QAbstractState), pointer :: state

        if (this%initial_state_index > 0 .and. this%initial_state_index <= this%child_count) then
            state => this%child_states(this%initial_state_index)%state
        else
            state => null()
        end if
    end function qstate_initial_state

    subroutine qstate_set_error_state(this, error)
        class(QState), intent(inout) :: this
        logical, intent(in) :: error
        this%error_state = error
    end subroutine qstate_set_error_state

    function qstate_is_error_state(this) result(error)
        class(QState), intent(in) :: this
        logical :: error
        error = this%error_state
    end function qstate_is_error_state

    subroutine qstate_add_substate(this, state)
        class(QState), intent(inout) :: this
        class(QAbstractState), target :: state

        type(QAbstractState), dimension(:), allocatable :: temp
        integer :: new_size

        ! Check if state is already added
        integer :: i
        do i = 1, this%child_count
            if (associated(this%child_states(i)%state, state)) return
        end do

        ! Resize array if needed
        if (this%child_count >= size(this%child_states)) then
            new_size = size(this%child_states) * 2
            allocate(temp(new_size))
            temp(1:size(this%child_states)) = this%child_states
            call move_alloc(temp, this%child_states)
        end if

        this%child_count = this%child_count + 1
        this%child_states(this%child_count)%state => state
        call state%set_parent(this)
    end subroutine qstate_add_substate

    subroutine qstate_remove_substate(this, state)
        class(QState), intent(inout) :: this
        class(QAbstractState), target :: state

        integer :: i, j
        do i = 1, this%child_count
            if (associated(this%child_states(i)%state, state)) then
                ! Shift remaining states
                do j = i, this%child_count - 1
                    this%child_states(j) = this%child_states(j + 1)
                end do
                this%child_count = this%child_count - 1
                exit
            end if
        end do
    end subroutine qstate_remove_substate

    function qstate_substates(this) result(states)
        class(QState), intent(in) :: this
        type(QAbstractState), dimension(:), allocatable :: states

        allocate(states(this%child_count))
        states = this%child_states(1:this%child_count)
    end function qstate_substates

    subroutine qstate_on_entry(this, event)
        class(QState), intent(inout) :: this
        class(forge_event), intent(in) :: event

        call this%entered_signal%emit()
    end subroutine qstate_on_entry

    subroutine qstate_on_exit(this, event)
        class(QState), intent(inout) :: this
        class(forge_event), intent(in) :: event

        call this%exited_signal%emit()
    end subroutine qstate_on_exit

    subroutine qstate_assign_property(this, object, name, value)
        class(QState), intent(inout) :: this
        class(forge_qobject), intent(in) :: object
        character(len=*), intent(in) :: name
        class(*), intent(in) :: value

        ! Implementation would store property assignments for state transitions
        ! For now, this is a placeholder
        call this%properties_assigned_signal%emit()
    end subroutine qstate_assign_property

    function qstate_finished_signal(this) result(sig)
        class(QState), intent(in) :: this
        type(signal_void) :: sig
        sig = this%finished_signal
    end function qstate_finished_signal

    function qstate_properties_assigned_signal(this) result(sig)
        class(QState), intent(in) :: this
        type(signal_void) :: sig
        sig = this%properties_assigned_signal
    end function qstate_properties_assigned_signal

    function qstate_child_mode_changed_signal(this) result(sig)
        class(QState), intent(in) :: this
        type(signal_void) :: sig
        sig = this%child_mode_changed_signal
    end function qstate_child_mode_changed_signal

    subroutine qstate_add_transition(this, transition)
        class(QState), intent(inout) :: this
        class(QAbstractTransition), intent(in) :: transition

        type(QAbstractTransition), dimension(:), allocatable :: temp
        integer :: new_size

        ! Resize array if needed
        if (size(this%transitions_) <= 0 .or. &
            size(this%transitions_) >= size(this%transitions_)) then
            new_size = max(10, size(this%transitions_) * 2)
            allocate(temp(new_size))
            if (allocated(this%transitions_)) then
                temp(1:size(this%transitions_)) = this%transitions_
                deallocate(this%transitions_)
            end if
            allocate(this%transitions_(new_size))
            if (allocated(temp)) then
                this%transitions_(1:size(temp)) = temp
                deallocate(temp)
            end if
        end if

        ! Add transition
        integer :: i
        do i = 1, size(this%transitions_)
            if (.not. allocated(this%transitions_(i)%transition)) then
                allocate(this%transitions_(i)%transition, source=transition)
                exit
            end if
        end do
    end subroutine qstate_add_transition

    subroutine qstate_remove_transition(this, transition)
        class(QState), intent(inout) :: this
        class(QAbstractTransition), intent(in) :: transition

        integer :: i
        do i = 1, size(this%transitions_)
            if (allocated(this%transitions_(i)%transition)) then
                ! Compare transitions (simplified comparison)
                if (associated(this%transitions_(i)%transition%source_state(), transition%source_state()) .and. &
                    associated(this%transitions_(i)%transition%target_state(), transition%target_state())) then
                    deallocate(this%transitions_(i)%transition)
                    exit
                end if
            end if
        end do
    end subroutine qstate_remove_transition

    function qstate_transitions(this) result(transitions_list)
        class(QState), intent(in) :: this
        type(QAbstractTransition), dimension(:), allocatable :: transitions_list

        integer :: count, i, j
        count = 0
        do i = 1, size(this%transitions_)
            if (allocated(this%transitions_(i)%transition)) count = count + 1
        end do

        allocate(transitions_list(count))
        j = 1
        do i = 1, size(this%transitions_)
            if (allocated(this%transitions_(i)%transition)) then
                transitions_list(j) = this%transitions_(i)%transition
                j = j + 1
            end if
        end do
    end function qstate_transitions

    ! ========== QFinalState Implementation ==========

    subroutine qfinalstate_init(this, parent)
        class(QFinalState), intent(inout) :: this
        class(QAbstractState), optional, target :: parent

        call this%QState%init(parent)
        call this%meta_object%set_class_name("QFinalState")
    end subroutine qfinalstate_init

    subroutine qfinalstate_on_entry(this, event)
        class(QFinalState), intent(inout) :: this
        class(forge_event), intent(in) :: event

        ! Call parent onEntry
        call this%QState%on_entry(event)

        ! Final state specific behavior - could emit finished signal
        if (associated(this%machine)) then
            call this%machine%finished_signal%emit()
        end if
    end subroutine qfinalstate_on_entry

    subroutine qfinalstate_on_exit(this, event)
        class(QFinalState), intent(inout) :: this
        class(forge_event), intent(in) :: event

        call this%QState%on_exit(event)
    end subroutine qfinalstate_on_exit

    ! ========== QHistoryState Implementation ==========

    subroutine qhistorystate_init(this, parent, history_type)
        class(QHistoryState), intent(inout) :: this
        class(QAbstractState), optional, target :: parent
        integer, optional, intent(in) :: history_type

        call this%QState%init(parent)
        call this%meta_object%set_class_name("QHistoryState")

        if (present(history_type)) then
            this%history_type = history_type
        end if
    end subroutine qhistorystate_init

    subroutine qhistorystate_set_history_type(this, history_type)
        class(QHistoryState), intent(inout) :: this
        integer, intent(in) :: history_type
        this%history_type = history_type
    end subroutine qhistorystate_set_history_type

    function qhistorystate_history_type(this) result(type_val)
        class(QHistoryState), intent(in) :: this
        integer :: type_val
        type_val = this%history_type
    end function qhistorystate_history_type

    subroutine qhistorystate_set_default_state(this, state)
        class(QHistoryState), intent(inout) :: this
        class(QAbstractState), target :: state
        this%default_state => state
    end subroutine qhistorystate_set_default_state

    function qhistorystate_default_state(this) result(state)
        class(QHistoryState), intent(in) :: this
        class(QAbstractState), pointer :: state
        state => this%default_state
    end function qhistorystate_default_state

    subroutine qhistorystate_on_entry(this, event)
        class(QHistoryState), intent(inout) :: this
        class(forge_event), intent(in) :: event

        ! History state transitions to the remembered state
        ! Implementation would restore the appropriate state from history
        call this%QState%on_entry(event)
    end subroutine qhistorystate_on_entry

    subroutine qhistorystate_on_exit(this, event)
        class(QHistoryState), intent(inout) :: this
        class(forge_event), intent(in) :: event

        call this%QState%on_exit(event)
    end subroutine qhistorystate_on_exit

    ! ========== QStateMachine Implementation ==========

    subroutine qstatemachine_init(this, parent)
        class(QStateMachine), intent(inout) :: this
        class(QAbstractState), optional, target :: parent

        call this%QState%init(parent)
        call this%meta_object%set_class_name("QStateMachine")

        allocate(this%state_history(this%max_history_size))
    end subroutine qstatemachine_init

    subroutine qstatemachine_start(this)
        class(QStateMachine), intent(inout) :: this

        if (.not. this%running) then
            this%running = .true.
            call this%running_changed_signal%emit(this)

            ! Enter initial state
            if (associated(this%initial_state)) then
                call this%enter_state(this%initial_state)
            end if

            call this%started_signal%emit()
        end if
    end subroutine qstatemachine_start

    subroutine qstatemachine_stop(this)
        class(QStateMachine), intent(inout) :: this

        if (this%running) then
            this%running = .false.
            call this%running_changed_signal%emit(this)
            call this%stopped_signal%emit()
        end if
    end subroutine qstatemachine_stop

    function qstatemachine_is_running(this) result(running)
        class(QStateMachine), intent(in) :: this
        logical :: running
        running = this%running
    end function qstatemachine_is_running

    subroutine qstatemachine_set_running(this, running)
        class(QStateMachine), intent(inout) :: this
        logical, intent(in) :: running

        if (running .neqv. this%running) then
            if (running) then
                call this%start()
            else
                call this%stop()
            end if
        end if
    end subroutine qstatemachine_set_running

    subroutine qstatemachine_set_initial_state(this, state)
        class(QStateMachine), intent(inout) :: this
        class(QAbstractState), target :: state
        this%initial_state => state
    end subroutine qstatemachine_set_initial_state

    function qstatemachine_initial_state(this) result(state)
        class(QStateMachine), intent(in) :: this
        class(QAbstractState), pointer :: state
        state => this%initial_state
    end function qstatemachine_initial_state

    subroutine qstatemachine_set_error_state(this, state)
        class(QStateMachine), intent(inout) :: this
        class(QAbstractState), target :: state
        this%error_state => state
    end subroutine qstatemachine_set_error_state

    function qstatemachine_error_state(this) result(state)
        class(QStateMachine), intent(in) :: this
        class(QAbstractState), pointer :: state
        state => this%error_state
    end function qstatemachine_error_state

    function qstatemachine_current_state(this) result(state)
        class(QStateMachine), intent(in) :: this
        class(QAbstractState), pointer :: state
        state => this%current_state
    end function qstatemachine_current_state

    subroutine qstatemachine_post_event(this, event, priority)
        class(QStateMachine), intent(inout) :: this
        class(forge_event), intent(in) :: event
        integer, optional, intent(in) :: priority

        ! Queue event for processing
        ! Implementation would add to event queue
        call this%process_event(event)
    end subroutine qstatemachine_post_event

    subroutine qstatemachine_post_delayed_event(this, event, delay)
        class(QStateMachine), intent(inout) :: this
        class(forge_event), intent(in) :: event
        integer, intent(in) :: delay

        ! Implementation would schedule delayed event
        ! For now, process immediately
        call this%process_event(event)
    end subroutine qstatemachine_post_delayed_event

    subroutine qstatemachine_cancel_delayed_event(this, event)
        class(QStateMachine), intent(inout) :: this
        class(forge_event), intent(in) :: event

        ! Implementation would remove from delayed event queue
    end subroutine qstatemachine_cancel_delayed_event

    subroutine qstatemachine_clear_error(this)
        class(QStateMachine), intent(inout) :: this

        ! Clear error state
        this%error_state => null()
    end subroutine qstatemachine_clear_error

    function qstatemachine_configuration(this) result(states)
        class(QStateMachine), intent(in) :: this
        type(QAbstractState), dimension(:), allocatable :: states

        ! Return current configuration of active states
        allocate(states(1))
        if (associated(this%current_state)) then
            states(1)%state => this%current_state
        end if
    end function qstatemachine_configuration

    function qstatemachine_started_signal(this) result(sig)
        class(QStateMachine), intent(in) :: this
        type(signal_void) :: sig
        sig = this%started_signal
    end function qstatemachine_started_signal

    function qstatemachine_stopped_signal(this) result(sig)
        class(QStateMachine), intent(in) :: this
        type(signal_void) :: sig
        sig = this%stopped_signal
    end function qstatemachine_stopped_signal

    function qstatemachine_finished_signal(this) result(sig)
        class(QStateMachine), intent(in) :: this
        type(signal_void) :: sig
        sig = this%finished_signal
    end function qstatemachine_finished_signal

    function qstatemachine_running_changed_signal(this) result(sig)
        class(QStateMachine), intent(in) :: this
        type(signal_qobject) :: sig
        sig = this%running_changed_signal
    end function qstatemachine_running_changed_signal

    function qstatemachine_configuration_changed_signal(this) result(sig)
        class(QStateMachine), intent(in) :: this
        type(signal_qobject) :: sig
        sig = this%configuration_changed_signal
    end function qstatemachine_configuration_changed_signal

    subroutine qstatemachine_on_entry(this, event)
        class(QStateMachine), intent(inout) :: this
        class(forge_event), intent(in) :: event

        call this%QState%on_entry(event)
    end subroutine qstatemachine_on_entry

    subroutine qstatemachine_on_exit(this, event)
        class(QStateMachine), intent(inout) :: this
        class(forge_event), intent(in) :: event

        call this%QState%on_exit(event)
    end subroutine qstatemachine_on_exit

    subroutine qstatemachine_process_event(this, event)
        class(QStateMachine), intent(inout) :: this
        class(forge_event), intent(in) :: event

        if (.not. this%running) return

        this%processing_events = .true.

        ! Find and execute transitions
        call this%begin_select_transitions(event)
        call this%end_select_transitions(event)

        this%processing_events = .false.
    end subroutine qstatemachine_process_event

    subroutine qstatemachine_begin_select_transitions(this, event)
        class(QStateMachine), intent(inout) :: this
        class(forge_event), intent(in) :: event

        ! Implementation would select enabled transitions
        ! For now, this is a placeholder
    end subroutine qstatemachine_begin_select_transitions

    subroutine qstatemachine_end_select_transitions(this, event)
        class(QStateMachine), intent(inout) :: this
        class(forge_event), intent(in) :: event

        ! Implementation would execute selected transitions
        ! For now, this is a placeholder
    end subroutine qstatemachine_end_select_transitions

    subroutine qstatemachine_begin_microstep(this, event)
        class(QStateMachine), intent(inout) :: this
        class(forge_event), intent(in) :: event

        ! Implementation would begin microstep processing
        ! For now, this is a placeholder
    end subroutine qstatemachine_begin_microstep

    subroutine qstatemachine_end_microstep(this, event)
        class(QStateMachine), intent(inout) :: this
        class(forge_event), intent(in) :: event

        ! Implementation would end microstep processing
        ! For now, this is a placeholder
    end subroutine qstatemachine_end_microstep

    ! Helper subroutine to enter a state
    subroutine enter_state(this, state)
        class(QStateMachine), intent(inout) :: this
        class(QAbstractState), intent(in) :: state

        ! Exit current state if any
        if (associated(this%current_state)) then
            call this%current_state%onExit(forge_event(QEvent_None))
        end if

        ! Enter new state
        this%current_state => state
        call state%onEntry(forge_event(QEvent_None))

        ! Update history
        call this%add_to_history(state)
    end subroutine enter_state

    ! Helper subroutine to add state to history
    subroutine add_to_history(this, state)
        class(QStateMachine), intent(inout) :: this
        class(QAbstractState), intent(in) :: state

        if (this%history_size < this%max_history_size) then
            this%history_size = this%history_size + 1
            this%state_history(this%history_size)%state => state
        else
            ! Shift history and add new state
            integer :: i
            do i = 1, this%max_history_size - 1
                this%state_history(i) = this%state_history(i + 1)
            end do
            this%state_history(this%max_history_size)%state => state
        end if
    end subroutine add_to_history

end module forge_state_machine