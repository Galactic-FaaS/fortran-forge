# Threading Examples

This section demonstrates ForGE Qt's concurrent programming capabilities, including worker threads, thread pools, and synchronization primitives.

## Worker Threads

### Basic Worker Thread

**File:** `examples/thread_basic_worker/thread_basic_worker.f90`

Demonstrates:
- Worker thread creation and management
- Background task execution
- Progress reporting to UI thread
- Thread lifecycle management

```fortran
type(forge_worker_thread) :: worker
call worker%set_name("background_task")
call worker%on_progress(on_progress_update)
call worker%start()
```

### Concurrent Tasks

**File:** `examples/thread_concurrent_tasks/thread_concurrent_tasks.f90`

Demonstrates:
- Multiple concurrent worker threads
- Task coordination and synchronization
- Progress aggregation
- Dynamic task management

## Thread Pools

### Thread Pool Management

**File:** `examples/thread_thread_pool/thread_thread_pool.f90`

Demonstrates:
- Thread pool creation and configuration
- Task submission and queuing
- Dynamic pool resizing
- Resource management

```fortran
type(forge_thread_pool) :: pool
call pool%set_max_threads(4)
call pool%submit_task(my_task_function)
```

## Threading Architecture

ForGE Qt provides several threading abstractions:

1. **Worker Threads:** Individual background threads
2. **Thread Pools:** Managed pool of reusable threads
3. **Thread-Local Storage:** Thread-specific data
4. **Synchronization Primitives:** Mutexes, semaphores

### Thread Communication

Threads communicate through:
- **Signals and Slots:** Thread-safe connections
- **Event Posting:** Cross-thread event delivery
- **Shared Data:** Protected by synchronization primitives

## Synchronization

ForGE Qt provides synchronization through:

- **QMutex:** Mutual exclusion
- **QReadWriteLock:** Reader-writer locking
- **QSemaphore:** Resource counting
- **QWaitCondition:** Thread coordination

```fortran
type(forge_mutex) :: mutex
call mutex%lock()
! Critical section
call mutex%unlock()
```

## Thread-Safe Design

Key principles for thread-safe code:

1. **Immutability:** Use immutable data where possible
2. **Message Passing:** Communicate via signals/slots
3. **Atomic Operations:** For simple state changes
4. **Proper Synchronization:** Lock access to shared data

## Performance Considerations

1. **Thread Creation Overhead:** Reuse threads when possible
2. **Context Switching:** Minimize thread switches
3. **Lock Contention:** Reduce lock contention
4. **Memory Barriers:** Ensure memory visibility

## Thread Pool Best Practices

```fortran
! Configure pool for optimal performance
call pool%set_max_threads(ideal_thread_count())
call pool%set_expiry_timeout(30000)  ! 30 seconds

! Submit tasks
do i = 1, num_tasks
    call pool%submit_task(task_functions(i))
end do

! Wait for completion
call pool%wait_for_done()
```

## Running Threading Examples

```bash
# Build and run threading examples
cmake --build build --target thread_basic_worker
./build/examples/thread_basic_worker/thread_basic_worker

# Test thread pool
cmake --build build --target thread_thread_pool
./build/examples/thread_thread_pool/thread_thread_pool
```

## Threading Patterns

| Pattern | Examples | Use Case |
|---------|----------|----------|
| **Worker Thread** | Basic Worker | Single background task |
| **Concurrent Tasks** | Concurrent Tasks | Multiple independent tasks |
| **Thread Pool** | Thread Pool | Task queuing and management |

## Common Pitfalls

1. **Race Conditions:** Unprotected shared data access
2. **Deadlocks:** Circular lock dependencies
3. **Starvation:** Threads unable to progress
4. **False Sharing:** Cache line contention

## Debugging Threaded Code

ForGE Qt provides debugging support:
- Thread naming for identification
- Thread-safe logging
- Deadlock detection (in debug builds)
- Performance profiling

These examples demonstrate safe and effective concurrent programming patterns in ForGE Qt applications.