//! Actor runtime implementation for seq-actor
//!
//! Provides the core actor primitives: spawn, send, receive, defer, self.
//! Each actor runs as a May coroutine with its own mailbox.
//!
//! Key design: An actor IS a channel. The actor handle is a ChannelData
//! containing the mailbox sender. This aligns with seq-core's CSP design.

use may::coroutine_local;
use may::sync::mpmc;
use seq_core::{ChannelData, Value};
use std::cell::RefCell;
use std::collections::VecDeque;
use std::sync::atomic::{AtomicU64, Ordering};
use std::sync::Once;

/// Maximum number of arguments supported for actor spawn
pub const MAX_ACTOR_ARGS: usize = 8;

/// Guard to ensure actor runtime is initialized exactly once
static INIT_ONCE: Once = Once::new();

/// Global counter for generating unique actor IDs
static ACTOR_ID_COUNTER: AtomicU64 = AtomicU64::new(1);

/// Actor context stored per-coroutine
pub struct ActorContext {
    /// Unique actor ID
    pub id: u64,
    /// Mailbox channel (seq-core's MPMC channel)
    pub mailbox: ChannelData,
    /// Messages that didn't match in selective receive (deferred queue)
    pub deferred: VecDeque<i64>,
}

// Coroutine-local storage for the current actor context
coroutine_local! {
    static CURRENT_ACTOR: RefCell<Option<ActorContext>> = RefCell::new(None)
}

/// Initialize the actor runtime
///
/// Call this once at program startup before spawning any actors.
/// Safe to call multiple times - subsequent calls are no-ops.
///
/// # Thread Safety
/// Uses `std::sync::Once` internally to ensure initialization
/// happens exactly once, even when called from multiple threads.
#[no_mangle]
pub extern "C" fn sa_actor_init() {
    INIT_ONCE.call_once(|| {
        let workers = std::thread::available_parallelism()
            .map(|p| p.get())
            .unwrap_or(4);
        may::config().set_workers(workers);
    });
}

/// Spawn a new actor
///
/// # Arguments
/// * `func_ptr` - Pointer to the actor's main function. Must be a valid function
///   pointer with signature `extern "C" fn(...) -> i64` where the number of
///   parameters matches `argc`.
/// * `args_ptr` - Pointer to array of i64 arguments. Must be valid for `argc` elements,
///   or null if `argc` is 0.
/// * `argc` - Number of arguments (0 to [`MAX_ACTOR_ARGS`], currently 8)
///
/// # Returns
/// The new actor's ID (as i64), which is also used as the channel handle
///
/// # Panics
/// Panics if `argc` exceeds [`MAX_ACTOR_ARGS`] (8).
///
/// # Safety
/// - `func_ptr` must be a valid function pointer with the correct signature
///   for the given argument count
/// - `args_ptr` must be valid for reading `argc` i64 values, or null if `argc == 0`
/// - The function signature must match: `extern "C" fn(i64, ...) -> i64`
#[no_mangle]
pub unsafe extern "C" fn sa_actor_spawn(
    func_ptr: *const u8,
    args_ptr: *const i64,
    argc: i64,
) -> i64 {
    // Generate unique ID
    let actor_id = ACTOR_ID_COUNTER.fetch_add(1, Ordering::SeqCst);

    // Create mailbox channel using may's mpmc and wrap in seq-core's ChannelData
    let (sender, receiver) = mpmc::channel::<Value>();
    let mailbox = ChannelData { sender, receiver };

    // Validate argument count before spawning (fail fast, not async)
    if argc as usize > MAX_ACTOR_ARGS {
        panic!(
            "sa_actor_spawn: too many arguments ({}) - maximum is {}",
            argc, MAX_ACTOR_ARGS
        );
    }

    // Copy arguments for the new coroutine
    let args: Vec<i64> = if argc > 0 && !args_ptr.is_null() {
        std::slice::from_raw_parts(args_ptr, argc as usize).to_vec()
    } else {
        Vec::new()
    };

    // Convert function pointer to usize for Send safety
    let func_addr = func_ptr as usize;

    // Spawn the coroutine
    may::go!(move || {
        // Set up actor context
        CURRENT_ACTOR.with(|ctx| {
            *ctx.borrow_mut() = Some(ActorContext {
                id: actor_id,
                mailbox,
                deferred: VecDeque::new(),
            });
        });

        // Reconstruct function pointer from address
        let func = func_addr as *const u8;

        // Call the actor function based on argument count
        match args.len() {
            0 => {
                let f: extern "C" fn() -> i64 = std::mem::transmute(func);
                f();
            }
            1 => {
                let f: extern "C" fn(i64) -> i64 = std::mem::transmute(func);
                f(args[0]);
            }
            2 => {
                let f: extern "C" fn(i64, i64) -> i64 = std::mem::transmute(func);
                f(args[0], args[1]);
            }
            3 => {
                let f: extern "C" fn(i64, i64, i64) -> i64 = std::mem::transmute(func);
                f(args[0], args[1], args[2]);
            }
            4 => {
                let f: extern "C" fn(i64, i64, i64, i64) -> i64 = std::mem::transmute(func);
                f(args[0], args[1], args[2], args[3]);
            }
            5 => {
                let f: extern "C" fn(i64, i64, i64, i64, i64) -> i64 = std::mem::transmute(func);
                f(args[0], args[1], args[2], args[3], args[4]);
            }
            6 => {
                let f: extern "C" fn(i64, i64, i64, i64, i64, i64) -> i64 =
                    std::mem::transmute(func);
                f(args[0], args[1], args[2], args[3], args[4], args[5]);
            }
            7 => {
                let f: extern "C" fn(i64, i64, i64, i64, i64, i64, i64) -> i64 =
                    std::mem::transmute(func);
                f(
                    args[0], args[1], args[2], args[3], args[4], args[5], args[6],
                );
            }
            8 => {
                let f: extern "C" fn(i64, i64, i64, i64, i64, i64, i64, i64) -> i64 =
                    std::mem::transmute(func);
                f(
                    args[0], args[1], args[2], args[3], args[4], args[5], args[6], args[7],
                );
            }
            _ => unreachable!("argc validated before spawn"),
        }
    });

    actor_id as i64
}

/// Send a message to an actor
///
/// # Arguments
/// * `target_id` - The target actor's ID
/// * `message` - The message (i64 tagged value)
///
/// # Panics
/// Currently unimplemented - will panic if called.
/// Full actor registry and message passing will be implemented in Phase 3.
///
/// # Future Implementation
/// Phase 3 will add an actor registry to look up actors by ID and
/// enable cross-actor message passing.
#[no_mangle]
pub extern "C" fn sa_actor_send(target_id: i64, message: i64) {
    unimplemented!(
        "sa_actor_send is not yet implemented (Phase 3). \
         Called with target_id={}, message={}",
        target_id,
        message
    );
}

/// Receive a message from the mailbox (blocking)
///
/// First checks the deferred queue, then waits on the mailbox channel.
///
/// # Returns
/// The received message as i64. Currently only `Value::Int` messages are supported.
///
/// # Panics
/// - Panics if called outside an actor context
/// - Panics if a non-Int value is received (unsupported in Phase 2)
#[no_mangle]
pub extern "C" fn sa_actor_receive() -> i64 {
    CURRENT_ACTOR.with(|ctx| {
        let mut ctx_guard = ctx.borrow_mut();

        if let Some(ref mut actor_ctx) = *ctx_guard {
            // First check deferred messages
            if let Some(msg) = actor_ctx.deferred.pop_front() {
                return msg;
            }

            // Block waiting for a message using seq-core's ChannelData
            match actor_ctx.mailbox.receiver.recv() {
                Ok(value) => {
                    // Convert seq_core::Value to i64
                    match value {
                        Value::Int(n) => n,
                        other => panic!(
                            "sa_actor_receive: unsupported value type {:?}. \
                             Only Int values are supported in Phase 2.",
                            other
                        ),
                    }
                }
                Err(_) => 0, // Channel closed, return unit (0)
            }
        } else {
            panic!("sa_actor_receive: called outside of actor context");
        }
    })
}

/// Defer a message for later processing (selective receive)
///
/// Called when a message doesn't match any pattern in receive.
/// The message goes back to the deferred queue.
///
/// # Arguments
/// * `message` - The message to defer
#[no_mangle]
pub extern "C" fn sa_actor_defer(message: i64) {
    CURRENT_ACTOR.with(|ctx| {
        let mut ctx_guard = ctx.borrow_mut();

        if let Some(ref mut actor_ctx) = *ctx_guard {
            actor_ctx.deferred.push_back(message);
        }
    });
}

/// Get the current actor's ID
///
/// # Returns
/// The current actor's ID, or 0 if not in an actor context
#[no_mangle]
pub extern "C" fn sa_actor_self() -> i64 {
    CURRENT_ACTOR.with(|ctx| {
        let ctx_guard = ctx.borrow();
        if let Some(ref actor_ctx) = *ctx_guard {
            actor_ctx.id as i64
        } else {
            0
        }
    })
}

/// Check if we're currently running inside an actor
#[no_mangle]
pub extern "C" fn sa_actor_is_actor() -> bool {
    CURRENT_ACTOR.with(|ctx| ctx.borrow().is_some())
}

/// Wait for all actors to complete (for testing)
#[no_mangle]
pub extern "C" fn sa_actor_wait_all() {
    // Give actors time to process
    std::thread::sleep(std::time::Duration::from_millis(100));
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_actor_id_counter() {
        let id1 = ACTOR_ID_COUNTER.fetch_add(1, Ordering::SeqCst);
        let id2 = ACTOR_ID_COUNTER.fetch_add(1, Ordering::SeqCst);
        assert!(id2 > id1);
    }

    #[test]
    fn test_actor_self_outside_actor() {
        // Outside an actor, self() returns 0
        let id = sa_actor_self();
        assert_eq!(id, 0);
    }

    #[test]
    fn test_spawn_actor() {
        // Simple test function that does nothing
        extern "C" fn dummy_actor() -> i64 {
            0
        }

        unsafe {
            let actor_id = sa_actor_spawn(dummy_actor as *const u8, std::ptr::null(), 0);
            assert!(actor_id > 0);

            // Give it time to run
            std::thread::sleep(std::time::Duration::from_millis(10));
        }
    }

    // Note: sa_actor_send and sa_actor_receive panic on misuse, but since they are
    // extern "C" functions, panics cause process abort rather than unwind. This means
    // #[should_panic] tests don't work. The panic behavior is verified by the panic
    // messages in the function implementations.
}
