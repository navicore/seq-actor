# seq-actor Language Design

## Vision

**seq-actor** is a compiled actor language producing static binaries (like Go), combining:
- Seq's definition syntax (`: name ... ;`)
- Erlang's actor model (selective receive, become, supervision)
- Affine types (values used at most once, explicit copy for reuse)
- Strong static typing with inference
- First-class actors as values

---

## Core Design Decisions

| Aspect | Choice |
|--------|--------|
| Execution model | Named variables, stack hidden internally |
| Function syntax | `: name \| pattern -> body \| pattern -> body ;` |
| Assignment | Affine (at most once use, can drop) |
| Actor references | First-class actor values |
| Receive | Selective receive (non-matching stays in mailbox) |
| State | Erlang-style `become` with new state |
| Types | Strong static, inferred, exhaustive pattern checks |
| Supervision | Built-in primitives |
| Compilation | LLVM IR → clang → static binary |

---

## 1. Syntax Design

### 1.1 Function Definitions

```
# Single clause
: double (x) -> x * 2 ;

# Multiple clauses with pattern matching
: factorial
  | (0) -> 1
  | (n) -> n * factorial(n - 1)
;

# Pattern matching on structures
: handle_message
  | ({add, x, y}) -> x + y
  | ({sub, x, y}) -> x - y
  | ({neg, x})    -> 0 - x
;
```

### 1.2 Type Annotations (Optional - Inferred)

```
: factorial (n: Int) -> Int
  | (0) -> 1
  | (n) -> n * factorial(n - 1)
;
```

### 1.3 Let Bindings (Affine)

```
: example (x) ->
  let y = x * 2       # y bound
  let z = y + 1       # y consumed here
  z                   # z consumed as return value
;

# Multiple use requires copy
: example2 (x) ->
  let (a, b) = copy(x)   # explicit copy
  a + b
;
```

### 1.4 Data Types

```
# Tagged tuples (like Erlang atoms + tuples)
{ok, value}
{error, "message"}
{add, 1, 2}

# Union types (algebraic data types)
type Option[T] =
  | None
  | Some(value: T)
;

type Result[T, E] =
  | Ok(value: T)
  | Err(error: E)
;

type Message =
  | Increment(amount: Int)
  | GetCount(reply_to: Actor[Int])
  | Shutdown
;
```

### 1.5 Pattern Matching

```
: handle (msg: Message, state: Int) -> Int
  | (Increment(n), s) -> s + n
  | (GetCount(a), s)  ->
      let (s1, s2) = copy(s)
      send(a, s1)
      s2
  | (Shutdown, _)     -> 0
;
```

---

## 2. Actor Model

### 2.1 Spawning Actors

```
# spawn returns an Actor[MessageType] value
let counter = spawn counter_actor(0) ;

# Actors are first-class values
: start_workers (n: Int) -> List[Actor[WorkerMsg]]
  | (0) -> []
  | (n) -> [spawn worker() | start_workers(n - 1)]
;
```

### 2.2 Sending Messages

```
# send is non-blocking, consumes the message (affine)
send(counter, Increment(5))
send(counter, GetCount(self()))
```

### 2.3 Receive Blocks (Selective)

```
: counter_actor (state: Int) ->
  receive
    | Increment(n) ->
        become counter_actor(state + n)
    | GetCount(from) ->
        let (s1, s2) = copy(state)
        send(from, s1)
        become counter_actor(s2)
    | Shutdown ->
        # Actor terminates (no become)
        ()
  after 5000 ->
    # Timeout after 5 seconds
    become counter_actor(state)
  end
;
```

**Selective receive semantics:**
- Patterns are tried in order against mailbox
- First matching message is removed and handled
- Non-matching messages remain in mailbox for later
- Timeout clause optional

### 2.4 Become (State Machines)

```
: traffic_light () ->
  become red_state()
;

: red_state () ->
  receive
    | Timer -> become green_state()
  end
;

: green_state () ->
  receive
    | Timer -> become yellow_state()
  end
;

: yellow_state () ->
  receive
    | Timer -> become red_state()
  end
;
```

`become` is tail-recursive state transition - no stack growth.

---

## 3. Type System

### 3.1 Primitive Types

- `Int` - 64-bit signed integer
- `Float` - 64-bit IEEE 754
- `Bool` - true/false
- `String` - UTF-8 text
- `()` - Unit type

### 3.2 Composite Types

- `{tag, ...}` - Tagged tuples
- `List[T]` - Linked lists
- `Map[K, V]` - Hash maps
- `Actor[M]` - Actor accepting messages of type M
- `Channel[T]` - CSP channel carrying type T

### 3.3 Affine Type Rules

```
# Values used at most once
let x = expensive_computation()
process(x)     # x consumed
# x no longer available

# Explicit copy for multiple uses
let x = get_value()
let (a, b) = copy(x)
use_twice(a, b)

# Dropping is allowed (affine, not linear)
let x = compute()
# x goes out of scope unused - OK

# Copy is shallow for immutable data
# Deep copy for mutable/owned resources
```

**Note:** Primitive types (Int, Float, Bool, String, Unit) are implicitly copyable and don't require explicit `copy()`.

### 3.4 Type Inference

```
# Types inferred from usage
: add_one (x) -> x + 1 ;        # inferred: Int -> Int

# Explicit when needed for polymorphism
: identity[T] (x: T) -> T = x ;
```

---

## 4. Concurrency Primitives

### 4.1 Actors

```
spawn(actor_fn, initial_state)  # -> Actor[M]
send(actor, message)             # non-blocking, consumes message
self()                           # -> Actor[M] (current actor's handle)
```

### 4.2 CSP Channels (Optional Secondary Model)

```
let ch = channel[Int]()
spawn_sender(ch)

# In another actor
let value = recv(ch)   # blocking receive from channel
```

### 4.3 Select (Multiple Sources)

```
select
  | recv(ch1) as msg -> handle_ch1(msg)
  | recv(ch2) as msg -> handle_ch2(msg)
  | receive Timeout  -> handle_timeout()
  after 1000         -> handle_deadline()
end
```

---

## 5. Supervision

### 5.1 Built-in Primitives

```
# Link actors (bidirectional failure notification)
link(actor)

# Monitor (unidirectional, receive Down message)
let ref = monitor(actor)

# Trap exits (receive failures as messages instead of crashing)
trap_exits(true)
```

### 5.2 Supervisor Pattern

```
type ChildSpec =
  | Worker(start: () -> Actor[Any], restart: RestartStrategy)
;

type RestartStrategy =
  | Permanent    # always restart
  | Transient    # restart only on abnormal exit
  | Temporary    # never restart
;

: supervisor (children: List[ChildSpec]) ->
  trap_exits(true)
  let actors = start_children(children)
  supervisor_loop(children, actors)
;

: supervisor_loop (specs, actors) ->
  receive
    | Exit(actor, Reason) ->
        let (new_specs, new_actors) = handle_exit(specs, actors, actor, Reason)
        become supervisor_loop(new_specs, new_actors)
  end
;
```

---

## 6. Built-in Operations

### 6.1 Arithmetic
`+`, `-`, `*`, `/`, `%` (type-inferred for Int/Float)

### 6.2 Comparison
`==`, `!=`, `<`, `>`, `<=`, `>=`

### 6.3 Boolean
`and`, `or`, `not`

### 6.4 String
`concat(s1, s2)`, `length(s)`, `split(s, delim)`

### 6.5 List
`[h | t]` (cons), `head(l)`, `tail(l)`, `map(l, f)`, `filter(l, f)`, `fold(l, init, f)`

### 6.6 I/O
`print(s)`, `println(s)`, `read_line()`

---

## 7. Example Programs

### 7.1 Hello World

```
: main () ->
  println("Hello, World!")
;
```

### 7.2 Counter Actor

```
type CounterMsg =
  | Inc(n: Int)
  | Dec(n: Int)
  | Get(reply: Actor[Int])
  | Stop
;

: counter (state: Int) ->
  receive
    | Inc(n) -> become counter(state + n)
    | Dec(n) -> become counter(state - n)
    | Get(a) ->
        let (s1, s2) = copy(state)
        send(a, s1)
        become counter(s2)
    | Stop -> ()
  end
;

: main () ->
  let c = spawn counter(0)
  send(c, Inc(5))
  send(c, Inc(3))
  send(c, Get(self()))
  receive
    | n -> println(int_to_string(n))  # prints 8
  end
;
```

### 7.3 Ping-Pong

```
type PingMsg = | Ping(from: Actor[PongMsg]) ;
type PongMsg = | Pong(from: Actor[PingMsg]) ;

: pinger (count: Int, ponger: Actor[PingMsg]) ->
  | (0, _) ->
      println("Pinger done")
  | (n, p) ->
      send(p, Ping(self()))
      receive
        | Pong(_) -> become pinger(n - 1, p)
      end
;

: ponger () ->
  receive
    | Ping(from) ->
        send(from, Pong(self()))
        become ponger()
  end
;

: main () ->
  let pong = spawn ponger()
  let ping = spawn pinger(10, pong)
  # wait for completion...
;
```

---

## 8. Compilation Strategy

### 8.1 Pipeline

```
Source (.act)
    ↓
[LEXER/PARSER]
    ↓
AST (functions, types, actors)
    ↓
[TYPE CHECKER] (affine analysis, exhaustiveness)
    ↓
Typed AST
    ↓
[CODEGEN] → LLVM IR text
    ↓
[CLANG] -O3 + link runtime
    ↓
Static Binary
```

### 8.2 Runtime Library

Reuse/extend seq-core:
- Arena allocation
- Value representation (40-byte tagged union)
- May green threads for actors
- MPMC channels for mailboxes
- Add: actor registry, selective receive queue, supervision tree

### 8.3 Actor Compilation

Each actor function compiles to:
1. Entry point that initializes mailbox
2. Receive loop that pattern-matches mailbox
3. Become = tail call to new state function
4. Stack managed per-actor (strand)

---

## 9. Implementation Phases

### Phase 1: Core Language (No Actors) ✅ COMPLETED
- [x] Lexer and parser for function definitions
- [x] Type checker with affine analysis
- [x] LLVM IR codegen for basic expressions
- [x] Pattern matching compilation
- [x] Static binary output

### Phase 2: Basic Actors
- [ ] spawn/send/receive primitives
- [ ] Actor[M] type and first-class actor values
- [ ] Mailbox implementation (selective receive)
- [ ] become as tail-call state transition
- [ ] self() builtin

### Phase 3: Full Concurrency
- [ ] Timeouts in receive
- [ ] select over multiple sources
- [ ] CSP channels as secondary model
- [ ] Proper scheduler integration

### Phase 4: Supervision
- [ ] link/monitor/trap_exits
- [ ] Supervisor pattern library
- [ ] Built-in restart strategies
- [ ] Process registry

### Phase 5: Polish
- [ ] Standard library (lists, maps, strings, I/O)
- [ ] Error messages and diagnostics
- [ ] Documentation
- [ ] Performance optimization

---

## 10. Design Decisions (Resolved)

**Error Handling**: Both Result types + let-it-crash
- `Result[T, E]` for expected, recoverable errors
- Actor crashes for unexpected errors, supervisors restart
- Keeps code clean while enabling robust systems

**Distribution**: Design for future, implement single-node first
- Keep syntax location-transparent (Actor[M] could be local or remote)
- No networking in initial implementation
- Message types should be serialization-friendly

**Module System**: Simple includes like Seq
- `include "path/file.act"` for local files
- `include std:lists` for standard library
- Flat namespace, no complex module hierarchy

**FFI**: Seq-style include ffi with manifests
- `include ffi:libname` with TOML manifest describing C functions
- Reuse Seq's FFI approach for familiarity

**File Extension**: `.act`

---

## 11. Project Structure

```
seq-actor/
├── Cargo.toml                 # Workspace root
├── crates/
│   ├── compiler/
│   │   ├── src/
│   │   │   ├── main.rs        # CLI driver (build, run, check)
│   │   │   ├── lib.rs         # Library interface
│   │   │   ├── lexer.rs       # Tokenizer
│   │   │   ├── parser.rs      # Parser -> AST
│   │   │   ├── ast.rs         # AST definitions
│   │   │   ├── types.rs       # Type definitions
│   │   │   ├── typechecker.rs # Type inference + checking
│   │   │   ├── affine.rs      # Affine/linearity analysis
│   │   │   ├── resolver.rs    # Include resolution
│   │   │   └── codegen/
│   │   │       ├── mod.rs
│   │   │       ├── program.rs # Top-level IR generation
│   │   │       ├── functions.rs
│   │   │       ├── patterns.rs # Pattern match compilation
│   │   │       ├── actors.rs   # Actor-specific codegen
│   │   │       └── runtime.rs  # Runtime function decls
│   │   └── Cargo.toml
│   └── runtime/
│       ├── src/
│       │   ├── lib.rs
│       │   ├── value.rs       # Value representation
│       │   ├── actor.rs       # Actor spawning, lifecycle
│       │   ├── mailbox.rs     # Selective receive queue
│       │   ├── scheduler.rs   # May green thread integration
│       │   ├── supervisor.rs  # Supervision primitives
│       │   └── builtins.rs    # Built-in functions
│       └── Cargo.toml
├── std/                       # Standard library (.act files)
│   ├── prelude.act
│   ├── lists.act
│   ├── maps.act
│   └── io.act
└── examples/
    ├── hello.act
    ├── counter.act
    ├── ping_pong.act
    └── supervisor_example.act
```

---

## 12. Current Implementation Status

**Phase 1 Complete** - The core language without actors is fully implemented:

- **Compiler** (`crates/compiler/`):
  - Lexer tokenizes seq-actor syntax
  - Parser produces full AST with pattern matching
  - Type checker with inference and affine analysis (primitives implicitly copyable)
  - LLVM IR code generation
  - CLI: `seqact build`, `seqact run`, `seqact check`, `seqact emit-ir`

- **Runtime** (`crates/runtime/`):
  - Memory allocation
  - Integer and float operations
  - String operations (print, concat, length, conversions)
  - List operations (head, tail, cons, map, filter, fold)
  - 40-byte tagged value representation

- **Standard Library** (`std/`):
  - `prelude.act` - Result, Option types
  - `lists.act` - List operations
  - `io.act` - I/O helpers

- **Examples** (`examples/`):
  - `hello.act` - Hello World
  - `factorial.act` - Recursive factorial
  - `counter.act` - Stateful counter actor (syntax demo)
  - `ping_pong.act` - Two-actor communication (syntax demo)
  - `traffic_light.act` - State machine with become (syntax demo)

**39 tests passing** across compiler and runtime.
