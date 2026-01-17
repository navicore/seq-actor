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

### 1.0 Whitespace Policy

**Whitespace is never significant.** The grammar is fully explicit with keywords and delimiters.

```
# All of these are identical to the parser:

receive | Msg -> handle(Msg) end

receive
  | Msg -> handle(Msg)
end

receive
| Msg ->
handle(Msg)
    end
```

**Delimiters:**
- `;` closes function definitions
- `end` closes blocks (`receive`, `match`, `supervised`, `select`)
- Parentheses, brackets for grouping and lists

**Rationale:** Code formatters and linters can enforce style. The parser accepts any valid token sequence. No Python-style indentation errors.

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
send(counter, GetCount(actor:self()))
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
spawn actor_fn(initial_state)    # -> Actor[M] (keyword, inside supervised block)
send(actor, message)             # non-blocking, consumes message
actor:self()                     # -> Actor[M] (current actor's handle)
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

### 5.3 Supervision Design Philosophy

**Goal**: Make supervision a first-class language feature, not a library pattern.

In Erlang/OTP, supervision is powerful but implemented as a library (OTP behaviours). Developers must understand supervisor specs, child specs, and the gen_server protocol. We want supervision to feel as natural as `spawn` and `send`.

**Lesson from Scala/Akka**: Good defaults that are invisible lead to developers who never learn the feature exists. When they finally need custom supervision, they don't know where to start.

**Decision: Mandatory `supervised` Blocks**

Programs without actors need no supervision syntax. The requirement applies only to `spawn`:

```
# Valid: no actors, no supervision needed
: main () ->
  io:println("Hello, World!")
;

# Valid: actors require supervised block
: main () ->
  supervised
    let worker = spawn process_data()
  end
;
```

All `spawn` calls must occur inside a `supervised` block:

```
# All spawns require a supervised block
: main () ->
  supervised                       # one_for_one is default, can omit
    let db = spawn database()
    let cache = spawn cache(db)
    server_loop(db, cache)
  end
;

# Spawn outside supervised block = compile error
: bad_example () ->
  let x = spawn worker()           # ERROR: spawn outside supervised block
;
```

**Rationale**: Even when using the default strategy, developers see the `supervised` keyword every time they spawn actors. The concept stays visible. When the day comes that they need `one_for_all`, they already know the syntax - just add one word.

**Explicit Strategies**:

```
# Default: independent actors, restart individually
supervised
  let a = spawn worker()
  let b = spawn worker()
end

# one_for_one: same as default, but explicit
supervised one_for_one
  let a = spawn worker()
  let b = spawn worker()
end

# one_for_all: any failure restarts all
supervised one_for_all
  let parser = spawn parser()
  let transformer = spawn transformer(parser)
  let writer = spawn writer(transformer)
end

# rest_for_one: failure restarts actor and all after it
supervised rest_for_one
  let leader = spawn leader()
  let follower1 = spawn follower(leader)
  let follower2 = spawn follower(leader)
end
```

**Nested Supervision Trees**:

```
: main () ->
  supervised
    let logger = spawn logger()

    supervised one_for_all
      let db = spawn database()
      let replica = spawn replica(db)
    end

    supervised
      let w1 = spawn worker()
      let w2 = spawn worker()
    end
  end
;
```

Nesting creates the supervision hierarchy. Inner blocks are supervised by the enclosing context.

**Type-Level Restart Behavior**:

Individual actor restart policy (Permanent/Transient/Temporary) is declared in the type:

```
type Worker = Actor[WorkerMsg, Permanent] ;    # always restart on failure
type Cache = Actor[CacheMsg, Transient] ;      # restart only on abnormal exit
type Task = Actor[TaskMsg, Temporary] ;        # never restart

: main () ->
  supervised
    let w = spawn worker()    # inferred Permanent from type
    let t = spawn task()      # inferred Temporary - won't restart
  end
;
```

Two concepts work together:
- **Supervision strategy** (one_for_one, etc.): How failures affect *sibling* actors
- **Restart policy** (Permanent, etc.): Whether *this* actor restarts at all

**Built-in Supervision Tree**:

The runtime maintains the supervision tree automatically:
- Every actor has a parent supervisor
- Automatic cleanup on parent termination
- Built-in restart counting and backoff (runtime-enforced limits)
- Process registry integration

**Design Considerations for Distribution**:

While initial implementation is single-node, the supervision design should be distribution-ready:
- Actor references must be location-transparent
- Supervision relationships may span nodes
- Network partitions need explicit handling strategy

This is an area requiring careful design before implementation. The goal is that local supervision patterns "just work" when actors move to remote nodes.

### 5.4 Entities and Built-in Journaling

**The Problem**: Stackful actors use ~2KB each. A million actors = 2GB. For use cases like IoT digital twins, we need millions of *logical* actors with only thousands *active*.

**The Solution**: Entities - actors with automatic persistence and resurrection.

**Entity vs Actor:**

| Keyword | Stack | Journaled | Passivates | Survives restart |
|---------|-------|-----------|------------|------------------|
| `actor` | Always allocated | No | No | No |
| `entity` | On-demand | Yes | Yes (when idle) | Yes |

**Entity Syntax:**

```
# Entity has stable identity and persisted state
entity User (id: UserId) durable state: UserState ->
  receive
    | Deposit(n) ->
        become User(id) with state: {balance: state.balance + n}
    | GetBalance(reply) ->
        send(reply, state.balance)
        become User(id) with state
  end
;

# Regular actor - ephemeral
: worker (data) ->
  receive
    | Process -> do_work(data)
  end
;
```

**Durability Modes** (per-entity-type):

```
# durable: reply waits for journal commit (safe, higher latency)
entity Account (id) durable state: AccountState -> ...

# async: journal in background, reply immediate (fast, crash may lose recent state)
entity SensorTwin (id) async state: SensorState -> ...

# volatile: no journal, passivates to memory only (cache use case)
entity Cache (id) volatile state: CachedValue -> ...
```

**How It Works:**

```
Runtime startup:
  1. Open/create <program>.journal
  2. Replay journal to build entity index (id → state)
  3. All entities start passivated (no stack allocated)

Message arrives for entity:
  1. Lookup state in index
  2. Allocate stack, activate entity
  3. Deliver message
  4. On state change: append to journal (per durability mode)

Entity idle timeout:
  1. Passivate: free stack, keep state in index
  2. Entity still "exists", just sleeping

Process crash/restart:
  1. Journal replay restores all entity state
  2. Entities resume where they left off
```

**Journal Format:**

Simple append-only log, managed by runtime:

```
# Conceptually (actual format is binary):
[ts:1234567890] [entity:user:42] {balance: 150, name: "Alice"}
[ts:1234567891] [entity:user:42] {balance: 200, name: "Alice"}
[ts:1234567892] [entity:sensor:99] {temp: 72.5, updated: 1234567892}
```

**Compaction** happens automatically:
- Snapshot current state of all entities
- Write fresh journal with snapshots only
- Delete old journal

**Memory Comparison:**

| Approach | 1M logical actors |
|----------|-------------------|
| All active (regular actors) | ~2GB |
| 10K active + 990K passivated (entities) | ~20MB + journal file |

**What We're NOT Doing:**
- Event sourcing (too complex as built-in; users can build on top)
- External databases (journal is a file, no dependencies)
- Bring-your-own persistence (opinionated > flexible)
- Distribution (single-node first)

**What We ARE Doing:**
- Built-in crash recovery for entities
- Millions of logical entities, thousands active in memory
- Zero-configuration persistence
- Transparent resurrection on message arrival

**This is a language-level feature.** No libraries to configure. Entities just survive.

---

## 6. Built-in Operations

### 6.1 Namespacing Convention

Built-in functions use `module:function` syntax, visually distinct and consistent with the language's style:

```
io:println("Hello")
str:concat(a, b)
list:map(xs, f)
int:to_string(42)
```

**Rationale**:
- Avoids ad-hoc naming that leads to breaking changes
- Clearly identifies built-ins vs user-defined functions
- Extensible to user modules in the future
- No ambiguity with function definitions (`: name` vs `module:name`)

### 6.2 Operators (Infix)

Arithmetic: `+`, `-`, `*`, `/`, `%` (type-inferred for Int/Float)

Comparison: `==`, `!=`, `<`, `>`, `<=`, `>=`

Boolean: `and`, `or`, `not`

### 6.3 Standard Modules

**`io:` - Input/Output**
```
io:print(s)           # print string, no newline
io:println(s)         # print string with newline
io:read_line()        # read line from stdin -> String
```

**`str:` - String Operations**
```
str:concat(s1, s2)    # concatenate strings
str:length(s)         # string length -> Int
str:split(s, delim)   # split by delimiter -> List[String]
str:join(xs, delim)   # join list with delimiter -> String
str:slice(s, start, end)  # substring
```

**`int:` - Integer Operations**
```
int:to_string(n)      # Int -> String
int:parse(s)          # String -> Result[Int, ParseError]
int:abs(n)            # absolute value
int:min(a, b)         # minimum
int:max(a, b)         # maximum
```

**`float:` - Float Operations**
```
float:to_string(f)    # Float -> String
float:parse(s)        # String -> Result[Float, ParseError]
float:floor(f)        # floor -> Int
float:ceil(f)         # ceiling -> Int
float:round(f)        # round -> Int
```

**`list:` - List Operations**
```
list:head(xs)         # first element -> Option[T]
list:tail(xs)         # rest of list -> Option[List[T]]
list:length(xs)       # length -> Int
list:map(xs, f)       # apply f to each element
list:filter(xs, p)    # keep elements where p is true
list:fold(xs, init, f)  # reduce list
list:append(xs, ys)   # concatenate lists
list:reverse(xs)      # reverse list
```

**`map:` - Hash Map Operations** (future)
```
map:new()             # create empty map
map:get(m, k)         # get value -> Option[V]
map:put(m, k, v)      # insert/update -> Map[K,V]
map:remove(m, k)      # remove key -> Map[K,V]
map:keys(m)           # all keys -> List[K]
map:values(m)         # all values -> List[V]
```

**`actor:` - Actor Operations**
```
actor:self()          # current actor's reference -> Actor[M]
actor:send(a, msg)    # send message (also available as send(a, msg))
actor:spawn(f)        # spawn actor (usually via spawn keyword)
```

### 6.4 List Syntax

List construction uses special syntax:
- `[]` - empty list
- `[1, 2, 3]` - list literal
- `[h | t]` - cons (head and tail)

---

## 7. Example Programs

### 7.1 Hello World

```
: main () ->
  io:println("Hello, World!")
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
  supervised
    let c = spawn counter(0)
    send(c, Inc(5))
    send(c, Inc(3))
    send(c, Get(actor:self()))
    receive
      | n -> io:println(int:to_string(n))  # prints 8
    end
  end
;
```

### 7.3 Ping-Pong

```
type PingMsg = | Ping(from: Actor[PongMsg]) ;
type PongMsg = | Pong(from: Actor[PingMsg]) ;

: pinger (count: Int, ponger: Actor[PingMsg]) ->
  | (0, _) ->
      io:println("Pinger done")
  | (n, p) ->
      send(p, Ping(actor:self()))
      receive
        | Pong(_) -> become pinger(n - 1, p)
      end
;

: ponger () ->
  receive
    | Ping(from) ->
        send(from, Pong(actor:self()))
        become ponger()
  end
;

: main () ->
  supervised
    let pong = spawn ponger()
    let ping = spawn pinger(10, pong)
    # wait for completion...
  end
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
- [ ] `supervised ... end` block parsing and semantics
- [ ] spawn/send/receive primitives
- [ ] Actor[M] type and first-class actor values
- [ ] Mailbox implementation (selective receive)
- [ ] `become` as tail-call state transition
- [ ] `actor:self()` builtin
- [ ] Stackful coroutines (green threads via May)

### Phase 3: Supervision
- [ ] Supervision strategies (one_for_one, one_for_all, rest_for_one)
- [ ] Type-level restart policies (Permanent, Transient, Temporary)
- [ ] link/monitor/trap_exits primitives
- [ ] Built-in restart counting and backoff
- [ ] Process registry

### Phase 4: Entities and Journaling
- [ ] `entity` keyword and syntax
- [ ] Durability modes (durable, async, volatile)
- [ ] Journal file format and append
- [ ] State serialization
- [ ] Passivation on idle timeout
- [ ] Resurrection on message arrival
- [ ] Journal compaction

### Phase 5: Full Concurrency
- [ ] Timeouts in receive (`after` clause)
- [ ] `select` over multiple sources
- [ ] CSP channels as secondary model
- [ ] Scheduler tuning and preemption

### Phase 6: Polish
- [ ] Standard library (module:function style)
- [ ] Error messages and diagnostics
- [ ] Documentation
- [ ] Performance optimization
- [ ] Code formatter / linter

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

**Built-in Namespacing**: `module:function` syntax
- All built-ins use `io:println`, `str:concat`, `list:map`, etc.
- Clearly distinguishes built-ins from user-defined functions
- Avoids ad-hoc naming that leads to breaking changes
- No ambiguity with function definition syntax (`: name`)
- Aliasing (e.g., `alias say = io:println`) considered but deferred - start simple

**Whitespace Policy**: Never significant
- Grammar is fully explicit with keywords and delimiters
- `end` closes all blocks (`receive`, `match`, `supervised`, `select`)
- Parser accepts any valid token sequence
- Formatters/linters enforce style, not the compiler

**Supervision**: First-class, mandatory for actors
- `supervised ... end` block required for all `spawn` calls
- Even with default strategy, the keyword is visible
- Prevents "invisible defaults nobody learns" problem (Scala/Akka lesson)
- Supervision strategy per-block, restart policy per-actor-type

**Execution Model**: Stackful coroutines
- Each actor has its own stack (~2KB)
- `receive` suspends coroutine, returns to scheduler
- Built-in IO automatically yields
- Sequential code looks sequential (no async/await coloring)

**Persistence**: Built-in journaling, not library
- `entity` keyword for actors that survive restart
- Durability modes declared per-entity-type (durable/async/volatile)
- Journal file managed by runtime, no external dependencies
- Opinionated > flexible (no bring-your-own persistence)
- Enables millions of logical entities with thousands active

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
