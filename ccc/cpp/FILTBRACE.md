# FILTBRACE.md - Brace Insertion Filter Specification

## Purpose

`filtbrace.c` is a pull-based token filter that inserts synthetic `{` and `}` tokens around single-statement bodies of control structures:

- `if (cond) stmt;` → `if (cond) { stmt; }`
- `else stmt;` → `else { stmt; }`
- `while (cond) stmt;` → `while (cond) { stmt; }`
- `for (...) stmt;` → `for (...) { stmt; }`
- `do stmt; while (cond);` → `do { stmt; } while (cond);`

This normalization simplifies downstream processing by guaranteeing all control structure bodies are braced.

---

## API

```c
void filtbrace_init(void (*up)(struct token *));
void filtbrace(struct token *out);
void filtbraceChk(void);  /* DEBUG only: verify brace balance at EOF */
```

The filter is initialized with an upstream token source function. Call `filtbrace()` repeatedly to get filtered tokens.

---

## State Machine

### States

| State | Value | Description |
|-------|-------|-------------|
| `ST_NORMAL` | 0 | Default pass-through mode |
| `ST_COND` | 1 | Inside condition parentheses (IF/WHILE/FOR) |
| `ST_PENDING` | 2 | After condition, checking if next token is `{` |
| `ST_BODY` | 3 | Inside synthetic single-statement body |
| `ST_ELSE_CHK` | 4 | After IF body, checking for ELSE before closing |
| `ST_DO_WHILE` | 5 | After DO body, waiting for `while (cond)` |

### State Transition Diagram

```
                    IF/WHILE/FOR
    ST_NORMAL ────────────────────► ST_COND
        │                              │
        │ ELSE/DO                      │ balanced RPAR
        │                              │ (depth==0)
        ▼                              ▼
    ST_PENDING ◄───────────────────────┘
        │
        ├── BEGIN found ──────────► ST_NORMAL (user braces, no insertion)
        │                           or ST_BODY (if inside synthetic body, track depth)
        │
        ├── else if ──────────────► ST_COND (continue with IF, no brace needed)
        │
        └── other token ──────────► ST_BODY (emit synthetic {, push stack)
                                       │
                                       │ SEMI at depth==0
                                       ▼
                              ┌─── ST_ELSE_CHK (if ctrl was IF)
                              │
                              ├─── ST_DO_WHILE (if ctrl was DO)
                              │
                              └─── ST_BODY/ST_NORMAL (WHILE/FOR/ELSE body complete)

    ST_ELSE_CHK:
        ├── ELSE found ───► ST_PENDING (emit }, pop IF, start ELSE)
        │
        └── no ELSE ──────► cascade closing: emit }, pop, check outer levels
                            eventually → ST_NORMAL

    ST_DO_WHILE:
        ├── WHILE found ──► ST_COND (ctrl_type=DO, wait for condition close)
        │
        └── other ────────► ST_NORMAL (syntax error, pass through)
```

---

## Global State Variables

| Variable | Type | Purpose |
|----------|------|---------|
| `state` | int | Current state machine state |
| `depth` | int | Parenthesis/bracket depth counter |
| `ctrl_type` | unsigned char | Current control keyword (IF/ELSE/WHILE/FOR/DO) |
| `saved_ctrl` | struct token | Saved control keyword for deferred processing |
| `has_saved` | int | Flag: 1 if `saved_ctrl` contains valid token |
| `pb` | struct pendbuf | Pending output token queue (dynamic circular buffer) |
| `stk` | struct stkent* | Dynamic array of synthetic body contexts |
| `stk_sp` | int | Stack pointer (0 = empty) |
| `stk_alloc` | int | Current stack capacity (grows by doubling) |

---

## Buffer Management

### Pending Buffer (`pb`)

A circular buffer (struct pendbuf) used to queue tokens for later output:

```c
struct pendbuf {
    struct token *buf;  /* dynamically allocated array */
    int size;           /* current capacity */
    int rd;             /* read index */
    int wr;             /* write index */
};
```

**Operations:**
- `pend_init(&pb, 8)` - Initialize with capacity 8, grows as needed
- `pend_push(&pb, &t)` - Enqueue token (copies token via `tokcpy`)
- `pend_has(&pb)` - Returns true if queue non-empty
- `pend_pop(&pb, out)` - Dequeue token to output
- `pend_tok(&pb, type)` - Enqueue synthetic token of given type

**Usage pattern:**
1. Queue tokens and synthetic braces with `pend_push`/`pend_tok`
2. At function entry, drain pending buffer before processing new input
3. Pending buffer has priority over saved tokens

### Saved Token (`saved_ctrl`, `has_saved`)

Single token save slot for deferred processing of control keywords:

**Used when:**
- Nested control structure found in ST_PENDING (e.g., `if (a) if (b)`)
- Need to emit `{` first, then process the saved control keyword
- In ST_ELSE_CHK when cascading closes, token is saved while emitting `}`

**Processing order:**
1. First: drain `pb` (pending buffer)
2. Second: restore `saved_ctrl` if `has_saved`
3. Third: read from upstream

---

## Stack Management

### Body Stack (`stk`, `stk_sp`, `stk_alloc`)

Tracks nested synthetic body insertions. Dynamically allocated and grows as needed:

```c
struct stkent {
    unsigned char ctrl_type;  /* IF/ELSE/WHILE/FOR/DO */
    unsigned char is_else;    /* 1 if this is an ELSE body */
};
static struct stkent *stk;    /* dynamically allocated */
static int stk_sp;            /* current stack depth */
static int stk_alloc;         /* current capacity */
```

**Initialization:** `stk_alloc = 8`, allocates 8 entries initially.

**Push:** When inserting synthetic `{`:
- `push_body(ctrl, is_else)` - Record control type and whether it's ELSE
- Grows array (doubles capacity) when full

**Pop:** When emitting synthetic `}`:
- `pop_body()` - Remove top entry, decrement `stk_sp`

**Stack depth determines:**
- Whether to return to ST_BODY or ST_NORMAL after body closes
- Whether outer IF needs else-check when inner body closes
- Cascade behavior for nested IFs

---

## Detailed State Behaviors

### ST_NORMAL

**Entry:** Initial state, or after completing a control structure.

**Transitions:**
| Input | Action | Next State |
|-------|--------|------------|
| IF, WHILE, FOR | Set `ctrl_type`, reset `depth` | ST_COND |
| ELSE, DO | Set `ctrl_type` | ST_PENDING |
| other | Pass through | ST_NORMAL |

### ST_COND

**Purpose:** Track parenthesis depth in condition expressions.

**Transitions:**
| Input | Action | Next State |
|-------|--------|------------|
| LPAR | `depth++` | ST_COND |
| RPAR | `depth--`; if depth==0 and ctrl_type==DO, done | ST_NORMAL (DO) or ST_PENDING |
| other | Pass through | ST_COND |

**Note:** DO's `while(cond)` doesn't need brace insertion (body already handled).

### ST_PENDING

**Purpose:** Check if user wrote braces; if not, insert synthetic `{`.

**Transitions:**
| Input | Action | Next State |
|-------|--------|------------|
| BEGIN | User braces present | ST_NORMAL (no stack) or ST_BODY (inside stack, depth=1) |
| IF after ELSE | `else if` pattern, no brace | ST_COND (ctrl_type=IF) |
| IF, WHILE, FOR | Push stack, save token, emit `{` | ST_COND |
| DO, ELSE | Push stack, save token, emit `{` | ST_PENDING |
| other | Push stack, queue token, emit `{` | ST_BODY |

**Emitting `{`:**
- Call `emit_begin(out)` which does `toksynth(out, BEGIN)`
- Track synthetic balance (DEBUG)
- Return immediately; caller receives the `{`

### ST_BODY

**Purpose:** Track depth inside synthetic body, detect statement end.

**Depth tracking:**
| Token | Effect |
|-------|--------|
| BEGIN, LPAR, LBRACK | `depth++` |
| END, RPAR, RBRACK | `depth--` |

**When depth == 0:**
| Input | Action | Next State |
|-------|--------|------------|
| IF, WHILE, FOR | Set `ctrl_type`, reset `depth` | ST_COND |
| ELSE | If top of stack is IF: queue `}`, pop; then queue ELSE | ST_PENDING |
| DO | Set `ctrl_type` | ST_PENDING |
| END | User brace closed nested control; handle per ctrl_type | varies |
| SEMI | Statement complete; close body based on stack state | varies |
| other | Pass through | ST_BODY |

**SEMI handling (body complete):**
1. Queue SEMI
2. Check `stk[stk_sp-1]`:
   - If `is_else`: queue `}`, pop, check outer IF for else-check
   - If `ctrl_type == IF`: don't emit `}` yet → ST_ELSE_CHK
   - If `ctrl_type == DO`: queue `}`, pop → ST_DO_WHILE
   - If `ctrl_type == WHILE/FOR`: queue `}`, pop → ST_BODY or ST_NORMAL
3. Pop pending and return

### ST_ELSE_CHK

**Purpose:** After IF body statement, check for ELSE before emitting `}`.

**Transitions:**
| Input | Action | Next State |
|-------|--------|------------|
| ELSE | Queue `}`, pop IF, queue ELSE | ST_PENDING |
| other | Cascade close all pending bodies | ST_NORMAL |

**Cascade closing logic (no ELSE found):**

```
while stk_sp > 0:
    if stk[stk_sp-1] is IF (not is_else):
        queue_end(), pop_body()
        if next level is also IF (not is_else):
            save current token
            state = ST_ELSE_CHK
            return queued }
    else if stk[stk_sp-1] is_else:
        pop_body()  # ELSE body already closed
    else:
        queue_end(), pop_body()  # WHILE/FOR
```

**Key behavior:** Cascading handles nested IFs like:
```c
if (a) if (b) x;   // Inner if body ends, outer if needs else-check
```

### ST_DO_WHILE

**Purpose:** After DO body closes, wait for `while (cond);`.

**Transitions:**
| Input | Action | Next State |
|-------|--------|------------|
| WHILE | Set `ctrl_type=DO`, reset `depth` | ST_COND |
| other | Syntax error, pass through | ST_NORMAL |

---

## Special Cases

### else if Pattern

When ELSE followed by IF:
- No synthetic braces inserted around `if(...)`
- Treated as: `else` `if (cond) { body }`
- NOT as: `else { if (cond) { body } }`

**Detection:** In ST_PENDING, if `ctrl_type == ELSE && t.type == IF`

### Nested Control Structures

Example: `if (a) while (b) x;`

**Sequence:**
1. ST_NORMAL → see IF → ST_COND
2. ST_COND → balanced RPAR → ST_PENDING
3. ST_PENDING → see WHILE → push(IF), save WHILE, emit `{` → ST_COND
4. ST_COND (ctrl=WHILE) → balanced RPAR → ST_PENDING
5. ST_PENDING → see `x` → push(WHILE), queue `x`, emit `{` → ST_BODY
6. ST_BODY → see SEMI → queue SEMI, queue `}`, pop → ST_BODY
7. ST_BODY (stk: IF) → see EOF or next → queue `}`, pop → ST_NORMAL

**Output:** `if (a) { while (b) { x; } }`

### User-Braced Control Inside Synthetic Body

Example: `if (a) { ... }` where outer control already inserted brace

When BEGIN found in ST_PENDING and `stk_sp > 0`:
- Don't insert synthetic brace
- Track user's brace depth in ST_BODY
- When END seen at depth 0, close synthetic body

### Multiple Nested IFs Without ELSE

Example: `if (a) if (b) if (c) x;`

Each IF pushes to stack. When SEMI seen:
1. Innermost IF (c) → ST_ELSE_CHK
2. No ELSE → queue `}`, pop, check outer (b)
3. Outer (b) is IF → save token, stay ST_ELSE_CHK, return `}`
4. Repeat until stack empty

---

## Output Token Ordering

The filter must maintain correct token order. Output priority:

1. **Pending buffer** - Always drain first
2. **Saved token** - Process before reading upstream
3. **Current token** - After processing, pass through or emit

**Synthetic token emission pattern:**
```
emit_begin(out) → return immediately with { in out
queue_end()     → add } to pending buffer
```

---

## Invariants

1. **All buffers dynamic** - Stack and pending buffer grow as needed
2. **At most one saved token** - `has_saved` is 0 or 1
3. **Synthetic braces balanced** - DEBUG: `synth_balance` must be 0 at EOF
4. **All output braces balanced** - DEBUG: `out_balance` must be 0 at EOF

---

## Known Limitations / Concerns

1. **No error recovery:** Malformed input (missing `;`, unbalanced parens) can leave state machine stuck.

2. **Complex cascade logic:** The ST_ELSE_CHK cascade has multiple paths and conditional saves; difficult to verify correctness.

3. **ELSE handling in ST_BODY:** Special case for ELSE after single-stmt body may not cover all edge cases.

4. **END handling in ST_BODY:** User-braced control ending inside synthetic body has nested conditionals.

5. **ctrl_type variable dual use:** Used both for "current pending control" and "what's on top of stack" - relies on stack entries having own ctrl_type.

6. **Depth variable reuse:** `depth` means different things in different states (paren depth in ST_COND, bracket/paren depth in ST_BODY).

---

## Test Scenarios

### Basic Cases
- `if (a) x;` → `if (a) { x; }`
- `while (a) x;` → `while (a) { x; }`
- `for (;;) x;` → `for (;;) { x; }`
- `do x; while (a);` → `do { x; } while (a);`
- `if (a) x; else y;` → `if (a) { x; } else { y; }`

### Already Braced
- `if (a) { x; }` → unchanged
- `if (a) { x; } else { y; }` → unchanged

### else if
- `if (a) x; else if (b) y;` → `if (a) { x; } else if (b) { y; }`

### Nested Control
- `if (a) if (b) x;` → `if (a) { if (b) { x; } }`
- `if (a) while (b) x;` → `if (a) { while (b) { x; } }`
- `while (a) if (b) x;` → `while (a) { if (b) { x; } }`

### Nested with ELSE
- `if (a) if (b) x; else y;` → `if (a) { if (b) { x; } else { y; } }`
- `if (a) if (b) x; else y; else z;` → complex: which IF gets which ELSE?

### Deeply Nested
- `if (a) if (b) if (c) x;` → all three braced, all three need else-check cascade

### User Braces Inside Synthetic
- `if (a) { if (b) x; }` → only outer brace from user, inner gets synthetic
