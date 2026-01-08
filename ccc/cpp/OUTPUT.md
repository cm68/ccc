# Unified I/O Buffer Architecture (Design)

**Note:** This document describes a proposed architecture for future implementation.
The current cpp uses simpler buffer management.

The input and output buffer stacks share the same underlying mechanism. Reading from a spilled output buffer during replay is structurally identical to reading an include file.

## Buffer Size

```c
#define TBSIZE 512      /* Matches Micronix disk block size */
```

Using the native disk block size (512 bytes) ensures efficient I/O without partial block reads/writes.

## Unified Data Structure

Extend the existing `struct textbuf` to handle both directions:

```c
struct textbuf {
    char fd;                /* file descriptor, -1 for memory-only */
    char *name;             /* filename or macro/buffer name */
    char *storage;          /* buffer memory */
    short offset;           /* current position (read or write) */
    short valid;            /* valid bytes (read) or capacity marker */
    long file_size;         /* bytes in file (for output spill) */
    char saved_column;      /* parent's column (for input) */
    char direction;         /* 'r' = reading, 'w' = writing */
    struct textbuf *prev;   /* stack link */
};

static struct textbuf *tbtop;   /* Input stack top (existing) */
static struct textbuf *obtop;   /* Output stack top */
```

## Shared Operations

### Buffer Allocation
Same for both input and output:

```c
struct textbuf *tbAlloc(char dir) {
    struct textbuf *t = malloc(sizeof(*t));
    t->fd = -1;
    t->storage = malloc(TBSIZE);
    t->offset = 0;
    t->valid = 0;
    t->file_size = 0;
    t->direction = dir;
    t->prev = NULL;
    return t;
}
```

### File Fill (Input) / Spill (Output)

The file I/O is nearly identical:

```c
/* Fill buffer from file (input direction) */
int tbFill(struct textbuf *t) {
    if (t->fd < 0) return 0;
    t->valid = read(t->fd, t->storage, TBSIZE);
    t->offset = 0;
    return t->valid;
}

/* Spill buffer to file (output direction) */
void tbSpill(struct textbuf *t) {
    if (t->fd < 0) {
        char tmp[] = "/tmp/cppXXXXXX";
        t->fd = mkstemp(tmp);
        unlink(tmp);
    }
    write(t->fd, t->storage, t->offset);
    t->file_size += t->offset;
    t->offset = 0;
}
```

### Replay = Reading from Output Buffer

When replaying an output buffer, we're doing exactly what we do for include files:

```c
void tbReplay(struct textbuf *t, struct textbuf **stack) {
    /* Pop from output stack */
    *stack = t->prev;

    /* Replay file portion (if any) by reading it back */
    if (t->fd >= 0) {
        lseek(t->fd, 0, SEEK_SET);
        while (tbFill(t) > 0) {
            /* Write to parent output buffer or direct */
            outbufWrite(t->storage, t->valid);
        }
        close(t->fd);
    }
    /* Replay memory portion */
    if (t->offset > 0) {
        outbufWrite(t->storage, t->offset);
    }

    free(t->storage);
    free(t);
}
```

## Parallel Usage

| Operation        | Input Stack (tbtop)    | Output Stack (obtop)    |
|------------------|------------------------|-------------------------|
| Push file        | `insertfile()`         | n/a                     |
| Push macro       | `insertmacro()`        | n/a                     |
| Push buffer      | n/a                    | `outbufPush()`          |
| Read char        | `advance()` + `tbFill` | n/a                     |
| Write data       | n/a                    | `outbufWrite()` + spill |
| Pop              | auto in `advance()`    | `outbufPop()`           |
| Replay           | n/a                    | `tbReplay()` uses fill  |

## Key Insight

**Replay uses the same `tbFill()` as include file reading.**

When we spill an output buffer to a temp file and later replay it:
1. `lseek(fd, 0, SEEK_SET)` - rewind to start
2. `tbFill()` - read 512-byte block into buffer (same as include file)
3. Write block to parent/output
4. Repeat until exhausted
5. Close and free

This is structurally identical to processing an include file, just with output destination instead of lexer input.

## API

```c
/* Shared helpers */
struct textbuf *tbAlloc(char dir);
void tbFree(struct textbuf *t);
int tbFill(struct textbuf *t);      /* read from fd into storage */
void tbSpill(struct textbuf *t);    /* write storage to fd */

/* Input-specific (existing) */
void insertfile(char *name, int sys);
void insertmacro(char *name, char *text);
void advance(void);

/* Output-specific (new) */
void outbufPush(void);
void outbufWrite(char *data, int len);
void outbufReplay(void);
void outbufPop(void);
```

## Use Cases

All diversions use the output stack:

1. **Loop lowering** - buffer body, emit prefix/suffix around replay
2. **Declaration init splitting** - buffer init exprs, emit after decls
3. **Block declarations** - hoist decls to block start
4. **Any reordering** - general mechanism for out-of-order emission

## Memory Model

- **TBSIZE**: 512 bytes (Micronix disk block size)
- **File spill**: Both input includes and output diversions can exceed memory
- **Stack depth**: Malloc-limited, typically 8+ levels sufficient
- **Per-level cost**: ~520 bytes (struct + buffer)

The unified approach ensures consistent behavior and shares tested code between the input and output paths.
