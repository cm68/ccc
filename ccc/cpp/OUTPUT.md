# Output Buffer Stack (io.c)

The output buffer stack enables out-of-order token emission for transformations like loop lowering. It shares the `struct textbuf` infrastructure with the input stack.

## Buffer Size

```c
#define TBSIZE 512      /* Matches Micronix disk block size */
```

Using the native disk block size ensures efficient I/O without partial block reads/writes.

## Data Structure

The output stack reuses `struct textbuf` from the input system:

```c
struct textbuf {
    int fd;                   /* file descriptor, -1 for memory-only */
    char *name;               /* filename or buffer name */
    char *storage;            /* buffer memory */
    short offset;             /* current write position */
    short valid;              /* bytes written to file (spill tracking) */
    short lineno;             /* not used for output */
    short saved_column;       /* not used for output */
    struct textbuf *prev;     /* stack link */
};

static struct textbuf *obtop = NULL;   /* Output stack top */
```

## API

```c
void outbufPush(void);              /* Start buffering to new level */
void outbufPop(void);               /* Replay buffer and free */
void outbufWrite(char *s, int len); /* Write to current buffer */
```

## Operation

### Push

`outbufPush()` allocates a new buffer and pushes it onto the output stack:

```c
void outbufPush(void) {
    struct textbuf *t = malloc(sizeof(*t));
    t->fd = -1;
    t->storage = malloc(TBSIZE);
    t->offset = 0;
    t->valid = 0;
    t->prev = obtop;
    obtop = t;
}
```

### Write

`outbufWrite()` appends data to the current buffer, spilling to a temp file if needed:

```c
void outbufWrite(char *s, int len) {
    if (!obtop) {
        /* No buffer - write directly to output */
        write(lexFd, s, len);
        return;
    }
    while (len > 0) {
        int space = TBSIZE - obtop->offset;
        int chunk = (len < space) ? len : space;
        memcpy(obtop->storage + obtop->offset, s, chunk);
        obtop->offset += chunk;
        s += chunk;
        len -= chunk;
        if (obtop->offset >= TBSIZE) {
            /* Spill to temp file */
            if (obtop->fd < 0) {
                char tmp[] = "/tmp/cppXXXXXX";
                obtop->fd = mkstemp(tmp);
                unlink(tmp);
            }
            write(obtop->fd, obtop->storage, obtop->offset);
            obtop->valid += obtop->offset;
            obtop->offset = 0;
        }
    }
}
```

### Pop (Replay)

`outbufPop()` replays the buffered content to the parent level:

```c
void outbufPop(void) {
    struct textbuf *t = obtop;
    obtop = t->prev;

    /* Replay file portion (if spilled) */
    if (t->fd >= 0) {
        char buf[TBSIZE];
        int n;
        lseek(t->fd, 0, SEEK_SET);
        while ((n = read(t->fd, buf, TBSIZE)) > 0)
            outbufWrite(buf, n);
        close(t->fd);
    }

    /* Replay memory portion */
    if (t->offset > 0)
        outbufWrite(t->storage, t->offset);

    free(t->storage);
    free(t);
}
```

## Use Cases

### Loop Lowering

The primary use case is loop lowering in knr.c:

```c
/* WHILE loop transformation */
while (cond) { body }

/* Becomes: */
{
    __W1T:
    if (!(cond)) goto __W1B;
    { body }           /* <- body buffered, replayed here */
    goto __W1T;
    __W1B: ;
}
```

The loop body must be buffered because we need to emit the opening brace, label, and condition test before the body, but we don't see those until after we've started processing the body tokens.

### Nesting

Output buffers nest correctly for nested loops:

```c
while (a) {
    while (b) { inner }
}
```

Each nested loop pushes its own buffer. When the inner loop completes, its buffer is replayed into the outer loop's buffer.

## Memory Model

- **Per-level cost**: ~520 bytes (struct + TBSIZE buffer)
- **Spill threshold**: 512 bytes triggers temp file creation
- **Stack depth**: Limited only by malloc (typically 8+ levels sufficient)
- **Temp files**: Auto-deleted via unlink() after mkstemp()

## Integration

The emit functions in emit.c check for an active output buffer:

```c
void emitToken(token_t t) {
    char buf[2];
    buf[0] = t;
    buf[1] = 0;
    if (obtop)
        outbufWrite(buf, 1);
    else
        write(lexFd, buf, 1);
}
```

This allows transparent buffering - the token filter can push/pop buffers without the emit layer needing special handling.
