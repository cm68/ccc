# Micronix System Call Library (libu)

This library provides system call wrappers for Micronix, a Unix-like
operating system for Z80.

## System Call Interface

System calls use the `rst 08h` instruction with a system call number
in the following byte. Arguments are passed either in registers or
via an indirect argument block.

### Direct System Calls

Simple system calls take arguments in registers:
and are inline with our code flow.

```
    rst 08h
    .db NN          ; system call number
```

Where `NN` is the system call number. Arguments in HL (and DE for
some calls) are passed directly to the kernel.

### Indirect System Calls

System calls with multiple arguments use an indirect block:
this theoretically allows having shared read only text, since we 
don't have to write the system call block with our arguments.

```
    rst 08h
    .db 00h         ; indirect system call
    .dw argblock    ; pointer to argument block
```

The argument block has format:

```
argblock:
    .db 0cfh        ; magic byte (identifies indirect call)
    .db NN          ; actual system call number
    .dw arg1        ; first argument
    .dw arg2        ; second argument (if needed)
    ...
```

### Return Values

- On success: carry flag clear, result in HL
- On error: carry flag set, error code in HL (saved to `_errno`)

## System Call Numbers

| Number | Name    | Arguments                              |
|--------|---------|----------------------------------------|
| 0x01   | exit    | HL = exit status                       |
| 0x02   | fork    | (none)                                 |
| 0x03   | read    | indirect: fd, buf, count               |
| 0x04   | write   | indirect: fd, buf, count               |
| 0x05   | open    | indirect: path, flags                  |
| 0x06   | close   | HL = fd                                |
| 0x07   | wait    | returns status in DE                   |
| 0x08   | creat   | indirect: path, mode                   |
| 0x09   | link    | indirect: oldpath, newpath             |
| 0x0a   | unlink  | indirect: path                         |
| 0x0b   | exec    | indirect: path, argv                   |
| 0x0c   | chdir   | indirect: path                         |
| 0x0d   | time    | returns time in HL:DE                  |
| 0x0e   | mknod   | indirect: path, mode, dev              |
| 0x0f   | chmod   | indirect: path, mode                   |
| 0x10   | chown   | indirect: path, owner                  |
| 0x11   | break   | indirect: addr                         |
| 0x12   | stat    | indirect: path, buf                    |
| 0x13   | seek    | indirect: fd, offset, whence           |
| 0x14   | getpid  | (none)                                 |
| 0x15   | mount   | indirect: dev, dir, flags              |
| 0x16   | umount  | indirect: target                       |
| 0x17   | setuid  | HL = uid                               |
| 0x18   | getuid  | (none)                                 |
| 0x19   | stime   | HL:DE = time                           |
| 0x1b   | alarm   | indirect: seconds                      |
| 0x1c   | fstat   | indirect: fd, buf                      |
| 0x1d   | pause   | indirect: (none)                       |
| 0x1f   | stty    | indirect: fd, buf                      |
| 0x20   | gtty    | indirect: fd, buf                      |
| 0x21   | access  | indirect: path, mode                   |
| 0x22   | nice    | HL = increment                         |
| 0x23   | sleep   | HL = seconds                           |
| 0x24   | sync    | (none)                                 |
| 0x25   | kill    | indirect: pid, sig                     |
| 0x29   | dup     | HL = fd                                |
| 0x2a   | pipe    | returns fds in HL, DE                  |
| 0x30   | signal  | indirect: signum, handler              |

## Calling Convention

The ccc compiler uses the following calling convention:

- Arguments pushed right-to-left onto stack
- Return value in HL (16-bit) or HL:DE (32-bit)
- Caller cleans up stack after call

This differs from the original Whitesmith's C compiler which returned
values in BC.

## Files

- `*.s` - Assembly source for system call wrappers
- `errno.s` - Global errno variable
- Files using `c.ent`/`c.ret` are compiled C code requiring the
  Whitesmith's runtime library

## Building

Use the Makefile to assemble all files into `libu.a`:

```
make -C libsrc/libu
```
