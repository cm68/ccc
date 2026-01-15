# Runtime Helper Functions

Helper functions called by pass2 code generator, implemented in `libsrc/libc`.

## Calling Conventions

**32-bit values (long/float):** HLDE register pair
- DE = low word (bits 0-15)
- HL = high word (bits 16-31)

**16-bit values:** HL or DE

**8-bit values:** A register

**Binary operations:** First arg in registers, second arg pushed on stack (low word first, then high word for 32-bit).

**Shift operations:** Value in registers, count in B.

**Pointer operations:** Address in HL.

---

## Integer (16-bit) Helpers

### Arithmetic
| Helper | Description | Input | Output |
|--------|-------------|-------|--------|
| `__mul16` | HL * DE | HL, DE | HL |
| `__div16` | HL / DE (signed) | HL, DE | HL |
| `__mod16` | HL % DE (signed) | HL, DE | HL |

### Shifts
| Helper | Description | Input | Output |
|--------|-------------|-------|--------|
| `shal` | HL << B (left shift) | HL, B | HL |
| `shar` | HL >> B (arithmetic right) | HL, B | HL |
| `shlr` | HL >> B (logical right) | HL, B | HL |

### Assignment Shifts
| Helper | Description | Input | Output |
|--------|-------------|-------|--------|
| `asll` | *HL <<= B | HL=ptr, B | HL |
| `asar` | *HL >>= B (arithmetic) | HL=ptr, B | HL |
| `aslr` | *HL >>= B (logical) | HL=ptr, B | HL |

---

## Long (32-bit) Helpers

### Arithmetic
| Helper | Description | Input | Output |
|--------|-------------|-------|--------|
| `ladd` | HLDE + stack | HLDE, stack | HLDE |
| `alsub` | HLDE - stack (signed) | HLDE, stack | HLDE |
| `llsub` | HLDE - stack (unsigned) | HLDE, stack | HLDE |
| `almul` | HLDE * stack (signed) | HLDE, stack | HLDE |
| `llmul` | HLDE * stack (unsigned) | HLDE, stack | HLDE |
| `aldiv` | HLDE / stack (signed) | HLDE, stack | HLDE |
| `lldiv` | HLDE / stack (unsigned) | HLDE, stack | HLDE |
| `almod` | HLDE % stack (signed) | HLDE, stack | HLDE |
| `llmod` | HLDE % stack (unsigned) | HLDE, stack | HLDE |

### Bitwise
| Helper | Description | Input | Output |
|--------|-------------|-------|--------|
| `lland` | HLDE & stack | HLDE, stack | HLDE |
| `llor` | HLDE \| stack | HLDE, stack | HLDE |
| `llxor` | HLDE ^ stack | HLDE, stack | HLDE |
| `lcom` | ~HLDE (complement) | HLDE | HLDE |

### Shifts
| Helper | Description | Input | Output |
|--------|-------------|-------|--------|
| `lllsh` | HLDE << B (left) | HLDE, B | HLDE |
| `alrsh` | HLDE >> B (arithmetic right) | HLDE, B | HLDE |
| `llrsh` | HLDE >> B (logical right) | HLDE, B | HLDE |

### Comparison
| Helper | Description | Input | Output |
|--------|-------------|-------|--------|
| `lrelop` | Compare HLDE vs stack | HLDE, stack | flags |

Sets flags as if subtraction performed. Use:
- Z/NZ for == / !=
- C/NC for < / >= (unsigned or after sign check)

### Increment/Decrement
| Helper | Description | Input | Output |
|--------|-------------|-------|--------|
| `lainc` | ++(*HL) signed | HL=ptr | HLDE (old value) |
| `llinc` | ++(*HL) unsigned | HL=ptr | HLDE (old value) |
| `ladec` | --(*HL) signed | HL=ptr | HLDE (old value) |
| `lldec` | --(*HL) unsigned | HL=ptr | HLDE (old value) |

### Load/Store
| Helper | Description | Input | Output |
|--------|-------------|-------|--------|
| `lld` | Load 32-bit from memory | HL=ptr | HLDE |
| `lstde` | Store HLDE to memory | HL=ptr, HLDE | - |

### Assignment Operators
| Helper | Description | Input | Output |
|--------|-------------|-------|--------|
| `asaladd` | *HL += stack (signed) | HL=ptr, stack | HLDE |
| `asalsub` | *HL -= stack (signed) | HL=ptr, stack | HLDE |
| `asalmul` | *HL *= stack (signed) | HL=ptr, stack | HLDE |
| `asaldiv` | *HL /= stack (signed) | HL=ptr, stack | HLDE |
| `asalmod` | *HL %= stack (signed) | HL=ptr, stack | HLDE |
| `asaland` | *HL &= stack | HL=ptr, stack | HLDE |
| `asalor` | *HL \|= stack | HL=ptr, stack | HLDE |
| `asalxor` | *HL ^= stack | HL=ptr, stack | HLDE |
| `asallsh` | *HL <<= B | HL=ptr, B | HLDE |
| `asalrsh` | *HL >>= B (arithmetic) | HL=ptr, B | HLDE |
| `asllrsh` | *HL >>= B (logical) | HL=ptr, B | HLDE |

---

## Float (32-bit IEEE) Helpers

### Arithmetic
| Helper | Description | Input | Output |
|--------|-------------|-------|--------|
| `fladd` | HLDE + stack | HLDE, stack | HLDE |
| `flsub` | HLDE - stack | HLDE, stack | HLDE |
| `flmul` | HLDE * stack | HLDE, stack | HLDE |
| `fldiv` | HLDE / stack | HLDE, stack | HLDE |

### Comparison
| Helper | Description | Input | Output |
|--------|-------------|-------|--------|
| `frelop` | Compare HLDE vs stack | HLDE, stack | flags |

### Increment/Decrement
| Helper | Description | Input | Output |
|--------|-------------|-------|--------|
| `lfinc` | ++(*HL) | HL=ptr | HLDE |
| `lfdec` | --(*HL) | HL=ptr | HLDE |

### Conversion
| Helper | Description | Input | Output |
|--------|-------------|-------|--------|
| `ftol` | Float to long | HLDE | HLDE |
| `altof` | Signed long to float | HLDE | HLDE |
| `lltof` | Unsigned long to float | HLDE | HLDE |
| `aitof` | Signed int to float | HL | HLDE |
| `litof` | Unsigned int to float | HL | HLDE |
| `abtof` | Signed byte to float | A | HLDE |
| `lbtof` | Unsigned byte to float | A | HLDE |

### Utility
| Helper | Description | Input | Output |
|--------|-------------|-------|--------|
| `fpnorm` | Normalize float | HLDE | HLDE |
| `frexp` | Extract mantissa/exponent | HLDE, HL=ptr | HLDE, *HL=exp |
| `ldexp` | Load exponent | HLDE, exp | HLDE |

---

## Notes

1. **Signed vs Unsigned:** For addition, subtraction, and bitwise ops, signed and unsigned versions are identical. Division, modulo, and right shifts differ.

2. **Stack cleanup:** Helpers clean their stack arguments before returning.

3. **Negation:** Long negation is done inline (two's complement of HLDE). Float negation flips sign bit (xor H with 0x80).

4. **Float format:** 32-bit IEEE standard (libraries being updated). Both `float` and `double` are 32-bit on Z80.

5. **Source files:** Located in `../../libsrc/libc/`:
   - `ladd.s`, `lsub.s`, `lmul.s`, `ldiv.s` - long arithmetic
   - `land.s`, `lor.s`, `lxor.s`, `lcom.s` - long bitwise
   - `allsh.s`, `alrsh.s`, `llrsh.s` - long shifts
   - `lrelop.s` - long comparison
   - `linc.s` - long inc/dec
   - `lldst.s` - long load/store
   - `float.s`, `asfloat.s` - float arithmetic
   - `frelop.s` - float comparison
   - `finc.s` - float inc/dec
   - `ftol.s`, `ltof.s` - float conversion
