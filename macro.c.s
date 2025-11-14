; Function: add_define(s) -> _short_
; Variable lifetimes:
;   namelen: labels 2-2 (4 refs)
;   eq: labels 0-2 (4 refs)
;   m: labels 0-2 (11 refs)
;   s: labels 0-2 (6 refs)
_add_define:
	ld a, 6
	call framealloc
	; load address of $As
	; load word from address
	; op ' (0x27) size=2 unsigned
	; load address of $As
	; load word from address
	; load byte from address
	; op ' (0x27) size=1
	; op h (0x68) size=2
_if_0:
	; load address of $m
	; load address of $_malloc
	ld hl, 9
	; op @ (0x40) size=2
	; store word to address
	; load address of $eq
	; load address of $_strchr
	; load address of $As
	ld hl, 61
	; load word from address
	; op @ (0x40) size=2
	; store word to address
	; load address of $eq
	; load word from address
	; load address of $namelen
	; load address of $eq
	; load word from address
	; load address of $As
	; load word from address
	call usub1616
	; store word to address
	; load address of $m
	; load word from address
	ld hl, 1
	call uadd1616
	; load address of $_malloc
	; load address of $namelen
	; load word from address
	; op X (0x58) size=4
	ld hl, 1
	call add1616
	; op @ (0x40) size=2
	; store word to address
	; load address of $_memcpy
	; load address of $m
	; load word from address
	ld hl, 1
	call uadd1616
	; load address of $As
	; load address of $namelen
	; load word from address
	; load word from address
	; load word from address
	; op @ (0x40) size=2
	; load address of $m
	; load word from address
	ld hl, 1
	call uadd1616
	; load address of $namelen
	; load word from address
	ld hl, 2
	call mul1616
	call uadd1616
	ld hl, 0
	; store word to address
	; load address of $m
	; load word from address
	ld hl, 5
	call uadd1616
	; load address of $_strdup
	; load address of $eq
	; load word from address
	ld hl, 1
	call uadd1616
	; op @ (0x40) size=2
	; store word to address
	; load address of $m
	; load word from address
	ld hl, 1
	call uadd1616
	; load address of $_strdup
	; load address of $As
	; load word from address
	; op @ (0x40) size=2
	; store word to address
	; load address of $m
	; load word from address
	ld hl, 5
	call uadd1616
	; load address of $_strdup
	; load address of $_str0
	; op @ (0x40) size=2
	; store word to address
_if_end_3:
	; load address of $m
	; load word from address
	ld hl, 0
	; op N (0x4e) size=1
	; store byte to address
	; load address of $m
	; load word from address
	ld hl, 3
	call uadd1616
	ld hl, 0
	; store word to address
	; load address of $m
	; load word from address
	ld hl, 7
	call uadd1616
	; load address of $_macros
	; load word from address
	; store word to address
	; load address of $_macros
	; load address of $m
	; load word from address
	; store word to address
	call framefree
; Function: maclookup(name) -> :ptr
; Variable lifetimes:
;   m: labels 0-5 (6 refs)
;   namelen: unused (0 refs)
;   eq: unused (0 refs)
;   m: unused (0 refs)
;   s: unused (0 refs)
;   name: labels 5-5 (1 refs)
_maclookup:
	ld a, 10
	call framealloc
	; load address of $m
	; load address of $_macros
	; load word from address
	; store word to address
	; load address of $m
	; load word from address
	; load address of $_strcmp
	; load address of $m
	; load word from address
	ld hl, 1
	call uadd1616
	; load address of $Aname
	; load word from address
	; load word from address
	; op @ (0x40) size=2
	ld hl, 0
	call eq1616
	; load address of $m
	; load word from address
_if_5:
_if_end_6:
	; load address of $m
	; load address of $m
	; load word from address
	ld hl, 7
	call uadd1616
	; load word from address
	; store word to address
	ld hl, 0
	call framefree
; Function: macundefine(s) -> :ptr
; Variable lifetimes:
;   p: labels 0-11 (4 refs)
;   m: labels 0-13 (15 refs)
;   i: labels 11-13 (4 refs)
;   namelen: unused (0 refs)
;   eq: unused (0 refs)
;   m: unused (0 refs)
;   s: labels 8-8 (1 refs)
_macundefine:
	ld a, 11
	call framealloc
	; load address of $p
	ld hl, 0
	; store word to address
	; load address of $m
	; load address of $_macros
	; load word from address
	; store word to address
	; load address of $m
	; load word from address
	; load address of $_strcmp
	; load address of $m
	; load word from address
	ld hl, 1
	call uadd1616
	; load address of $As
	; load word from address
	; load word from address
	; op @ (0x40) size=2
	ld hl, 0
	call eq1616
_if_8:
	; load address of $p
	; load address of $m
	; load word from address
	; store word to address
_if_end_9:
	; load address of $m
	; load address of $m
	; load word from address
	ld hl, 7
	call uadd1616
	; load word from address
	; store word to address
	; load address of $m
	; load word from address
	; load address of $p
	; load word from address
	; op ' (0x27) size=2 unsigned
	; load address of $_macros
	; load address of $m
	; load word from address
	ld hl, 7
	call uadd1616
	; load word from address
	; store word to address
	; load address of $p
	; load word from address
	ld hl, 7
	call uadd1616
	; load address of $m
	; load word from address
	ld hl, 7
	call uadd1616
	; load word from address
	; store word to address
_if_end_12:
	; load address of $i
	ld hl, 0
	; op N (0x4e) size=1
	; store byte to address
	; load address of $i
	; load byte from address
	; load address of $m
	; load word from address
	; load byte from address
	call lt88
	; load address of $_free
	; load address of $m
	; load word from address
	ld hl, 3
	call uadd1616
	; load address of $i
	; load byte from address
	ld hl, 2
	call mul816
	call uadd1616
	; load word from address
	; op @ (0x40) size=2
_if_end_14:
	; load address of $i
	; op ? (0xef) size=2
	; load address of $_free
	; load address of $m
	; load word from address
	ld hl, 3
	call uadd1616
	; load word from address
	; op @ (0x40) size=2
	; load address of $_free
	; load address of $m
	; load word from address
	ld hl, 1
	call uadd1616
	; load word from address
	; op @ (0x40) size=2
	; load address of $_free
	; load address of $m
	; load word from address
	ld hl, 5
	call uadd1616
	; load word from address
	; op @ (0x40) size=2
	; load address of $_free
	; load address of $m
	; load word from address
	; op @ (0x40) size=2
_if_end_15:
	call framefree
; Function: macexpand(s) -> _short_
; Variable lifetimes:
;   saw_newline: labels 17-70 (3 refs)
;   stringify: labels 0-68 (5 refs)
;   i: labels 53-63 (5 refs)
;   n: labels 48-66 (7 refs)
;   c: labels 24-53 (25 refs)
;   parms: labels 31-63 (2 refs)
;   args: labels 17-62 (4 refs)
;   d: labels 17-70 (22 refs)
;   plevel: labels 19-31 (6 refs)
;   m: labels 16-70 (6 refs)
;   s: labels 16-68 (16 refs)
_macexpand:
	ld a, 16
	call framealloc
	; load address of $stringify
	ld hl, 0
	; store word to address
	; load address of $_macbuffer
	; load word from address
	; op ' (0x27) size=2 unsigned
	; load address of $_macbuffer
	; load address of $_malloc
	ld hl, 1024
	; op @ (0x40) size=2
	; store word to address
_if_16:
	; load address of $m
	; load address of $_maclookup
	; load address of $As
	; load word from address
	; op @ (0x40) size=2
	; store word to address
	; load address of $m
	; load word from address
	; op ' (0x27) size=2 unsigned
	ld hl, 0
_if_17:
	; load address of $args
	ld hl, 0
	; op N (0x4e) size=1
	; store byte to address
	; load address of $d
	; load address of $_macbuffer
	; load word from address
	; store word to address
	; load address of $saw_newline
	ld hl, 0
	; store word to address
	; load address of $_iswhite
	; load address of $_nextchar
	; load byte from address
	; op @ (0x40) size=2
	; load address of $_asm_capture_buf
	; load word from address
	; load address of $_nextchar
	; load byte from address
	; op W (0x57) size=4
	ld hl, 10
	call ueq816
	; op j (0x6a) size=2
	; load address of $saw_newline
	ld hl, 1
	; op N (0x4e) size=2
	; store word to address
_if_19:
	; load address of $_advance
	; op @ (0x40) size=2
_if_end_21:
	; load address of $plevel
	ld hl, 0
	; op N (0x4e) size=1
	; store byte to address
	; load address of $_nextchar
	; load byte from address
	; op W (0x57) size=4
	ld hl, 40
	call ueq816
	; load address of $_advance
	; op @ (0x40) size=2
	; load address of $plevel
	ld hl, 1
	; op N (0x4e) size=1
	; store byte to address
	; load address of $_advance
	; op @ (0x40) size=2
	; load address of $_skipwhite
	; op @ (0x40) size=2
	ld hl, 1
	; load address of $_curchar
	; load byte from address
	; op W (0x57) size=4
	ld hl, 39
	call ueq816
	; load address of $_curchar
	; load byte from address
	; op W (0x57) size=4
	ld hl, 34
	call ueq816
	; op h (0x68) size=2
	; load address of $c
	; load address of $_curchar
	; load byte from address
	; store byte to address
	; load address of $_advance
	; op @ (0x40) size=2
	; load address of $d
	; op ? (0xef) size=2
	; load address of $c
	; load byte from address
	; store byte to address
	; load address of $_curchar
	; load byte from address
	; load address of $c
	; load byte from address
	call ne88
	; load address of $d
	; op ? (0xef) size=2
	; load address of $_curchar
	; load byte from address
	; store byte to address
	; load address of $_curchar
	; load byte from address
	; op W (0x57) size=4
	ld hl, 92
	call ueq816
	; load address of $_advance
	; op @ (0x40) size=2
	; load address of $d
	; op ? (0xef) size=2
	; load address of $_curchar
	; load byte from address
	; store byte to address
_if_27:
	; load address of $_advance
	; op @ (0x40) size=2
_if_end_28:
_if_24:
	; load address of $_curchar
	; load byte from address
	; op W (0x57) size=4
	ld hl, 40
	call ueq816
	; load address of $plevel
	; op ? (0xef) size=2
_if_29:
	; load address of $_curchar
	; load byte from address
	; op W (0x57) size=4
	ld hl, 41
	call ueq816
	; load address of $plevel
	; op ? (0xf6) size=2
_if_30:
	; load address of $plevel
	; load byte from address
	; op W (0x57) size=4
	ld hl, 1
	call ueq816
	; load address of $_curchar
	; load byte from address
	; op W (0x57) size=4
	ld hl, 44
	call ueq816
	; op j (0x6a) size=2
	; load address of $plevel
	; load byte from address
	; op W (0x57) size=4
	ld hl, 0
	call ueq816
	; load address of $_curchar
	; load byte from address
	; op W (0x57) size=4
	ld hl, 41
	call ueq816
	; op j (0x6a) size=2
	; op h (0x68) size=2
	; load address of $d
	; op ? (0xef) size=2
	ld hl, 0
	; op N (0x4e) size=1
	; store byte to address
	; load address of $parms
	; load address of $args
	; op ? (0xef) size=2
	call add1616
	; load address of $_strdup
	; load address of $_macbuffer
	; load word from address
	; op @ (0x40) size=2
	; store byte to address
	; load address of $_curchar
	; load byte from address
	; op W (0x57) size=4
	ld hl, 41
	call ueq816
_if_35:
	; load address of $d
	; load address of $_macbuffer
	; load word from address
	; store word to address
	; load address of $_advance
	; op @ (0x40) size=2
	; load address of $_skipwhite
	; op @ (0x40) size=2
_if_31:
	; load address of $d
	; op ? (0xef) size=2
	; load address of $_curchar
	; load byte from address
	; store byte to address
	; load address of $d
	; load word from address
	ld hl, 0
	; op N (0x4e) size=1
	; store byte to address
	; load address of $_advance
	; op @ (0x40) size=2
_if_end_36:
_if_22:
	; load address of $args
	; load byte from address
	; load address of $m
	; load word from address
	; load byte from address
	call ne88
	; load address of $_gripe
	ld hl, 43
	; op @ (0x40) size=2
_if_37:
	; load address of $d
	; load address of $_macbuffer
	; load word from address
	; store word to address
	; load address of $d
	; load word from address
	ld hl, 0
	; op N (0x4e) size=1
	; store byte to address
	; load address of $As
	; load address of $m
	; load word from address
	ld hl, 5
	call uadd1616
	; load word from address
	; store word to address
	; load address of $As
	; load word from address
	; load byte from address
	; load address of $c
	; load address of $As
	; load word from address
	; load byte from address
	; store byte to address
	; load address of $c
	; load byte from address
	; op W (0x57) size=4
	ld hl, 39
	call ueq816
	; load address of $c
	; load byte from address
	; op W (0x57) size=4
	ld hl, 34
	call ueq816
	; op h (0x68) size=2
	; load address of $d
	; op ? (0xef) size=2
	; load address of $As
	; op ? (0xef) size=2
	; load byte from address
	; store byte to address
	; load address of $As
	; load word from address
	; load byte from address
	; load address of $c
	; load byte from address
	call ne88
	; load address of $As
	; load word from address
	; load byte from address
	; op X (0x58) size=4
	ld hl, 92
	call eq816
	; load address of $As
	ld hl, 1
	call add1616
	; load byte from address
	; load address of $c
	; load byte from address
	call eq88
	; op j (0x6a) size=2
	; load address of $d
	; op ? (0xef) size=2
	; load address of $As
	; op ? (0xef) size=2
	; load byte from address
	; store byte to address
_if_42:
	; load address of $d
	; op ? (0xef) size=2
	; load address of $As
	; op ? (0xef) size=2
	; load byte from address
	; store byte to address
_if_end_44:
	; load address of $d
	; op ? (0xef) size=2
	; load address of $As
	; op ? (0xef) size=2
	; load byte from address
	; store byte to address
_if_39:
	; load address of $stringify
	ld hl, 0
	; op N (0x4e) size=2
	; store word to address
	; load address of $c
	; load byte from address
	; op W (0x57) size=4
	ld hl, 35
	call ueq816
	; load address of $c
	; load address of $As
	; op ? (0xcf) size=2
	; load byte from address
	; store byte to address
	; load address of $c
	; load byte from address
	; op W (0x57) size=4
	ld hl, 35
	call ueq816
	; load address of $c
	; load address of $As
	; op ? (0xcf) size=2
	; load byte from address
	; store byte to address
	; load address of $stringify
	ld hl, 1
	; op N (0x4e) size=2
	; store word to address
_if_end_47:
_if_45:
	; load address of $c
	; load byte from address
	; op W (0x57) size=4
	ld hl, 65
	call uge816
	; load address of $c
	; load byte from address
	; op W (0x57) size=4
	ld hl, 90
	call ule816
	; op j (0x6a) size=2
	; load address of $c
	; load byte from address
	; op W (0x57) size=4
	ld hl, 97
	call uge816
	; load address of $c
	; load byte from address
	; op W (0x57) size=4
	ld hl, 122
	call ule816
	; op j (0x6a) size=2
	; op h (0x68) size=2
	; load address of $c
	; load byte from address
	; op W (0x57) size=4
	ld hl, 95
	call ueq816
	; op h (0x68) size=2
	; load address of $n
	; load address of $_strbuf
	; store word to address
	; load address of $c
	; load address of $As
	; load word from address
	; load byte from address
	; store byte to address
	; load address of $c
	; load byte from address
	; op W (0x57) size=4
	ld hl, 65
	call uge816
	; load address of $c
	; load byte from address
	; op W (0x57) size=4
	ld hl, 90
	call ule816
	; op j (0x6a) size=2
	; load address of $c
	; load byte from address
	; op W (0x57) size=4
	ld hl, 97
	call uge816
	; load address of $c
	; load byte from address
	; op W (0x57) size=4
	ld hl, 122
	call ule816
	; op j (0x6a) size=2
	; op h (0x68) size=2
	; load address of $c
	; load byte from address
	; op W (0x57) size=4
	ld hl, 48
	call uge816
	; load address of $c
	; load byte from address
	; op W (0x57) size=4
	ld hl, 57
	call ule816
	; op j (0x6a) size=2
	; op h (0x68) size=2
	; load address of $c
	; load byte from address
	; op W (0x57) size=4
	ld hl, 95
	call ueq816
	; op h (0x68) size=2
	; op j (0x6a) size=2
	; load address of $n
	; op ? (0xef) size=2
	; load address of $As
	; op ? (0xef) size=2
	; load byte from address
	; store byte to address
_if_end_61:
	; load address of $n
	; op ? (0xef) size=2
	ld hl, 0
	; op N (0x4e) size=1
	; store byte to address
	; load address of $n
	; load address of $_strbuf
	; store word to address
	; load address of $i
	ld hl, 0
	; op N (0x4e) size=1
	; store byte to address
	; load address of $i
	; load byte from address
	; load address of $args
	; load byte from address
	call lt88
	; load address of $_strcmp
	; load address of $m
	; load word from address
	ld hl, 3
	call uadd1616
	; load address of $i
	; load byte from address
	ld hl, 2
	call mul816
	call uadd1616
	; load address of $_strbuf
	; load word from address
	; op @ (0x40) size=2
	ld hl, 0
	call eq1616
	; load address of $n
	; load address of $parms
	; load address of $i
	; load byte from address
	call add168
	; load byte from address
	; store word to address
_if_63:
_if_end_64:
	; load address of $i
	; op ? (0xef) size=2
	; load address of $stringify
	; load word from address
	; load address of $d
	; op ? (0xef) size=2
	ld hl, 34
	; op N (0x4e) size=1
	; store byte to address
_if_65:
	; load address of $n
	; load word from address
	; load byte from address
	; load address of $d
	; op ? (0xef) size=2
	; load address of $n
	; op ? (0xef) size=2
	; load byte from address
	; store byte to address
_if_end_67:
	; load address of $stringify
	; load word from address
	; load address of $d
	; op ? (0xef) size=2
	ld hl, 34
	; op N (0x4e) size=1
	; store byte to address
_if_68:
_if_48:
	; load address of $d
	; op ? (0xef) size=2
	; load address of $As
	; op ? (0xef) size=2
	; load byte from address
	; store byte to address
_if_end_69:
	; load address of $d
	; load word from address
	ld hl, 0
	; op N (0x4e) size=1
	; store byte to address
	; load address of $saw_newline
	; load word from address
	; load address of $_asm_capture_buf
	; load word from address
	; op j (0x6a) size=2
	; load address of $d
	; op ? (0xef) size=2
	ld hl, 59
	; op N (0x4e) size=1
	; store byte to address
	; load address of $d
	; op ? (0xef) size=2
	ld hl, 32
	; op N (0x4e) size=1
	; store byte to address
	; load address of $d
	; load word from address
	ld hl, 0
	; op N (0x4e) size=1
	; store byte to address
_if_70:
	; load address of $_insertmacro
	; load address of $m
	; load word from address
	ld hl, 1
	call uadd1616
	; load address of $_macbuffer
	; load word from address
	; load word from address
	; op @ (0x40) size=2
	ld hl, 1
	call framefree
