; Function: gripe(errcode:_uchar_) -> _short_
; Variable lifetimes:
;   idx: labels 12-14 (5 refs)
;   idx: unused (0 refs)
;   idx: unused (0 refs)
;   in_fatal: unused (0 refs)
;   last_errcode: unused (0 refs)
;   last_lineno: unused (0 refs)
;   t: labels 4-8 (10 refs)
;   i: labels 0-14 (8 refs)
;   errcode: labels 0-23 (6 refs)
_gripe:
	ld a, 15
	call framealloc
	; load address of $Serror_gripe_in_fatal_2
	; load word from address
	; op ' (0x27) size=2
	; load address of $_lineno
	; load word from address
	; load address of $Serror_gripe_last_lineno_0
	; load word from address
	call eq1616
	; op j (0x6a) size=2
	; load address of $Aerrcode
	; load byte from address
	; load address of $Serror_gripe_last_errcode_1
	; load byte from address
	call eq88
	; op j (0x6a) size=2
	; load address of $_fdprintf
	; load address of $_lineno
	; load word from address
	; load address of $_str63
	ld hl, 2
	; op @ (0x40) size=2
	; load address of $Serror_gripe_in_fatal_2
	ld hl, 1
	; op N (0x4e) size=2
	; store word to address
	; load address of $_fatal
	; load address of $Aerrcode
	; load byte from address
	; op @ (0x40) size=2
_if_0:
	; load address of $Serror_gripe_last_lineno_0
	; load address of $_lineno
	; load word from address
	; store word to address
	; load address of $Serror_gripe_last_errcode_1
	; load address of $Aerrcode
	; load byte from address
	; store byte to address
	; load address of $i
	; load address of $Aerrcode
	; load byte from address
	; op W (0x57) size=2
	; store word to address
	; load address of $i
	; load word from address
	; op X (0x58) size=4
	ld hl, 62
	call gt1616
	; load address of $i
	ld hl, 62
	; op N (0x4e) size=2
	; store word to address
_if_3:
	; load address of $_fdprintf
	; load address of $_filename
	; load address of $_lineno
	; load address of $_column
	; load address of $Aerrcode
	; load address of $_errmsg
	; load address of $i
	; load word from address
	call add1616
	; load byte from address
	; load byte from address
	; load word from address
	; load word from address
	; load word from address
	; load address of $_str64
	ld hl, 2
	; op @ (0x40) size=2
	; load address of $_tbtop
	; load word from address
	; load address of $t
	; load address of $_tbtop
	; load word from address
	; store word to address
	; load address of $t
	; load word from address
	; load address of $t
	; load word from address
	; load word from address
	; op X (0x58) size=4
	ld hl, -1
	call ne1616
	; load address of $t
	; load word from address
	ld hl, 2
	call uadd1616
	; load word from address
	; op j (0x6a) size=2
	; load address of $t
	; load word from address
	; load address of $_tbtop
	; load word from address
	call une1616
	; load address of $_filename
	; load word from address
	; load address of $_strcmp
	; load address of $_filename
	; load address of $t
	; load word from address
	ld hl, 2
	call uadd1616
	; load word from address
	; load word from address
	; op @ (0x40) size=2
	ld hl, 0
	call ne1616
	; op j (0x6a) size=2
	; op h (0x68) size=2
	; load address of $_fdprintf
	; load address of $t
	; load word from address
	ld hl, 2
	call uadd1616
	; load address of $t
	; load word from address
	ld hl, 10
	call uadd1616
	; load word from address
	; load word from address
	; load address of $_str65
	ld hl, 2
	; op @ (0x40) size=2
_if_8:
_if_6:
_if_end_11:
	; load address of $t
	; load address of $t
	; load word from address
	ld hl, 14
	call uadd1616
	; load word from address
	; store word to address
_if_4:
	; load address of $_fdprintf
	ld hl, 10
	; load address of $_str66
	ld hl, 2
	; op @ (0x40) size=2
	; load address of $i
	ld hl, 0
	; op N (0x4e) size=2
	; store word to address
	; load address of $i
	; load word from address
	; op X (0x58) size=4
	ld hl, 10
	call lt1616
	; load address of $idx
	; load address of $_token_history_index
	; load word from address
	; load address of $i
	; load word from address
	call add1616
	; op X (0x58) size=4
	ld hl, 10
	call mod1616
	; store word to address
	; load address of $_token_history
	; load address of $idx
	; load word from address
	ld hl, 5
	call mul1616
	call add1616
	; load byte from address
	; load address of $_tokenname
	; load address of $_token_history
	; load address of $idx
	; load word from address
	ld hl, 5
	call mul1616
	call add1616
	; load byte from address
	call add168
	; load byte from address
	; load address of $_fdprintf
	; load address of $_tokenname
	; load address of $_token_history
	; load address of $idx
	; load word from address
	ld hl, 5
	call mul1616
	call add1616
	; load byte from address
	call add168
	; load byte from address
	; load address of $_str67
	ld hl, 2
	; op @ (0x40) size=2
	; load address of $_fdprintf
	; load address of $_token_history
	; load address of $idx
	; load word from address
	ld hl, 5
	call mul1616
	call add1616
	; load byte from address
	; load address of $_str68
	ld hl, 2
	; op @ (0x40) size=2
_if_end_15:
_if_13:
_if_end_16:
	; load address of $i
	; op ? (0xef) size=2
	; load address of $_fdprintf
	; load address of $_str69
	ld hl, 2
	; op @ (0x40) size=2
	; load address of $_fdprintf
	; load address of $_str70
	ld hl, 2
	; op @ (0x40) size=2
	; load address of $_next
	; load byte from address
	; load address of $_tokenname
	; load address of $_next
	; load byte from address
	call add168
	; load byte from address
	; load address of $_fdprintf
	; load address of $_tokenname
	; load address of $_next
	; load byte from address
	call add168
	; load byte from address
	; load address of $_str71
	ld hl, 2
	; op @ (0x40) size=2
	; load address of $_fdprintf
	; load address of $_next
	; load byte from address
	; load address of $_str72
	ld hl, 2
	; op @ (0x40) size=2
_if_end_19:
	; load address of $_next
	; load byte from address
	; op W (0x57) size=4
	ld hl, 53
	call ueq816
	; load address of $_next
	ld hl, 1
	call add1616
	; load word from address
	; op j (0x6a) size=2
	; load address of $_fdprintf
	; load address of $_next
	ld hl, 1
	call add1616
	; load word from address
	; load address of $_str73
	ld hl, 2
	; op @ (0x40) size=2
	; load address of $_next
	; load byte from address
	; op W (0x57) size=4
	ld hl, 57
	call ueq816
	; load address of $_fdprintf
	; load address of $_next
	ld hl, 1
	call add1616
	; load long from address
	; load address of $_str74
	ld hl, 2
	; op @ (0x40) size=2
_if_23:
_if_end_22:
	; load address of $_fdprintf
	; load address of $_str75
	ld hl, 2
	; op @ (0x40) size=2
_if_end_24:
	; load address of $_fdprintf
	; load address of $_str76
	ld hl, 2
	; op @ (0x40) size=2
	; load address of $_error
	; load address of $Aerrcode
	; load byte from address
	; op W (0x57) size=2
	; store word to address
	call framefree
; Function: dump_symbols() -> _short_
; Variable lifetimes:
;   max_types: labels 32-36 (3 refs)
;   type_count: labels 32-33 (3 refs)
;   idx: unused (0 refs)
;   max_types: unused (0 refs)
;   type_count: unused (0 refs)
;   idx: unused (0 refs)
;   idx: unused (0 refs)
;   idx: unused (0 refs)
;   t: labels 32-36 (7 refs)
;   n: labels 27-29 (5 refs)
;   i: labels 25-29 (5 refs)
;   in_fatal: unused (0 refs)
;   last_errcode: unused (0 refs)
;   last_lineno: unused (0 refs)
;   t: unused (0 refs)
;   i: unused (0 refs)
;   errcode: unused (0 refs)
_dump_symbols:
	ld a, 32
	call framealloc
	; load address of $_fdprintf
	; load address of $_str77
	ld hl, 2
	; op @ (0x40) size=2
	; load address of $_fdprintf
	; load address of $_lexlevel
	; load address of $_lastname
	; load word from address
	; load word from address
	; load address of $_str78
	ld hl, 2
	; op @ (0x40) size=2
	; load address of $_fdprintf
	; load address of $_str79
	ld hl, 2
	; op @ (0x40) size=2
	; load address of $_names
	; load word from address
	; load address of $_lastname
	; load word from address
	; op X (0x58) size=4
	ld hl, 0
	call ge1616
	; op j (0x6a) size=2
	; load address of $i
	ld hl, 0
	; op N (0x4e) size=2
	; store word to address
	; load address of $i
	; load word from address
	; load address of $_lastname
	; load word from address
	call le1616
	; load address of $n
	; load address of $_names
	; load address of $i
	; load word from address
	ld hl, 2
	call mul1616
	call add1616
	; load word from address
	; store word to address
	; load address of $n
	; load word from address
	; load address of $_fdprintf
	; load address of $i
	; load address of $n
	; load word from address
	ld hl, 2
	call uadd1616
	; load byte from address
	; load address of $_str81
	; op , (0x2c) size=2
	; op : (0x3a) size=2
	; op ? (0x3f) size=2
	; load word from address
	; load address of $_str80
	ld hl, 2
	; op @ (0x40) size=2
	; load address of $n
	; load word from address
	ld hl, 7
	call uadd1616
	; load word from address
	; load address of $_fdprintf
	; load address of $_str85
	ld hl, 2
	; op @ (0x40) size=2
	; load address of $_dump_type
	; load address of $n
	; load word from address
	ld hl, 7
	call uadd1616
	ld hl, 0
	; load word from address
	; op @ (0x40) size=2
_if_29:
	; load address of $_fdprintf
	; load address of $_str86
	ld hl, 2
	; op @ (0x40) size=2
_if_28:
_if_end_30:
	; load address of $i
	; op ? (0xef) size=2
	; load address of $_fdprintf
	; load address of $_str87
	ld hl, 2
	; op @ (0x40) size=2
_if_end_31:
	; load address of $_fdprintf
	; load address of $_str88
	ld hl, 2
	; op @ (0x40) size=2
	; load address of $_types
	; load word from address
	; load address of $type_count
	ld hl, 0
	; store word to address
	; load address of $max_types
	ld hl, 1000
	; store word to address
	; load address of $t
	; load address of $_types
	; load word from address
	; store word to address
	; load address of $t
	; load word from address
	; load address of $type_count
	; load word from address
	; load address of $max_types
	; load word from address
	call lt1616
	; op j (0x6a) size=2
	; load address of $_fdprintf
	; load address of $t
	; load word from address
	; load address of $_str89
	ld hl, 2
	; op @ (0x40) size=2
	; load address of $_dump_type
	; load address of $t
	ld hl, 0
	; load word from address
	; op @ (0x40) size=2
	; load address of $_fdprintf
	; load address of $_str90
	ld hl, 2
	; op @ (0x40) size=2
	; load address of $type_count
	; op ? (0xef) size=2
_if_end_35:
	; load address of $t
	; load address of $t
	; load word from address
	ld hl, 11
	call uadd1616
	; load word from address
	; store word to address
	; load address of $t
	; load word from address
	; load address of $_fdprintf
	; load address of $max_types
	; load word from address
	; load address of $_str91
	ld hl, 2
	; op @ (0x40) size=2
_if_36:
	; load address of $_fdprintf
	; load address of $_str92
	ld hl, 2
	; op @ (0x40) size=2
_if_end_37:
	; load address of $_fdprintf
	; load address of $_str93
	ld hl, 2
	; op @ (0x40) size=2
	call framefree
; Function: fatal(errcode:_uchar_) -> :ptr
; Variable lifetimes:
;   t: unused (0 refs)
;   n: unused (0 refs)
;   i: unused (0 refs)
;   in_fatal: unused (0 refs)
;   last_errcode: unused (0 refs)
;   last_lineno: unused (0 refs)
;   t: unused (0 refs)
;   i: unused (0 refs)
;   errcode: labels 0-0 (2 refs)
_fatal:
	ld a, 15
	call framealloc
	; load address of $_gripe
	; load address of $Aerrcode
	; load byte from address
	; op @ (0x40) size=2
	; load address of $_fdprintf
	; load address of $_str94
	ld hl, 2
	; op @ (0x40) size=2
	; load address of $_dump_symbols
	; op @ (0x40) size=2
	; load address of $_exit
	; load address of $Aerrcode
	; load byte from address
	; op \ (0x5c) size=1
	; op @ (0x40) size=2
	call framefree
; Function: recover(skipto:_uchar_, errcode:_uchar_) -> _short_
; Variable lifetimes:
;   max_types: unused (0 refs)
;   type_count: unused (0 refs)
;   idx: unused (0 refs)
;   t: unused (0 refs)
;   n: unused (0 refs)
;   i: unused (0 refs)
;   in_fatal: unused (0 refs)
;   last_errcode: unused (0 refs)
;   last_lineno: unused (0 refs)
;   t: unused (0 refs)
;   i: unused (0 refs)
;   errcode: labels 0-0 (1 refs)
;   skipto: labels 38-38 (1 refs)
_recover:
	ld a, 21
	call framealloc
	; load address of $_gripe
	; load address of $Aerrcode
	; load byte from address
	; op @ (0x40) size=2
	; load address of $_cur
	; load byte from address
	; load address of $Askipto
	; load byte from address
	call ne88
	; load address of $_cur
	; load byte from address
	; op W (0x57) size=4
	ld hl, 0
	call une816
	; op j (0x6a) size=2
	; load address of $_gettoken
	; op @ (0x40) size=2
_if_end_40:
	call framefree
; Function: need(errcode:_uchar_, skipto:_uchar_, check:_uchar_) -> _short_
; Variable lifetimes:
;   max_types: unused (0 refs)
;   type_count: unused (0 refs)
;   idx: unused (0 refs)
;   t: unused (0 refs)
;   n: unused (0 refs)
;   i: unused (0 refs)
;   in_fatal: unused (0 refs)
;   last_errcode: unused (0 refs)
;   last_lineno: unused (0 refs)
;   t: unused (0 refs)
;   i: unused (0 refs)
;   check: labels 41-41 (1 refs)
;   skipto: labels 41-41 (1 refs)
;   errcode: labels 41-41 (1 refs)
_need:
	ld a, 21
	call framealloc
	; load address of $_cur
	; load byte from address
	; load address of $Acheck
	; load byte from address
	call eq88
	; load address of $_gettoken
	; op @ (0x40) size=2
_if_41:
	; load address of $_recover
	; load address of $Aerrcode
	; load address of $Askipto
	; load byte from address
	; load byte from address
	; op @ (0x40) size=2
	call framefree
; Function: expect(errcode:_uchar_, check:_uchar_) -> _short_
; Variable lifetimes:
;   max_types: unused (0 refs)
;   type_count: unused (0 refs)
;   idx: unused (0 refs)
;   t: unused (0 refs)
;   n: unused (0 refs)
;   i: unused (0 refs)
;   in_fatal: unused (0 refs)
;   last_errcode: unused (0 refs)
;   last_lineno: unused (0 refs)
;   t: unused (0 refs)
;   i: unused (0 refs)
;   check: labels 42-42 (1 refs)
;   errcode: labels 42-42 (1 refs)
_expect:
	ld a, 21
	call framealloc
	; load address of $_cur
	; load byte from address
	; load address of $Acheck
	; load byte from address
	call ne88
	; load address of $_gripe
	; load address of $Aerrcode
	; load byte from address
	; op @ (0x40) size=2
_if_42:
	; load address of $_gettoken
	; op @ (0x40) size=2
	call framefree
