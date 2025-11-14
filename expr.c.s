; Function: makeexpr(left, op:_uchar_) -> :ptr
; Variable lifetimes:
;   e: labels 0-0 (4 refs)
;   op: labels 0-0 (1 refs)
;   left: labels 0-0 (1 refs)
_makeexpr:
	ld a, 2
	call framealloc
	; load address of $e
	; load address of $_calloc
	ld hl, 27
	ld hl, 1
	; op @ (0x40) size=2
	; store word to address
	; load address of $e
	; load word from address
	ld hl, 2
	call uadd1616
	; load address of $Aop
	; load byte from address
	; store byte to address
	; load address of $e
	; load word from address
	ld hl, 3
	call uadd1616
	; load address of $Aleft
	; load word from address
	; store word to address
	; load address of $e
	; load word from address
	call framefree
; Function: makeexpr_init(flags:_short_, v:_ulong_, type, left, op:_uchar_) -> :ptr
; Variable lifetimes:
;   e: labels 0-0 (5 refs)
;   op: labels 0-0 (1 refs)
;   left: labels 0-0 (1 refs)
;   type: labels 0-0 (2 refs)
;   v: labels 0-0 (1 refs)
;   flags: labels 0-0 (1 refs)
_makeexpr_init:
	ld a, 2
	call framealloc
	; load address of $e
	; load address of $_makeexpr
	; load address of $Aop
	; load address of $Aleft
	; load word from address
	; load byte from address
	; op @ (0x40) size=2
	; store word to address
	; load address of $Atype
	; load word from address
	; load address of $e
	; load word from address
	ld hl, 13
	call uadd1616
	; load address of $Atype
	; load word from address
	; store word to address
_if_0:
	; load address of $e
	; load word from address
	ld hl, 17
	call uadd1616
	; load address of $Av
	; load long from address
	; store long to address
	; load address of $e
	; load word from address
	; load address of $Aflags
	; load word from address
	; store word to address
	; load address of $e
	; load word from address
	call framefree
; Function: destroy_expr(e) -> :ptr
; Variable lifetimes:
;   e: labels 1-3 (6 refs)
_destroy_expr:
	ld a, 0
	call framealloc
	; load address of $Ae
	; load word from address
	; op ' (0x27) size=2 unsigned
_if_1:
	; load address of $Ae
	; load word from address
	ld hl, 3
	call uadd1616
	; load word from address
	; load address of $_destroy_expr
	; load address of $Ae
	; load word from address
	ld hl, 3
	call uadd1616
	; load word from address
	; op @ (0x40) size=2
_if_2:
	; load address of $Ae
	; load word from address
	ld hl, 5
	call uadd1616
	; load word from address
	; load address of $_destroy_expr
	; load address of $Ae
	; load word from address
	ld hl, 5
	call uadd1616
	; load word from address
	; op @ (0x40) size=2
_if_3:
	; load address of $_free
	; load address of $Ae
	; load word from address
	; op @ (0x40) size=2
	call framefree
; Function: lvalue(e) -> :ptr
; Variable lifetimes:
;   e: labels 4-4 (1 refs)
_lvalue:
	ld a, 0
	call framealloc
	; load address of $Ae
	; load word from address
	ld hl, 2
	call uadd1616
	; load byte from address
	; op W (0x57) size=4
	ld hl, 77
	call ueq816
	ld hl, 1
_if_4:
	ld hl, 0
	call framefree
; Function: unop_set(e) -> :ptr
; Variable lifetimes:
;   e: labels 0-0 (4 refs)
_unop_set:
	ld a, 0
	call framealloc
	; load address of $Ae
	; load word from address
	ld hl, 13
	call uadd1616
	; load address of $Ae
	; load word from address
	ld hl, 3
	call uadd1616
	; load word from address
	ld hl, 13
	call uadd1616
	; load word from address
	; store word to address
	; load address of $Ae
	; load word from address
	ld hl, 3
	call uadd1616
	; load word from address
	ld hl, 7
	call uadd1616
	; load address of $Ae
	; load word from address
	; store word to address
	call framefree
; Function: binop_pri(t:_uchar_) -> _uchar_
; Variable lifetimes:
;   t: labels 5-5 (3 refs)
_binop_pri:
	ld a, 0
	call framealloc
	; load address of $At
	; load byte from address
	; op W (0x57) size=4
	ld hl, 37
	call ult816
	; load address of $At
	; load byte from address
	; op W (0x57) size=4
	ld hl, 254
	call ugt816
	; op h (0x68) size=2
	ld hl, 0
_if_5:
	; load address of $Sexpr_op_pri
	; load address of $At
	; load byte from address
	; op W (0x57) size=4
	ld hl, 37
	call usub816
	call add1632
	; load byte from address
	call framefree
