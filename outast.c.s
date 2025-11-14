; Function: get_size_suffix(t) -> :ptr
; Variable lifetimes:
;   t: labels 0-9 (7 refs)
_get_size_suffix:
	ld a, 0
	call framealloc
	; load address of $At
	; load word from address
	; op ' (0x27) size=2 unsigned
	ld hl, 115
_if_0:
	; load address of $At
	; load word from address
	ld hl, 10
	call uadd1616
	; load byte from address
	; op W (0x57) size=4
	ld hl, 16
	call uand816
	ld hl, 112
_if_1:
	; load address of $At
	; load word from address
	ld hl, 2
	call uadd1616
	; load word from address
	; op X (0x58) size=4
	ld hl, 1
	call eq1616
	ld hl, 98
	; load address of $At
	; load word from address
	ld hl, 2
	call uadd1616
	; load word from address
	; op X (0x58) size=4
	ld hl, 2
	call eq1616
	ld hl, 115
	; load address of $At
	; load word from address
	ld hl, 2
	call uadd1616
	; load word from address
	; op X (0x58) size=4
	ld hl, 4
	call eq1616
	; load address of $At
	; load word from address
	ld hl, 10
	call uadd1616
	; load byte from address
	; op W (0x57) size=4
	ld hl, 64
	call uand816
	ld hl, 102
_if_7:
	ld hl, 108
	; load address of $At
	; load word from address
	ld hl, 2
	call uadd1616
	; load word from address
	; op X (0x58) size=4
	ld hl, 8
	call eq1616
	ld hl, 100
_if_9:
_if_end_8:
_if_end_5:
_if_end_3:
	ld hl, 115
	call framefree
; Function: emit_child(e) -> :ptr
; Variable lifetimes:
;   e: labels 10-10 (2 refs)
_emit_child:
	ld a, 0
	call framealloc
	; load address of $Ae
	; load word from address
	; load address of $_fdprintf
	; load address of $_ast_fd
	; load address of $_str0
	; load word from address
	; op @ (0x40) size=2
	; load address of $Soutast_emit_expr
	; load address of $Ae
	; load word from address
	; op @ (0x40) size=2
_if_10:
	call framefree
