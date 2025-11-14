; Function: generate_loop_label(prefix) -> :ptr
; Variable lifetimes:
;   label: labels 0-0 (3 refs)
;   prefix: labels 0-0 (1 refs)
_generate_loop_label:
	ld a, 2
	call framealloc
	; load address of $label
	; load address of $_malloc
	ld hl, 32
	; op @ (0x40) size=2
	; store word to address
	; load address of $_sprintf
	; load address of $label
	; load address of $Aprefix
	; load address of $Sparse_loop_label_counter
	; op ? (0xef) size=2
	; load word from address
	; load address of $_str0
	; load word from address
	; op @ (0x40) size=2
	; load address of $label
	; load word from address
	call framefree
; Function: find_enclosing_loop(is_continue:_short_, parent) -> :ptr
; Variable lifetimes:
;   label: unused (0 refs)
;   prefix: unused (0 refs)
;   parent: labels 0-4 (9 refs)
;   is_continue: labels 4-4 (1 refs)
_find_enclosing_loop:
	ld a, 4
	call framealloc
	; load address of $Aparent
	; load word from address
	; load address of $Aparent
	; load word from address
	ld hl, 8
	call uadd1616
	; load byte from address
	; op W (0x57) size=4
	ld hl, 87
	call ueq816
	; load address of $Aparent
	; load word from address
	ld hl, 8
	call uadd1616
	; load byte from address
	; op W (0x57) size=4
	ld hl, 70
	call ueq816
	; op h (0x68) size=2
	; load address of $Aparent
	; load word from address
	ld hl, 8
	call uadd1616
	; load byte from address
	; op W (0x57) size=4
	ld hl, 68
	call ueq816
	; op h (0x68) size=2
	; load address of $Aparent
	; load word from address
_if_1:
	; load address of $Aparent
	; load word from address
	ld hl, 8
	call uadd1616
	; load byte from address
	; op W (0x57) size=4
	ld hl, 83
	call ueq816
	; load address of $Ais_continue
	; load word from address
	; op ' (0x27) size=2
	; op j (0x6a) size=2
	; load address of $Aparent
	; load word from address
_if_4:
	; load address of $Aparent
	; load address of $Aparent
	; load word from address
	; load word from address
	; store word to address
_if_end_6:
	ld hl, 0
	; op N (0x4e) size=2 unsigned
	call framefree
; Function: mangle_static_name(var) -> _short_
; Variable lifetimes:
;   len: labels 8-10 (6 refs)
;   mangled: labels 8-10 (7 refs)
;   label: unused (0 refs)
;   prefix: unused (0 refs)
;   var: labels 7-10 (8 refs)
_mangle_static_name:
	ld a, 8
	call framealloc
	; load address of $_source_file_root
	; load word from address
	; op ' (0x27) size=2 unsigned
	; load address of $_strdup
	; load address of $Avar
	; load word from address
	; load word from address
	; op @ (0x40) size=2
_if_7:
	; load address of $Avar
	; load word from address
	ld hl, 3
	call uadd1616
	; load word from address
	; op X (0x58) size=4
	ld hl, 1
	call eq1616
	; load address of $len
	; load address of $_strlen
	; load address of $_source_file_root
	; load word from address
	; op @ (0x40) size=2
	ld hl, 1
	call add1616
	; load address of $_strlen
	; load address of $Avar
	; load word from address
	; load word from address
	; op @ (0x40) size=2
	call add1616
	ld hl, 1
	call add1616
	; op N (0x4e) size=2
	; store word to address
	; load address of $mangled
	; load address of $_malloc
	; load address of $len
	; load word from address
	; op @ (0x40) size=2
	; store word to address
	; load address of $_sprintf
	; load address of $mangled
	; load address of $_source_file_root
	; load address of $Avar
	; load word from address
	; load word from address
	; load word from address
	; load address of $_str1
	; load word from address
	; op @ (0x40) size=2
	; load address of $_current_function
	; load word from address
	; load address of $len
	; load address of $_strlen
	; load address of $_source_file_root
	; load word from address
	; op @ (0x40) size=2
	ld hl, 1
	call add1616
	; load address of $_strlen
	; load address of $_current_function
	; load word from address
	; load word from address
	; op @ (0x40) size=2
	call add1616
	ld hl, 1
	call add1616
	; load address of $_strlen
	; load address of $Avar
	; load word from address
	; load word from address
	; op @ (0x40) size=2
	call add1616
	ld hl, 1
	call add1616
	ld hl, 10
	call add1616
	ld hl, 1
	call add1616
	; op N (0x4e) size=2
	; store word to address
	; load address of $mangled
	; load address of $_malloc
	; load address of $len
	; load word from address
	; op @ (0x40) size=2
	; store word to address
	; load address of $_sprintf
	; load address of $mangled
	; load address of $_source_file_root
	; load address of $_current_function
	; load word from address
	; load address of $Avar
	; load word from address
	; load address of $_static_counter
	; op ? (0xef) size=2
	; load word from address
	; load word from address
	; load word from address
	; load address of $_str2
	; load word from address
	; op @ (0x40) size=2
	; load address of $len
	; load address of $_strlen
	; load address of $_source_file_root
	; load word from address
	; op @ (0x40) size=2
	ld hl, 1
	call add1616
	; load address of $_strlen
	; load address of $Avar
	; load word from address
	; load word from address
	; op @ (0x40) size=2
	call add1616
	ld hl, 1
	call add1616
	; op N (0x4e) size=2
	; store word to address
	; load address of $mangled
	; load address of $_malloc
	; load address of $len
	; load word from address
	; op @ (0x40) size=2
	; store word to address
	; load address of $_sprintf
	; load address of $mangled
	; load address of $_source_file_root
	; load address of $Avar
	; load word from address
	; load word from address
	; load word from address
	; load address of $_str3
	; load word from address
	; op @ (0x40) size=2
_if_end_11:
_if_end_9:
	; load address of $mangled
	; load word from address
	call framefree
