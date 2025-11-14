; Function: is_type_token(t:_uchar_) -> _uchar_
; Variable lifetimes:
;   t: labels 0-0 (14 refs)
_is_type_token:
	ld a, 0
	call framealloc
	; load address of $At
	; load byte from address
	; op W (0x57) size=4
	ld hl, 99
	call ueq816
	; load address of $At
	; load byte from address
	; op W (0x57) size=4
	ld hl, 115
	call ueq816
	; op h (0x68) size=2
	; load address of $At
	; load byte from address
	; op W (0x57) size=4
	ld hl, 105
	call ueq816
	; op h (0x68) size=2
	; load address of $At
	; load byte from address
	; op W (0x57) size=4
	ld hl, 108
	call ueq816
	; op h (0x68) size=2
	; load address of $At
	; load byte from address
	; op W (0x57) size=4
	ld hl, 102
	call ueq816
	; op h (0x68) size=2
	; load address of $At
	; load byte from address
	; op W (0x57) size=4
	ld hl, 100
	call ueq816
	; op h (0x68) size=2
	; load address of $At
	; load byte from address
	; op W (0x57) size=4
	ld hl, 118
	call ueq816
	; op h (0x68) size=2
	; load address of $At
	; load byte from address
	; op W (0x57) size=4
	ld hl, 117
	call ueq816
	; op h (0x68) size=2
	; load address of $At
	; load byte from address
	; op W (0x57) size=4
	ld hl, 97
	call ueq816
	; op h (0x68) size=2
	; load address of $At
	; load byte from address
	; op W (0x57) size=4
	ld hl, 109
	call ueq816
	; op h (0x68) size=2
	; load address of $At
	; load byte from address
	; op W (0x57) size=4
	ld hl, 101
	call ueq816
	; op h (0x68) size=2
	; load address of $At
	; load byte from address
	; op W (0x57) size=4
	ld hl, 107
	call ueq816
	; op h (0x68) size=2
	; load address of $At
	; load byte from address
	; op W (0x57) size=4
	ld hl, 52
	call ueq816
	; op h (0x68) size=2
	; load address of $At
	; load byte from address
	; op W (0x57) size=4
	ld hl, 116
	call ueq816
	; op h (0x68) size=2
	call framefree
; Function: parse_pointer_prefix(basetype) -> :ptr
; Variable lifetimes:
;   t: labels 0-14 (4 refs)
;   basetype: labels 0-0 (1 refs)
_parse_pointer_prefix:
	ld a, 2
	call framealloc
	; load address of $t
	; load address of $Abasetype
	; load word from address
	; store word to address
	; load address of $_cur
	; load byte from address
	; op W (0x57) size=4
	ld hl, 42
	call ueq816
	; load address of $_gettoken
	; op @ (0x40) size=2
	; load address of $_cur
	; load byte from address
	; op W (0x57) size=4
	ld hl, 107
	call ueq816
	; load address of $_cur
	; load byte from address
	; op W (0x57) size=4
	ld hl, 52
	call ueq816
	; op h (0x68) size=2
	; load address of $_gettoken
	; op @ (0x40) size=2
_if_end_16:
	; load address of $t
	; load address of $_get_type
	; load address of $t
	ld hl, 0
	; load word from address
	ld hl, 16
	; op @ (0x40) size=2
	; store word to address
_if_end_17:
	; load address of $t
	; load word from address
	call framefree
; Function: parse_param_name(bufsize:_short_, buf, allow_anonymous:_uchar_) -> _uchar_
; Variable lifetimes:
;   allow_anonymous: labels 19-19 (1 refs)
;   buf: labels 18-19 (5 refs)
;   bufsize: labels 18-18 (2 refs)
_parse_param_name:
	ld a, 0
	call framealloc
	; load address of $_cur
	; load byte from address
	; op W (0x57) size=4
	ld hl, 53
	call ueq816
	; load address of $_strncpy
	; load address of $Abuf
	; load address of $_cur
	ld hl, 1
	call add1616
	; load address of $Abufsize
	; load word from address
	; op X (0x58) size=4
	ld hl, 1
	call sub1616
	; load word from address
	; load word from address
	; op @ (0x40) size=2
	; load address of $Abuf
	; load address of $Abufsize
	; load word from address
	; op X (0x58) size=4
	ld hl, 1
	call sub1616
	call add1632
	ld hl, 0
	; op N (0x4e) size=1
	; store byte to address
	; load address of $_gettoken
	; op @ (0x40) size=2
	; load address of $Abuf
	; load word from address
_if_18:
	; load address of $Aallow_anonymous
	; load byte from address
	; load address of $Abuf
	ld hl, 0
	call add1616
	ld hl, 0
	; op N (0x4e) size=1
	; store byte to address
	; load address of $Abuf
	; load word from address
_if_19:
	ld hl, 0
	; op N (0x4e) size=2 unsigned
	call framefree
; Function: create_param_entry(type, name) -> :ptr
; Variable lifetimes:
;   arg: labels 0-0 (7 refs)
;   name: labels 0-0 (2 refs)
;   type: labels 0-0 (1 refs)
_create_param_entry:
	ld a, 2
	call framealloc
	; load address of $arg
	; load address of $_calloc
	ld hl, 21
	ld hl, 1
	; op @ (0x40) size=2
	; store word to address
	; load address of $arg
	; load word from address
	; load address of $Aname
	; load word from address
	; load address of $_strdup
	; load address of $Aname
	; load word from address
	; op @ (0x40) size=2
	; load address of $_strdup
	; load address of $_str0
	; op @ (0x40) size=2
	; op : (0x3a) size=2
	; op ? (0x3f) size=2
	; store word to address
	; load address of $arg
	; load word from address
	ld hl, 7
	call uadd1616
	; load address of $Atype
	; load word from address
	; store word to address
	; load address of $arg
	; load word from address
	ld hl, 3
	call uadd1616
	; load address of $_lexlevel
	; load word from address
	; op X (0x58) size=4
	ld hl, 1
	call add1616
	; op N (0x4e) size=2
	; store word to address
	; load address of $arg
	; load word from address
	ld hl, 2
	call uadd1616
	ld hl, 0
	; op N (0x4e) size=1
	; store byte to address
	; load address of $arg
	; load word from address
	ld hl, 20
	call uadd1616
	ld hl, 9
	; op N (0x4e) size=1
	; store byte to address
	; load address of $arg
	; load word from address
	call framefree
