; Function: pop_scope() -> :ptr
; Variable lifetimes:
;   n: labels 0-1 (2 refs)
_pop_scope:
	ld a, 2
	call framealloc
	; load address of $_lexlevel
	; op ? (0xf6) size=2
	; load address of $_lastname
	; load word from address
	; op X (0x58) size=4
	ld hl, 0
	call ge1616
	; load address of $n
	; load address of $_names
	; load address of $_lastname
	; load word from address
	ld hl, 2
	call mul1616
	call add1616
	; load word from address
	; store word to address
	; load address of $n
	; load word from address
	ld hl, 3
	call uadd1616
	; load word from address
	; load address of $_lexlevel
	; load word from address
	call le1616
_if_1:
	; load address of $_names
	; load address of $_lastname
	; load word from address
	ld hl, 2
	call mul1616
	call add1616
	ld hl, 0
	; store word to address
	; load address of $_lastname
	; op ? (0xf6) size=2
_if_end_2:
	call framefree
; Function: push_scope(n) -> :ptr
; Variable lifetimes:
;   n: unused (0 refs)
_push_scope:
	ld a, 0
	call framealloc
	; load address of $_lexlevel
	; op ? (0xef) size=2
	call framefree
; Function: lookup_name(is_tag:_uchar_, name) -> _short_
; Variable lifetimes:
;   i: labels 0-5 (4 refs)
;   n: labels 3-5 (6 refs)
;   name: labels 5-5 (2 refs)
;   is_tag: labels 5-5 (1 refs)
_lookup_name:
	ld a, 4
	call framealloc
	; load address of $i
	; load address of $_lastname
	; load word from address
	; store word to address
	; load address of $i
	; load word from address
	; op X (0x58) size=4
	ld hl, 0
	call ge1616
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
	; op ' (0x27) size=2 unsigned
_if_4:
	; load address of $n
	; load word from address
	ld hl, 2
	call uadd1616
	; load byte from address
	; load address of $Ais_tag
	; load byte from address
	call eq88
	; load address of $Aname
	ld hl, 0
	call add1616
	; load byte from address
	; load address of $n
	; load word from address
	ld hl, 0
	call uadd1616
	; load word from address
	call eq816
	; op j (0x6a) size=2
	; load address of $_strcmp
	; load address of $Aname
	; load address of $n
	; load word from address
	; load word from address
	; load word from address
	; op @ (0x40) size=2
	ld hl, 0
	call eq1616
	; op j (0x6a) size=2
	; load address of $n
	; load word from address
_if_5:
_if_end_8:
	; load address of $i
	; op ? (0xf6) size=2
	ld hl, 0
	call framefree
; Function: lookup_element(t, name) -> :ptr
; Variable lifetimes:
;   n: labels 0-10 (6 refs)
;   name: labels 10-10 (1 refs)
;   t: labels 0-0 (1 refs)
_lookup_element:
	ld a, 2
	call framealloc
	; load address of $n
	; load address of $At
	; load word from address
	ld hl, 6
	call uadd1616
	; load word from address
	; store word to address
	; load address of $n
	; load word from address
	; load address of $_strcmp
	; load address of $Aname
	; load address of $n
	; load word from address
	; load word from address
	; load word from address
	; op @ (0x40) size=2
	ld hl, 0
	call eq1616
	; load address of $n
	; load word from address
_if_10:
_if_end_11:
	; load address of $n
	; load address of $n
	; load word from address
	ld hl, 5
	call uadd1616
	; load word from address
	; store word to address
	ld hl, 0
	call framefree
; Function: dump_name(n) -> :ptr
; Variable lifetimes:
;   n: labels 12-21 (16 refs)
_dump_name:
	ld a, 0
	call framealloc
	; load address of $_fdprintf
	; load address of $_str19
	ld hl, 2
	; op @ (0x40) size=2
	; load address of $An
	; load word from address
	; op ' (0x27) size=2 unsigned
	; load address of $_printf
	; load address of $_str20
	; op @ (0x40) size=2
_if_12:
	; load address of $_fdprintf
	; load address of $An
	; load word from address
	; load address of $An
	; load word from address
	ld hl, 2
	call uadd1616
	; load byte from address
	; load address of $_str22
	; load address of $_str23
	; op : (0x3a) size=2
	; op ? (0x3f) size=2
	; load word from address
	; load address of $_str21
	ld hl, 2
	; op @ (0x40) size=2
	; load address of $An
	; load word from address
	ld hl, 9
	call uadd1616
	; load byte from address
	; load address of $_fdprintf
	; load address of $_str24
	ld hl, 2
	; op @ (0x40) size=2
	; load address of $An
	; load word from address
	ld hl, 9
	call uadd1616
	; load byte from address
	; op W (0x57) size=4
	ld hl, 1
	call uand816
	; load address of $_printf
	; load address of $_str25
	; op @ (0x40) size=2
_if_14:
	; load address of $An
	; load word from address
	ld hl, 9
	call uadd1616
	; load byte from address
	; op W (0x57) size=4
	ld hl, 4
	call uand816
	; load address of $_printf
	; load address of $_str26
	; op @ (0x40) size=2
_if_15:
	; load address of $An
	; load word from address
	ld hl, 9
	call uadd1616
	; load byte from address
	; op W (0x57) size=4
	ld hl, 2
	call uand816
	; load address of $_printf
	; load address of $_str27
	; op @ (0x40) size=2
_if_16:
	; load address of $An
	; load word from address
	ld hl, 9
	call uadd1616
	; load byte from address
	; op W (0x57) size=4
	ld hl, 32
	call uand816
	; load address of $_printf
	; load address of $_str28
	; op @ (0x40) size=2
_if_17:
	; load address of $An
	; load word from address
	ld hl, 9
	call uadd1616
	; load byte from address
	; op W (0x57) size=4
	ld hl, 8
	call uand816
	; load address of $_printf
	; load address of $_str29
	; op @ (0x40) size=2
_if_18:
	; load address of $An
	; load word from address
	ld hl, 9
	call uadd1616
	; load byte from address
	; op W (0x57) size=4
	ld hl, 16
	call uand816
	; load address of $_printf
	; load address of $_str30
	; op @ (0x40) size=2
_if_19:
	; load address of $An
	; load word from address
	ld hl, 9
	call uadd1616
	; load byte from address
	; op W (0x57) size=4
	ld hl, 64
	call uand816
	; load address of $_printf
	; load address of $_str31
	; op @ (0x40) size=2
_if_20:
_if_13:
	; load address of $_fdprintf
	; load address of $_str32
	ld hl, 2
	; op @ (0x40) size=2
	; load address of $An
	; load word from address
	ld hl, 7
	call uadd1616
	; load word from address
	; load address of $_dump_type
	; load address of $An
	; load word from address
	ld hl, 7
	call uadd1616
	ld hl, 0
	; load word from address
	; op @ (0x40) size=2
_if_21:
	; load address of $_fdprintf
	; load address of $An
	; load word from address
	ld hl, 10
	call uadd1616
	; load address of $An
	; load word from address
	ld hl, 12
	call uadd1616
	; load address of $An
	; load word from address
	ld hl, 14
	call uadd1616
	; load word from address
	; load word from address
	; load word from address
	; load address of $_str33
	ld hl, 2
	; op @ (0x40) size=2
	call framefree
; Function: new_name(is_tag:_uchar_, t, k:_uchar_, name) -> _short_
; Variable lifetimes:
;   i: labels 23-29 (5 refs)
;   n: labels 25-29 (14 refs)
;   n: unused (0 refs)
;   name: labels 26-29 (3 refs)
;   k: labels 29-29 (1 refs)
;   t: labels 29-29 (1 refs)
;   is_tag: labels 26-29 (2 refs)
_new_name:
	ld a, 6
	call framealloc
	; load address of $_names
	; load word from address
	; op ' (0x27) size=2 unsigned
	; load address of $_names
	; load address of $_malloc
	ld hl, 20000
	; op @ (0x40) size=2
	; store word to address
	; load address of $_lastname
	ld hl, -1
	; op N (0x4e) size=2
	; store word to address
_if_22:
	; load address of $_lastname
	; load word from address
	; op X (0x58) size=4
	ld hl, 10000
	call eq1616
	; load address of $_gripe
	ld hl, 24
	; op @ (0x40) size=2
	ld hl, 0
_if_23:
	; load address of $i
	; load address of $_lastname
	; load word from address
	; store word to address
	; load address of $i
	; load word from address
	; op X (0x58) size=4
	ld hl, 0
	call ge1616
	; load address of $_names
	; load address of $i
	; load word from address
	ld hl, 2
	call mul1616
	call add1616
	; load word from address
	ld hl, 3
	call uadd1616
	; load word from address
	; load address of $_lexlevel
	; load word from address
	call lt1616
_if_25:
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
	ld hl, 2
	call uadd1616
	; load byte from address
	; load address of $Ais_tag
	; load byte from address
	call eq88
	; load address of $Aname
	ld hl, 0
	call add1616
	; load byte from address
	; load address of $n
	; load word from address
	ld hl, 0
	call uadd1616
	; load word from address
	call eq816
	; op j (0x6a) size=2
	; load address of $_strcmp
	; load address of $Aname
	; load address of $n
	; load word from address
	; load word from address
	; load word from address
	; op @ (0x40) size=2
	ld hl, 0
	call eq1616
	; op j (0x6a) size=2
	; load address of $n
	; load word from address
	ld hl, 9
	call uadd1616
	; load byte from address
	; op W (0x57) size=4
	ld hl, 1
	call uand816
	; load address of $n
	; load word from address
_if_29:
	; load address of $_gripe
	ld hl, 25
	; op @ (0x40) size=2
	ld hl, 0
_if_26:
_if_end_30:
	; load address of $i
	; op ? (0xf6) size=2
	; load address of $n
	; load address of $_calloc
	ld hl, 21
	ld hl, 1
	; op @ (0x40) size=2
	; store word to address
	; load address of $n
	; load word from address
	; load address of $_strdup
	; load address of $Aname
	; load word from address
	; op @ (0x40) size=2
	; store word to address
	; load address of $n
	; load word from address
	ld hl, 7
	call uadd1616
	; load address of $At
	; load word from address
	; store word to address
	; load address of $n
	; load word from address
	ld hl, 3
	call uadd1616
	; load address of $_lexlevel
	; load word from address
	; store word to address
	; load address of $n
	; load word from address
	ld hl, 2
	call uadd1616
	; load address of $Ais_tag
	; load byte from address
	; store byte to address
	; load address of $n
	; load word from address
	ld hl, 20
	call uadd1616
	; load address of $Ak
	; load byte from address
	; store byte to address
	; load address of $_names
	; load address of $_lastname
	; op ? (0xcf) size=2
	ld hl, 2
	call mul1616
	call add1616
	; load address of $n
	; load word from address
	; store word to address
	; load address of $n
	; load word from address
	call framefree
; Function: add_name(n) -> _short_
; Variable lifetimes:
;   i: labels 32-39 (10 refs)
;   n: labels 35-39 (9 refs)
_add_name:
	ld a, 2
	call framealloc
	; load address of $_names
	; load word from address
	; op ' (0x27) size=2 unsigned
	; load address of $_names
	; load address of $_malloc
	ld hl, 20000
	; op @ (0x40) size=2
	; store word to address
	; load address of $_lastname
	ld hl, -1
	; op N (0x4e) size=2
	; store word to address
_if_31:
	; load address of $_lastname
	; load word from address
	; op X (0x58) size=4
	ld hl, 10000
	call eq1616
	; load address of $_gripe
	ld hl, 24
	; op @ (0x40) size=2
_if_32:
	; load address of $i
	; load address of $_lastname
	; load word from address
	; store word to address
	; load address of $i
	; load word from address
	; op X (0x58) size=4
	ld hl, 0
	call ge1616
	; load address of $_names
	; load address of $i
	; load word from address
	ld hl, 2
	call mul1616
	call add1616
	; load word from address
	ld hl, 3
	call uadd1616
	; load word from address
	; load address of $_lexlevel
	; load word from address
	call lt1616
_if_34:
	; load address of $_names
	; load address of $i
	; load word from address
	ld hl, 2
	call mul1616
	call add1616
	; load word from address
	ld hl, 2
	call uadd1616
	; load byte from address
	; load address of $An
	; load word from address
	ld hl, 2
	call uadd1616
	; load byte from address
	call eq88
	; load address of $An
	; load word from address
	ld hl, 0
	call uadd1616
	; load word from address
	; load address of $_names
	; load address of $i
	; load word from address
	ld hl, 2
	call mul1616
	call add1616
	; load word from address
	ld hl, 0
	call uadd1616
	; load word from address
	call eq1616
	; op j (0x6a) size=2
	; load address of $_strcmp
	; load address of $An
	; load word from address
	; load address of $_names
	; load address of $i
	; load word from address
	ld hl, 2
	call mul1616
	call add1616
	; load word from address
	; load word from address
	; load word from address
	; op @ (0x40) size=2
	ld hl, 0
	call eq1616
	; op j (0x6a) size=2
	; load address of $_names
	; load address of $i
	; load word from address
	ld hl, 2
	call mul1616
	call add1616
	; load word from address
	ld hl, 9
	call uadd1616
	; load byte from address
	; op W (0x57) size=4
	ld hl, 1
	call uand816
	; load address of $_names
	; load address of $i
	; load word from address
	ld hl, 2
	call mul1616
	call add1616
	; load word from address
	ld hl, 9
	call uadd1616
	; load address of $An
	; load word from address
	ld hl, 9
	call uadd1616
	; load byte from address
	; store byte to address
	; load address of $An
	; load word from address
	ld hl, 18
	call uadd1616
	; load word from address
	; load address of $_names
	; load address of $i
	; load word from address
	ld hl, 2
	call mul1616
	call add1616
	; load word from address
	ld hl, 18
	call uadd1616
	; load address of $An
	; load word from address
	ld hl, 18
	call uadd1616
	; load word from address
	; store word to address
_if_39:
	; load address of $_free
	; load address of $An
	; load word from address
	; op @ (0x40) size=2
_if_38:
	; load address of $_gripe
	ld hl, 25
	; op @ (0x40) size=2
_if_35:
_if_end_40:
	; load address of $i
	; op ? (0xf6) size=2
	; load address of $An
	; load word from address
	ld hl, 3
	call uadd1616
	; load address of $_lexlevel
	; load word from address
	; store word to address
	; load address of $_names
	; load address of $_lastname
	; op ? (0xcf) size=2
	ld hl, 2
	call mul1616
	call add1616
	; load address of $An
	; load word from address
	; store word to address
	call framefree
; Function: dump_type(lv:_short_, t) -> _short_
; Variable lifetimes:
;   i: labels 43-59 (8 refs)
;   i: unused (0 refs)
;   arg_num: labels 50-54 (3 refs)
;   i: unused (0 refs)
;   arg_num: unused (0 refs)
;   i: unused (0 refs)
;   i: unused (0 refs)
;   i: unused (0 refs)
;   param_count: labels 0-50 (4 refs)
;   param: labels 46-54 (11 refs)
;   n: unused (0 refs)
;   t: labels 41-59 (11 refs)
;   lv: labels 42-59 (7 refs)
_dump_type:
	ld a, 21
	call framealloc
	; load address of $param_count
	ld hl, 0
	; store byte to address
	; load address of $At
	; load word from address
	; op ' (0x27) size=2 unsigned
_if_41:
	; load address of $Alv
	; load word from address
	; op X (0x58) size=4
	ld hl, 20
	call gt1616
	; load address of $_fdprintf
	; load address of $_str34
	ld hl, 2
	; op @ (0x40) size=2
_if_42:
	; load address of $Alv
	; load word from address
	; load address of $i
	; load address of $Alv
	; load word from address
	; store word to address
	; load address of $i
	; op ? (0xf6) size=2
	; load address of $_fdprintf
	; load address of $_str35
	ld hl, 2
	; op @ (0x40) size=2
_if_end_45:
_if_43:
	; load address of $At
	; load word from address
	ld hl, 10
	call uadd1616
	; load byte from address
	; op W (0x57) size=4
	ld hl, 8
	call uand816
	; load address of $param
	; load address of $At
	; load word from address
	ld hl, 6
	call uadd1616
	; load word from address
	; store word to address
	; load address of $param
	; load word from address
	; load address of $param_count
	; op ? (0xef) size=2
_if_end_48:
	; load address of $param
	; load address of $param
	; load word from address
	ld hl, 5
	call uadd1616
	; load word from address
	; store word to address
_if_46:
	; load address of $At
	; load word from address
	ld hl, 10
	call uadd1616
	; load byte from address
	; op W (0x57) size=4
	ld hl, 8
	call uand816
	; load address of $_fdprintf
	; load address of $At
	; load word from address
	ld hl, 10
	call uadd1616
	; load address of $_bitdef
	; load address of $param_count
	; load byte from address
	; op @ (0x40) size=2
	; load byte from address
	; load address of $_str36
	ld hl, 2
	; op @ (0x40) size=2
	; load address of $param_count
	; load byte from address
	; op W (0x57) size=4
	ld hl, 0
	call ugt816
	; load address of $arg_num
	ld hl, 0
	; store word to address
	; load address of $param
	; load address of $At
	; load word from address
	ld hl, 6
	call uadd1616
	; load word from address
	; store word to address
	; load address of $param
	; load word from address
	; load address of $i
	ld hl, 0
	; op N (0x4e) size=2
	; store word to address
	; load address of $i
	; load word from address
	; load address of $Alv
	; load word from address
	call le1616
	; load address of $_fdprintf
	; load address of $_str37
	ld hl, 2
	; op @ (0x40) size=2
_if_end_53:
	; load address of $i
	; op ? (0xef) size=2
	; load address of $_fdprintf
	; load address of $arg_num
	; load word from address
	; load address of $_str38
	ld hl, 2
	; op @ (0x40) size=2
	; load address of $param
	; load word from address
	ld hl, 7
	call uadd1616
	; load word from address
	; load address of $param
	; load word from address
	ld hl, 7
	call uadd1616
	; load word from address
	; load word from address
	; op j (0x6a) size=2
	; load address of $_fdprintf
	; load address of $param
	; load word from address
	ld hl, 7
	call uadd1616
	; load word from address
	; load word from address
	; load address of $_str39
	ld hl, 2
	; op @ (0x40) size=2
	; load address of $_fdprintf
	; load address of $_str40
	ld hl, 2
	; op @ (0x40) size=2
_if_end_56:
	; load address of $_fdprintf
	; load address of $_str41
	ld hl, 2
	; op @ (0x40) size=2
	; load address of $arg_num
	; op ? (0xef) size=2
_if_end_57:
	; load address of $param
	; load address of $param
	; load word from address
	ld hl, 5
	call uadd1616
	; load word from address
	; store word to address
_if_50:
	; load address of $At
	; load word from address
	ld hl, 8
	call uadd1616
	; load word from address
	; load address of $i
	ld hl, 0
	; op N (0x4e) size=2
	; store word to address
	; load address of $i
	; load word from address
	; load address of $Alv
	; load word from address
	call le1616
	; load address of $_fdprintf
	; load address of $_str42
	ld hl, 2
	; op @ (0x40) size=2
_if_end_60:
	; load address of $i
	; op ? (0xef) size=2
	; load address of $_fdprintf
	; load address of $_str43
	ld hl, 2
	; op @ (0x40) size=2
	; load address of $_dump_type
	; load address of $At
	; load word from address
	ld hl, 8
	call uadd1616
	; load address of $Alv
	; load word from address
	; op X (0x58) size=4
	ld hl, 2
	call add1616
	; load word from address
	; op @ (0x40) size=2
_if_58:
	; load address of $_fdprintf
	; load address of $At
	; load word from address
	; load word from address
	; load address of $At
	; load word from address
	; load word from address
	; op , (0x2c) size=2
	; op : (0x3a) size=2
	; op ? (0x3f) size=2
	; load address of $_str44
	ld hl, 2
	; op @ (0x40) size=2
	; load address of $_dump_type
	; load address of $At
	; load word from address
	ld hl, 8
	call uadd1616
	; load address of $Alv
	; op ? (0xcf) size=2
	; load word from address
	; op @ (0x40) size=2
_if_end_61:
	call framefree
; Function: compare_param_lists(p2, p1) -> _short_
; Variable lifetimes:
;   i: unused (0 refs)
;   arg_num: unused (0 refs)
;   i: unused (0 refs)
;   param_count: unused (0 refs)
;   param: unused (0 refs)
;   t: unused (0 refs)
;   lv: unused (0 refs)
;   n: unused (0 refs)
;   p1: labels 62-64 (5 refs)
;   p2: labels 62-64 (5 refs)
_compare_param_lists:
	ld a, 15
	call framealloc
	; load address of $Ap1
	; load word from address
	; load address of $Ap2
	; load word from address
	; op j (0x6a) size=2
	; load address of $Ap1
	; load word from address
	ld hl, 7
	call uadd1616
	; load word from address
	; load address of $Ap2
	; load word from address
	ld hl, 7
	call uadd1616
	; load word from address
	call une1616
	ld hl, 0
_if_64:
	; load address of $Ap1
	; load address of $Ap1
	; load word from address
	ld hl, 5
	call uadd1616
	; load word from address
	; store word to address
	; load address of $Ap2
	; load address of $Ap2
	; load word from address
	ld hl, 5
	call uadd1616
	; load word from address
	; store word to address
_if_end_65:
	; load address of $Ap1
	; load word from address
	ld hl, 0
	; op N (0x4e) size=2 unsigned
	call ueq1616
	; load address of $Ap2
	; load word from address
	ld hl, 0
	; op N (0x4e) size=2 unsigned
	call ueq1616
	; op j (0x6a) size=2
	call framefree
; Function: compatible_function_types(t2, t1) -> _uchar_
; Variable lifetimes:
;   param_count: unused (0 refs)
;   param: unused (0 refs)
;   t: unused (0 refs)
;   lv: unused (0 refs)
;   n: unused (0 refs)
;   t1: labels 67-72 (5 refs)
;   t2: labels 67-72 (5 refs)
_compatible_function_types:
	ld a, 9
	call framealloc
	; load address of $At1
	; load word from address
	; op ' (0x27) size=2 unsigned
	; load address of $At2
	; load word from address
	; op ' (0x27) size=2 unsigned
	; op h (0x68) size=2
	ld hl, 0
_if_67:
	; load address of $At1
	; load word from address
	ld hl, 10
	call uadd1616
	; load byte from address
	; op W (0x57) size=4
	ld hl, 8
	call uand816
	; op ' (0x27) size=4
	; load address of $At2
	; load word from address
	ld hl, 10
	call uadd1616
	; load byte from address
	; op W (0x57) size=4
	ld hl, 8
	call uand816
	; op ' (0x27) size=4
	; op h (0x68) size=2
	ld hl, 0
_if_69:
	; load address of $At1
	; load word from address
	ld hl, 8
	call uadd1616
	; load word from address
	; load address of $At2
	; load word from address
	ld hl, 8
	call uadd1616
	; load word from address
	call une1616
	ld hl, 0
_if_71:
	; load address of $At1
	; load word from address
	ld hl, 10
	call uadd1616
	; load byte from address
	; op W (0x57) size=4
	ld hl, 128
	call uand816
	; load address of $At2
	; load word from address
	ld hl, 10
	call uadd1616
	; load byte from address
	; op W (0x57) size=4
	ld hl, 128
	call uand816
	call ne3232
	ld hl, 0
_if_72:
	; load address of $Stype_compare_param_lists
	; load address of $At1
	; load word from address
	ld hl, 6
	call uadd1616
	; load address of $At2
	; load word from address
	ld hl, 6
	call uadd1616
	; load word from address
	; load word from address
	; op @ (0x40) size=2
	call framefree
