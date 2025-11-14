; Function: test_if(x:_short_) -> _short_
; Variable lifetimes:
;   x: labels 0-0 (1 refs)
_test_if:
	ld a, 0
	call framealloc
	; load address of $Ax
	; load word from address
	; op X (0x58) size=4
	ld hl, 0
	call gt1616
	ld hl, 1
_if_0:
	call framefree
; Function: test_if_else(x:_short_) -> _short_
; Variable lifetimes:
;   x: labels 1-1 (1 refs)
_test_if_else:
	ld a, 0
	call framealloc
	; load address of $Ax
	; load word from address
	; op X (0x58) size=4
	ld hl, 0
	call gt1616
	ld hl, 1
	ld hl, 0
_if_end_2:
	call framefree
; Function: test_if_else_if(x:_short_) -> _short_
; Variable lifetimes:
;   x: labels 3-5 (2 refs)
_test_if_else_if:
	ld a, 0
	call framealloc
	; load address of $Ax
	; load word from address
	; op X (0x58) size=4
	ld hl, 0
	call gt1616
	ld hl, 1
	; load address of $Ax
	; load word from address
	; op X (0x58) size=4
	ld hl, 0
	call lt1616
	ld hl, -1
	ld hl, 0
_if_end_6:
_if_end_4:
	call framefree
; Function: test_nested_if(y:_short_, x:_short_) -> _short_
; Variable lifetimes:
;   x: labels 7-7 (1 refs)
;   y: labels 8-8 (1 refs)
_test_nested_if:
	ld a, 0
	call framealloc
	; load address of $Ax
	; load word from address
	; op X (0x58) size=4
	ld hl, 0
	call gt1616
	; load address of $Ay
	; load word from address
	; op X (0x58) size=4
	ld hl, 0
	call gt1616
	ld hl, 1
_if_8:
_if_7:
	ld hl, 0
	call framefree
; Function: test_while(n:_short_) -> _short_
; Variable lifetimes:
;   i: labels 0-9 (5 refs)
;   n: labels 9-9 (1 refs)
_test_while:
	ld a, 2
	call framealloc
	; load address of $i
	ld hl, 0
	; store word to address
	; load address of $i
	; load word from address
	; load address of $An
	; load word from address
	call lt1616
	; load address of $i
	; load address of $i
	; load word from address
	; op X (0x58) size=4
	ld hl, 1
	call add1616
	; op N (0x4e) size=2
	; store word to address
_if_end_10:
	; load address of $i
	; load word from address
	call framefree
; Function: test_do_while(n:_short_) -> _short_
; Variable lifetimes:
;   i: labels 0-11 (5 refs)
;   n: labels 11-11 (1 refs)
_test_do_while:
	ld a, 2
	call framealloc
	; load address of $i
	ld hl, 0
	; store word to address
	; load address of $i
	; load address of $i
	; load word from address
	; op X (0x58) size=4
	ld hl, 1
	call add1616
	; op N (0x4e) size=2
	; store word to address
	; load address of $i
	; load word from address
	; load address of $An
	; load word from address
	call lt1616
_if_end_12:
	; load address of $i
	; load word from address
	call framefree
; Function: test_for(n:_short_) -> _short_
; Variable lifetimes:
;   sum: labels 0-13 (4 refs)
;   i: labels 0-13 (5 refs)
;   n: labels 13-13 (1 refs)
_test_for:
	ld a, 4
	call framealloc
	; load address of $sum
	ld hl, 0
	; store word to address
	; load address of $i
	ld hl, 0
	; op N (0x4e) size=2
	; store word to address
	; load address of $i
	; load word from address
	; load address of $An
	; load word from address
	call lt1616
	; load address of $sum
	; load address of $sum
	; load word from address
	; load address of $i
	; load word from address
	call add1616
	; store word to address
_if_end_14:
	; load address of $i
	; load address of $i
	; load word from address
	; op X (0x58) size=4
	ld hl, 1
	call add1616
	; op N (0x4e) size=2
	; store word to address
	; load address of $sum
	; load word from address
	call framefree
; Function: test_for_empty() -> _short_
; Variable lifetimes:
;   i: labels 0-15 (5 refs)
_test_for_empty:
	ld a, 2
	call framealloc
	; load address of $i
	ld hl, 0
	; store word to address
	; load address of $i
	; load word from address
	; op X (0x58) size=4
	ld hl, 10
	call ge1616
_if_15:
	; load address of $i
	; load address of $i
	; load word from address
	; op X (0x58) size=4
	ld hl, 1
	call add1616
	; op N (0x4e) size=2
	; store word to address
	; load address of $i
	; load word from address
	call framefree
; Function: test_break(n:_short_) -> _short_
; Variable lifetimes:
;   i: labels 0-17 (5 refs)
;   n: labels 17-17 (1 refs)
_test_break:
	ld a, 2
	call framealloc
	; load address of $i
	ld hl, 0
	; store word to address
	ld hl, 1
	; load address of $i
	; load word from address
	; load address of $An
	; load word from address
	call ge1616
_if_17:
	; load address of $i
	; load address of $i
	; load word from address
	; op X (0x58) size=4
	ld hl, 1
	call add1616
	; op N (0x4e) size=2
	; store word to address
_if_end_18:
	; load address of $i
	; load word from address
	call framefree
; Function: test_continue(n:_short_) -> _short_
; Variable lifetimes:
;   count: labels 0-20 (4 refs)
;   i: labels 0-20 (5 refs)
;   n: labels 19-19 (1 refs)
_test_continue:
	ld a, 4
	call framealloc
	; load address of $i
	ld hl, 0
	; store word to address
	; load address of $count
	ld hl, 0
	; store word to address
	; load address of $i
	; load word from address
	; load address of $An
	; load word from address
	call lt1616
	; load address of $i
	; load address of $i
	; load word from address
	; op X (0x58) size=4
	ld hl, 1
	call add1616
	; op N (0x4e) size=2
	; store word to address
	; load address of $i
	; load word from address
	; op X (0x58) size=4
	ld hl, 2
	call mod1616
	ld hl, 0
	call eq3216
_if_20:
	; load address of $count
	; load address of $count
	; load word from address
	; op X (0x58) size=4
	ld hl, 1
	call add1616
	; op N (0x4e) size=2
	; store word to address
_if_end_21:
	; load address of $count
	; load word from address
	call framefree
; Function: test_switch(x:_short_) -> _short_
; Variable lifetimes:
;   x: labels 0-0 (1 refs)
_test_switch:
	ld a, 0
	call framealloc
	; load address of $Ax
	; load word from address
	ld hl, 0
	ld hl, 100
	ld hl, 1
	ld hl, 200
	ld hl, 2
	ld hl, 300
	ld hl, -1
	call framefree
; Function: test_switch_fallthrough(x:_short_) -> _short_
; Variable lifetimes:
;   result: labels 0-0 (5 refs)
;   x: labels 0-0 (1 refs)
_test_switch_fallthrough:
	ld a, 2
	call framealloc
	; load address of $result
	ld hl, 0
	; store word to address
	; load address of $Ax
	; load word from address
	ld hl, 0
	ld hl, 1
	; load address of $result
	ld hl, 10
	; op N (0x4e) size=2
	; store word to address
	ld hl, 2
	; load address of $result
	ld hl, 20
	; op N (0x4e) size=2
	; store word to address
	; load address of $result
	ld hl, -1
	; op N (0x4e) size=2
	; store word to address
	; load address of $result
	; load word from address
	call framefree
; Function: test_goto(n:_short_) -> _short_
; Variable lifetimes:
;   i: labels 0-22 (5 refs)
;   n: labels 22-22 (1 refs)
_test_goto:
	ld a, 2
	call framealloc
	; load address of $i
	ld hl, 0
	; store word to address
	; load address of $i
	; load word from address
	; load address of $An
	; load word from address
	call ge1616
_if_22:
	; load address of $i
	; load address of $i
	; load word from address
	; op X (0x58) size=4
	ld hl, 1
	call add1616
	; op N (0x4e) size=2
	; store word to address
	; load address of $i
	; load word from address
	call framefree
; Function: test_nested_loops(n:_short_, m:_short_) -> _short_
; Variable lifetimes:
;   count: labels 0-24 (4 refs)
;   j: labels 23-24 (4 refs)
;   i: labels 0-24 (4 refs)
;   m: labels 23-23 (1 refs)
;   n: labels 24-24 (1 refs)
_test_nested_loops:
	ld a, 6
	call framealloc
	; load address of $count
	ld hl, 0
	; store word to address
	; load address of $i
	ld hl, 0
	; op N (0x4e) size=2
	; store word to address
	; load address of $i
	; load word from address
	; load address of $Am
	; load word from address
	call lt1616
	; load address of $j
	ld hl, 0
	; op N (0x4e) size=2
	; store word to address
	; load address of $j
	; load word from address
	; load address of $An
	; load word from address
	call lt1616
	; load address of $count
	; load address of $count
	; load word from address
	; op X (0x58) size=4
	ld hl, 1
	call add1616
	; op N (0x4e) size=2
	; store word to address
_if_end_25:
	; load address of $j
	; load address of $j
	; load word from address
	; op X (0x58) size=4
	ld hl, 1
	call add1616
	; op N (0x4e) size=2
	; store word to address
_if_end_26:
	; load address of $i
	; load address of $i
	; load word from address
	; op X (0x58) size=4
	ld hl, 1
	call add1616
	; op N (0x4e) size=2
	; store word to address
	; load address of $count
	; load word from address
	call framefree
; Function: test_blocks(x:_short_) -> _short_
; Variable lifetimes:
;   temp: labels 0-0 (2 refs)
;   result: labels 0-0 (3 refs)
;   x: labels 0-0 (1 refs)
_test_blocks:
	ld a, 4
	call framealloc
	; load address of $result
	ld hl, 0
	; store word to address
	; load address of $temp
	; load address of $Ax
	; load word from address
	; op X (0x58) size=4
	ld hl, 2
	call mul1616
	; store word to address
	; load address of $result
	; load address of $temp
	; load word from address
	; op X (0x58) size=4
	ld hl, 5
	call add1616
	; op N (0x4e) size=2
	; store word to address
	; load address of $result
	; load word from address
	call framefree
; Function: test_complex_if(x:_short_) -> _short_
; Variable lifetimes:
;   c: labels 27-27 (2 refs)
;   b: labels 27-27 (2 refs)
;   a: labels 27-27 (2 refs)
;   x: labels 27-27 (3 refs)
_test_complex_if:
	ld a, 6
	call framealloc
	; load address of $Ax
	; load word from address
	; op X (0x58) size=4
	ld hl, 10
	call gt1616
	; load address of $a
	; load address of $Ax
	; load word from address
	; op X (0x58) size=4
	ld hl, 2
	call mul1616
	; store word to address
	; load address of $b
	; load address of $a
	; load word from address
	; op X (0x58) size=4
	ld hl, 5
	call add1616
	; store word to address
	; load address of $b
	; load word from address
	; load address of $c
	; load address of $Ax
	; load word from address
	; op X (0x58) size=4
	ld hl, 1
	call add1616
	; store word to address
	; load address of $c
	; load word from address
_if_end_28:
	call framefree
; Function: test_return_expr(y:_short_, x:_short_) -> _short_
; Variable lifetimes:
;   x: labels 0-0 (1 refs)
;   y: labels 0-0 (1 refs)
_test_return_expr:
	ld a, 0
	call framealloc
	; load address of $Ax
	; load word from address
	; load address of $Ay
	; load word from address
	call mul1616
	; op X (0x58) size=4
	ld hl, 10
	call add1616
	call framefree
; Function: test_return_void() -> _void_
_test_return_void:
