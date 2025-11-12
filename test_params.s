; Function: add(y:_short_, x:_short_) -> _short_
_add:
	; load address of $result
	; load address of $Ax
	; load word from address
	; load address of $Ay
	; load word from address
	call add1616
	; store word to address
	; load address of $result
	; load word from address
; Function: test_multi(c:_long_, b:_short_, a:_char_) -> _short_
_test_multi:
	; load address of $local_a
	; load address of $Aa
	; load byte from address
	; store byte to address
	; load address of $local_b
	; load address of $Ab
	; load word from address
	; store word to address
	; load address of $local_b
	; load word from address
