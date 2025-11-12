; Function: test_mul() -> _short_
_test_mul:
	; load address of $result
	; load address of $_a
	; load byte from address
	; op X (0x58) size=2
	; load address of $_b
	; load word from address
	call mul816
	; store word to address
	; load address of $result
	; load word from address
; Function: test_unsigned_mul() -> _short_
_test_unsigned_mul:
	; load address of $result
	; load address of $_ua
	; load byte from address
	; op W (0x57) size=2
	; load address of $_ub
	; load word from address
	call umul816
	; store word to address
	; load address of $result
	; load word from address
; Function: test_div() -> _short_
_test_div:
	; load address of $result
	; load address of $_b
	; load word from address
	; load address of $_a
	; load byte from address
	; op X (0x58) size=2
	call div168
	; store word to address
	; load address of $result
	; load word from address
