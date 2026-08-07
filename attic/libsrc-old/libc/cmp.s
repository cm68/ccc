;
; cmp.s - common comparison return points
;
.globl ret_true, ret_false

.text

; common return points for comparisons
; return NZ (A=1) if true, Z (A=0) if false
ret_true::
	ld	a,1
	or	a		; set NZ
	ret
ret_false::
	xor	a		; set Z
	ret
;
; vim: tabstop=4 shiftwidth=4 noexpandtab:
;
