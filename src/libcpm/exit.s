	global	_exit, __cpm_clean

	psect	text
_exit:
	ld	(80h),hl	;store exit status
	call	__cpm_clean
	jp	0		;Warm boot CP/M
