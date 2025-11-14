; Function: timeout_handler(sig:_short_) -> _short_
; Variable lifetimes:
;   sig: unused (0 refs)
_timeout_handler:
	ld a, 0
	call framealloc
	; load address of $_fdprintf
	; load address of $_str0
	ld hl, 2
	; op @ (0x40) size=2
	; load address of $_fdprintf
	; load address of $_str1
	ld hl, 2
	; op @ (0x40) size=2
	; load address of $_exit
	ld hl, 1
	; op @ (0x40) size=2
	call framefree
; Function: usage(complaint) -> :ptr
; Variable lifetimes:
;   sig: unused (0 refs)
;   complaint: labels 0-0 (2 refs)
_usage:
	ld a, 2
	call framealloc
	; load address of $Acomplaint
	; load word from address
	; load address of $_fdprintf
	; load address of $Acomplaint
	; load word from address
	; load address of $_str2
	ld hl, 2
	; op @ (0x40) size=2
_if_0:
	; load address of $_fdprintf
	; load address of $_progname
	; load word from address
	; load address of $_str3
	ld hl, 2
	; op @ (0x40) size=2
	; load address of $_fdprintf
	; load address of $_str4
	ld hl, 2
	; op @ (0x40) size=2
	; load address of $_fdprintf
	; load address of $_str5
	ld hl, 2
	; op @ (0x40) size=2
	; load address of $_exit
	ld hl, 1
	; op @ (0x40) size=2
	call framefree
; Function: make_output_name(input_file) -> _short_
; Variable lifetimes:
;   len: labels 3-3 (5 refs)
;   output: labels 3-3 (5 refs)
;   dot: labels 1-3 (3 refs)
;   basename_start: labels 0-3 (8 refs)
;   complaint: unused (0 refs)
;   sig: unused (0 refs)
;   input_file: labels 0-1 (2 refs)
_make_output_name:
	ld a, 12
	call framealloc
	; load address of $basename_start
	; load address of $_strrchr
	; load address of $Ainput_file
	ld hl, 47
	; load word from address
	; op @ (0x40) size=2
	; store word to address
	; load address of $basename_start
	; load word from address
	; load address of $basename_start
	; op ? (0xef) size=2
	; load address of $basename_start
	; load address of $Ainput_file
	; load word from address
	; store word to address
_if_end_2:
	; load address of $dot
	; load address of $_strrchr
	; load address of $basename_start
	ld hl, 46
	; load word from address
	; op @ (0x40) size=2
	; store word to address
	; load address of $dot
	; load word from address
	; load address of $len
	; load address of $dot
	; load word from address
	; load address of $basename_start
	; load word from address
	call usub1616
	; store word to address
	; load address of $len
	; load address of $_strlen
	; load address of $basename_start
	; load word from address
	; op @ (0x40) size=2
	; store word to address
_if_end_4:
	; load address of $output
	; load address of $_malloc
	; load address of $len
	; load word from address
	; op X (0x58) size=4
	ld hl, 3
	call add1616
	; op @ (0x40) size=2
	; store word to address
	; load address of $_strncpy
	; load address of $output
	; load address of $basename_start
	; load address of $len
	; load word from address
	; load word from address
	; load word from address
	; op @ (0x40) size=2
	; load address of $output
	; load address of $len
	; load word from address
	call add1616
	ld hl, 0
	; op N (0x4e) size=1
	; store byte to address
	; load address of $_strcat
	; load address of $output
	; load address of $_str6
	; load word from address
	; op @ (0x40) size=2
	; load address of $output
	; load word from address
	call framefree
