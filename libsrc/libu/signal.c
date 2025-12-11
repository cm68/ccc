/*
 * this is a hack to make the signal handler save registers in and out
 */

unsigned short stab[15];
extern struct tramp { char v[6]; } jtab[];

short 
signal(unsigned short sig, unsigned short handler)
{
	short ret;

	if (sig < 1 || sig > 15) {
		return -1;
	}
	if (handler == 0 || handler == 1) {
		ret = _signal(sig, handler);
	} else {
		stab[sig - 1] = handler;
		ret = _signal(sig, &jtab[sig - 1]);
	}
	if (!(ret == 1 || ret == 0 || ret == -1)) {
		ret = stab[sig - 1];
	} 
	return ret;
}

asm {
.text:
__signal:
		pop		de
		pop		hl
		ld		(func),hl
		pop		hl
		ld		(sig),hl
		push	hl
		push	hl
		push	de
		rst		08h
		.db		0
		.dw		sys
		ret		nc
		ld		(_errno),hl
		ld		hl,-1
		ret

.data:
sys:	.db	0xcf
		.db 0x30
sig:	.dw	0
func:	.dw	0

.text:

_jtab:
		push 	hl
		ld 		hl,(_stab)
		jr 		sigcall

		push 	hl
		ld 		hl,(_stab+2)
		jr 		sigcall

		push	hl
		ld		hl,(_stab+4)
		jr		sigcall

		push	hl
		ld		hl,(_stab+6)
		jr 		sigcall

		push	hl
		ld		hl,(_stab+8)
		jr 		sigcall

		push	hl
		ld		hl,(_stab+10)
		jr 		sigcall

		push	hl
		ld		hl,(_stab+12)
		jr 		sigcall

		push	hl
		ld		hl,(_stab+14)
		jr 		sigcall

		push	hl
		ld		hl,(_stab+16)
		jr 		sigcall

		push	hl
		ld		hl,(_stab+18)
		jr 		sigcall

		push	hl
		ld		hl,(_stab+20)
		jr 		sigcall

		push	hl
		ld		hl,(_stab+22)
		jr 		sigcall

		push	hl
		ld		hl,(_stab+24)
		jr 		sigcall

		push	hl
		ld		hl,(_stab+26)
		jr 		sigcall

		push	hl
		ld		hl,(_stab+28)
		jr 		sigcall

sigcall:
		push	de
		push	bc
		exx
		push	hl
		push	de
		push	bc
		exx
		ex		af,af'
		push	af
		ex		af,af'
		push	af
		push	ix
		push	iy
		call	sjmp
		pop	 	iy
		pop	 	ix
		pop	 	af
		ex	 	af,af'
		pop	 	af
		ex	 	af,af'
		pop	 	af
		exx
		pop	 	bc
		pop		de
		pop	 	hl
		exx
		pop	 	bc
		pop		de
		pop	 	hl
		ret

sjmp:
		jp		(hl)
}

/*
 * vim: tabstop=4 shiftwidth=4 noexpandtab:
 */
