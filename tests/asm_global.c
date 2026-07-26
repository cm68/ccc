/*
 * Test asm block at file scope, mixed with globals and a function.
 * File-scope asm goes straight to the assembly stream; a block inside
 * a function rides the AST so it stays in place relative to the code.
 */

asm {
thunk::
	ld a,42
	ret
}

int g;

char msg[] = "it's \"quoted\"";

void setg()
{
	g = 1;
	asm {
		ld hl,7
		ld (_g),hl
	}
	g = 2;
}
