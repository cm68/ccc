/*
 * block scaffolding: scope open/close, local capture, asm text and
 * the switch case cursor - split from parse.c so the statement
 * machine travels alone.
 */

#include <stdlib.h>
#include "p1core.h"
#include "p1expr.h"
#include "p1type.h"
#include "p1name.h"
#include "p1stmt.h"
#include "p1lex.h"
#include "p1swcnt.h"

char *blockname();


/*
 * Parse a braced block body.
 * Used by control structures that now always have braces.
 * Handles scope push/pop and expects BEGIN...END.
 * If emitHdr is true and phase==2, emit block header.
 */
void
parseBlockEx(int emitHdr)
{
	expect(BEGIN, ER_S_SB);
	pushScope(blockname());
	/* In phase 1, register block entry for nested blocks */
	if (phase == 1 && lexlevel > 2)
		enterBlkCnt();
	/* In phase 2, emit block header since we consumed BEGIN */
	if (emitHdr && phase == 2) {
		unsigned char cnt = popBlkCnt();
		emit1(AST_BLOCK);
		emit1(0);  /* no decls - hoisted to function */
		emit1(cnt);
	}
	statement();
	popScope();
	expect(END, ER_S_CC);
}

void
parseBlock(void)
{
	parseBlockEx(1);  /* emit header by default */
}

/*
 * Capture local variables from the current scope level
 * Returns a linked list of name structures (shallow copies)
 */
/*
 * One captured local, from the name that declared it.  Both the walk
 * at the end of a function and popScope's grab of a nested block's
 * locals make these, and they must agree about which fields survive.
 */
struct local *
mklocal(struct name *n)
{
	struct local *c = (struct local *)galloc(sizeof(struct local));

	c->id = n->id;
	c->type = n->type;
	c->kind = n->kind;
	c->level = n->level;
	c->sclass = n->sclass;
	c->static_id = n->static_id;
	c->ref_count = n->w.r.ref_count;
	c->agg_refs = n->w.r.agg_refs;
	c->reg = n->w.r.reg;
	c->addr_taken = n->w.r.addr_taken;
	c->blkid = n->w.r.blkid;
	c->frm_off = n->w.r.frm_off;
	c->next = NULL;
	return c;
}

struct local *
capLocals(void)
{
	struct local *locals_list = NULL;
	struct local *tail = NULL;
	struct name *n;
	struct local *copy;

	/* Traverse chain - current level names are at head */
	for (n = names; n->level == lexlevel; n = n->chain) {
		/* Skip tags, typedefs, and functions */
		if (n->is_tag || n->kind == kfdef)
			continue;
		/*
		 * kfdef is only the ones defined here.  A function
		 * DECLARED in a block - "extern short _pnum(), _fnum();",
		 * which is how K&R names a routine returning other than
		 * int - is an ordinary name carrying a function type, and
		 * it was captured as a local: the frame grew a slot for it
		 * and the call became "ld hl,(_pnum)", an indirect jump
		 * through the first two bytes of the routine's own code.
		 * doprnt declares _pnum that way, so printf("%d") ran off
		 * into the weeds and took the format loop with it.
		 *
		 * An extern names something with static storage wherever
		 * it is written, so it is never a frame slot either.
		 */
		if ((n->type && (n->type->flags & TF_FUNC)) ||
		    (n->sclass & SC_EXTERN))
			continue;

		/* Capture this variable */
		if (n->kind == kvar || n->kind == klocal || n->kind == kfunarg) {
			copy = mklocal(n);

			if (!locals_list) {
				locals_list = copy;
				tail = copy;
			} else {
				tail->next = copy;
				tail = copy;
			}
		}
	}

	/*
	 * Then the ones declared in nested blocks, which popScope put
	 * aside on its way out because this walk can only see one level.
	 * They come in the order they were declared, which is the order
	 * the blocks were entered, and that is what lets the frame
	 * allocator overlay siblings in a single pass.
	 */
	if (blockLocals) {
		if (tail)
			tail->next = blockLocals;
		else
			locals_list = blockLocals;
		clrblklocs();
	}

	return locals_list;
}

/*
 * Collect one slice of an asm block, leaving cur on the token after it.
 * cpp emits the ASM keyword, then one or more ASM tokens carrying the
 * raw text - a block over 255 bytes arrives in line-boundary slices,
 * each its own statement - then a SEMI after the last.  The keyword
 * and the text tokens share a type; the text is the one holding a
 * string, which is how a second slice is told from a second block.
 * Call with cur on the keyword or on a continuation slice.  Returns
 * malloc'd text (caller frees), or NULL for an empty block.
 */
char *
getAsmText(void)
{
	char *text = NULL;

	if (!cur.v.str)
		gettoken();		/* consume the keyword */
	if (cur.type == ASM && cur.v.str) {
		text = cur.v.str;
		cur.v.str = NULL;	/* we own it now */
		gettoken();
	}
	if (cur.type == SEMI)
		gettoken();
	return text;
}

/*
 * Parse statements recursively - the heart of the compiler frontend
 *
 * This function implements the statement parser for C, handling all control
 * flow structures, expressions, declarations, and blocks. It uses recursive
 * descent to parse nested statements.
 *
 * Statement types handled:
 *   - Blocks: { ... } with lexical scoping
 *   - Control flow: if/else, switch/case/default (loops handled by cpp)
 *   - Jumps: return, goto, labels (break/continue handled by cpp)
 *   - Expressions: function calls, assignments, operators
 *   - Declarations: local variables, typedefs (scoped to current block)
 *   - Inline assembly: asm { ... }
 *
 * Lexical scoping:
 *   - Each block pushes a new scope, pops on exit
 *   - Local variables are captured before scope pop to preserve metadata
 *   - Nested blocks can shadow outer names
 *
 * Two-phase operation:
 *   Phase 1: Build symbol table, count statements for streaming
 *   Phase 2: Emit AST bytecode using counts from phase 1
 */

/*
 * Moved here from decl.c: freeing a local's storage is the other
 * end of mklocal, and keeping it beside the parser meant the parser
 * needed the structure.
 */
/*
 * Free a function's locals list
 */
void
freeLocals(struct local *local)
{
	struct local *next;
	while (local) {
		next = local->next;
		free(local);
		local = next;
	}
}
