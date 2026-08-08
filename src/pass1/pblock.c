/*
 * block scaffolding: scope open/close, local capture, asm text and
 * the switch case cursor - split from parse.c so the statement
 * machine travels alone.
 */

#include "p1core.h"
#include "p1expr.h"
#include "p1type.h"
#include "p1name.h"
#include "p1stmt.h"
#include "p1lex.h"

char *blockname();

extern unsigned char ifHasElse[];
extern unsigned short ifCount;
extern unsigned short ifEmitIdx;

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
 * The phase-2 statement arms, one worker each.  statement() carried
 * a bank of "hoisted locals - shared across cases to reduce stack
 * frame", which is the opposite of what this compiler wants: it does
 * no lifetime analysis, by design, so sharing a frame is sharing two
 * registers among locals that never coexist.  The function boundary
 * is the lifetime analysis.
 */

/* the case the current switch is up to, shared by CASE and DEFAULT */
struct swcase *
nextCase(void)
{
    register struct swtab *sw;

    sw = &swList[swEmitStack[swEmitDepth - 1]];
    return &sw->cases[sw->emitIdx++];
}

/* if <condition> <statement> [else ...], cur on the IF */
void
stIf2(void)
{
    unsigned char hasElse;
    struct expr *e1;

    for (;;) {
        hasElse = (ifHasElse[ifEmitIdx >> 3] >> (ifEmitIdx & 7)) & 1;
        ifEmitIdx++;
        gettoken();
        expect(LPAR, ER_S_NP);
        e1 = parseExpr(PRI_ALL);
        expect(RPAR, ER_S_NP);
        /* Emit: IF nlabels cond then has_else [else] */
        /* fold first: emitExpr folds internally and may replace the
         * root node, leaving our e1 dangling for FreeExpr */
        e1 = foldTree(e1);
        emit1(IF);
        emit1(cntCondLbls(e1));
        emitExpr(e1);
        FreeExpr(e1);
        parseBlock();
        emit1(hasElse);  /* has_else comes after then block */
        if (cur.type == ELSE) {
            gettoken();
            if (cur.type == IF)
                continue;       /* else if: run the arm again */
            parseBlock();
        }
        return;
    }
}

void
stRet2(void)
{
    struct expr *e1;

    e1 = NULL;
    gettoken();
    if (cur.type != SEMI)
        e1 = parseExpr(PRI_ALL);
    expect(SEMI, ER_S_SN);
    /* a struct has no value to return - return its address */
    if (e1 && e1->type && (e1->type->flags & TF_AGGREGATE) &&
        !(e1->type->flags & (TF_POINTER | TF_ARRAY | TF_FUNC)))
        gripe(ER_E_AG);
    /* Emit: RETURN has_value [expr] */
    e1 = foldTree(e1);
    emit1(RETURN);
    emit1(e1 ? 1 : 0);
    if (e1) {
        /*
         * The value has to arrive at the width the function
         * was declared to return, and only the tree knows
         * whether to sign- or zero-extend.  Without this,
         * "long f() { return 7; }" loaded HL alone - which is
         * the high half of a long, so it returned 458752 and
         * whatever DE happened to hold.
         */
        emitOperand(e1, curFunc ? curFunc->type->sub : NULL);
        FreeExpr(e1);
    }
}

void
stSwitch2(void)
{
    struct expr *e1;
    unsigned char sw_idx;

    gettoken();
    expect(LPAR, ER_S_NP);
    e1 = parseExpr(PRI_ALL);
    expect(RPAR, ER_S_NP);
    expect(BEGIN, ER_S_SB);
    /* Get this switch's index and push onto emit stack */
    sw_idx = swEmitIdx++;
    swEmitStack[swEmitDepth++] = sw_idx;
    swList[sw_idx].emitIdx = 0;
    /* Emit switch header: SWITCH has_label case_count expr */
    /* has_label=0 since cpp handles break lowering */
    e1 = foldTree(e1);
    emit1(SWITCH);
    emit1(0);  /* no label - cpp lowered break to goto */
    emit1(popCount());
    emitExpr(e1);
    FreeExpr(e1);
    /* Parse body - CASE/DEFAULT emit themselves */
    statement();
    swEmitDepth--;
    expect(END, ER_S_CC);
}

void
stExpr2(void)
{
    struct expr *e;

    e = parseExpr(PRI_ALL);
    expect(SEMI, ER_S_SN);
    /* Convert postinc/postdec to preinc/predec since result unused */
    if (e && (e->op == INCR || e->op == DECR) && (e->flags & E_POSTFIX))
        e->flags &= ~E_POSTFIX;
    /* Emit expression statement directly (no EXPR wrapper) */
    e = foldTree(e);
    emitExpr(e);
    FreeExpr(e);
}

void
stGoto2(void)
{
    unsigned short lblid;

    gettoken();
    if (cur.type != SYM) {
        recover(ER_S_GL, SEMI);
        return;
    }
    /* Copy the label before gettoken overwrites cur.v.id */
    lblid = cur.v.id;
    gettoken();
    expect(SEMI, ER_S_SN);
    /* Emit: GOTO label */
    emit1(GOTO);
    emitS(nameOf(lblid));
}
