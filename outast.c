/*
 * AST serialization for second pass
 * Hex-based format: names as <2-hex-len><hex-bytes>, numbers as hex with '.'
 */
#include "cc1.h"

/* Forward declarations */
static void emitTypeInfo(struct type *type);
static int assignFrmOff(struct name *func);

/*
 * Jump-to-jump optimization: when LABEL is followed only by GOTO,
 * record mapping so GOTOs to that label can go directly to target.
 */
#define MAX_JMP_MAP 32
static struct {
	char *label;   /* label name */
	char *target;  /* goto target */
} jmpMap[MAX_JMP_MAP];
static int jmpMapCnt;

/* Find target for a label, returns label itself if no mapping */
static char *
resolveJmp(char *label)
{
	int i, j;
	char *cur = label;
	/* Follow chain (max 8 hops to avoid loops) */
	for (j = 0; j < 8; j++) {
		for (i = 0; i < jmpMapCnt; i++) {
			if (jmpMap[i].label && cur &&
			    strcmp(jmpMap[i].label, cur) == 0) {
				cur = jmpMap[i].target;
				break;
			}
		}
		if (i == jmpMapCnt) break;  /* no mapping found */
	}
	return cur;
}

/* Add a jump mapping: gotos to 'from' should go to 'to' instead */
static void
addJmpMap(const char *from, const char *to)
{
	if (jmpMapCnt < MAX_JMP_MAP && from && to) {
		jmpMap[jmpMapCnt].label = strdup(from);
		jmpMap[jmpMapCnt].target = strdup(to);
		jmpMapCnt++;
	}
}

/*
 * Variable usage analysis for register allocation
 * Walk expression/statement trees counting variable references
 */

/* Find a local/param variable by name in function scope - currently unused
 * but kept for potential future use in pass1 optimizations */
#if 0
static struct name *
findLocal(const char *name, struct stmt *body)
{
	struct name *n;
	if (!body || !body->locals || !name)
		return NULL;
	for (n = body->locals; n; n = n->next) {
		if (n->name && strcmp(n->name, name) == 0)
			return n;
	}
	return NULL;
}
#endif

/* Find a variable in body->locals by name */
static struct name *
findInLocals(const char *name, struct stmt *body)
{
	struct name *n;
	if (!body || !body->locals || !name)
		return NULL;
	for (n = body->locals; n; n = n->next) {
		if (n->name && strcmp(n->name, name) == 0)
			return n;
	}
	return NULL;
}

/* Increment ref_count for a variable (capped at 255) */
static void
incRef(struct name *n)
{
	if (n && n->ref_count < 255)
		n->ref_count++;
}

/* Increment agg_refs for a variable (capped at 255) */
static void
incAgg(struct name *n)
{
	if (n && n->agg_refs < 255)
		n->agg_refs++;
}

/* Forward declarations */
static void analyzeExpr(struct expr *e, struct stmt *body);
static void analyzeStmt(struct stmt *s, struct stmt *body);

/* Analyze expression tree for variable usage */
static void
analyzeExpr(struct expr *e, struct stmt *body)
{
	struct name *var, *local;
	struct expr *left, *ll, *lll;
	unsigned char op;

	if (!e)
		return;

	op = e->op;
	left = e->left;

	/* Count variable references from SYM nodes
	 * Look up the variable in body->locals by name since e->var
	 * points to the original symbol table entry (now gone) while
	 * body->locals contains copies made by capLocals() */
	if (op == SYM && e->var) {
		var = (struct name *)e->var;
		if (var->name) {
			local = findInLocals(var->name, body);
			if (local)
				incRef(local);
		}
	}

	/* Detect struct member access patterns for IX allocation:
	 * Pattern: DEREF(ADD(DEREF($ptr), offset)) - reading member
	 * Pattern: ASSIGN(ADD(DEREF($ptr), offset), val) - writing member
	 * The inner DEREF($ptr) must be a pointer variable */
	ll = left ? left->left : 0;
	lll = ll ? ll->left : 0;
	if ((op == DEREF || op == '=') && left && left->op == '+' &&
	    ll && ll->op == DEREF && lll && lll->op == SYM && lll->var &&
	    left->right && left->right->op == CONST) {
		var = (struct name *)lll->var;
		if (var->name) {
			local = findInLocals(var->name, body);
			if (local && local->type && (local->type->flags & TF_POINTER))
				incAgg(local);
		}
	}

	/* Recurse */
	analyzeExpr(left, body);
	analyzeExpr(e->right, body);
	analyzeExpr(e->next, body);
}

/* Analyze statement tree for variable usage */
static void
analyzeStmt(struct stmt *s, struct stmt *body)
{
	if (!s)
		return;

	/* Analyze expressions in this statement */
	analyzeExpr(s->left, body);
	analyzeExpr(s->right, body);
	analyzeExpr(s->middle, body);

	/* Recurse into child statements */
	analyzeStmt(s->chain, body);
	analyzeStmt(s->otherwise, body);
	analyzeStmt(s->next, body);
}

/*
 * Register allocation for local variables and parameters
 * Called after analyzeStmt() has computed ref_count and agg_refs
 *
 * Allocation priority:
 *   1. IX register: allocated to struct pointer with highest agg_refs
 *   2. BC register: allocated to word variable with highest ref_count
 *   3. B/C registers: allocated to byte variables by ref_count
 *
 * Variables excluded:
 *   - Arrays (must remain on stack for &arr[i])
 *   - Unused variables (ref_count == 0)
 *   - Single-use variables (ref_count == 1) - no benefit
 */
static void
allocRegs(struct stmt *body)
{
	struct name *n, *best;
	int bc_used = 0;  /* BC allocated? (precludes B and C) */
	int b_used = 0, c_used = 0;
	int ix_used = 0;
	int has_reg_hint = 0;

	if (!body || !body->locals)
		return;

	/* Check if any locals have explicit 'register' storage class */
	for (n = body->locals; n; n = n->next) {
		if (n->sclass & SC_REGISTER)
			has_reg_hint = 1;
	}

	/* First: allocate register-marked variables with preferences */
	if (has_reg_hint) {
		int has_reg_byte = 0;
		/* Check if any register-marked bytes exist */
		for (n = body->locals; n; n = n->next) {
			if ((n->sclass & SC_REGISTER) && n->type &&
			    n->type->size == 1)
				has_reg_byte = 1;
		}
		/* Pass 1: pointers prefer IX */
		for (n = body->locals; n; n = n->next) {
			if (!(n->sclass & SC_REGISTER) || n->reg != REG_NONE)
				continue;
			if (n->type && (n->type->flags & TF_POINTER) && !ix_used) {
				n->reg = REG_IX;
				ix_used = 1;
			}
		}
		/* Pass 2: words - prefer IX if bytes need B/C, else BC */
		for (n = body->locals; n; n = n->next) {
			if (!(n->sclass & SC_REGISTER) || n->reg != REG_NONE)
				continue;
			if (n->type && n->type->size == 2) {
				if (has_reg_byte && !ix_used) {
					n->reg = REG_IX;
					ix_used = 1;
				} else if (!bc_used) {
					n->reg = REG_BC;
					bc_used = 1;
				} else if (!ix_used) {
					n->reg = REG_IX;
					ix_used = 1;
				}
			}
		}
		/* Pass 3: bytes get B then C */
		for (n = body->locals; n; n = n->next) {
			if (!(n->sclass & SC_REGISTER) || n->reg != REG_NONE)
				continue;
			if (n->type && n->type->size == 1 && !bc_used) {
				if (!b_used) {
					n->reg = REG_B;
					b_used = 1;
				} else if (!c_used) {
					n->reg = REG_C;
					c_used = 1;
				}
			}
		}
	}

	/* Second: allocate remaining registers to unmarked vars by usage */

	/* IX to struct pointer with highest agg_refs */
	if (!ix_used) {
		best = NULL;
		for (n = body->locals; n; n = n->next) {
			if (n->reg != REG_NONE)
				continue;
			if (n->type && (n->type->flags & TF_POINTER) &&
			    n->agg_refs > 0 && n->ref_count > 1) {
				if (!best || n->agg_refs > best->agg_refs)
					best = n;
			}
		}
		if (best) {
			best->reg = REG_IX;
			ix_used = 1;
		}
	}

	/* BC (or IX if BC taken) to word variable with highest ref_count */
	if (!bc_used || !ix_used) {
		best = NULL;
		for (n = body->locals; n; n = n->next) {
			if (n->reg != REG_NONE)
				continue;
			if (n->type && (n->type->flags & (TF_ARRAY | TF_AGGREGATE)))
				continue;
			if (n->ref_count <= 1)
				continue;
			if (n->type && n->type->size == 2) {
				if (!best || n->ref_count > best->ref_count)
					best = n;
			}
		}
		if (best) {
			if (!bc_used) {
				best->reg = REG_BC;
				bc_used = 1;
			} else {
				best->reg = REG_IX;
				ix_used = 1;
			}
		}
	}

	/* B and C to byte variables (if BC not used as word) */
	if (!bc_used) {
		for (n = body->locals; n; n = n->next) {
			if (n->reg != REG_NONE)
				continue;
			if (n->type && (n->type->flags & (TF_ARRAY | TF_AGGREGATE)))
				continue;
			if (n->ref_count <= 1)
				continue;
			if (n->type && n->type->size == 1) {
				if (!b_used) {
					n->reg = REG_B;
					b_used = 1;
				} else if (!c_used) {
					n->reg = REG_C;
					c_used = 1;
				}
				if (b_used && c_used)
					break;
			}
		}
	}
}

/* Collect locals from nested blocks into function body's locals list */
static void
collectLocl(struct stmt *s, struct stmt *body)
{
	struct name *tail;
	if (!s || !body)
		return;
	/* If this nested block has locals, append them to body->locals */
	if (s != body && s->locals) {
		/* Find tail of body->locals */
		tail = body->locals;
		if (tail) {
			while (tail->next)
				tail = tail->next;
			tail->next = s->locals;
		} else {
			body->locals = s->locals;
		}
		s->locals = NULL;  /* Moved to body */
	}
	/* Recurse */
	collectLocl(s->chain, body);
	collectLocl(s->otherwise, body);
	collectLocl(s->next, body);
}

/* Entry point: analyze function and allocate registers, returns frame size */
static int
analyzeFunc(struct name *func)
{
	struct name *n;

	if (!func || !func->u.body)
		return 0;

	/* First collect all locals from nested blocks */
	collectLocl(func->u.body, func->u.body);

	/* Initialize all locals/params to no register, zero counts */
	for (n = func->u.body->locals; n; n = n->next) {
		n->ref_count = 0;
		n->agg_refs = 0;
		n->reg = REG_NONE;
	}

	/* Walk the tree counting references */
	analyzeStmt(func->u.body, func->u.body);

	/* Allocate registers based on usage */
	allocRegs(func->u.body);

	/* Assign frame offsets to non-register vars, return frame size */
	return assignFrmOff(func);
}

/*
 * Assign stack frame offsets to parameters and locals.
 * Params get positive offsets (above FP), locals get negative (below FP).
 * Register-allocated variables get frm_off=0 (not on stack).
 * Returns frame size (bytes needed for locals on stack).
 */
static int
assignFrmOff(struct name *func)
{
	struct name *n;
	struct stmt *body;
	int off;

	if (!func || !func->type || !func->u.body)
		return 0;
	body = func->u.body;

	/* Parameters: positive offsets starting at +4 (skip saved FP + ret addr) */
	off = 4;
	for (n = func->type->elem; n; n = n->next) {
		if (n->type && n->type->size == 0)
			continue;  /* skip void */
		/* Find matching local to set its frm_off */
		if (body->locals && n->name) {
			struct name *local;
			for (local = body->locals; local; local = local->next) {
				if (local->name && strcmp(local->name, n->name) == 0) {
					/* Params always need frame offset (passed on stack) */
					local->frm_off = off;
					break;
				}
			}
		}
		off += (n->type && n->type->size > 2) ? n->type->size : 2;
	}

	/* Locals: negative offsets for non-register vars */
	off = 0;
	for (n = body->locals; n; n = n->next) {
		if (n->kind == funarg)
			continue;
		if (n->reg) {
			n->frm_off = 0;  /* in register, not on stack */
		} else {
			int sz = (n->type) ? n->type->size : 2;
			if (sz < 1) sz = 2;
			off += sz;
			n->frm_off = -off;
		}
	}
	return off;  /* frame size = total local stack space */
}

/*
 * Get size suffix for memory operations based on type
 * Returns: 'b' (byte), 's' (short/int), 'l' (long), 'p' (pointer),
 * 'f' (float), 'd' (double), 'v' (void)
 * Uppercase B/S/L for unsigned types
 */
static char
typeSfx(struct type *t)
{
	char c;
	if (!t)
		return 's';  /* default to short */

	if (t->flags & TF_POINTER)
		return 'p';

	/* Check primitive types by size */
	if (t->size == 0)
		return 'v';  /* void */
	else if (t->size == 1)
		c = 'b';  /* char/byte */
	else if (t->size == 2)
		c = 's';  /* short/int */
	else if (t->size == 4) {
		if (t->flags & TF_FLOAT)
			return 'f';  /* float/double */
		c = 'l';  /* long */
	} else
		c = 's';  /* default to short */

	/* Uppercase for unsigned */
	if (t->flags & TF_UNSIGNED)
		c = c - 'a' + 'A';
	return c;
}

/* Emit number as 4 hex digits (unsigned 16-bit) */
static void
emitHexNum(long v)
{
	fdprintf(astFd, "%04lx", (unsigned long)(v & 0xffff));
}

/* Emit number as 8 hex digits (two's complement) - for constants */
static void
emitHexNum32(long v)
{
	fdprintf(astFd, "%08lx", (unsigned long)v);
}

/* Emit string as hex-length-prefixed ASCII */
static void
emitHexName(const char *s)
{
	int len = strlen(s);
	fdprintf(astFd, "%02x%s", len, s);
}

/* Helper: build label name from base+suffix (4 rotating buffers) */
static char *
mkLbl(const char *base, const char *suffix)
{
	static char buf[4][32];
	static int idx;
	char *p = buf[idx++ & 3];
	snprintf(p, 32, "%s%s", base, suffix);
	return p;
}

/* Emit a label or goto statement with hex-encoded name */
static void
emitLG(char op, const char *base, const char *suffix)
{
	char *name = mkLbl(base, suffix);
	fdprintf(astFd, "%c", op);
	/* For GOTO, resolve through jump map */
	emitHexName(op == 'G' ? resolveJmp(name) : name);
}
#define emitLabel(b,s) emitLG('L',b,s)
#define emitGoto(b,s)  emitLG('G',b,s)

/*
 * Helper: emit child expression (if non-null)
 */
static void emitExpr(struct expr *e);  /* forward declaration */

static void
emitChild(struct expr *e)
{
	if (e)
		emitExpr(e);
}

/*
 * Context for counting intermediate labels in conditions.
 * See CONDITIONS.md for full explanation.
 */
#define CTX_TOP      0  /* top-level condition */
#define CTX_OR_LEFT  1  /* left child of || */
#define CTX_OR_RIGHT 2  /* right child of || */
#define CTX_AND_LEFT 3  /* left child of && */
#define CTX_AND_RIGHT 4 /* right child of && */

/*
 * Count intermediate labels needed for short-circuit evaluation.
 * Returns number of labels beyond the basic yes/el/no.
 */
static unsigned char
cntCondLbls(struct expr *e, unsigned char ctx)
{
	struct expr *left, *right;
	unsigned char op, count = 0;

	if (!e)
		return 0;

	op = e->op;
	left = e->left;
	right = e->right;

	/* NOT just inverts sense, pass through */
	if (op == '!') {
		return cntCondLbls(left, ctx);
	}

	/* OR (||) */
	if (op == LOR) {
		/* || inside && (right side) or at top needs intermediate label */
		if (ctx == CTX_AND_RIGHT || ctx == CTX_TOP) {
			count = 1;
			count += cntCondLbls(left, CTX_OR_LEFT);
			count += cntCondLbls(right, CTX_OR_RIGHT);
		} else {
			/* chained || shares parent's target */
			count = cntCondLbls(left, CTX_OR_LEFT);
			count += cntCondLbls(right, ctx);
		}
		return count;
	}

	/* AND (&&) */
	if (op == LAND) {
		/* && inside || (right side) or at top needs intermediate label */
		if (ctx == CTX_OR_RIGHT || ctx == CTX_TOP) {
			count = 1;
			count += cntCondLbls(left, CTX_AND_LEFT);
			count += cntCondLbls(right, CTX_AND_RIGHT);
		} else {
			/* chained && shares parent's target */
			count = cntCondLbls(left, CTX_AND_LEFT);
			count += cntCondLbls(right, ctx);
		}
		return count;
	}

	/* Leaf node - no intermediate labels needed */
	return 0;
}

/*
 * Output an expression in paren-free format
 * Constants: just the value (hex with dot)
 * Symbols: $name
 * Binary ops: op width left right
 * Unary ops: op width operand
 * Memory ops annotated with size: Mb expr, =l lvalue rvalue
 * Empty/null expression: _
 */
static void
emitExpr(struct expr *e)
{
	struct name *sym;
	struct expr *left, *right;
	struct type *type;
	unsigned char op;
	char *name;

	if (!e) {
		fdprintf(astFd, "_");
		return;
	}

	op = e->op;
	left = e->left;
	right = e->right;
	type = e->type;

	switch (op) {
	case CONST:
		fdprintf(astFd, "#%c", typeSfx(type));
		emitHexNum32(e->v);
		break;

	case SYM:
		if (e->var) {
			char fullname[256];
			sym = (struct name *)e->var;
			/* extern/global get underscore prefix, statics use mangled name */
			if ((sym->sclass & SC_EXTERN) ||
			    (sym->level == 1 && !(sym->sclass & SC_STATIC)))
				snprintf(fullname, sizeof(fullname), "_%s", sym->name);
			else {
				name = sym->mangled_name ? sym->mangled_name : sym->name;
				snprintf(fullname, sizeof(fullname), "%s", name);
			}
			fdprintf(astFd, "$");
			emitHexName(fullname);
		} else {
			fdprintf(astFd, "$01?");
		}
		break;

	case STRING:
		/* String literals - emit the string data and reference it */
		if (e->var) {
			struct name *strname = (struct name *)e->var;
			/* Emit string literal if not already emitted */
			emitStrLit(strname);
			/* Synthetic string names are local - no _ prefix */
			fdprintf(astFd, "$");
			emitHexName(strname->name);
		} else {
			/* Fallback to address if name not available */
			fdprintf(astFd, "S");
			emitHexNum(e->v);
		}
		break;

	case CALL:
		/* Function call: @type count. func arg1 arg2 ... */
		{
			int argc = 0;
			struct expr *arg;
			char ret_type = typeSfx(type);
			for (arg = right; arg; arg = arg->next) argc++;
			fdprintf(astFd, "@%c%02x", ret_type, argc);
			emitChild(left);
			for (arg = right; arg; arg = arg->next)
				emitChild(arg);
		}
		break;

	case NARROW:
	case WIDEN:
	case SEXT:
		/* Cast operators with destination width annotation */
		{
			char size_suffix = typeSfx(type);
			unsigned char op_char = (op == NARROW) ? 'N' :
			    (op == WIDEN) ? 'W' : AST_SEXT;
			fdprintf(astFd, "%c%c", op_char, size_suffix);
			emitChild(left);
		}
		break;

	case COPY:
		/* Memory copy operator: Y length. dest src */
		fdprintf(astFd, "Y");
		emitHexNum(e->v);  /* v field contains byte count */
		emitChild(left);
		emitChild(right);
		break;

	case INCR:
	case DECR:
		/* Increment/decrement operators: emit with increment amount */
		/* For pointers, amount is size of pointed-to type */
		/* For scalars, amount is 1 */
		{
			unsigned char op_char;
			int amount = 1;
			char size_suffix = typeSfx(type);

			if (op == INCR) {
				op_char = (e->flags & E_POSTFIX) ? AST_POSTINC : AST_PREINC;
			} else {
				op_char = (e->flags & E_POSTFIX) ? AST_POSTDEC : AST_PREDEC;
			}

			/* Calculate increment amount based on type */
			if (type && (type->flags & TF_POINTER) && type->sub) {
				amount = type->sub->size;
			}

			fdprintf(astFd, "%c%c", op_char, size_suffix);
			emitChild(left);
			emitHexNum(amount);
		}
		break;

	case BFEXTRACT:
		/* Bitfield extract: AST_BFEXTRACT offset width addr */
		{
			struct name *member = (struct name *)e->var;
			fdprintf(astFd, "%c", AST_BFEXTRACT);
			if (member) {
				fdprintf(astFd, "%02x%02x", member->bitoff, member->width);
			} else {
				fdprintf(astFd, "0000");  /* fallback */
			}
			emitChild(left);
		}
		break;

	case BFASSIGN:
		/* Bitfield assign: AST_BFASSIGN offset width addr value */
		{
			struct name *member = (struct name *)e->var;
			fdprintf(astFd, "%c", AST_BFASSIGN);
			if (member) {
				fdprintf(astFd, "%02x%02x", member->bitoff, member->width);
			} else {
				fdprintf(astFd, "0000");  /* fallback */
			}
			emitChild(left);
			emitChild(right);
		}
		break;

	case QUES:
		/* Ternary: ?w nlabels cond then else - flatten the COLON node */
		{
			unsigned char nlabels = cntCondLbls(left, CTX_TOP);
			fdprintf(astFd, "?%c%02x", typeSfx(type), nlabels);
			emitChild(left);
			if (right && right->op == COLON) {
				emitChild(right->left);
				emitChild(right->right);
			}
		}
		break;

	case SUBEQ:
	case ANDEQ:
	case MODEQ:
		/* Compound assignment operators with high-bit tokens - map to ASCII */
		{
			unsigned char op_char = (op == SUBEQ) ? AST_SUBEQ :
			    (op == ANDEQ) ? AST_ANDEQ : AST_MODEQ;
			fdprintf(astFd, "%c%c", op_char, typeSfx(type));
			emitChild(left);
			emitChild(right);
		}
		break;

	case INITLIST:
		/* Nested initializer list - emit contents */
		{
			struct expr *it;
			int cnt = 0;
			for (it = left; it; it = it->next) cnt++;
			fdprintf(astFd, "{%02x", cnt);
			for (it = left; it; it = it->next)
				emitExpr(it);
			fdprintf(astFd, "}");
		}
		break;

	default:
		/* Optimize: *++p -> (++p, *p) using comma operator
		 * This lets pass2 see simple inc + simple deref */
		if (op == DEREF && left &&
		    (left->op == INCR || left->op == DECR) &&
		    !(left->flags & E_POSTFIX)) {
			/* Emit: ,type (++p) (M type p) */
			fdprintf(astFd, ",%c", typeSfx(type));
			emitExpr(left);  /* the ++p */
			fdprintf(astFd, "M%c", typeSfx(type));
			emitExpr(left->left);  /* just p */
			break;
		}
		/* All operators get width suffix: op width operands... */
		fdprintf(astFd, "%c%c", op, typeSfx(type));
		emitChild(left);
		emitChild(right);
		break;
	}
}

/*
 * Output type information for AST
 * Hex format: 'a' count. elemsize. for arrays, 'p' for ptr, size char otherwise
 */
static void
emitTypeInfo(struct type *type)
{
	if (!type)
		return;

	/* For arrays: a count. elemsize. */
	if (type->flags & TF_ARRAY) {
		int elemsize = type->sub ? type->sub->size : 0;
		fdprintf(astFd, "a");
		emitHexNum(type->count);
		emitHexNum(elemsize);
		return;
	}

	/* For pointers: p */
	if (type->flags & TF_POINTER) {
		fdprintf(astFd, "p");
		return;
	}

	/* For aggregates: r size. */
	if (type->flags & TF_AGGREGATE) {
		fdprintf(astFd, "r");
		emitHexNum(type->size);
		return;
	}

	/* For primitives: size char */
	fdprintf(astFd, "%c", typeSfx(type));
}

/*
 * Count statements in a chain
 */
/*
 * Check if a statement will be eliminated by DCE
 */
static int
isDCE(struct stmt *st)
{
	if (!st || st->op != IF)
		return 0;
	/* IF with constant condition and no output */
	if (st->left && (st->left->flags & E_CONST)) {
		if (st->left->v == 0 && !st->otherwise)
			return 1;  /* if(0) with no else */
		if (st->left->v != 0 && !st->chain)
			return 1;  /* if(non-zero) with empty then */
	}
	return 0;
}

static int
countStmts(struct stmt *st)
{
	int count = 0;
	while (st) {
		if (!isDCE(st))
			count++;
		st = st->next;
	}
	return count;
}

/*
 * Output a statement in paren-free format
 * Each statement type has its own format with counted children
 */
static void
emitStmt(struct stmt *st)
{
	if (!st)
		return;

	/* Output this statement */
	switch (st->op) {
	case BEGIN:
		{
			int stmt_count = countStmts(st->chain);
			struct stmt *s;

			/* Emit: B 00 stmt_count stmts...
			 * All locals hoisted to function prolog, so decl_count=0 */
			fdprintf(astFd, "B00%02x", stmt_count);

			/* Emit statements */
			for (s = st->chain; s; s = s->next)
				emitStmt(s);
		}
		break;

	case IF:
		/* Dead code elimination for constant conditions */
		if (st->left && (st->left->flags & E_CONST)) {
			if (st->left->v == 0)
				emitStmt(st->otherwise);  /* if (0) - emit only else */
			else
				emitStmt(st->chain);      /* if (non-zero) - emit only then */
		} else {
			/* If: I flags nlabels cond then [else]
			 * flags: bit 0 = has_else
			 * nlabels: intermediate labels for ||/&& short-circuit */
			unsigned char nlabels = cntCondLbls(st->left, CTX_TOP);
			fdprintf(astFd, "I%02x%02x", st->otherwise ? 1 : 0, nlabels);
			emitExpr(st->left);
			if (st->chain)
				emitStmt(st->chain);
			else
				fdprintf(astFd, ";");  /* empty statement */
			emitStmt(st->otherwise);
		}
		break;

	case WHILE:
		/* Emit as labeled sequence wrapped in block
		 * All loops have labels assigned by parse.c */
		{
			unsigned char nlabels = cntCondLbls(st->left, CTX_TOP);
			/* WHILE has no incr, so _continue goes straight to _top */
			addJmpMap(mkLbl(st->label, "_continue"),
			          mkLbl(st->label, "_top"));
			fdprintf(astFd, "B0005");  /* 5 stmts: label, if, label, goto, label */
			emitLabel(st->label, "_top");
			fdprintf(astFd, "I01%02x", nlabels);  /* has else */
			emitExpr(st->left);
			if (st->chain)
				emitStmt(st->chain);
			else
				fdprintf(astFd, ";");
			/* else: block with goto break */
			fdprintf(astFd, "B0001");
			emitGoto(st->label, "_break");
			emitLabel(st->label, "_continue");
			emitGoto(st->label, "_top");
			emitLabel(st->label, "_break");
		}
		break;

	case DO:
		/* Emit as labeled sequence wrapped in block
		 * All loops have labels assigned by parse.c */
		{
			unsigned char nlabels = cntCondLbls(st->left, CTX_TOP);
			fdprintf(astFd, "B0005");  /* 5 stmts: top, body, test, if, break */
			emitLabel(st->label, "_top");
			if (st->chain)
				emitStmt(st->chain);
			else
				fdprintf(astFd, ";");
			emitLabel(st->label, "_test");
			fdprintf(astFd, "I00%02x", nlabels);  /* no else */
			emitExpr(st->left);
			emitGoto(st->label, "_top");
			emitLabel(st->label, "_break");
		}
		break;

	case FOR:
		/* Emit as labeled sequence wrapped in block
		 * All loops have labels assigned by parse.c */
		{
			unsigned char nlabels = cntCondLbls(st->middle, CTX_TOP);
			/* Count statements: init? + top + (if or body) + continue + incr? + goto + break */
			int stmt_count = 5;  /* top, (if or body), continue, goto, break */
			if (st->left) stmt_count++;   /* init */
			if (st->right) stmt_count++;  /* incr */
			/* If no increment, _continue goes straight to _top */
			if (!st->right)
				addJmpMap(mkLbl(st->label, "_continue"),
				          mkLbl(st->label, "_top"));
			fdprintf(astFd, "B00%02x", stmt_count);
			if (st->left) {
				fdprintf(astFd, "E");
				emitExpr(st->left);
			}
			emitLabel(st->label, "_top");
			if (st->middle) {
				fdprintf(astFd, "I01%02x", nlabels);  /* has else */
				emitExpr(st->middle);
				if (st->chain)
					emitStmt(st->chain);
				else
					fdprintf(astFd, ";");
				fdprintf(astFd, "B0001");
				emitGoto(st->label, "_break");
			} else {
				if (st->chain)
					emitStmt(st->chain);
				else
					fdprintf(astFd, ";");
			}
			emitLabel(st->label, "_continue");
			if (st->right) {
				fdprintf(astFd, "E");
				emitExpr(st->right);
			}
			emitGoto(st->label, "_top");
			emitLabel(st->label, "_break");
		}
		break;

	case SWITCH:
		/* Switch: S has_label. [hexlabel] case_count. expr cases... */
		{
			struct stmt *s, *body, *t;
			int case_count = 0, body_count;
			for (s = st->chain; s; s = s->next)
				if (s->op == CASE || s->op == DEFAULT)
					case_count++;
			fdprintf(astFd, "S%02x", st->label ? 1 : 0);
			if (st->label)
				emitHexName(st->label);
			fdprintf(astFd, "%02x", case_count);
			emitExpr(st->left);
			for (s = st->chain; s; ) {
				if (s->op == CASE || s->op == DEFAULT) {
					body = s->next;
					body_count = 0;
					for (t = body; t && t->op != CASE && t->op != DEFAULT; t = t->next)
						body_count++;
					fdprintf(astFd, "%c%02x", s->op == CASE ? 'C' : 'O', body_count);
					if (s->op == CASE)
						emitExpr(s->left);
					for (t = body; t && t->op != CASE && t->op != DEFAULT; t = t->next)
						emitStmt(t);
					s = body;
					while (s && s->op != CASE && s->op != DEFAULT)
						s = s->next;
				} else {
					emitStmt(s);
					s = s->next;
				}
			}
		}
		break;

	case CASE:
		/* Case labels are handled by SWITCH - this shouldn't be called directly */
		fdprintf(astFd, "C00");
		emitExpr(st->left);
		break;

	case DEFAULT:
		/* Default labels are handled by SWITCH - this shouldn't be called directly */
		fdprintf(astFd, "O00");
		break;

	case RETURN:
		/* Return: R has_value [expr] */
		fdprintf(astFd, "R%02x", st->left ? 1 : 0);
		if (st->left)
			emitExpr(st->left);
		break;

	case BREAK:
		fdprintf(astFd, "K");
		break;

	case CONTINUE:
		fdprintf(astFd, "N");
		break;

	case GOTO:
		fdprintf(astFd, "G");
		emitHexName(resolveJmp(st->label ? st->label : "?"));
		break;

	case LABEL:
		fdprintf(astFd, "L");
		emitHexName(st->label ? st->label : "?");
		break;

	case EXPR:
		/* Convert postinc/postdec to preinc/predec since result unused */
		if (st->left && (st->left->op == INCR || st->left->op == DECR) &&
		    (st->left->flags & E_POSTFIX)) {
			st->left->flags &= ~E_POSTFIX;  /* Make it prefix */
		}
		fdprintf(astFd, "E");
		emitExpr(st->left);
		break;

	case ';':
		fdprintf(astFd, ";");
		break;

	case ASM:
		{
			int len = st->label ? strlen(st->label) : 0;
			int i;
			fdprintf(astFd, "A%04x", len);
			for (i = 0; i < len; i++)
				fdprintf(astFd, "%02x", (unsigned char)st->label[i]);
		}
		break;

	default:
		fdprintf(astFd, "X%d.", st->op);  /* unknown */
		break;
	}
	/* Note: st->next is handled by caller (block counts statements) */
}

/*
 * Count function parameters
 */
static int
countParams(struct type *functype)
{
	int count = 0;
	struct name *param;
	if (functype && (functype->flags & TF_FUNC)) {
		for (param = functype->elem; param; param = param->next) {
			if (param->type && param->type->size == 0)
				continue;  /* skip void */
			count++;
		}
	}
	return count;
}

/*
 * Output function parameter declarations
 * Format: d suffix name reg off d suffix name reg off ...
 * reg is 2 hex digits: 00=none, 01=B, 02=C, 03=BC, 04=IX
 * off is 2 hex digits: signed frame offset (params positive, locals negative)
 */
static void
emitPrmDecls(struct type *functype, struct stmt *body)
{
	struct name *param, *local, *found;

	if (functype && (functype->flags & TF_FUNC)) {
		for (param = functype->elem; param; param = param->next) {
			/* Skip void parameters - (void) means no params */
			if (param->type && param->type->size == 0)
				continue;
			/* Look up register/offset from body locals */
			found = NULL;
			if (body && body->locals && param->name) {
				for (local = body->locals; local; local = local->next) {
					if (local->name && strcmp(local->name, param->name) == 0) {
						found = local;
						break;
					}
				}
			}
			/* Emit as: d suffix hexname reg off */
			fdprintf(astFd, "d%c", typeSfx(param->type));
			if (param->name && param->name[0] != '\0')
				emitHexName(param->name);
			else
				emitHexName("_");  /* anonymous parameter */
			fdprintf(astFd, "%02x%02x",
				found ? found->reg : 0,
				found ? (unsigned char)found->frm_off : 0);
		}
	}
}

/* Count local variables (non-params) */
static int
countLocals(struct stmt *body)
{
	struct name *local;
	int count = 0;

	if (!body || !body->locals)
		return 0;

	for (local = body->locals; local; local = local->next) {
		if (local->kind != funarg)
			count++;
	}
	return count;
}

/*
 * Emit local variable declarations (non-params) at function prolog
 * All locals are hoisted to function level - no scope tracking needed
 */
static void
emitLocals(struct stmt *body)
{
	struct name *local;
	const char *lname;

	if (!body || !body->locals)
		return;

	for (local = body->locals; local; local = local->next) {
		if (local->kind == funarg)
			continue;  /* params already emitted */
		lname = local->mangled_name ? local->mangled_name : local->name;
		fdprintf(astFd, "d%c", typeSfx(local->type));
		emitHexName(lname);
		fdprintf(astFd, "%02x%02x", local->reg,
			(unsigned char)local->frm_off);
	}
}

/*
 * Output a global asm block in AST format
 * Format: A len hexdata (same as inline asm but at top level)
 */
void
emitGlobalAsm(struct stmt *st)
{
	int len, i;
	if (!st || !st->label)
		return;
	len = strlen(st->label);
	fdprintf(astFd, "\nA%04x", len);
	for (i = 0; i < len; i++)
		fdprintf(astFd, "%02x", (unsigned char)st->label[i]);
	fdprintf(astFd, "\n");
}

/*
 * Output a function in AST format
 * Format: F rettype hexname param_count local_count frm_size params... locals... body
 * All locals hoisted to function prolog - no declarations in blocks
 */
void
emitFunction(struct name *func)
{
	char func_name[256];
	char ret_suffix;
	int frm_size, param_count, local_count;

	if (!func || !func->u.body)
		return;

	/* Analyze variable usage and allocate registers BEFORE emission */
	frm_size = analyzeFunc(func);

	/* Static functions use mangled name, public get underscore prefix */
	if (func->mangled_name) {
		snprintf(func_name, sizeof(func_name), "%s", func->mangled_name);
	} else {
		snprintf(func_name, sizeof(func_name), "_%s", func->name);
	}

	/* Get return type suffix (void uses 'v') */
	if (func->type && func->type->sub)
		ret_suffix = typeSfx(func->type->sub);
	else
		ret_suffix = 'v';  /* void */

	fdprintf(astFd, "\nF%c", ret_suffix);
	emitHexName(func_name);

	/* Output param count, local count, and frame size */
	param_count = func->type ? countParams(func->type) : 0;
	local_count = countLocals(func->u.body);
	fdprintf(astFd, "%02x%02x%02x", param_count, local_count, frm_size);

	/* Emit parameter declarations */
	if (func->type)
		emitPrmDecls(func->type, func->u.body);

	/* Emit local variable declarations (hoisted from all blocks) */
	emitLocals(func->u.body);

	/* Reset jump map for this function */
	jmpMapCnt = 0;

	/* Output function body */
	fdprintf(astFd, "\n");
	emitStmt(func->u.body);
	fdprintf(astFd, "\n");
}

/*
 * Emit an initializer list (linked via next pointers)
 * Used for array/struct initializers like {1, 2, 3}
 * elem_type: type of array elements for width annotation
 * Format: [ width count. items...
 */
/*
 * Emit struct initializer with field types from struct definition
 */
static void emitInit(struct expr *init, struct type *type);
static void emitInitList(struct expr *init, struct type *elem_type);

static void
emitStInit(struct expr *init, struct type *stype)
{
	struct expr *val;
	struct name *field, *fields[32];
	int count = 0, nfields = 0, i;

	/* Count initializer items */
	for (val = init; val; val = val->next)
		count++;

	/* Build forward-order field array (struct elem list is reversed) */
	if (stype && (stype->flags & TF_AGGREGATE)) {
		for (field = stype->elem; field && nfields < 32; field = field->next)
			fields[nfields++] = field;
	}

	fdprintf(astFd, "{%02x", count);

	/* Emit each initializer with corresponding field's type */
	i = nfields - 1;  /* Start from last field (first in source order) */
	for (val = init; val; val = val->next) {
		field = (i >= 0) ? fields[i--] : NULL;
		emitInit(val, field ? field->type : NULL);
	}
	fdprintf(astFd, "}");
}

/*
 * Recursively emit an initializer with expected type
 * type: expected type from declaration (array element type or struct field type)
 */
static void
emitInit(struct expr *init, struct type *type)
{
	if (init->op == INITLIST) {
		/* Nested aggregate - type tells us struct vs array */
		if (type->flags & TF_AGGREGATE) {
			emitStInit(init->left, type);
		} else if (type->flags & TF_ARRAY) {
			emitInitList(init->left, type->sub);
		}
	} else if (init->op == CONST && type) {
		/* Scalar constant - use declared type */
		struct type *saved = init->type;
		init->type = type;
		emitExpr(init);
		init->type = saved;
	} else {
		emitExpr(init);
	}
}

static void
emitInitList(struct expr *init, struct type *elem_type)
{
	struct expr *item;
	char width;
	int count = 0;

	/* Count items and get element width */
	for (item = init; item; item = item->next)
		count++;
	width = typeSfx(elem_type);

	fdprintf(astFd, "[%c%02x", width, count);
	for (item = init; item; item = item->next) {
		emitInit(item, elem_type);
	}
}

/*
 * Emit a single string literal immediately
 * Called when string literal is created during parsing
 * Format: U hexname hexdata
 */
void
emitStrLit(struct name *strname)
{
	cstring str;
	unsigned char len;
	unsigned char *data;
	int j;

	if (!strname || !strname->u.init || strname->u.init->op != STRING)
		return;

	/* Only emit once */
	if (strname->emitted)
		return;
	strname->emitted = 1;

	str = (cstring)strname->u.init->v;
	if (!str)
		return;

	len = (unsigned char)str[0];
	data = (unsigned char *)str + 1;

	/* Output: U hexname hexdata */
	fdprintf(astFd, "\nU");
	emitHexName(strname->name);
	fdprintf(astFd, "%02x", len);
	for (j = 0; j < len; j++)
		fdprintf(astFd, "%02x", data[j]);
	fdprintf(astFd, "\n");

	/* Don't free string data - needed for array size inference in char[] = "str" */
}

/*
 * Output a global variable declaration with optional initializer
 * Format: Z $hexname type has_init. [init-expr]
 */
void
emitGv(struct name *var)
{
	char fullname[256];
	char *name;

	if (!var || !var->type)
		return;

	/*
	 * For char[] = "string", emit the string directly with var name
	 * and skip the Z record
	 */
	if ((var->type->flags & TF_ARRAY) && var->type->sub &&
	    var->type->sub->size == 1 &&
	    var->u.init && var->u.init->op == STRING && !var->u.init->next) {
		/* Emit string literal with the variable's name */
		struct name *strname = (struct name *)var->u.init->var;
		if (strname) {
			emitStrLit(strname);
		}
		return;
	}

	/*
	 * For pointers initialized to string literals, emit the string
	 * BEFORE the Z record so it doesn't interrupt the record
	 */
	if ((var->type->flags & TF_POINTER) &&
	    var->u.init && var->u.init->op == STRING && !var->u.init->next) {
		struct name *strname = (struct name *)var->u.init->var;
		if (strname) {
			emitStrLit(strname);
		}
	}

	/*
	 * For arrays with initializer lists containing strings, emit all
	 * strings BEFORE the Z record so they don't interrupt it.
	 * Must recursively descend into INITLIST nodes for struct arrays.
	 */
	if ((var->type->flags & TF_ARRAY) && var->u.init && var->u.init->next) {
		struct expr *item, *inner;
		for (item = var->u.init; item; item = item->next) {
			if (item->op == STRING && item->var) {
				emitStrLit((struct name *)item->var);
			} else if (item->op == INITLIST) {
				/* Descend into nested initializer list */
				for (inner = item->left; inner; inner = inner->next) {
					if (inner->op == STRING && inner->var)
						emitStrLit((struct name *)inner->var);
				}
			}
		}
	}

	fdprintf(astFd, "\nZ$");

	/* Static uses mangled name, public gets underscore prefix */
	if (var->sclass & SC_STATIC) {
		name = var->mangled_name ? var->mangled_name : var->name;
		snprintf(fullname, sizeof(fullname), "%s", name);
	} else {
		snprintf(fullname, sizeof(fullname), "_%s", var->name);
	}
	emitHexName(fullname);

	emitTypeInfo(var->type);

	fdprintf(astFd, "%02x", var->u.init ? 1 : 0);
	if (var->u.init) {
		if (var->u.init->next) {
			struct type *elem_type =
			    (var->type && (var->type->flags & TF_ARRAY)) ?
			    var->type->sub : var->type;
			emitInitList(var->u.init, elem_type);
		} else {
			emitExpr(var->u.init);
		}
	}
	fdprintf(astFd, "\n");
}

/*
 * vim: tabstop=4 shiftwidth=4 expandtab:
 */
