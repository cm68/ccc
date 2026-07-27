/*
 * generate expression trees
 */
#include "cc1.h"

#ifdef DEBUG
int exprAllocCnt = 0;
int exprCurCnt = 0;
int exprHighWater = 0;
#endif

/*
 * Combined operator priority and flags table
 * Table covers 0x20-0x7f (96 bytes), indexed by (tok - 0x20)
 * Bits 0-3: priority (0-15), Bits 4-7: flags
 */
#define TF_ASN 0x10   /* assignment op: = += -= etc */
#define TF_CMP 0x20   /* comparison: < > <= >= == != */
#define TF_LOG 0x40   /* logical: && || */

#define P(p) (p)                     /* priority only */
#define F(f) (f)                     /* flags only */
#define PF(p,f) ((p) | (f))          /* priority + flags */

/*
 * Indexed by (token - 0x20) where tokens are from lexeme.h
 * Token values: STAR=36, PLUS=40, MINUS=41, DIV=43, MOD=44,
 * RSHIFT=45, LSHIFT=46, AND=47, OR=48, XOR=49, LAND=53, LOR=54,
 * EQ=60, NEQ=61, LE=62, LT=63, GE=64, GT=65, PLUSEQ=70..ASSIGN=80, QUES=90
 */
static unsigned char oppri[96] = {
/*0x20  32  33  34  35  STAR  37  38  39 */  0,0,0,0,P(3),0,0,0,
/*0x28  PLUS MINUS 42  DIV   MOD   RSHIFT LSHIFT AND */
        P(4),P(4),0,P(3),P(3),P(5),P(5),P(8),
/*0x30  OR    XOR   50  51  52  LAND           LOR            55 */
        P(10),P(9),0,0,0,PF(11,TF_LOG),PF(12,TF_LOG),0,
/*0x38  56  57  58  59  EQ             NEQ            LE             LT */
        0,0,0,0,PF(6,TF_CMP),PF(7,TF_CMP),PF(6,TF_CMP),PF(6,TF_CMP),
/*0x40  GE             GT             66  67  68  69  PLUSEQ         SUBEQ */
        PF(6,TF_CMP),PF(6,TF_CMP),0,0,0,0,PF(14,TF_ASN),PF(14,TF_ASN),
/*0x48  MULTEQ         DIVEQ          MODEQ          RSHIFTEQ       LSHIFTEQ       ANDEQ          OREQ           XOREQ */
        PF(14,TF_ASN),PF(14,TF_ASN),PF(14,TF_ASN),PF(14,TF_ASN),PF(14,TF_ASN),PF(14,TF_ASN),PF(14,TF_ASN),PF(14,TF_ASN),
/*0x50  ASSIGN         81  82  83  84  85  86  87 */
        PF(14,TF_ASN),0,0,0,0,0,0,0,
/*0x58  88  89  QUES  91  92  93  94  95 */
        0,0,P(13),0,0,0,0,0,
/*0x60  96  97  98  99  100 101 102 103 */  0,0,0,0,0,0,0,0,
/*0x68  104 105 106 107 108 109 110 111 */  0,0,0,0,0,0,0,0,
/*0x70  112 113 114 115 116 117 118 119 */  0,0,0,0,0,0,0,0,
/*0x78  120 121 122 123 124 125 126 127 */  0,0,0,0,0,0,0,0
};
#undef P
#undef F
#undef PF

#define OPPRI(t) ((unsigned char)(t) >= 0x20 && (unsigned char)(t) < 0x80 ? oppri[(t) - 0x20] : 0)

/* Assignment operators all have TF_ASN flag in oppri table */
#define IS_ASSIGN(t) (OPPRI(t) & TF_ASN)
#define IS_CMPLOG(t) (OPPRI(t) & (TF_CMP | TF_LOG))

/* Check if token is a type keyword - exported for declare.c */
int
isTypeToken(unsigned char t)
{
    /* Type keywords are in range 128-138 (INT, CHAR, ... VOID);
     * ENUM never reaches pass1 - cpp lowers enums to unsigned char */
    return (t >= INT && t <= VOID);
}

/*
 * Counter for generating synthetic string literal names (prefix "str")
 * Exported so it can be reset between phases
 */
char globalStrCtr = 0;

/*
 * Create a new expression tree node
 *
 * Allocates and zero-initializes an expression structure, setting the
 * operator and left child pointer. This is the basic expression node
 * allocator used throughout the parser.
 *
 * Parameters:
 *   op   - Operator token (e.g., PLUS, DEREF, CONST)
 *   left - Left child expression (can be NULL)
 *
 * Returns:
 *   Pointer to newly allocated and initialized expression node
 */
struct expr *
mkexpr(unsigned char op, struct expr *left)
{
	struct expr *e;

	e = (struct expr *)galloc(sizeof(*e));
	e->op = op;
	e->left = left;
#ifdef DEBUG
	exprAllocCnt++;
	exprCurCnt++;
	if (exprCurCnt > exprHighWater)
		exprHighWater = exprCurCnt;
#endif
	return e;
}

/*
 * Create expression node with type, value, and flags initialized
 *
 * Convenience wrapper around mkexpr() that also sets the type, value (v),
 * and flags fields. This reduces code duplication for common expression
 * construction patterns, especially for constants and typed operations.
 *
 * Parameters:
 *   op    - Operator token
 *   left  - Left child expression (can be NULL)
 *   type  - Type of the expression (pass NULL to skip setting)
 *   v     - Constant value (for CONST nodes) or other numeric data
 *   flags - Expression flags (E_CONST, E_RESOLVED, etc.)
 *
 * Returns:
 *   Pointer to newly allocated expression with all fields initialized
 */
struct expr *
mkexprI(unsigned char op, struct expr *left, struct type *type,
	 unsigned long v, int flags)
{
	struct expr *e;

	e = mkexpr(op, left);
	if (type) {
		e->type = type;
	}
	e->v = v;
	e->flags = flags;
	return e;
}

/*
 * Free just an expression node without its children
 * Used when restructuring trees (e.g., unwrapping DEREF)
 */
static void
freeNode(struct expr *e)
{
	if (!e) return;
	free(e);
#ifdef DEBUG
	exprCurCnt--;
#endif
}

/*
 * Unwrap a DEREF node, returning the child and freeing the DEREF
 * Saves the dereferenced type before unwrapping
 * Returns: the unwrapped type (what was e->type before unwrapping)
 */
static struct type *
unwrapDeref(struct expr **ep)
{
	struct expr *e = *ep;
	struct type *t;
	if (!e || e->op != DEREF) return e ? e->type : (struct type *)0;
	t = e->type;
	*ep = e->left;
	e->left = NULL;
	freeNode(e);
	return t;
}

/*
 * Free an expression tree recursively
 *
 * Performs a post-order traversal of the expression tree, freeing all
 * child nodes before freeing the parent. This ensures proper memory
 * deallocation without leaks.
 *
 * Note: Does not free any associated type structures (those are managed
 * in the global type cache) or symbol name strings (those are managed
 * by the name table).
 *
 * Parameters:
 *   e - Root of expression tree to free (NULL-safe)
 */
void
FreeExpr(struct expr *e)
{
	if (!e)
		return;
	FreeExpr(e->left);
	FreeExpr(e->right);
	FreeExpr(e->next);
	/* For STRING expressions with synthetic name, free the name and init expr */
	if (e->op == STRING && e->var) {
		struct name *strname = (struct name *)e->var;
		FreeExpr(strname->u.init);
		free((void *)e->v);
		free(strname);
	}
	free(e);
#ifdef DEBUG
	exprCurCnt--;
#endif
}

/*
 * Get binary operator precedence priority
 * Uses combined oppri[] table (bits 0-3 = priority)
 * Lexeme.h token values handled explicitly since they don't match ASCII
 */
unsigned char
binopPri(unsigned char t)
{
    /* COMMA is below table range */
    if (t == COMMA)
        return 15;
    /* Use table lookup for tokens in range 0x20-0x7f */
    if (t >= 0x20 && t < 0x80)
        return oppri[t - 0x20] & 0x0f;
    return 0;
}

static struct expr *scaleptr(struct expr *e);

/*
 * Skip an expression without building a tree (phase 1)
 * Consumes tokens to stay synchronized with the lexer.
 * Mirrors parseExpr structure but doesn't allocate.
 */
static void
skipExpr(unsigned char pri)
{
    unsigned char p, is_assign;
    struct name *np = NULL;
    char namebuf[32];
    char *symname;
    int size;

    /* Handle prefix/primary */
    switch (cur.type) {
    case NUMBER:
    case LNUMBER:
    case FNUMBER:
        gettoken();
        break;

    case STRING:
        /* Phase 1: emit string literal data */
        size = (unsigned char)cur.v.str[0];
        symname = (char *)cur.v.str + 1;
        fmtstr(namebuf, "str%d", globalStrCtr++);
        setSeg(SEG_TEXT);
        asmLabel(namebuf);
        asmDbStr((unsigned char *)symname, size);
        gettoken();
        while (cur.type == STRING)
            gettoken();
        break;

    case SYM:
        np = findName(cur.v.name, 0);
        if (np && np->level > 1 && np->kind != kelem &&
            !(np->type->flags & (TF_FUNC|TF_ARRAY))) {
            if (np->w.r.ref_count < 255)
                np->w.r.ref_count++;
        }
        gettoken();
        break;

    case LPAR:
        gettoken();
        if (isCastStart()) {
            parseTypeName();
            expect(RPAR, ER_E_SP);
            skipExpr(OP_PRI_MULT - 1);
        } else {
            skipExpr(0);
            expect(RPAR, ER_E_SP);
        }
        break;

    case MINUS:
    case TWIDDLE:
    case BANG:
    case STAR:
    case AND:
    case INCR:
    case DECR:
        gettoken();
        skipExpr(OP_PRI_MULT - 1);
        break;

    case SIZEOF:
        gettoken();
        if (cur.type == LPAR) {
            gettoken();
            if (isCastStart()) {
                parseTypeName();
                expect(RPAR, ER_E_SP);
            } else {
                skipExpr(0);
                expect(RPAR, ER_E_SP);
            }
        } else {
            skipExpr(OP_PRI_MULT - 1);
        }
        break;

    default:
        return;
    }

    /* Handle postfix operators */
    while (cur.type == LPAR || cur.type == LBRACK || cur.type == DOT ||
           cur.type == ARROW || cur.type == INCR || cur.type == DECR) {
        if (cur.type == LBRACK) {
            gettoken();
            skipExpr(0);
            expect(RBRACK, ER_E_SP);
        } else if (cur.type == LPAR) {
            gettoken();
            if (cur.type != RPAR) {
                skipExpr(OP_PRI_COMMA);
                while (cur.type == COMMA) {
                    gettoken();
                    skipExpr(OP_PRI_COMMA);
                }
            }
            expect(RPAR, ER_E_SP);
        } else if (cur.type == DOT || cur.type == ARROW) {
            /* Track aggregate ref for register allocation */
            if (np && np->level > 1 && np->w.r.agg_refs < 255)
                np->w.r.agg_refs++;
            gettoken();
            if (cur.type == SYM)
                gettoken();
        } else {
            gettoken();
        }
    }

    /* Handle binary operators */
    while (1) {
        p = binopPri(cur.type);
        if (p == 0 || (pri != 0 && p >= pri))
            break;
        if (cur.type == QUES) {
            gettoken();
            skipExpr(0);
            expect(COLON, ER_E_SP);
            skipExpr(0);
        } else {
            is_assign = IS_ASSIGN(cur.type);
            gettoken();
            skipExpr(is_assign ? 0 : p);
        }
    }
}

/*
 * Process increment/decrement (prefix or postfix)
 */
static struct expr *
mkIncDec(struct expr *operand, unsigned char inc_op, unsigned char is_postfix)
{
    struct type *value_type;
    struct expr *e;

    if (!operand || operand->op != DEREF) {
        gripe(ER_E_LV);
        FreeExpr(operand);
        return NULL;
    }
    value_type = unwrapDeref(&operand);
    e = mkexpr(inc_op, operand);
    e->left->up = e;
    e->type = value_type;
    if (is_postfix)
        e->flags |= E_POSTFIX;
    return e;
}

/* Check if type is scalar (not pointer/array/func/aggregate) */
#define IS_SCALAR(t) (!((t)->flags & (TF_POINTER|TF_ARRAY|TF_FUNC|TF_AGGREGATE)))

/*
 * A struct or union has no value here: it cannot be assigned, passed
 * or returned.  Its address is the only handle on it, which is what
 * the member operators work through.
 *
 * This has to be diagnosed rather than left alone.  Nothing downstream
 * knows an aggregate from a scalar of the same size - the width is
 * picked from the byte count, so a four byte struct became a long and
 * anything of another size fell to the default and copied two bytes,
 * quietly losing the rest.
 */
static int
isaggr(struct type *t)
{
	return t && (t->flags & TF_AGGREGATE) &&
	    !(t->flags & (TF_POINTER | TF_ARRAY | TF_FUNC));
}

/*
 * Assigning one pointer to another only means anything when they point
 * at the same thing.  Arrays decay, so an array of T and a pointer to
 * T count as the same here, and void goes with anything.
 *
 * Only fires when both sides are pointers, so pointer arithmetic like
 * "p += 4" is left alone.
 */
static int
ptrcompat(struct type *lt, struct type *rt)
{
	struct type *a, *b;

	if (!lt || !rt)
		return 1;
	if (!(lt->flags & (TF_POINTER | TF_ARRAY)) ||
	    !(rt->flags & (TF_POINTER | TF_ARRAY)))
		return 1;
	a = lt->sub;
	b = rt->sub;
	if (!a || !b || a == b)
		return 1;
	if (a->size == 0 || b->size == 0)
		return 1;			/* void * */
	/* a function pointer's sub is its return type - leave those */
	if ((a->flags | b->flags) & TF_FUNC)
		return 1;
	if (a->size != b->size)
		return 0;
	if ((a->flags & TF_UNSIGNED) != (b->flags & TF_UNSIGNED))
		return 0;
	/* aggregates are not interned, so two of them are two types */
	if ((a->flags | b->flags) & TF_AGGREGATE)
		return 0;
	return 1;
}

/*
 * Fold a single node if both operands are constants.
 * Returns folded CONST expr, or original expr if not foldable.
 */
static struct expr *
foldNode(struct expr *e)
{
    unsigned lv, rv;
    int rel = 0, sgn;
    struct expr *left, *right, *result;

    if (!e || !e->left || !e->right)
        return e;

    left = e->left;
    right = e->right;

    /* Identity folding: x + 0 -> x, x * 1 -> x, etc. */
    if (right->flags & E_CONST) {
        rv = right->v;
        result = NULL;
        switch (e->op) {
        case PLUS:
        case MINUS:
        case OR:
        case XOR:
        case LSHIFT:
        case RSHIFT:
            if (rv == 0) result = left;  /* x +/- 0 -> x */
            break;
        case STAR:
        case DIV:
            if (rv == 1) result = left;  /* x * 1 -> x */
            break;
        case AND:
        case AMPER:
            if (rv == 0xffff) result = left;  /* x & ~0 -> x */
            break;
        }
        if (result) {
            e->left = NULL;
            FreeExpr(right);
            e->right = NULL;
            freeNode(e);
            return result;
        }
    }

    /* Both operands must be constants for full folding */
    if (!(left->flags & E_CONST) || !(right->flags & E_CONST))
        return e;

    lv = left->v;
    rv = right->v;
    sgn = !(left->type->flags & TF_UNSIGNED) &&
          !(right->type->flags & TF_UNSIGNED);

    switch (e->op) {
    case PLUS:   lv += rv; break;
    case MINUS:  lv -= rv; break;
    case STAR:   lv *= rv; break;
    case DIV:    if (rv) lv /= rv; break;
    case MOD:    if (rv) lv %= rv; break;
    case LSHIFT: lv <<= rv; break;
    case RSHIFT: lv >>= rv; break;
    case AND:
    case AMPER:  lv &= rv; break;
    case OR:     lv |= rv; break;
    case XOR:    lv ^= rv; break;
    /*
     * Relations fold too, and have to: a comparison of two constants
     * reaches pass2 as a comparison of two constants, which matches no
     * rule and emits nothing.  The result is an int, not the type of
     * what was compared, so the type is fixed up below.
     *
     * lv and rv are unsigned, so the signed relations need casting
     * back.  Both operands being signed is what decides it: if either
     * is unsigned the usual conversions make the comparison unsigned.
     */
    case EQ:     lv = (lv == rv); rel = 1; break;
    case NEQ:    lv = (lv != rv); rel = 1; break;
    case LT:     lv = sgn ? ((long)lv <  (long)rv) : (lv <  rv); rel = 1; break;
    case GT:     lv = sgn ? ((long)lv >  (long)rv) : (lv >  rv); rel = 1; break;
    case LE:     lv = sgn ? ((long)lv <= (long)rv) : (lv <= rv); rel = 1; break;
    case GE:     lv = sgn ? ((long)lv >= (long)rv) : (lv >= rv); rel = 1; break;
    default:
        return e;
    }

    /* Reuse left node as constant */
    left->op = CONST;
    left->v = lv;
    left->type = rel ? inttype : e->type;
    left->flags = E_CONST;
    left->left = NULL;
    left->right = NULL;
    FreeExpr(right);
    e->left = NULL;
    e->right = NULL;
    freeNode(e);
    return left;
}

/*
 * Walk expression tree bottom-up and fold constants.
 * Called before emitting to AST.
 */
struct expr *
foldTree(struct expr *e)
{
    struct expr **pp;

    if (!e)
        return NULL;
    e->left = foldTree(e->left);
    /* Walk right child and any next chain (function arguments) */
    for (pp = &e->right; *pp; pp = &(*pp)->next)
        *pp = foldTree(*pp);
    return foldNode(e);
}

/*
 * Parse an expression using precedence climbing algorithm
 *
 * Recursive descent parser for C expressions that implements operator
 * precedence using the precedence climbing method. This function handles:
 *   - Primary expressions (constants, variables, strings)
 *   - Prefix operators (unary -, ~, !, *, &, ++, --, sizeof)
 *   - Binary operators (arithmetic, logical, bitwise, comparison)
 *   - Postfix operators (++, --, [], (), ., ->)
 *   - Ternary conditional (? :)
 *   - Type casts
 *   - Function calls
 *   - Constant folding during parsing
 *   - Type conversions and checking
 *
 * The precedence climbing works by:
 *   1. Parse left operand (primary or prefix expression)
 *   2. While next token is binary operator with priority >= current priority:
 *      - Recursively parse right operand with operator's priority
 *      - Combine into binary expression tree
 *   3. Handle postfix operators (highest precedence)
 *
 * Lower priority numbers bind tighter (higher precedence). Passing priority 0
 * parses any expression. Passing higher priorities stops at lower-precedence
 * operators, enabling recursive parsing of right operands.
 *
 * Parameters:
 *   pri - Minimum operator priority to parse (0 = parse any expression,
 *         higher values stop at lower-precedence operators)
 *
 * Returns:
 *   Expression tree root, or NULL on parse error
 */
/*
 * parseExpr is split into parsePrefix / parsePostfix / the binary
 * precedence climb below, mostly so each piece stays small enough
 * for the hitech optimizer's fixed arenas.
 *
 * parsePrefix - terminals, prefix/unary operators, casts, sizeof.
 */
static struct expr *
parsePrefix(void)
{
	unsigned char uop, inc_op;
	struct expr *e = 0;
	struct expr *e1;
	struct type *t, *tp;
	struct name *np;
	unsigned int uofs;
	char namebuf[32];
	char *symname;
	union { float f; unsigned long u; } fu;
	unsigned long uval;
	long sval;

	switch (cur.type) {   // prefix

    case LNUMBER:
        /* the source said L, so it is a long however small it is */
        e = mkexprI(CONST, 0, longtype, (unsigned long)cur.v.numeric,
                    E_CONST);
        gettoken();
        break;

    case NUMBER:
        sval = cur.v.numeric;
        /* Inline: determine smallest type for constant */
        if (sval < 0)
            t = (sval >= -128) ? chartype : (sval >= -32768) ? inttype : longtype;
        else if (sval <= 255)
            t = uchartype;
        else if (sval <= 65535)
            t = ushorttype;
        else
            /* sval >= 0 always fits in long here; spelling this as
             * sval <= 2147483647L trips a zc3 bug (rewritten to
             * < LONG_MIN, always false) */
            t = longtype;
        e = mkexprI(CONST, 0, t, (unsigned long)sval, E_CONST);
        gettoken();
        break;

    case FNUMBER:
        /* Store float bit pattern in v */
        fu.f = cur.v.fval;
        e = mkexprI(CONST, 0, floattype, fu.u, E_CONST);
        gettoken();
        break;

    case STRING:
        /* Phase 2: create expression referencing string emitted in phase 1 */
        fmtstr(namebuf, "str%d", globalStrCtr++);
        np = (struct name *)galloc(sizeof(struct name));
        strncpy(np->name, namebuf, 15);
        np->name[15] = 0;
        np->type = getType(TF_POINTER, chartype, 0);
        np->kind = kvar;
        np->level = 1;
        e = mkexprI(STRING, 0, np->type, 0, E_CONST);
        e->var = (struct var *)np;
        gettoken();
        break;

    case SYM:
        /* Symbol reference - SYM = address
         * For variables: wrap in DEREF to get value
         * For functions: return address (decay to function pointer)
         */
        /* Save symbol name before gettoken() overwrites cur.v.name */
        symname = strdup(cur.v.name);

        np = findName(symname, 0);

        /* Peek at next token to enable implicit function declarations */
        gettoken();

        if (!np) {
            /* Undefined symbol */
            /* K&R extension: if followed by '(', implicitly declare as
             * function returning int */
            if (cur.type == LPAR) {
                /* Create implicit function declaration: int name() */
                tp = (struct type *)permalloc(sizeof(struct type));
                tp->flags = TF_FUNC;
                tp->sub = inttype;  /* Return type: int */
                tp->elem = NULL;    /* No parameter info */

                np = (struct name *)galloc(sizeof(struct name));
                /* Initialize in struct field order */
                strncpy(np->name, symname, 15);
                np->name[15] = 0;
                np->type = tp;
                /* chain set by addName */
                np->kind = kvar;
                np->level = 1;  /* Global scope */
                /* is_tag = 0 from calloc */
                np->sclass = SC_EXTERN;

                np = addName(np);

#ifdef DEBUG
                if (VERBOSE(V_SYM)) {
                    fdprintf(2, "Implicit declaration: int %s()\n", symname);
                }
#endif
            } else {
                /* Not a function call - report error */
#ifdef DEBUG
                fdprintf(2, "bad op (not fn): %d sym=%s\n", cur.type, symname);
#endif
                gripe(ER_E_UO);
                e = mkexprI(CONST, 0, inttype, 0, 0);
                free(symname);
                break;
            }
        }

        if (np->kind == kelem) {
            /* Enum constant: treat as integer constant.
             * uofs intermediate: zc3 miscompiles the direct
             * uchar -> ulong argument promotion (clobbers the
             * loaded byte), yielding 0 for every enum. */
            uofs = np->w.m.offset;
            e = mkexprI(CONST, 0, inttype, uofs, E_CONST);
        } else {
            e1 = mkexprI(SYM, 0, np->type, 0, 0);
            e1->var = (struct var *)np;

            // Functions and arrays decay to pointers (addresses)
            // Only wrap non-functions in DEREF to get their value
            if (np->type->flags & TF_FUNC)
                e = e1;  // Function: return address
            else if (np->type->flags & TF_ARRAY)
                e = e1;  // Array: decays to pointer
            else
                e = mkexprI(DEREF, e1, np->type, 0, 0);  // Variable: wrap in DEREF
        }
        free(symname);
        /* Note: gettoken() already called above for lookahead */
        break;

    /* unary operators */
    case LPAR:      // parenthesized expression or type cast
        gettoken();

        /* Check if this is a type cast: (type)expr */
        if (isCastStart()) {
            /* Parse the type name */
            tp = parseTypeName();
            expect(RPAR, ER_E_SP);

            /* Parse the expression being cast */
            /* Cast has unary precedence */
            e1 = parseExpr(OP_PRI_MULT - 1);

            /* Cast just changes the type - pass2 handles conversions */
            if (e1) {
                e1->type = tp;
                e = e1;
            } else {
                e = mkexprI(CONST, 0, tp, 0, 0);
            }
        } else {
            /*
             * Parenthesized expression: (expr)
             * parse inner expression with lowest precedence
             */
            e = parseExpr(0);
            expect(RPAR, ER_E_SP);
        }
        break;

    case MINUS:     // unary minus
    case TWIDDLE:   // bitwise not
    case BANG:      // logical not
        /*
         * The lexeme and the AST node are not the same thing: unary
         * minus becomes NEG, and "~" has to become NOT the same way.
         * Leaving it as the TWIDDLE lexeme meant pass2 never saw an
         * operator it recognised, so "~x" reduced to nothing at all -
         * no code, at either width, for the life of the compiler.
         */
        uop = (cur.type == MINUS) ? NEG :
              (cur.type == TWIDDLE) ? NOT : cur.type;
        gettoken();
        e1 = parseExpr(OP_PRI_MULT - 1);
        if (!e1) break;
        /* Fold unary ops on constants */
        if (e1->flags & E_CONST) {
            uval = e1->v;
            if (uop == NEG) uval = -uval;
            else if (uop == NOT) uval = ~uval;
            else if (uop == BANG) uval = !uval;
            e1->v = uval;
            e = e1;
        } else {
            e = mkexpr(uop, e1);
            /*
             * Negation and complement give back what they were handed;
             * "!" gives an int whatever it was applied to.  Taking the
             * operand's type made "!lv" a long, so "!lv != 0" looked
             * like 32-bit work and went looking for a helper to
             * compare a truth value nothing had widened.
             */
            e->type = (uop == BANG) ? inttype : e1->type;
            e1->up = e;
        }
        break;

    case STAR:      // dereference (unary)
        gettoken();
        e1 = parseExpr(OP_PRI_MULT - 1);
        if (!e1) break;
        /*
         * Dereferencing a pointer to a function is a no-op: what it
         * yields is the function, and what you can do with that is
         * call it, which needs the address the pointer already holds.
         * Wrapping it in a load meant "(*fp)()" tried to fetch
         * through the pointer and call whatever it found.
         */
        if (e1->type && (e1->type->flags & TF_POINTER) &&
            e1->type->sub && (e1->type->sub->flags & TF_FUNC)) {
            e = e1;
            break;
        }
        e = mkexpr(DEREF, e1);
        e1->up = e;
        if ((e1->type->flags & TF_POINTER) && e1->type->sub)
            e->type = e1->type->sub;
        else
            e->type = e1->type;
        break;

    case AND:       // address-of (unary)
        gettoken();
        e = parseExpr(OP_PRI_MULT - 1);
        if (!e) break;
        /* Mark variable as address-taken (can't use register) */
        if (e->op == DEREF && e->left->op == SYM)
            ((struct name *)e->left->var)->w.r.addr_taken = 1;
        /* Optimize: &(DEREF x) = x, since SYM already gives address */
        if (e->op == DEREF) {
            e1 = e;
            e = e->left;
            if (e->op == SYM)
                e->type = getType(TF_POINTER, e->type, 0);
            e1->left = NULL;
            freeNode(e1);
        } else if (e->type->flags & TF_ARRAY) {
            e->type = getType(TF_POINTER, e->type, 0);
        } else {
            e1 = mkexpr(AND, e);
            e->up = e1;
            e1->type = getType(TF_POINTER, e->type, 0);
            e = e1;
        }
        break;

    case SIZEOF:    /* sizeof(type), sizeof(expr), or sizeof expr */
        gettoken();
        if (cur.type == LPAR) {
            gettoken();  /* consume '(' */
            if (isCastStart()) {
                /* sizeof(type) */
                t = parseTypeName();
                expect(RPAR, ER_E_SP);
            } else {
                /* sizeof(expr) */
                e1 = parseExpr(0);
                t = e1 ? e1->type : (struct type *)0;
                FreeExpr(e1);
                expect(RPAR, ER_E_SP);
            }
        } else {
            /* sizeof expr (no parens) */
            e1 = parseExpr(OP_PRI_MULT - 1);
            t = e1 ? e1->type : (struct type *)0;
            FreeExpr(e1);
        }
        e = mkexprI(CONST, 0, inttype, t ? t->size : 0, E_CONST);
#ifdef DEBUG
        if (!t) fdprintf(2, "bad op (sizeof): no type\n");
#endif
        if (!t) gripe(ER_E_UO);
        break;

    case INCR:      // prefix increment: ++i
    case DECR:      // prefix decrement: --i
        inc_op = cur.type;
        gettoken();
        e = mkIncDec(parseExpr(OP_PRI_MULT - 1), inc_op, 0);
        break;

	default:
#ifdef DEBUG
		fdprintf(2, "bad op (expr): %d\n", cur.type);
#endif
		gripe(ER_E_UO);
		return 0;
    }

    return e;
}

/*
 * Postfix operators on a parsed operand: function calls, array
 * subscripts, struct access, increment/decrement.
 */
static struct expr *
parsePostfix(struct expr *e)
{
	unsigned char is_arrow, inc_op;
	struct expr *e1, *e2, *e3, *e4;
	struct type *t, *tp, *ft;
	struct name *np;
	unsigned int uofs;
	int elem_size;

    /*
     * Handle postfix operators: function calls, array subscripts,
     * struct access, increment/decrement
     */
    while (cur.type == LPAR || cur.type == LBRACK || cur.type == DOT ||
			cur.type == ARROW || cur.type == INCR || cur.type == DECR) {
        if (cur.type == LBRACK) {
            // Array subscript: arr[idx] = DEREF(ADD(base, idx * sizeof))
            gettoken();  // consume '['
            e1 = parseExpr(0);  /* index */
            expect(RBRACK, ER_E_SP);

            /*
             * An array's name is its address, so the load comes off
             * and the subscript is added to the address itself.  A
             * pointer's name is not: its value has to be read first
             * and the subscript added to that.
             *
             * Unwrapping both made "p[0]" the byte at the pointer
             * rather than the byte it points at - the low half of the
             * pointer itself.  "*p" was right all along, because that
             * path never unwrapped anything.
             */
            if (e && e->op == DEREF && e->type &&
                (e->type->flags & TF_ARRAY))
                tp = unwrapDeref(&e);
            else
                tp = e ? e->type : (struct type *)0;

            /* Get element size from type */
            elem_size = 2;  // default to short/int size
            if ((tp->flags & TF_POINTER) && tp->sub)
                elem_size = tp->sub->size;
            else if ((tp->flags & TF_ARRAY) && tp->sub)
                elem_size = tp->sub->size;

            // Scale index by element size: idx * sizeof(elem)
            if (elem_size == 1) {
                e2 = e1;  /* scaled = index */
            } else {
                e4 = mkexprI(CONST, 0, inttype,
						elem_size, E_CONST);  /* size_expr */

                e2 = mkexpr(STAR, e1);  /* scaled */
                e2->right = e4;
                e2->left->up = e2;
                e2->right->up = e2;
                e2->type = inttype;
            }

            // Add scaled offset to base: base + (idx * sizeof)
            e3 = mkexpr(PLUS, e);  /* addr */
            e3->right = e2;
            e3->left->up = e3;
            e3->right->up = e3;
            /* The ADD result is a pointer to the element type */
            if ((tp->flags & TF_ARRAY) && tp->sub)
                e3->type = getType(TF_POINTER, tp->sub, 0);
            else
                e3->type = tp;

            // Dereference to get element value
            e = mkexpr(DEREF, e3);
            e->left->up = e;
            if ((e->left->type->flags & (TF_POINTER|TF_ARRAY)) &&
                    e->left->type->sub)
                e->type = e->left->type->sub;
        } else if (cur.type == LPAR) {
            // Function call: expr(arg1, arg2, ...)
            gettoken();  // consume '('

            // Create CALL node with function expression as left operand
            e1 = mkexpr(CALL, e);  /* call */
            e1->left->up = e1;

            /*
             * The thing being called is either a function or a
             * pointer to one, and the return type is one step further
             * down in the second case.  Only the first was looked
             * for, so a call through a pointer had no type at all -
             * which nothing minded until the result was used.
             */
            ft = e->type;
            if (!(ft->flags & TF_FUNC) && (ft->flags & TF_POINTER) &&
                ft->sub && (ft->sub->flags & TF_FUNC))
                ft = ft->sub;

            // Set return type from function type
            if ((ft->flags & TF_FUNC) && ft->sub) {
                e1->type = ft->sub;
                /* a struct cannot come back by value either */
                if (isaggr(e1->type))
                    gripe(ER_E_AG);
            }
            /*
             * Whatever was called, the call has a type.  Leaving it
             * unset let a null type reach everything downstream, and
             * calling something this could not read the return type
             * of died the moment the answer was used - while throwing
             * the answer away was fine, which is a poor way to find
             * out.  A function of unknown type returns int.
             */
            if (!e1->type)
                e1->type = inttype;

            // Get first parameter for type coercion
            np = (ft->flags & TF_FUNC) ? ft->elem : 0;

            /* Parse argument list - pass2 handles type conversions */
            e3 = NULL;
            if (cur.type != RPAR) {
                e2 = parseExpr(OP_PRI_COMMA);
                if (e2) {
                    /* a struct cannot be passed - pass its address */
                    if (isaggr(e2->type))
                        gripe(ER_E_AG);
                    e2->flags |= E_FUNARG;
                    e1->right = e2;
                    e2->up = e1;
                    e3 = e2;
                }
                while (cur.type == COMMA) {
                    gettoken();
                    e2 = parseExpr(OP_PRI_COMMA);
                    if (e2) {
                        if (isaggr(e2->type))
                            gripe(ER_E_AG);
                        e2->flags |= E_FUNARG;
                        if (e3) {
                            e3->next = e2;
                        }
                        e3 = e2;
                    }
                }
            }

            expect(RPAR, ER_E_SP);

            // Result type will be determined later from function signature
            e = e1;
        } else if (cur.type == DOT || cur.type == ARROW) {
            // Struct member access: s.x or p->x
            is_arrow = (cur.type == ARROW);

            gettoken();  // consume '.' or '->'

            if (cur.type != SYM) {
#ifdef DEBUG
                fdprintf(2, "bad op (member): %d\n", cur.type);
#endif
                gripe(ER_E_UO);
                break;
            }

            // For s.x: e is DEREF(SYM s), unwrap to get SYM s
            // For p->x: e is DEREF(SYM p), keep as-is (pointer value)
            if (is_arrow) {
                e1 = e;  /* base - pointer value */
                /* Track aggregate ref for register allocation */
                if (e1->op == DEREF && e1->left && e1->left->op == SYM) {
                    struct name *vn = (struct name *)e1->left->var;
                    if (vn && vn->level > 1 && vn->w.r.agg_refs < 255)
                        vn->w.r.agg_refs++;
                }
            } else {
                // Unwrap DEREF to get address
                unwrapDeref(&e);
                e1 = e;  /* base */
                /* Track aggregate ref for register allocation */
                if (e1->op == SYM) {
                    struct name *vn = (struct name *)e1->var;
                    if (vn && vn->level > 1 && vn->w.r.agg_refs < 255)
                        vn->w.r.agg_refs++;
                }
            }

            // Look up member in struct/union
            np = NULL;  /* member */
            t = e1->type;
            /*
             * For both DOT and ARROW, if base type is pointer,
             * get the pointed-to type (DOT after array subscript
             * produces pointer type)
             */
            if (t->flags & TF_POINTER)
                t = t->sub;
            if (t && (t->flags & TF_AGGREGATE) && t->elem) {
                for (np = t->elem; np; np = np->next) {
                    if (strcmp(np->name, cur.v.name) == 0)
                        break;
                }
            }

            if (!np) {
#ifdef DEBUG
                fdprintf(2, "bad op (no member): %s\n", cur.v.name);
#endif
                gripe(ER_E_UO);
                gettoken();
                e = mkexprI(CONST, 0, NULL, 0, 0);
                break;
            }

            /*
             * Generate: DEREF(ADD(base, offset)) or BFEXTRACT
             * for bitfields.  uofs intermediate: zc3 miscompiles
             * the direct uchar -> ulong argument promotion.
             */
            uofs = np->w.m.offset;
            e2 = mkexprI(CONST, 0, inttype,
					uofs, E_CONST);  /* offset_expr */

            e3 = mkexpr(PLUS, e1);  /* addr */
            e3->right = e2;
            e3->left->up = e3;
            e3->right->up = e3;
            // addr is pointer to member, not pointer to base struct
            e3->type = getType(TF_POINTER, np->type, 0);

            // Check if this is a bitfield access
            if (np->kind == kbitfield) {
                /*
                 * Use BFEXTRACT operator with bitoff and
                 * width stored in expr
                 */
                e = mkexprI(BFEXTRACT, e3, np->type, 0, 0);
                e->left->up = e;
                /*
                 * Store bitfield info in the var field (repurpose it)
                 * We'll encode bitoff and width for the code generator
                 * Keep reference to member for bitoff/width
                 */
                e->var = (struct var *)np;
            } else if (np->type->flags & TF_ARRAY) {
                /* Array member: return address without DEREF */
                e = e3;
                e->type = np->type;
            } else {
                /* Non-array member: wrap in DEREF to get value */
                e = mkexprI(DEREF, e3, np->type, 0, 0);
                e->left->up = e;
            }

            gettoken();
        } else if (cur.type == INCR || cur.type == DECR) {
            // Postfix increment/decrement: i++ or i--
            inc_op = cur.type;
            gettoken();
            e = mkIncDec(e, inc_op, 1);
        }
    }

    return e;
}

struct expr *
parseExpr(unsigned char pri)
{
	unsigned char op, p, is_assignment;
	struct expr *e;
	struct expr *e1, *e2, *e3, *e4;
	struct type *assign_type;
	struct var *vp;

	assign_type = NULL;
	is_assignment = 0;

	/* Phase 1: just consume tokens, don't build tree */
	if (phase == 1) {
		skipExpr(pri);
		return NULL;
	}

	e = parsePrefix();
	if (!e) return 0;

	e = parsePostfix(e);
	if (!e) return 0;

    /*
     * the recursive nature of this expression parser will have exhausted
     * the unary operators and terminals by this point. now we have postfix
     * and binary operators to deal with
     */
    while (1) { // binary operators
        p = binopPri(cur.type);
        if (p == 0) {
            // not a binary operator, we're done
            break;
        }
        if (pri != 0 && p >= pri) {
            /*
             * operator has same or lower precedence
             * stop parsing at this level
             * (pri == 0 means PRI_ALL, so we parse all operators
             * regardless of precedence)
             */
            break;
        }

        // we have a binary operator with higher precedence (lower p value)
        op = cur.type;
        gettoken();

        /*
         * Special handling for ternary conditional operator:
         * condition ? true : false
         */
        if (op == QUES) {
            e1 = e;  /* condition */

            // Parse the true expression
            e2 = parseExpr(0);  /* true_expr */

            // Expect and consume COLON
            expect(COLON, ER_E_SP);

            /*
             * Parse the false expression (right-associative,
             * allow another ?: at same level). Use priority 0 to parse
             * everything, including nested ?: operators
             */
            e3 = parseExpr(0);  /* false_expr */

            // Build tree: QUES(condition, COLON(true_expr, false_expr))
            e4 = mkexpr(COLON, e2);  /* colon_node */
            e4->right = e3;
            if (e4->left) e4->left->up = e4;
            if (e4->right) e4->right->up = e4;

            e = mkexpr(QUES, e1);
            e->right = e4;
            e1->up = e;
            e4->up = e;

            /* Type is the type of the result expressions */
            if (e2)
                e->type = e2->type;

            /* Skip the rest of the loop and continue with next operator */
            continue;
        }

        /*
         * for assignment and compound assignments, unwrap DEREF
         * from left side to get lvalue address
         * Track the actual type being assigned
         */
        assign_type = NULL;
        is_assignment = IS_ASSIGN(op);

        if (is_assignment) {
            if (e->op == DEREF && isaggr(e->type)) {
                /* a struct is not an lvalue - take its address */
                gripe(ER_E_AG);
                FreeExpr(parseExpr(p));
                return e;
            }
            if (e->op == DEREF) {
                assign_type = unwrapDeref(&e);
            } else if (e->op == BFEXTRACT) {
                // Bitfield assignment - save info and change to BFASSIGN
                assign_type = e->type;
                vp = e->var;
                e1 = e;
                e = e->left;
                e1->left = NULL;
                freeNode(e1);
                e->var = vp;
                if (op == ASSIGN)
                    op = BFASSIGN;
            } else {
                /*
                 * Assignment requires an lvalue
                 * (dereference or bitfield)
                 */
                gripe(ER_E_LV);
                /*
                 * Skip this operator: parse and discard right side,
                 * then return left side
                 */
                FreeExpr(parseExpr(p));  // Parse and discard right side
                return e;  // Return left side unchanged
            }
        }

        /*
         * Parse right side based on associativity:
         * - For right-associative operators (assignments), use
         *   precedence 0 to allow chaining
         * - For left-associative operators, use precedence p to prevent
         *   same-precedence from nesting right
         */
        vp = e->var;
        e = mkexpr(op, e);
        e->left->up = e;
        if (is_assignment) {
            /*
             * Right-associative, so the right hand side has to admit
             * another assignment for "a = b = c" - but not a comma.
             * The right hand side of an assignment is an
             * assignment-expression, so "a = b, c" is "(a = b), c",
             * and in an argument list the comma separates arguments.
             * Parsing this at 0 swallowed the comma, and
             * "f(k += 4, l++)" became a call with one argument.
             */
            e->right = parseExpr(OP_PRI_COMMA);
        } else {
            /*
             * Left-associative: parse at same precedence to prevent
             * (a + b) + c from becoming a + (b + c)
             */
            e->right = parseExpr(p);
        }
        if (e->right) {
            e->right->up = e;
        }

        // For BFASSIGN, restore member info (bitoff, width)
        if (op == BFASSIGN && vp) {
            e->var = vp;
        }

        /* Determine result type */
        if (e->right) {
            if (is_assignment && assign_type) {
                if (!ptrcompat(assign_type, e->right->type))
                    gripe(ER_E_PT);
                e->type = assign_type;
            }
            else if (IS_CMPLOG(op))
                e->type = uchartype;
            else if (e->left->type->size >= e->right->type->size)
                e->type = e->left->type;
            else
                e->type = e->right->type;

            /*
             * Pointer arithmetic counts in elements, not bytes.  The
             * subscript path has always scaled - "p[2]" is right -
             * but the same sum written "p + 2" came through here and
             * did not, so it landed two bytes along instead of two
             * elements.  Done after the result type is settled,
             * because a difference of two pointers is an int and has
             * to say so.
             */
            if (op == PLUS || op == MINUS)
                e = scaleptr(e);
        } else {
            e->type = e->left->type;
        }
    }
    return e;
}

/*
 * Scale the integer side of pointer arithmetic by the element size.
 *
 * "p + n" advances by n elements, so the n has to be multiplied by
 * what p points at.  A difference between two pointers is the other
 * way round: the byte difference is divided.  An element size of one
 * needs neither, which is why char pointers always looked right.
 */
static struct expr *
scaleptr(struct expr *e)
{
    struct type *lt = e->left->type, *rt = e->right->type;
    int lp = (lt->flags & (TF_POINTER|TF_ARRAY)) != 0;
    int rp = (rt->flags & (TF_POINTER|TF_ARRAY)) != 0;
    struct type *pt = lp ? lt : rt;
    struct expr *n, *scaled, **side;
    int size;

    if (!lp && !rp)
        return e;                       /* ordinary arithmetic */
    if (!pt->sub || (size = pt->sub->size) == 1)
        return e;                       /* a byte needs no scaling */

    if (lp && rp) {
        /*
         * One pointer less another is how many elements apart they
         * are, so the byte difference is divided.  Anything else
         * between two pointers is not arithmetic and is left alone.
         */
        if (e->op != MINUS)
            return e;
        n = mkexprI(CONST, 0, inttype, (unsigned long)size, E_CONST);
        scaled = mkexpr(DIV, e);
        scaled->right = n;
        scaled->left->up = scaled;
        scaled->right->up = scaled;
        scaled->type = inttype;
        return scaled;
    }

    /* the integer is whichever side is not the pointer */
    side = lp ? &e->right : &e->left;
    if ((*side)->flags & E_CONST) {
        (*side)->v *= size;
        return e;
    }
    n = mkexprI(CONST, 0, inttype, (unsigned long)size, E_CONST);
    scaled = mkexpr(STAR, *side);
    scaled->right = n;
    scaled->left->up = scaled;
    scaled->right->up = scaled;
    scaled->type = inttype;
    *side = scaled;
    scaled->up = e;
    return e;
}

/*
 * Parse and evaluate a constant expression
 * Used for array sizes, case values, enum values, etc.
 */
unsigned long
parseConst(unsigned char token)
{
	struct expr *e;
	unsigned long val;
	unsigned char save_phase;

	/* Parse expression, stop before comma (for enum values) */
	save_phase = phase;
	phase = 2;
	e = parseExpr(15);
	phase = save_phase;

	if (!e) {
		gripe(ER_C_CE);
		return 0;
	}
	e = foldTree(e);	/* SECSIZE+2 style bounds */
	if (!(e->flags & E_CONST)) {
		gripe(ER_C_CE);
		FreeExpr(e);
		return 0;
	}
	val = e->v;
	FreeExpr(e);
	return val;
}

/*
 * vim: tabstop=4 shiftwidth=4 expandtab:
 */
