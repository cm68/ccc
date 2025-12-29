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
#define TF_TYPE 0x80  /* type keyword */

#define P(p) (p)                     /* priority only */
#define F(f) (f)                     /* flags only */
#define PF(p,f) ((p) | (f))          /* priority + flags */

static unsigned char oppri[96] = {
/*0x20  spc !  "  #  $  %    &    ' */  0,0,0,0,0,P(3),P(8),0,
/*0x28  (  )  *    +    ,     -    .    / */  0,0,P(3),P(4),P(15),P(4),P(1),P(3),
/*0x30  0             1             2             3  4            5  6             7 */
        PF(14,TF_ASN),PF(14,TF_ASN),PF(14,TF_ASN),0,F(TF_TYPE),0,PF(14,TF_ASN),0,
/*0x38  8  9  :  ;  <          =             >          ? */
        0,0,0,0,PF(6,TF_CMP),PF(14,TF_ASN),PF(6,TF_CMP),P(13),
/*0x40  @  A  B  C  D  E  F  G */  0,0,0,0,0,0,0,0,
/*0x48  H             I  J             K  L          M    N  O */
        PF(14,TF_ASN),0,PF(14,TF_ASN),0,PF(6,TF_CMP),P(1),0,0,
/*0x50  P             Q             R  S  T             U  V  W */
        PF(14,TF_ASN),PF(7,TF_CMP),0,0,PF(14,TF_ASN),0,0,0,
/*0x58  X             Y  Z  [  \  ]  ^    _ */
        PF(14,TF_ASN),0,0,0,0,0,P(9),0,
/*0x60  `  a           b  c           d           e           f           g */
        0,F(TF_TYPE),0,F(TF_TYPE),F(TF_TYPE),F(TF_TYPE),F(TF_TYPE),PF(6,TF_CMP),
/*0x68  h              i           j              k           l           m           n             o */
        PF(12,TF_LOG),F(TF_TYPE),PF(11,TF_LOG),F(TF_TYPE),F(TF_TYPE),F(TF_TYPE),PF(7,TF_CMP),0,
/*0x70  p  q  r  s           t           u           v           w */
        0,0,0,F(TF_TYPE),F(TF_TYPE),F(TF_TYPE),F(TF_TYPE),P(5),
/*0x78  x  y     z  {  |      }  ~  DEL */  0,P(5),0,0,P(10),0,0,0
};
#undef P
#undef F
#undef PF

#define OPPRI(t) ((unsigned char)(t) < 0x80 ? oppri[(t) - 0x20] : 0)

/* High-byte assignment operators: SUBEQ=0xdf MODEQ=0xfe ANDEQ=0xc6 */
#define IS_ASSIGN(t) ((OPPRI(t) & TF_ASN) || (t) == 0xdf || (t) == 0xfe || (t) == 0xc6)
#define IS_CMP(t)    (OPPRI(t) & TF_CMP)
#define IS_LOG(t)    (OPPRI(t) & TF_LOG)
#define IS_CMPLOG(t) (OPPRI(t) & (TF_CMP | TF_LOG))
#define IS_TYPE(t)   (OPPRI(t) & TF_TYPE)

/* Check if token is a type keyword - exported for declare.c */
int
isTypeToken(unsigned char t)
{
    return IS_TYPE(t);
}

/*
 * counter for generating synthetic string literal names
 */
static int stringCtr = 0;

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

	e = calloc(1, sizeof(*e));  // Zero-initialize all fields
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
frExp(struct expr *e)
{
	if (!e) {
		return;
	}
	if (e->left) {
		frExp(e->left);
	}
	if (e->right) {
		frExp(e->right);
	}
	/* Free linked list (e.g., function call arguments) */
	if (e->next) {
		frExp(e->next);
	}
	/* For STRING expressions, free the synthetic name and its init expr */
	if (e->op == STRING && e->var) {
		struct name *strname = (struct name *)e->var;
		if (strname->u.init) {
			frExp(strname->u.init);
			strname->u.init = NULL;
		}
		/* Free the counted string data */
		if (e->v) {
			free((void *)e->v);
		}
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
 * High-byte tokens (SUBEQ, MODEQ, ANDEQ) handled explicitly
 */
unsigned char
binopPri(unsigned char t)
{
    if (t < 0x80)
        return (t >= 0x20) ? (oppri[t - 0x20] & 0x0f) : 0;
    /* High-byte assignment operators: all priority 14 */
    if (t == 0xdf || t == 0xfe || t == 0xc6)
        return 14;
    return 0;
}

/*
 * Skip an expression without building a tree (phase 1)
 * Consumes tokens to stay synchronized with the lexer.
 * Mirrors parseExpr structure but doesn't allocate.
 */
static void
skipExpr(unsigned char pri)
{
    unsigned char p;

    /* Handle prefix/primary */
    switch (cur.type) {
    case NUMBER:
    case FNUMBER:
        gettoken();
        break;

    case STRING:
        gettoken();
        /* Handle adjacent string concatenation */
        while (cur.type == STRING)
            gettoken();
        break;

    case SYM:
        gettoken();
        break;

    case LPAR:
        gettoken();
        /* Check for cast: (type)expr */
        if (isCastStart()) {
            parseTypeName();  /* consume type, still need symbol table */
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
        gettoken();
        skipExpr(OP_PRI_MULT - 1);
        break;

    case STAR:
    case AND:
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

    case INCR:
    case DECR:
        gettoken();
        skipExpr(OP_PRI_MULT - 1);
        break;

    default:
        /* In phase 1, don't gripe - errors will be caught in phase 2 */
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
            /* Function call - skip arguments */
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
            gettoken();
            if (cur.type == SYM)
                gettoken();
        } else if (cur.type == INCR || cur.type == DECR) {
            gettoken();
        }
    }

    /* Handle binary operators */
    while (1) {
        p = binopPri(cur.type);
        if (p == 0)
            break;
        if (pri != 0 && p >= pri)
            break;

        if (cur.type == QUES) {
            gettoken();
            skipExpr(0);
            expect(COLON, ER_E_SP);
            skipExpr(0);
        } else {
            unsigned char is_assign = IS_ASSIGN(cur.type);
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
    struct type *value_type = operand ? operand->type : NULL;
    struct expr *e;

    if (operand && operand->op == DEREF) {
        struct expr *deref = operand;
        operand = operand->left;
        /* Free the orphaned DEREF node (but not its children) */
        deref->left = NULL;
        free(deref);
#ifdef DEBUG
        exprCurCnt--;
#endif
    } else {
        gripe(ER_E_LV);
        frExp(operand);
        operand = NULL;
    }
    e = mkexpr(inc_op, operand);
    if (e->left) {
        e->left->up = e;
        e->type = value_type;
    }
    if (is_postfix)
        e->flags |= E_POSTFIX;
    return e;
}

/*
 * Wrap expression in type conversion (NARROW/WIDEN/SEXT)
 * Fold constant conversions at compile time.
 */
static struct expr *
mkConv(struct expr *inner, struct type *tgt)
{
    struct type *src = inner->type;
    token_t op;
    struct expr *conv;

    if (tgt->size < src->size)
        op = NARROW;
    else if (src->flags & TF_UNSIGNED)
        op = WIDEN;
    else
        op = SEXT;

    /* Fold constant conversions */
    if (inner->op == CONST) {
        if (op == SEXT) {
            /* Sign extend based on source size */
            if (src->size == 1 && (inner->v & 0x80))
                inner->v |= 0xffffff00L;
            else if (src->size == 2 && (inner->v & 0x8000))
                inner->v |= 0xffff0000L;
        }
        inner->type = tgt;
        return inner;
    }

    conv = mkexprI(op, inner, tgt, 0, 0);
    conv->left->up = conv;
    return conv;
}

/* Check if type is scalar (not pointer/array/func/aggregate) */
#define IS_SCALAR(t) (!((t)->flags & (TF_POINTER|TF_ARRAY|TF_FUNC|TF_AGGREGATE)))

/*
 * Coerce expression to target type if sizes differ
 */
static struct expr *
coerceTypes(struct expr *e, struct type *tgt)
{
    if (!e || !tgt || !e->type)
        return e;
    if (e->type->size == tgt->size)
        return e;
    /* Don't convert pointers/arrays */
    if ((e->type->flags | tgt->flags) & (TF_POINTER|TF_ARRAY))
        return e;
    return mkConv(e, tgt);
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
 *   st  - Containing statement (for context, can be NULL)
 *
 * Returns:
 *   Expression tree root, or NULL on parse error
 */
struct expr *
parseExpr(unsigned char pri, struct stmt *st)
{
	/* Hoisted locals for stack reuse */
	unsigned char op, p, is_assignment, is_variadic, is_arrow;
	unsigned char uop, inc_op, compatible;
	unsigned char l_unsigned, r_unsigned;
	struct expr *e = 0;
	struct expr *e1, *e2, *e3, *e4;  /* temporaries for non-overlapping uses */
	struct type *assign_type, *t, *tp, *t2, *t3;
	struct var *vp;
	struct name *np;  /* general-purpose name pointer */
	char namebuf[32];
	char *symname;
	union { float f; unsigned long u; } fu;
	long sval;
	int src_size, tgt_size, src_unsigned, elem_size, size;
	int l_ptr, r_ptr;
	token_t cast_op;

	/* Initialize variables that need specific values */
	assign_type = NULL;
	vp = NULL;
	t2 = NULL;
	t3 = NULL;
	l_ptr = 0;
	r_ptr = 0;
	is_assignment = 0;

	/* Phase 1: just consume tokens, don't build tree */
	if (phase == 1) {
		skipExpr(pri);
		return NULL;
	}

	switch (cur.type) {   // prefix

    case NUMBER:
        sval = cur.v.numeric;
        e = mkexprI(CONST, 0, constType(sval), (unsigned long)sval, E_CONST);
        gettoken();
        break;

    case FNUMBER:
        /* Store float bit pattern in v */
        fu.f = cur.v.fval;
        e = mkexprI(CONST, 0, floattype, fu.u, E_CONST);
        gettoken();
        break;

    case STRING:
        /* string literals have type char* (pointer to char) */
        /* Lexer already concatenates adjacent strings; copy since buffer reused */
        e = mkexprI(STRING, 0, getType(TF_POINTER, chartype, 0), 0, 0);
        size = ((unsigned char *)cur.v.str)[0] + 1;  /* length byte + data */
        symname = malloc(size);
        memcpy(symname, cur.v.str, size);

        /* generate synthetic name for this string literal */
        sprintf(namebuf, "str%d", stringCtr++);

        /*
         * Allocate name structure directly without adding to
         * names[] lookup table. String literal names don't need to be
         * looked up, only emitted to AST. We don't free these - small
         * and process will exit
         */
        np = (struct name *)calloc(1, sizeof(struct name));
        if (np) {
            /* Initialize in struct field order */
            strncpy(np->name, namebuf, 15);
            np->name[15] = 0;
            np->type = e->type;
            /* chain = 0 (not in symbol table) */
            np->kind = var;
            np->level = 1;  /* Global scope */
            /* store pointer to counted string in the name's init field */
            np->u.init = mkexprI(STRING, 0, NULL,
					(unsigned long)symname, 0);
            /* also store in expression for immediate use */
            e->v = (unsigned long)symname;
            /* store reference to the named string in the expression */
            e->var = (struct var *)np;
            /* String emission deferred until name is finalized */
        }
        e->flags = E_CONST;
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
                tp = calloc(1, sizeof(struct type));
                tp->flags = TF_FUNC;
                tp->sub = inttype;  /* Return type: int */
                tp->elem = NULL;    /* No parameter info */

                np = calloc(1, sizeof(struct name));
                /* Initialize in struct field order */
                strncpy(np->name, symname, 15);
                np->name[15] = 0;
                np->type = tp;
                /* chain set by addName */
                np->kind = var;
                np->level = 1;  /* Global scope */
                /* is_tag = 0 from calloc */
                np->sclass = SC_EXTERN;

                addName(np);

#ifdef DEBUG
                if (VERBOSE(V_SYM)) {
                    fdprintf(2, "Implicit declaration: int %s()\n", symname);
                }
#endif
            } else {
                /* Not a function call - report error */
                gripe(ER_E_UO);
                e = mkexprI(CONST, 0, inttype, 0, 0);
                free(symname);
                break;
            }
        }

        /* Count references for register allocation (done in phase 1).
         * Only count locals and parameters, not globals or functions. */
        if (phase == 1 && np->level > 1 && np->kind != elem &&
            !(np->type && (np->type->flags & (TF_FUNC|TF_ARRAY)))) {
            if (np->ref_count < 255)
                np->ref_count++;
        }

        if (np->kind == elem) {
            // Enum constant: treat as integer constant
            e = mkexprI(CONST, 0, inttype, np->offset, E_CONST);
        } else {
            e1 = mkexprI(SYM, 0, np->type, 0, 0);
            e1->var = (struct var *)np;

            // Functions and arrays decay to pointers (addresses)
            // Only wrap non-functions in DEREF to get their value
            if (np->type && (np->type->flags & TF_FUNC)) {
                // Function name: return address (decay to function pointer)
                e = e1;
            } else if (np->type && (np->type->flags & TF_ARRAY)) {
                // Array name: decays to pointer to first element
                e = e1;
            } else {
                // Variable: wrap in DEREF to get value
                e = mkexprI(DEREF, e1, np->type, 0, 0);
            }
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
            e1 = parseExpr(OP_PRI_MULT - 1, st);

            /* Determine if cast needs runtime operation */
            if (tp && e1 && e1->type) {
                /* Pointer-to-pointer casts are just type reinterpretation */
                if ((tp->flags & TF_POINTER) &&
						(e1->type->flags & TF_POINTER)) {
                    e1->type = tp;
                    e = e1;
                }
                /* Scalar casts: determine which operation needed */
                else if (!(tp->flags &
						(TF_POINTER|TF_ARRAY|TF_FUNC)) &&
                         !(e1->type->flags &
						(TF_POINTER|TF_ARRAY|TF_FUNC))) {
                    src_size = e1->type->size;
                    tgt_size = tp->size;
                    src_unsigned = e1->type->flags & TF_UNSIGNED;

                    if (tgt_size == src_size) {
                        /*
                         * Same size: just reinterpret
                         * (e.g., int <-> unsigned int)
                         */
                        e1->type = tp;
                        e = e1;
                    } else {
                        if (tgt_size < src_size) {
                            /* Narrowing: truncate to smaller type */
                            cast_op = NARROW;
                        } else {
                            /* Widening: extend to larger type */
                            if (src_unsigned) {
                                cast_op = WIDEN;  /* zero extend unsigned */
                            } else {
                                cast_op = SEXT;   /* sign extend signed */
                            }
                        }

                        e = mkexprI(cast_op, e1, tp, 0, 0);
                    }
                }
                /* Mixed pointer/scalar casts: need conversion */
                else {
                    src_size = e1->type->size;
                    tgt_size = tp->size;

                    if (tgt_size == src_size) {
                        /* Same size: just reinterpret */
                        e1->type = tp;
                        e = e1;
                    } else {
                        cast_op = (tgt_size < src_size) ? NARROW : WIDEN;
                        e = mkexprI(cast_op, e1, tp, 0, 0);
                    }
                }
            } else {
                /* Shouldn't happen, but create NARROW as fallback */
                e = mkexprI(NARROW, e1, tp, 0, 0);
            }
        } else {
            /*
             * Parenthesized expression: (expr)
             * parse inner expression with lowest precedence
             */
            e = parseExpr(0, st);
            expect(RPAR, ER_E_SP);
        }
        break;

    case MINUS:     // unary minus
    case TWIDDLE:   // bitwise not
    case BANG:      // logical not
        uop = (cur.type == MINUS) ? NEG : cur.type;
        gettoken();
        e = mkexpr(uop, parseExpr(OP_PRI_MULT - 1, st));
        if (e->left) {
            e->type = e->left->type;
            e->left->up = e;
        }
        e = cfold(e);
        break;

    case STAR:      // dereference (unary)
        gettoken();
        e = mkexpr(DEREF, parseExpr(OP_PRI_MULT - 1, st));
        if (e->left) {
            e->left->up = e;
            // type will be determined later when we have full type info
            if (e->left->type && (e->left->type->flags & TF_POINTER) &&
					e->left->type->sub) {
                e->type = e->left->type->sub;
            } else {
                e->type = e->left->type;
            }
        }
        break;

    case AND:       // address-of (unary)
        gettoken();
        e = parseExpr(OP_PRI_MULT - 1, st);
        /* Mark variable as address-taken (can't use register) */
        if (e && e->op == DEREF && e->left && e->left->op == SYM &&
            e->left->var)
            ((struct name *)e->left->var)->addr_taken = 1;
        /* Optimize: &(DEREF x) = x, since SYM already gives address */
        if (e && e->op == DEREF) {
            e1 = e;
            e = e->left;
            /*
             * Wrap the type in a pointer. For &var where var is T,
             * the result type should be T* (pointer to T).
             */
            if (e->type) {
                e->type = getType(TF_POINTER, e->type, 0);
            }
            /* Free the orphaned DEREF node (but not its children) */
            e1->left = NULL;
            free(e1);
#ifdef DEBUG
            exprCurCnt--;
#endif
        } else if (e && e->type && (e->type->flags & TF_ARRAY)) {
            /* &array = array (just change type to pointer-to-array) */
            e->type = getType(TF_POINTER, e->type, 0);
        } else {
            e1 = mkexpr(AND, e);
            if (e) {
                e->up = e1;
                if (e->type) {
                    e1->type = getType(TF_POINTER, e->type, 0);
                }
            }
            e = e1;
        }
        break;

    case SIZEOF:    // sizeof operator
        gettoken();
        // Check if it's sizeof(type) or sizeof expr
        if (cur.type == LPAR) {
            // Could be sizeof(type) or sizeof(expr)
            // Try to parse as type first
            gettoken();  // consume '('

            t = getbasetype();
            if (t) {
                // It's sizeof(type)
                // Handle pointer modifiers (e.g., sizeof(int *))
                while (cur.type == STAR) {
                    gettoken();
                    t = getType(TF_POINTER, t, 0);
                }

                expect(RPAR, ER_E_SP);

                // Create constant expression with the size
                e = mkexprI(CONST, 0, inttype, t->size, E_CONST);
            } else {
                // It's sizeof(expr) - parse as expression
                e = parseExpr(0, st);
                expect(RPAR, ER_E_SP);

                // Create constant expression with the size
                // of the expression's type
                if (e && e->type) {
                    size = e->type->size;
                    frExp(e);  // we only needed it for the type
                    e = mkexprI(CONST, 0, inttype, size, E_CONST);
                } else {
                    gripe(ER_E_UO);  // couldn't determine type
                    e = mkexprI(CONST, 0, inttype, 0, E_CONST);
                }
            }
        } else {
            // sizeof expr (without parentheses)
            e1 = parseExpr(OP_PRI_MULT - 1, st);
            if (e1 && e1->type) {
                size = e1->type->size;
                frExp(e1);
                e = mkexprI(CONST, 0, inttype, size, E_CONST);
            } else {
                gripe(ER_E_UO);
                e = mkexprI(CONST, 0, inttype, 0, E_CONST);
            }
        }
        break;

    case INCR:      // prefix increment: ++i
    case DECR:      // prefix decrement: --i
        inc_op = cur.type;
        gettoken();
        e = mkIncDec(parseExpr(OP_PRI_MULT - 1, st), inc_op, 0);
        break;

	default:
		gripe(ER_E_UO);
		return 0;
    }

    /*
     * Handle postfix operators: function calls, array subscripts,
     * struct access, increment/decrement
     */
    while (cur.type == LPAR || cur.type == LBRACK || cur.type == DOT ||
			cur.type == ARROW || cur.type == INCR || cur.type == DECR) {
        if (cur.type == LBRACK) {
            // Array subscript: arr[idx] = DEREF(ADD(base, idx * sizeof))
            tp = NULL;

            gettoken();  // consume '['
            e1 = parseExpr(0, st);  /* index */
            expect(RBRACK, ER_E_SP);

            /*
             * Unwrap DEREF to get base address,
             * but save the dereferenced type
             */
            if (e && e->op == DEREF) {
                e2 = e;  /* deref - save for freeing */
                /* Save the actual type (not the address type) */
                tp = e->type;
                e = e->left;
                /* Free the orphaned DEREF node (but not its children) */
                e2->left = NULL;
                free(e2);
#ifdef DEBUG
                exprCurCnt--;
#endif
            } else {
                tp = e->type;
            }

            /*
             * Get element size from type
             * (use tp, not e->type which is the address)
             */
            elem_size = 2;  // default to short/int size
            if (tp) {
                if (tp->flags & TF_POINTER && tp->sub) {
                    elem_size = tp->sub->size;
                } else if (tp->flags & TF_ARRAY && tp->sub) {
                    elem_size = tp->sub->size;
                }
            }

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
                e2 = cfold(e2);
            }

            // Add scaled offset to base: base + (idx * sizeof)
            e3 = mkexpr(PLUS, e);  /* addr */
            e3->right = e2;
            e3->left->up = e3;
            e3->right->up = e3;
            /* The ADD result is a pointer to the element type */
            if (tp && (tp->flags & TF_ARRAY) &&
					tp->sub) {
                e3->type = getType(TF_POINTER, tp->sub, 0);
            } else if (tp && (tp->flags & TF_POINTER)) {
                e3->type = tp;  // pointer + offset = same pointer type
            } else {
                e3->type = tp;
            }

            // Fold nested constant offsets (e.g., (ptr + 2) + 0 -> ptr + 2)
            e3 = cfold(e3);

            // Dereference to get element value
            e = mkexpr(DEREF, e3);
            e->left->up = e;
            if (e->left->type) {
                if ((e->left->type->flags & TF_POINTER) &&
						e->left->type->sub) {
                    e->type = e->left->type->sub;
                } else if ((e->left->type->flags & TF_ARRAY) &&
						e->left->type->sub) {
                    e->type = e->left->type->sub;
                }
            }
        } else if (cur.type == LPAR) {
            // Function call: expr(arg1, arg2, ...)
            gettoken();  // consume '('

            // Create CALL node with function expression as left operand
            e1 = mkexpr(CALL, e);  /* call */
            e1->left->up = e1;

            // Set return type from function type
            if (e->type && (e->type->flags & TF_FUNC) && e->type->sub) {
                e1->type = e->type->sub;
            }

            // Get first parameter for type coercion
            np = (e->type && (e->type->flags & TF_FUNC)) ? e->type->elem : 0;
            is_variadic = e->type && (e->type->flags & TF_VARIADIC);

            // Parse argument list
            e3 = NULL;  /* lastarg */
            if (cur.type != RPAR) {
                // Parse first argument
                e2 = parseExpr(OP_PRI_COMMA, st);  /* arg */
                if (e2) {
                    e2->flags |= E_FUNARG;
                    // Coerce argument to parameter type
                    if (np && np->type)
                        e2 = coerceTypes(e2, np->type);
                    else if (is_variadic && e2->type &&
                             e2->type->size < 2)
                        e2 = mkConv(e2, inttype);
                    e1->right = e2;
                    e2->up = e1;
                    e3 = e2;
                    if (np) np = np->next;
                }

                // Parse remaining arguments
                while (cur.type == COMMA) {
                    gettoken();
                    e2 = parseExpr(OP_PRI_COMMA, st);  /* arg */
                    if (e2) {
                        e2->flags |= E_FUNARG;
                        // Coerce argument to parameter type
                        if (np && np->type)
                            e2 = coerceTypes(e2, np->type);
                        else if (is_variadic && e2->type &&
                                 e2->type->size < 2)
                            e2 = mkConv(e2, inttype);
                        if (e3) {
                            e3->next = e2;
                            e2->prev = e3;
                        }
                        e3 = e2;
                        if (np) np = np->next;
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
                gripe(ER_E_UO);
                break;
            }

            // For s.x: e is DEREF(SYM s), unwrap to get SYM s
            // For p->x: e is DEREF(SYM p), keep as-is (pointer value)
            if (is_arrow) {
                e1 = e;  /* base - pointer value */
            } else {
                // Unwrap DEREF to get address
                if (e && e->op == DEREF) {
                    e2 = e;  /* deref - save for freeing */
                    e1 = e->left;  /* base */
                    /* Free the orphaned DEREF node (but not its children) */
                    e2->left = NULL;
                    free(e2);
#ifdef DEBUG
                    exprCurCnt--;
#endif
                } else {
                    e1 = e;  /* base */
                }
            }

            // Look up member in struct/union
            np = NULL;  /* member */
            if (e1 && e1->type) {
                t = e1->type;
                /*
                 * For both DOT and ARROW, if base type is pointer,
                 * get the pointed-to type (DOT after array subscript
                 * produces pointer type)
                 */
                if (t->flags & TF_POINTER) {
                    t = t->sub;
                }
                if (t && (t->flags & TF_AGGREGATE) && t->elem) {
                    for (np = t->elem; np; np = np->next) {
                        if (strcmp(np->name, cur.v.name) == 0) {
                            break;
                        }
                    }
                }
            }

            if (!np) {
                gripe(ER_E_UO);
                gettoken();
                e = mkexprI(CONST, 0, NULL, 0, 0);
                break;
            }

            /*
             * Generate: DEREF(ADD(base, offset)) or BFEXTRACT
             * for bitfields
             */
            e2 = mkexprI(CONST, 0, inttype,
					np->offset, E_CONST);  /* offset_expr */

            e3 = mkexpr(PLUS, e1);  /* addr */
            e3->right = e2;
            e3->left->up = e3;
            e3->right->up = e3;
            // addr is pointer to member, not pointer to base struct
            e3->type = getType(TF_POINTER, np->type, 0);

            // Fold constant offset (e.g., x + 0 becomes x)
            e3 = cfold(e3);

            // Check if this is a bitfield access
            if (np->kind == bitfield) {
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
            } else if (np->type && (np->type->flags & TF_ARRAY)) {
                /*
                 * Array member: return address without DEREF
                 * Arrays decay to pointers but are not lvalues
                 * This prevents arr++ while allowing p = arr
                 */
                e = e3;
                e->type = np->type;  /* Keep array type for proper semantics */
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
            e2 = parseExpr(0, st);  /* true_expr */

            // Expect and consume COLON
            expect(COLON, ER_E_SP);

            /*
             * Parse the false expression (right-associative,
             * allow another ?: at same level). Use priority 0 to parse
             * everything, including nested ?: operators
             */
            e3 = parseExpr(0, st);  /* false_expr */

            // Build tree: QUES(condition, COLON(true_expr, false_expr))
            e4 = mkexpr(COLON, e2);  /* colon_node */
            e4->right = e3;
            if (e4->left) e4->left->up = e4;
            if (e4->right) e4->right->up = e4;

            e = mkexpr(QUES, e1);
            e->right = e4;
            if (e->left) e->left->up = e;
            if (e->right) e->right->up = e;

            /*
             * Type is the type of the result expressions
             * (should check compatibility)
             */
            if (e2 && e2->type) {
                e->type = e2->type;
            }

            e = cfold(e);
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
            if (e && e->op == DEREF) {
                e1 = e;  /* deref - save for freeing */
#ifdef DEBUG
                if (VERBOSE(V_ASSIGN)) {
                    if (e->type) {
                        fdprintf(2, "ASSIGN: unwrapping DEREF, "
								"type=%p (flags=0x%x, size=%d)\n",
                                e->type, e->type->flags, e->type->size);
                        if (e->type->sub) {
                            fdprintf(2, "        "
									"sub=%p (flags=0x%x, size=%d)\n",
                                    e->type->sub, e->type->sub->flags,
									e->type->sub->size);
                        }
                    }
                }
#endif
                /* Save the type before unwrapping */
                assign_type = e->type;
                e = e->left;  // unwrap to get address
                /* Free the orphaned DEREF node (but not its children) */
                e1->left = NULL;
                free(e1);
#ifdef DEBUG
                exprCurCnt--;
#endif
            } else if (e && e->op == BFEXTRACT) {
                e1 = e;  /* bfextr - save for freeing */
                // Bitfield assignment - change to BFASSIGN
                /* Save the type before unwrapping */
                assign_type = e->type;
                /*
                 * Keep the var field which has the member info
                 * (bitoff, width)
                 */
                vp = e->var;
                e = e->left;  // unwrap to get address
                /* Free the orphaned BFEXTRACT node (but not its children) */
                e1->left = NULL;
                free(e1);
#ifdef DEBUG
                exprCurCnt--;
#endif
                /*
                 * Store member info temporarily - we'll use it when
                 * creating BFASSIGN. Pass through member info
                 */
                e->var = vp;
                // Flag that we need BFASSIGN
                if (op == ASSIGN) {
                    op = BFASSIGN;
                }
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
                frExp(parseExpr(p, st));  // Parse and discard right side
                return e;  // Return left side unchanged
            }
        }

        /*
         * Check if this is a struct/array assignment requiring
         * memory copy. For aggregates (structs, arrays), use COPY
         * instead of ASSIGN. Check both direct aggregate type and
         * dereferenced pointer to aggregate
         */
        if (op == ASSIGN && assign_type &&
				(assign_type->flags & TF_AGGREGATE)) {
            op = COPY;  // Change to memory copy operator
        } else if (op == ASSIGN && e && e->type) {
            // Direct aggregate (not dereferenced)
            if (e->type->flags & TF_AGGREGATE) {
                assign_type = e->type;
                op = COPY;
            }
        }

        /*
         * Parse right side based on associativity:
         * - For right-associative operators (assignments), use
         *   precedence 0 to allow chaining
         * - For left-associative operators, use precedence p to prevent
         *   same-precedence from nesting right
         */
        vp = (e && e->var) ? e->var : NULL;
        e = mkexpr(op, e);
        e->left->up = e;
        if (is_assignment) {
            /*
             * Right-associative: parse at lowest precedence
             * to allow a = b = c
             */
            e->right = parseExpr(0, st);
        } else {
            /*
             * Left-associative: parse at same precedence to prevent
             * (a + b) + c from becoming a + (b + c)
             */
            e->right = parseExpr(p, st);
        }
        if (e->right) {
            e->right->up = e;
        }

        // For BFASSIGN, restore member info (bitoff, width)
        if (op == BFASSIGN && vp) {
            e->var = vp;
        }

        // For COPY operator, unwrap DEREF from right side to get address
        if (e->op == COPY && e->right && e->right->op == DEREF) {
            e1 = e->right;  /* deref - save for freeing */
            e->right = e->right->left;  // unwrap to get address
            /* Free the orphaned DEREF node (but not its children) */
            e1->left = NULL;
            free(e1);
#ifdef DEBUG
            exprCurCnt--;
#endif
        }

        /* For plain assignments, insert type conversion if needed.
         * Compound assignments (|=, +=, etc.) don't need WIDEN - pass2
         * can handle mixed sizes for the operation itself. */
        if (op == ASSIGN && e->left && e->right && e->left->type &&
				e->right->type) {
            t = assign_type ? assign_type : e->left->type;
            tp = e->right->type;

            if (IS_SCALAR(t) && IS_SCALAR(tp) &&
					t->size != tp->size) {
                if (e->right->op == CONST) {
                    e->right->type = t;
                } else {
                    e->right = mkConv(e->right, t);
                    e->right->up = e;
                }
            }

            // Check pointer type compatibility
            l_ptr = (t->flags & (TF_POINTER|TF_ARRAY));
            r_ptr = (tp->flags & (TF_POINTER|TF_ARRAY));

            if (l_ptr && r_ptr) {
                t2 = t->sub;
                t3 = tp->sub;

                // Both must have base types
                if (t2 && t3) {
                    compatible = 0;

                    // void* is compatible with any pointer type
                    if (t2 == voidtype || t3 == voidtype) {
                        compatible = 1;
                    }
                    // Check if base types are compatible
                    else if (t2 == t3) {
                        // Same type pointer - always compatible
                        compatible = 1;
                    } else if ((t2->flags & TF_AGGREGATE) &&
							(t3->flags & TF_AGGREGATE)) {
                        /*
                         * Both point to struct/union - must be same type
                         * For now, just check if pointers are equal
                         * (type unification)
                         */
                        compatible = (t2 == t3);
                    } else if (!(t2->flags & TF_AGGREGATE) &&
							!(t3->flags & TF_AGGREGATE)) {
                        /*
                         * Both point to non-aggregate types
                         * Check if they have same size and signedness
                         */
                        if (t2->size == t3->size) {
                            l_unsigned = (t2->flags & TF_UNSIGNED);
                            r_unsigned = (t3->flags & TF_UNSIGNED);
                            compatible = (l_unsigned == r_unsigned);
                        }
                    }

                    if (!compatible) {
#ifdef DEBUG
                        fdprintf(2, "INCOMPATIBLE POINTERS:\n");
                        fdprintf(2, "  Left:  t=%p "
								"(flags=0x%x, size=%d)\n",
								t, t->flags, t->size);
                        fdprintf(2, "         t2=%p "
								"(flags=0x%x, size=%d)\n",
								t2, t2->flags, t2->size);
                        if (t2 && (t2->flags & TF_POINTER) &&
								t2->sub) {
                            fdprintf(2, "         t2->sub=%p "
									"(flags=0x%x, size=%d)\n",
                                    t2->sub, t2->sub->flags,
									t2->sub->size);
                        }
                        fdprintf(2, "  Right: tp=%p "
								"(flags=0x%x, size=%d)\n",
								tp, tp->flags, tp->size);
                        fdprintf(2, "         t3=%p "
								"(flags=0x%x, size=%d)\n",
								t3, t3->flags, t3->size);
                        if (tp && (tp->flags & TF_POINTER) &&
								tp->sub) {
                            fdprintf(2, "         tp->sub=%p "
									"(flags=0x%x, size=%d)\n",
                                    tp->sub, tp->sub->flags,
									tp->sub->size);
                        }
#endif
                        gripe(ER_E_PT);  // incompatible pointer types
                    }
                }
            }
        }

        /*
         * Don't widen operands of binary expressions here - pass2 can
         * decide if widening is needed based on the operation. WIDEN is
         * only required for assignments and function call arguments.
         */

        // try to determine result type
        if (e->left && e->right) {
            // For ASSIGN and compound assignments, use the saved assign_type
            if (is_assignment && assign_type) {
                e->type = assign_type;
            }
            // Comparisons and logical ops produce boolean (byte) result
            else if (IS_CMPLOG(op)) {
                e->type = uchartype;
            }
            // For other operators, use the larger type as result type
            else if (e->left->type && e->right->type) {
                if (e->left->type->size >= e->right->type->size) {
                    e->type = e->left->type;
                } else {
                    e->type = e->right->type;
                }
            } else if (e->left->type) {
                e->type = e->left->type;
            } else if (e->right->type) {
                e->type = e->right->type;
            }

            // For COPY operator, store byte count in v field
            if (e->op == COPY) {
                if (assign_type) {
                    e->v = assign_type->size;
                    e->type = assign_type;  // Use the aggregate type
                } else if (e->type) {
                    e->v = e->type->size;
                }
            }
        } else if (e->left) {
            e->type = e->left->type;
        }

        e = normalize(e);
        e = cfold(e);
    }
    return e;
}


/*
 * vim: tabstop=4 shiftwidth=4 expandtab:
 */
