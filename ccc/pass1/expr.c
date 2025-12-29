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
	unsigned char op;
	struct expr *e = 0;
    unsigned char p;
	struct type *assign_type = NULL;
	unsigned char is_assignment = 0;
	struct var *member_info = NULL;
	struct var *savMemberInf = NULL;
	int l_ptr = 0;
	int r_ptr = 0;
	struct type *l_base = NULL;
	struct type *r_base = NULL;

	/* Phase 1: just consume tokens, don't build tree */
	if (phase == 1) {
		skipExpr(pri);
		return NULL;
	}

	switch (cur.type) {   // prefix

    case NUMBER: {
        long sval = cur.v.numeric;
        e = mkexprI(CONST, 0, constType(sval), (unsigned long)sval, E_CONST);
        gettoken();
        break;
    }

    case FNUMBER: {
        /* Store float bit pattern in v */
        union { float f; unsigned long u; } fu;
        fu.f = cur.v.fval;
        e = mkexprI(CONST, 0, floattype, fu.u, E_CONST);
        gettoken();
        break;
    }

    case STRING: {
        struct name *strname;
        char namebuf[32];
        unsigned char *combined_str;
        unsigned char *src_str;
        unsigned char total_len;
        int i;

        /* string literals have type char* (pointer to char) */
        e = mkexprI(STRING, 0, getType(TF_POINTER, chartype, 0), 0, 0);

        /* Copy first string - always allocate so we own the memory */
        src_str = (unsigned char *)cur.v.str;
        total_len = src_str[0];
        combined_str = (unsigned char *)malloc(total_len + 1);
        combined_str[0] = total_len;
        for (i = 0; i < total_len; i++) {
            combined_str[i + 1] = src_str[i + 1];
        }

        gettoken();

        /* Check for adjacent string literals and concatenate them */
        while (cur.type == STRING) {
            unsigned char *temp;
            unsigned char next_len;

            next_len = ((unsigned char *)cur.v.str)[0];

            /* Allocate new buffer for concatenated string */
            temp = (unsigned char *)malloc(total_len + next_len + 1);
            temp[0] = total_len + next_len;  /* new length */

            /* Copy first string */
            for (i = 0; i < total_len; i++) {
                temp[i + 1] = combined_str[i + 1];
            }

            /* Append next string */
            for (i = 0; i < next_len; i++) {
                temp[total_len + i + 1] = ((unsigned char *)cur.v.str)[i + 1];
            }

            /* Free old combined string */
            free(combined_str);

            combined_str = temp;
            total_len = temp[0];

            gettoken();  /* consume the STRING token */
        }

        /* generate synthetic name for this string literal */
        sprintf(namebuf, "str%d", stringCtr++);

        /*
         * Allocate name structure directly without adding to
         * names[] lookup table. String literal names don't need to be
         * looked up, only emitted to AST. We don't free these - small
         * and process will exit
         */
        strname = (struct name *)calloc(1, sizeof(struct name));
        if (strname) {
            /* Initialize in struct field order */
            strncpy(strname->name, namebuf, 15);
            strname->name[15] = 0;
            strname->type = e->type;
            /* chain = 0 (not in symbol table) */
            strname->kind = var;
            strname->level = 1;  /* Global scope */
            /* store pointer to counted string in the name's init field */
            strname->u.init = mkexprI(STRING, 0, NULL,
					(unsigned long)combined_str, 0);
            /* also store in expression for immediate use */
            e->v = (unsigned long)combined_str;
            /* store reference to the named string in the expression */
            e->var = (struct var *)strname;
            /* String emission deferred until name is finalized */
        }
        e->flags = E_CONST;
        break;
    }

    case SYM: {
        /* Symbol reference - SYM = address
         * For variables: wrap in DEREF to get value
         * For functions: return address (decay to function pointer)
         */
        struct name *n;
        struct expr *sym;
        char *symname;

        /* Save symbol name before gettoken() overwrites cur.v.name */
        symname = strdup(cur.v.name);

        n = findName(symname, 0);

        /* Peek at next token to enable implicit function declarations */
        gettoken();

        if (!n) {
            /* Undefined symbol */
            /* K&R extension: if followed by '(', implicitly declare as
             * function returning int */
            if (cur.type == LPAR) {
                struct type *functype;

                /* Create implicit function declaration: int name() */
                functype = calloc(1, sizeof(struct type));
                functype->flags = TF_FUNC;
                functype->sub = inttype;  /* Return type: int */
                functype->elem = NULL;    /* No parameter info */

                n = calloc(1, sizeof(struct name));
                /* Initialize in struct field order */
                strncpy(n->name, symname, 15);
                n->name[15] = 0;
                n->type = functype;
                /* chain set by addName */
                n->kind = var;
                n->level = 1;  /* Global scope */
                /* is_tag = 0 from calloc */
                n->sclass = SC_EXTERN;

                addName(n);

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
        if (phase == 1 && n->level > 1 && n->kind != elem &&
            !(n->type && (n->type->flags & (TF_FUNC|TF_ARRAY)))) {
            if (n->ref_count < 255)
                n->ref_count++;
        }

        if (n->kind == elem) {
            // Enum constant: treat as integer constant
            e = mkexprI(CONST, 0, inttype, n->offset, E_CONST);
        } else {
            sym = mkexprI(SYM, 0, n->type, 0, 0);
            sym->var = (struct var *)n;

            // Functions and arrays decay to pointers (addresses)
            // Only wrap non-functions in DEREF to get their value
            if (n->type && (n->type->flags & TF_FUNC)) {
                // Function name: return address (decay to function pointer)
                e = sym;
            } else if (n->type && (n->type->flags & TF_ARRAY)) {
                // Array name: decays to pointer to first element
                e = sym;
            } else {
                // Variable: wrap in DEREF to get value
                e = mkexprI(DEREF, sym, n->type, 0, 0);
            }
        }
        free(symname);
        /* Note: gettoken() already called above for lookahead */
        break;
    }

    /* unary operators */
    case LPAR:      // parenthesized expression or type cast
        gettoken();

        /* Check if this is a type cast: (type)expr */
        if (isCastStart()) {
            struct type *cast_type;
            struct expr *inner;

            /* Parse the type name */
            cast_type = parseTypeName();
            expect(RPAR, ER_E_SP);

            /* Parse the expression being cast */
            /* Cast has unary precedence */
            inner = parseExpr(OP_PRI_MULT - 1, st);

            /* Determine if cast needs runtime operation */
            if (cast_type && inner && inner->type) {
                /* Pointer-to-pointer casts are just type reinterpretation */
                if ((cast_type->flags & TF_POINTER) &&
						(inner->type->flags & TF_POINTER)) {
                    inner->type = cast_type;
                    e = inner;
                }
                /* Scalar casts: determine which operation needed */
                else if (!(cast_type->flags &
						(TF_POINTER|TF_ARRAY|TF_FUNC)) &&
                         !(inner->type->flags &
						(TF_POINTER|TF_ARRAY|TF_FUNC))) {
                    int src_size = inner->type->size;
                    int tgt_size = cast_type->size;
                    int src_unsigned = inner->type->flags & TF_UNSIGNED;

                    if (tgt_size == src_size) {
                        /*
                         * Same size: just reinterpret
                         * (e.g., int <-> unsigned int)
                         */
                        inner->type = cast_type;
                        e = inner;
                    } else {
                        token_t cast_op;

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

                        e = mkexprI(cast_op, inner, cast_type, 0, 0);
                    }
                }
                /* Mixed pointer/scalar casts: need conversion */
                else {
                    int src_size = inner->type->size;
                    int tgt_size = cast_type->size;

                    if (tgt_size == src_size) {
                        /* Same size: just reinterpret */
                        inner->type = cast_type;
                        e = inner;
                    } else {
                        token_t cast_op = (tgt_size < src_size) ? NARROW : WIDEN;
                        e = mkexprI(cast_op, inner, cast_type, 0, 0);
                    }
                }
            } else {
                /* Shouldn't happen, but create NARROW as fallback */
                e = mkexprI(NARROW, inner, cast_type, 0, 0);
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
    case BANG: {    // logical not
        unsigned char uop = (cur.type == MINUS) ? NEG : cur.type;
        gettoken();
        e = mkexpr(uop, parseExpr(OP_PRI_MULT - 1, st));
        if (e->left) {
            e->type = e->left->type;
            e->left->up = e;
        }
        e = cfold(e);
        break;
    }

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
            struct expr *deref = e;
            e = e->left;
            /*
             * Wrap the type in a pointer. For &var where var is T,
             * the result type should be T* (pointer to T).
             */
            if (e->type) {
                e->type = getType(TF_POINTER, e->type, 0);
            }
            /* Free the orphaned DEREF node (but not its children) */
            deref->left = NULL;
            free(deref);
#ifdef DEBUG
            exprCurCnt--;
#endif
        } else if (e && e->type && (e->type->flags & TF_ARRAY)) {
            /* &array = array (just change type to pointer-to-array) */
            e->type = getType(TF_POINTER, e->type, 0);
        } else {
            struct expr *addr = mkexpr(AND, e);
            if (e) {
                e->up = addr;
                if (e->type) {
                    addr->type = getType(TF_POINTER, e->type, 0);
                }
            }
            e = addr;
        }
        break;

    case SIZEOF:    // sizeof operator
        gettoken();
        // Check if it's sizeof(type) or sizeof expr
        if (cur.type == LPAR) {
            // Could be sizeof(type) or sizeof(expr)
            // Try to parse as type first
            struct type *t;
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
                    int size = e->type->size;
                    frExp(e);  // we only needed it for the type
                    e = mkexprI(CONST, 0, inttype, size, E_CONST);
                } else {
                    gripe(ER_E_UO);  // couldn't determine type
                    e = mkexprI(CONST, 0, inttype, 0, E_CONST);
                }
            }
        } else {
            // sizeof expr (without parentheses)
            struct expr *operand = parseExpr(OP_PRI_MULT - 1, st);
            if (operand && operand->type) {
                int size = operand->type->size;
                frExp(operand);
                e = mkexprI(CONST, 0, inttype, size, E_CONST);
            } else {
                gripe(ER_E_UO);
                e = mkexprI(CONST, 0, inttype, 0, E_CONST);
            }
        }
        break;

    case INCR:      // prefix increment: ++i
    case DECR: {    // prefix decrement: --i
        unsigned char inc_op = cur.type;
        gettoken();
        e = mkIncDec(parseExpr(OP_PRI_MULT - 1, st), inc_op, 0);
        break;
    }

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
            struct expr *index, *scaled, *addr, *size_expr;
            int elem_size;
            struct type *base_type = NULL;

            gettoken();  // consume '['
            index = parseExpr(0, st);
            expect(RBRACK, ER_E_SP);

            /*
             * Unwrap DEREF to get base address,
             * but save the dereferenced type
             */
            if (e && e->op == DEREF) {
                struct expr *deref = e;
                /* Save the actual type (not the address type) */
                base_type = e->type;
                e = e->left;
                /* Free the orphaned DEREF node (but not its children) */
                deref->left = NULL;
                free(deref);
#ifdef DEBUG
                exprCurCnt--;
#endif
            } else {
                base_type = e->type;
            }

            /*
             * Get element size from type
             * (use base_type, not e->type which is the address)
             */
            elem_size = 2;  // default to short/int size
            if (base_type) {
                if (base_type->flags & TF_POINTER && base_type->sub) {
                    elem_size = base_type->sub->size;
                } else if (base_type->flags & TF_ARRAY && base_type->sub) {
                    elem_size = base_type->sub->size;
                }
            }

            // Scale index by element size: idx * sizeof(elem)
            if (elem_size == 1) {
                scaled = index;
            } else {
                size_expr = mkexprI(CONST, 0, inttype,
						elem_size, E_CONST);

                scaled = mkexpr(STAR, index);
                scaled->right = size_expr;
                scaled->left->up = scaled;
                scaled->right->up = scaled;
                scaled->type = inttype;
                scaled = cfold(scaled);
            }

            // Add scaled offset to base: base + (idx * sizeof)
            addr = mkexpr(PLUS, e);
            addr->right = scaled;
            addr->left->up = addr;
            addr->right->up = addr;
            /* The ADD result is a pointer to the element type */
            if (base_type && (base_type->flags & TF_ARRAY) &&
					base_type->sub) {
                addr->type = getType(TF_POINTER, base_type->sub, 0);
            } else if (base_type && (base_type->flags & TF_POINTER)) {
                addr->type = base_type;  // pointer + offset = same pointer type
            } else {
                addr->type = base_type;
            }

            // Fold nested constant offsets (e.g., (ptr + 2) + 0 -> ptr + 2)
            addr = cfold(addr);

            // Dereference to get element value
            e = mkexpr(DEREF, addr);
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
            struct expr *call, *arg, *lastarg;
            struct name *param;
            unsigned char is_variadic;

            gettoken();  // consume '('

            // Create CALL node with function expression as left operand
            call = mkexpr(CALL, e);
            call->left->up = call;

            // Set return type from function type
            if (e->type && (e->type->flags & TF_FUNC) && e->type->sub) {
                call->type = e->type->sub;
            }

            // Get first parameter for type coercion
            param = (e->type && (e->type->flags & TF_FUNC)) ? e->type->elem : 0;
            is_variadic = e->type && (e->type->flags & TF_VARIADIC);

            // Parse argument list
            lastarg = NULL;
            if (cur.type != RPAR) {
                // Parse first argument
                arg = parseExpr(OP_PRI_COMMA, st);
                if (arg) {
                    arg->flags |= E_FUNARG;
                    // Coerce argument to parameter type
                    if (param && param->type)
                        arg = coerceTypes(arg, param->type);
                    else if (is_variadic && arg->type &&
                             arg->type->size < 2)
                        arg = mkConv(arg, inttype);
                    call->right = arg;
                    arg->up = call;
                    lastarg = arg;
                    if (param) param = param->next;
                }

                // Parse remaining arguments
                while (cur.type == COMMA) {
                    gettoken();
                    arg = parseExpr(OP_PRI_COMMA, st);
                    if (arg) {
                        arg->flags |= E_FUNARG;
                        // Coerce argument to parameter type
                        if (param && param->type)
                            arg = coerceTypes(arg, param->type);
                        else if (is_variadic && arg->type &&
                                 arg->type->size < 2)
                            arg = mkConv(arg, inttype);
                        if (lastarg) {
                            lastarg->next = arg;
                            arg->prev = lastarg;
                        }
                        lastarg = arg;
                        if (param) param = param->next;
                    }
                }
            }

            expect(RPAR, ER_E_SP);

            // Result type will be determined later from function signature
            e = call;
        } else if (cur.type == DOT || cur.type == ARROW) {
            // Struct member access: s.x or p->x
            struct expr *base, *offset_expr, *addr;
            struct name *member;
            unsigned char is_arrow = (cur.type == ARROW);

            gettoken();  // consume '.' or '->'

            if (cur.type != SYM) {
                gripe(ER_E_UO);
                break;
            }

            // For s.x: e is DEREF(SYM s), unwrap to get SYM s
            // For p->x: e is DEREF(SYM p), keep as-is (pointer value)
            if (is_arrow) {
                base = e;  // pointer value
            } else {
                // Unwrap DEREF to get address
                if (e && e->op == DEREF) {
                    struct expr *deref = e;
                    base = e->left;
                    /* Free the orphaned DEREF node (but not its children) */
                    deref->left = NULL;
                    free(deref);
#ifdef DEBUG
                    exprCurCnt--;
#endif
                } else {
                    base = e;
                }
            }

            // Look up member in struct/union
            member = NULL;
            if (base && base->type) {
                struct type *t = base->type;
                /*
                 * For both DOT and ARROW, if base type is pointer,
                 * get the pointed-to type (DOT after array subscript
                 * produces pointer type)
                 */
                if (t->flags & TF_POINTER) {
                    t = t->sub;
                }
                if (t && (t->flags & TF_AGGREGATE) && t->elem) {
                    for (member = t->elem; member; member = member->next) {
                        if (strcmp(member->name, cur.v.name) == 0) {
                            break;
                        }
                    }
                }
            }

            if (!member) {
                gripe(ER_E_UO);
                gettoken();
                e = mkexprI(CONST, 0, NULL, 0, 0);
                break;
            }

            /*
             * Generate: DEREF(ADD(base, offset)) or BFEXTRACT
             * for bitfields
             */
            offset_expr = mkexprI(CONST, 0, inttype,
					member->offset, E_CONST);

            addr = mkexpr(PLUS, base);
            addr->right = offset_expr;
            addr->left->up = addr;
            addr->right->up = addr;
            // addr is pointer to member, not pointer to base struct
            addr->type = getType(TF_POINTER, member->type, 0);

            // Fold constant offset (e.g., x + 0 becomes x)
            addr = cfold(addr);

            // Check if this is a bitfield access
            if (member->kind == bitfield) {
                /*
                 * Use BFEXTRACT operator with bitoff and
                 * width stored in expr
                 */
                e = mkexprI(BFEXTRACT, addr, member->type, 0, 0);
                e->left->up = e;
                /*
                 * Store bitfield info in the var field (repurpose it)
                 * We'll encode bitoff and width for the code generator
                 * Keep reference to member for bitoff/width
                 */
                e->var = (struct var *)member;
            } else if (member->type && (member->type->flags & TF_ARRAY)) {
                /*
                 * Array member: return address without DEREF
                 * Arrays decay to pointers but are not lvalues
                 * This prevents arr++ while allowing p = arr
                 */
                e = addr;
                e->type = member->type;  /* Keep array type for proper semantics */
            } else {
                /* Non-array member: wrap in DEREF to get value */
                e = mkexprI(DEREF, addr, member->type, 0, 0);
                e->left->up = e;
            }

            gettoken();
        } else if (cur.type == INCR || cur.type == DECR) {
            // Postfix increment/decrement: i++ or i--
            unsigned char inc_op = cur.type;
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
            struct expr *condition = e;
            struct expr *true_expr, *false_expr, *colon_node;

            // Parse the true expression
            true_expr = parseExpr(0, st);

            // Expect and consume COLON
            expect(COLON, ER_E_SP);

            /*
             * Parse the false expression (right-associative,
             * allow another ?: at same level). Use priority 0 to parse
             * everything, including nested ?: operators
             */
            false_expr = parseExpr(0, st);

            // Build tree: QUES(condition, COLON(true_expr, false_expr))
            colon_node = mkexpr(COLON, true_expr);
            colon_node->right = false_expr;
            if (colon_node->left) colon_node->left->up = colon_node;
            if (colon_node->right) colon_node->right->up = colon_node;

            e = mkexpr(QUES, condition);
            e->right = colon_node;
            if (e->left) e->left->up = e;
            if (e->right) e->right->up = e;

            /*
             * Type is the type of the result expressions
             * (should check compatibility)
             */
            if (true_expr && true_expr->type) {
                e->type = true_expr->type;
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
                struct expr *deref = e;
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
                deref->left = NULL;
                free(deref);
#ifdef DEBUG
                exprCurCnt--;
#endif
            } else if (e && e->op == BFEXTRACT) {
                struct expr *bfextr = e;
                // Bitfield assignment - change to BFASSIGN
                /* Save the type before unwrapping */
                assign_type = e->type;
                /*
                 * Keep the var field which has the member info
                 * (bitoff, width)
                 */
                member_info = e->var;
                e = e->left;  // unwrap to get address
                /* Free the orphaned BFEXTRACT node (but not its children) */
                bfextr->left = NULL;
                free(bfextr);
#ifdef DEBUG
                exprCurCnt--;
#endif
                /*
                 * Store member info temporarily - we'll use it when
                 * creating BFASSIGN. Pass through member info
                 */
                e->var = member_info;
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
        savMemberInf = (e && e->var) ? e->var : NULL;
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
        if (op == BFASSIGN && savMemberInf) {
            e->var = savMemberInf;
        }

        // For COPY operator, unwrap DEREF from right side to get address
        if (e->op == COPY && e->right && e->right->op == DEREF) {
            struct expr *deref = e->right;
            e->right = e->right->left;  // unwrap to get address
            /* Free the orphaned DEREF node (but not its children) */
            deref->left = NULL;
            free(deref);
#ifdef DEBUG
            exprCurCnt--;
#endif
        }

        /* For plain assignments, insert type conversion if needed.
         * Compound assignments (|=, +=, etc.) don't need WIDEN - pass2
         * can handle mixed sizes for the operation itself. */
        if (op == ASSIGN && e->left && e->right && e->left->type &&
				e->right->type) {
            struct type *ltype = assign_type ? assign_type : e->left->type;
            struct type *rtype = e->right->type;

            if (IS_SCALAR(ltype) && IS_SCALAR(rtype) &&
					ltype->size != rtype->size) {
                if (e->right->op == CONST) {
                    e->right->type = ltype;
                } else {
                    e->right = mkConv(e->right, ltype);
                    e->right->up = e;
                }
            }

            // Check pointer type compatibility
            l_ptr = (ltype->flags & (TF_POINTER|TF_ARRAY));
            r_ptr = (rtype->flags & (TF_POINTER|TF_ARRAY));

            if (l_ptr && r_ptr) {
                l_base = ltype->sub;
                r_base = rtype->sub;

                // Both must have base types
                if (l_base && r_base) {
                    unsigned char compatible = 0;

                    // void* is compatible with any pointer type
                    if (l_base == voidtype || r_base == voidtype) {
                        compatible = 1;
                    }
                    // Check if base types are compatible
                    else if (l_base == r_base) {
                        // Same type pointer - always compatible
                        compatible = 1;
                    } else if ((l_base->flags & TF_AGGREGATE) &&
							(r_base->flags & TF_AGGREGATE)) {
                        /*
                         * Both point to struct/union - must be same type
                         * For now, just check if pointers are equal
                         * (type unification)
                         */
                        compatible = (l_base == r_base);
                    } else if (!(l_base->flags & TF_AGGREGATE) &&
							!(r_base->flags & TF_AGGREGATE)) {
                        /*
                         * Both point to non-aggregate types
                         * Check if they have same size and signedness
                         */
                        if (l_base->size == r_base->size) {
                            unsigned char l_unsigned =
									(l_base->flags & TF_UNSIGNED);
                            unsigned char r_unsigned =
									(r_base->flags & TF_UNSIGNED);
                            compatible = (l_unsigned == r_unsigned);
                        }
                    }

                    if (!compatible) {
#ifdef DEBUG
                        fdprintf(2, "INCOMPATIBLE POINTERS:\n");
                        fdprintf(2, "  Left:  ltype=%p "
								"(flags=0x%x, size=%d)\n",
								ltype, ltype->flags, ltype->size);
                        fdprintf(2, "         l_base=%p "
								"(flags=0x%x, size=%d)\n",
								l_base, l_base->flags, l_base->size);
                        if (l_base && (l_base->flags & TF_POINTER) &&
								l_base->sub) {
                            fdprintf(2, "         l_base->sub=%p "
									"(flags=0x%x, size=%d)\n",
                                    l_base->sub, l_base->sub->flags,
									l_base->sub->size);
                        }
                        fdprintf(2, "  Right: rtype=%p "
								"(flags=0x%x, size=%d)\n",
								rtype, rtype->flags, rtype->size);
                        fdprintf(2, "         r_base=%p "
								"(flags=0x%x, size=%d)\n",
								r_base, r_base->flags, r_base->size);
                        if (rtype && (rtype->flags & TF_POINTER) &&
								rtype->sub) {
                            fdprintf(2, "         rtype->sub=%p "
									"(flags=0x%x, size=%d)\n",
                                    rtype->sub, rtype->sub->flags,
									rtype->sub->size);
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
