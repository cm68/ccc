/*
 * generate expression trees
 */
#include "cc1.h"

#include "op_pri.h"

/*
 * counter for generating synthetic string literal names
 */
static int stringCtr = 0;

/*
 * Determine smallest type that can hold a constant value
 */
static struct type *
constType(long v)
{
	if (v < 0)
		return (v >= -32768) ? inttype : longtype;
	if (v <= 32767)
		return inttype;
	if (v <= 65535)
		return ushorttype;
	return (v <= 2147483647L) ? longtype : ulongtype;
}

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
	free(e);
}

/*
 * Get binary operator precedence priority
 *
 * Looks up the precedence priority for a binary operator token in the
 * auto-generated op_pri[] table. Lower numbers bind tighter (higher
 * precedence). The table is indexed from OP_MIN to OP_MAX and contains
 * the encoded priority of each binary operator.
 *
 * Priority values:
 *   0 = not an operator (or invalid token)
 *   1 = highest precedence (e.g., array subscript, member access)
 *   ...
 *   14 = lowest binary precedence (e.g., comma operator)
 *
 * Parameters:
 *   t - Operator token to look up
 *
 * Returns:
 *   Precedence priority (0 if not a binary operator or out of range)
 */
unsigned char
binopPri(unsigned char t)
{
    if ((t < OP_MIN) || (t > OP_MAX)) {
        return 0;
    }
	return (op_pri[t - OP_MIN]);
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
        operand = operand->left;
    } else {
        gripe(ER_E_LV);
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
	unsigned char is_binary_op = 0;

	switch (cur.type) {   // prefix

    case NUMBER: {
        long sval = cur.v.numeric;
        e = mkexprI(CONST, 0, constType(sval), (unsigned long)sval, E_CONST);
        gettoken();
        break;
    }

    case STRING: {
        struct name *strname;
        char namebuf[32];
        unsigned char *combined_str;
        unsigned char first_len;
        unsigned char total_len;
        int i;

        /* string literals have type char* (pointer to char) */
        e = mkexprI(STRING, 0, getType(TF_POINTER, chartype, 0), 0, 0);

        /* Concatenate adjacent string literals */
        combined_str = (unsigned char *)cur.v.str;
        first_len = combined_str[0];
        total_len = first_len;

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

            /* Free old combined string if it was allocated */
            if (total_len != first_len) {
                free(combined_str);
            }

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
            strname->name = strdup(namebuf);
            strname->kind = var;
            strname->type = e->type;
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
                n->name = strdup(symname);
                n->kind = var;
                n->type = functype;
                n->level = 1;  /* Global scope */
                n->sclass = SC_EXTERN;
                n->is_tag = 0;

                addName(n);

                if (VERBOSE(V_SYM)) {
                    fdprintf(2, "Implicit declaration: int %s()\n", symname);
                }
            } else {
                /* Not a function call - report error */
                gripe(ER_E_UO);
                e = mkexprI(CONST, 0, inttype, 0, 0);
                free(symname);
                break;
            }
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
        /* Optimize: &(DEREF x) = x, since SYM already gives address */
        if (e && e->op == DEREF) {
            e = e->left;
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
                /* Save the actual type (not the address type) */
                base_type = e->type;
                e = e->left;
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
                    base = e->left;
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
        is_assignment = (op == ASSIGN || op == PLUSEQ ||
				op == SUBEQ || op == MULTEQ || op == DIVEQ ||
				op == MODEQ || op == ANDEQ || op == OREQ ||
				op == XOREQ || op == LSHIFTEQ || op == RSHIFTEQ ||
				op == LANDEQ || op == LOREQ);

        if (is_assignment) {
            if (e && e->op == DEREF) {
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
                /* Save the type before unwrapping */
                assign_type = e->type;
                e = e->left;  // unwrap to get address
            } else if (e && e->op == BFEXTRACT) {
                // Bitfield assignment - change to BFASSIGN
                /* Save the type before unwrapping */
                assign_type = e->type;
                /*
                 * Keep the var field which has the member info
                 * (bitoff, width)
                 */
                member_info = e->var;
                e = e->left;  // unwrap to get address
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
                parseExpr(p, st);  // Parse and discard right side
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
            e->right = e->right->left;  // unwrap to get address
        }

        /* For assignments, insert type conversion if needed */
        if (is_assignment && e->left && e->right && e->left->type &&
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
         * Widen operands of binary expressions if sizes mismatch
         */
        is_binary_op = (op == PLUS || op == MINUS ||
				op == STAR || op == DIV || op == MOD || op == AND ||
				op == OR || op == XOR || op == LSHIFT || op == RSHIFT ||
				op == LT || op == GT || op == LE || op == GE ||
				op == EQ || op == NEQ);

        if (is_binary_op && e->left && e->right && e->left->type &&
				e->right->type) {
            struct type *ltype = e->left->type;
            struct type *rtype = e->right->type;

            if (IS_SCALAR(ltype) && IS_SCALAR(rtype) &&
					ltype->size != rtype->size) {
                /* Prefer retyping constants over inserting conversions */
                struct expr **smaller, **larger;
                struct type *smallt, *larget;
                if (ltype->size < rtype->size) {
                    smaller = &e->left; larger = &e->right;
                    smallt = ltype; larget = rtype;
                } else {
                    smaller = &e->right; larger = &e->left;
                    smallt = rtype; larget = ltype;
                }
                if ((*smaller)->op == CONST)
                    (*smaller)->type = larget;
                else if ((*larger)->op == CONST)
                    (*larger)->type = smallt;
                else {
                    *smaller = mkConv(*smaller, larget);
                    (*smaller)->up = e;
                }
            }
        }

        // try to determine result type
        if (e->left && e->right) {
            // For ASSIGN and compound assignments, use the saved assign_type
            if (is_assignment && assign_type) {
                e->type = assign_type;
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

        e = cfold(e);
    }
    return e;
}

/*
 * Replace an expression node in the tree with a different node
 *
 * Substitutes 'in' for 'out' in the expression tree, updating all linkages
 * (next, prev, up) to maintain tree connectivity. Preserves E_FUNARG flag
 * if it was set on the original node. Frees the old node and returns the
 * new replacement node.
 *
 * This is used during constant folding and optimization passes to replace
 * complex expressions with simpler ones while maintaining the tree structure.
 *
 * Parameters:
 *   out - Expression node to be replaced (will be freed)
 *   in  - Replacement expression node
 *
 * Returns:
 *   The replacement node (in) with all linkages updated
 */
struct expr *
xreplace(struct expr *out, struct expr *in)
{
    in->next = out->next;
    in->prev = out->prev;
    if (out->prev) {
        out->prev->next = in;
    }
    if (out->next) {
        out->next->prev = in;
    }
    in->up = out->up;
    if (out->flags & E_FUNARG) {
        in->flags |= E_FUNARG;
    }
    free(out);
    return in;
}

/*
 * Constant fold an expression tree
 *
 * Performs compile-time evaluation of expressions with constant operands,
 * replacing complex operations with their computed results. This optimization:
 *   - Reduces code size and runtime overhead
 *   - Enables further optimizations (e.g., dead code elimination)
 *   - Implements required constant expression evaluation for:
 *     * Array sizes: int arr[5+3]
 *     * Case labels: case 2*3:
 *     * Enum values: enum { A = 1+2 }
 *     * Initializers for static/global variables
 *
 * Handles folding for:
 *   - Unary operators: NEG, BITNOT, NOT
 *   - Binary arithmetic: PLUS, MINUS, STAR, DIV, MOD
 *   - Binary bitwise: AND, OR, XOR, LSHIFT, RSHIFT
 *   - Binary logical: LAND, LOR
 *   - Comparisons: EQ, NE, LT, GT, LE, GE
 *   - Ternary conditional: QUES/COLON
 *
 * Type strength reduction: After folding, re-types constants to the smallest
 * type that can represent the value (char < short < long, signed/unsigned).
 *
 * Parameters:
 *   e - Expression tree to fold
 *
 * Returns:
 *   Folded expression (may be same node, a replacement, or completely new tree)
 */
struct expr *
cfold(struct expr *e)
{
	long val;
	long vl, vr;

    switch (e->op) {
    case NEG:
        if (e->left->op == CONST) {
            val = -e->left->v;
            e = xreplace(e, e->left);
            e->v = val;
            e->type = constType((long)val);
        }
        return e;
    case TWIDDLE:
        if (e->left->op == CONST) {
            val = ~e->left->v;
            e = xreplace(e, e->left);
            e->v = val;
        }
        return e;
    case BANG:
        if (e->left->op == CONST) {
            val = e->left->v ? 0 : 1;
            e = xreplace(e, e->left);
            e->v = val;
        }
        return e;
    case QUES:
        /*
         * Ternary conditional: condition ? true : false
         * Structure: QUES(condition, COLON(true_expr, false_expr))
         */
        if (e->left && e->left->op == CONST && e->right &&
				e->right->op == COLON) {
            struct expr *result;
            if (e->left->v) {
                // Condition is true, use true branch
                result = e->right->left;
            } else {
                // Condition is false, use false branch
                result = e->right->right;
            }
            if (result) {
                e = xreplace(e, result);
            }
        }
        return e;
    }
    if (!e->right) {
        gripe(ER_E_CF);
        return e;
    }

    // Algebraic simplifications for identity operations
    // These work even when one operand is not constant
    if (e->right->op == CONST) {
        long vr = e->right->v;
        switch (e->op) {
        case PLUS:
        case MINUS:
        case LSHIFT:
        case RSHIFT:
            if (vr == 0)
                return xreplace(e, e->left);
            break;
        case STAR:
            if (vr == 1)
                return xreplace(e, e->left);
            if (vr == 0)
                return xreplace(e, e->right);
            break;
        case DIV:
            if (vr == 1)
                return xreplace(e, e->left);
            break;
        }
        // Associative folding: (x + C1) + C2 -> x + (C1+C2)
        // This handles nested struct/union member offsets
        if (e->op == PLUS && e->left->op == PLUS &&
            e->left->right && e->left->right->op == CONST) {
            long c1 = e->left->right->v;
            long c2 = vr;
            e->right->v = c1 + c2;
            e->left = xreplace(e->left, e->left->left);
            e->left->up = e;
            return cfold(e);  // recurse in case of deeper nesting
        }
    }
    if (e->left->op == CONST) {
        long vl = e->left->v;
        switch (e->op) {
        case PLUS:
            // 0 + x = x
            if (vl == 0) {
                return xreplace(e, e->right);
            }
            break;
        case STAR:
            // 1 * x = x
            if (vl == 1) {
                return xreplace(e, e->right);
            }
            // 0 * x = 0
            if (vl == 0) {
                return xreplace(e, e->left);
            }
            break;
        }
    }

    if ((e->left->op != CONST) || (e->right->op != CONST)) {
        return e;
    }
    vl = e->left->v;
    vr = e->right->v;

    switch (e->op) {
    case PLUS:
        val = vl + vr;
        break;
    case MINUS:
        val = vl - vr;
        break;
    case STAR:  // multiplication
        val = vl * vr;
        break;
    case DIV:   // division
        if (vr == 0) {
            gripe(ER_E_CF);  // divide by zero - constant wont fold
            return e;
        }
        val = vl / vr;
        break;
    case MOD:   // modulo
        if (vr == 0) {
            gripe(ER_E_CF);  // modulo by zero - constant wont fold
            return e;
        }
        val = vl % vr;
        break;
    case AND:   // bitwise AND
        val = vl & vr;
        break;
    case OR:    // bitwise OR
        val = vl | vr;
        break;
    case XOR:   // bitwise XOR
        val = vl ^ vr;
        break;
    case LSHIFT:  // left shift
        val = vl << vr;
        break;
    case RSHIFT:  // right shift
        val = vl >> vr;
        break;
    case LAND:  // logical AND (&&)
        val = (vl && vr);
        break;
    case LOR:   // logical OR (||)
        val = (vl || vr);
        break;
    case LT:    // less than (<)
        val = (vl < vr);
        break;
    case GT:    // greater than (>)
        val = (vl > vr);
        break;
    case LE:    // less than or equal (<=)
        val = (vl <= vr);
        break;
    case GE:    // greater than or equal (>=)
        val = (vl >= vr);
        break;
    case EQ:    // equal (==)
        val = (vl == vr);
        break;
    case NEQ:   // not equal (!=)
        val = (vl != vr);
        break;
    default:
        return e;
    }
    e = xreplace(e, e->left);
    e->v = val;  // Store the computed constant value
    return e;
}

/*
 * Parse and evaluate a constant expression
 *
 * Parses an expression that must evaluate to a compile-time constant value.
 * This is used in contexts that require constant expressions:
 *   - Array dimensions: int arr[N]
 *   - Case labels: case N:
 *   - Enum initializers: enum { A = N }
 *   - Bitfield widths: unsigned x : N
 *   - Static initializers (limited cases)
 *
 * The parser stops at the comma operator (priority 15) to handle contexts
 * like enum { A = 10, B = 20 } where commas separate list items rather than
 * acting as operators.
 *
 * Generates an error (ER_C_CE) if:
 *   - Expression cannot be parsed
 *   - Expression is not constant (E_CONST flag not set)
 *
 * Parameters:
 *   token - Expected terminating token (used for error context)
 *
 * Returns:
 *   The computed constant value, or 0 on error
 */
unsigned long
parseConst(unsigned char token)
{
    struct expr *e;
    unsigned long val;

    // Parse constant expression, stopping before comma operator (priority 15)
    // This allows constants in contexts like enum { A = 10, B = 20 }
    // where we want to stop at the comma
    e = parseExpr(15, 0);
    if (!e) {
        gripe(ER_C_CE);
        return 0;
    }
    if (!(e->flags & E_CONST)) {
        gripe(ER_C_CE);
        return 0;
    }
    val = e->v;
    frExp(e);
    return val;
}

/*
 * vim: tabstop=4 shiftwidth=4 expandtab:
 */
