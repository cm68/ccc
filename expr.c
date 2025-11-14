/*
 * generate expression trees
 */
#include "cc1.h"

#include "op_pri.h"

/*
 * counter for generating synthetic string literal names
 */
static int string_counter = 0;

struct expr *
makeexpr(unsigned char op, struct expr *left)
{
	struct expr *e;

	e = calloc(1, sizeof(*e));  // Zero-initialize all fields
	e->op = op;
	e->left = left;
	return e;
}

/*
 * makeexpr wrapper that also sets type, value, and flags
 * pass NULL for type to skip setting it
 */
struct expr *
makeexpr_init(unsigned char op, struct expr *left, struct type *type, unsigned long v, int flags)
{
	struct expr *e;

	e = makeexpr(op, left);
	if (type) {
		e->type = type;
	}
	e->v = v;
	e->flags = flags;
	return e;
}

/*
 * deallocate an expression tree
 * this needs to not leak memory
 */
void
destroy_expr(struct expr *e)
{
	if (!e) {
		return;
	}
	if (e->left) {
		destroy_expr(e->left);
	}
	if (e->right) {
		destroy_expr(e->right);
	}
	free(e);
}

char
lvalue(struct expr *e)
{
	if (e->op == DEREF) {
		return 1;
	}
	return 0;
}

/*
 * worker function to factor out common expr stuff for unary ops
 */
void
unop_set(struct expr *e)
{
    e->type = e->left->type;
    e->left->up = e;
}

/*
 * the operator priority table is indexed from OP_MIN to OP_MAX, 
 * and contains the encoded priority of the binary operator.  
 * zero values mean not an operator.
 */
unsigned char
binop_pri(unsigned char t)
{
    if ((t < OP_MIN) || (t > OP_MAX)) {
        return 0;
    }
	return (op_pri[t - OP_MIN]);
}

/*
 * parse an expression
 */
struct expr *
parse_expr(unsigned char pri, struct stmt *st)
{
	unsigned char op;
	struct expr *e;
    unsigned char p;

	e = 0;  // initialize to avoid uninitialized use

	switch (cur.type) {   // prefix

    case NUMBER:
        e = makeexpr_init(CONST, 0, inttype, cur.v.numeric, E_CONST);
        gettoken();
        break;

    case STRING: {
        struct name *strname;
        char namebuf[32];
        /* string literals have type char* (pointer to char) */
        e = makeexpr_init(STRING, 0, get_type(TF_POINTER, chartype, 0), 0, 0);

        /* generate synthetic name for this string literal */
        sprintf(namebuf, "str%d", string_counter++);

        /* Allocate name structure directly without adding to names[] lookup table */
        /* String literal names don't need to be looked up, only emitted to AST */
        /* Note: We don't free these structures - they're small and process will exit */
        strname = (struct name *)calloc(1, sizeof(struct name));
        if (strname) {
            strname->name = strdup(namebuf);
            strname->kind = var;
            strname->type = e->type;
            strname->level = 1;  /* Global scope */
            /* store pointer to counted string in the name's init field */
            strname->u.init = makeexpr_init(STRING, 0, NULL, (unsigned long)cur.v.str, 0);
            /* also store in expression for immediate use */
            e->v = (unsigned long)cur.v.str;
            /* store reference to the named string in the expression */
            e->var = (struct var *)strname;
            /* Emit string literal immediately (string data freed in emit) */
            emit_string_literal(strname);
        }
        e->flags = E_CONST;
        gettoken();
        break;
    }

    case SYM: {
        /* Symbol reference - SYM = address
         * For variables: wrap in DEREF to get value
         * For functions: return address (decay to function pointer)
         */
        struct name *n;
        struct expr *sym;

        n = lookup_name(cur.v.name, 0);
        if (!n) {
            gripe(ER_E_UO);
            e = makeexpr_init(CONST, 0, inttype, 0, 0);
        } else if (n->kind == elem) {
            // Enum constant: treat as integer constant
            e = makeexpr_init(CONST, 0, inttype, n->offset, E_CONST);
        } else {
            sym = makeexpr_init(SYM, 0, n->type, 0, 0);
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
                e = makeexpr_init(DEREF, sym, n->type, 0, 0);
            }
        }
        gettoken();
        break;
    }

    /* unary operators */
    case LPAR:      // parenthesized expression or type cast
        gettoken();

        /* Check if this is a type cast: (type)expr */
        if (is_cast_start()) {
            struct type *cast_type;
            struct expr *inner;

            /* Parse the type name */
            cast_type = parse_type_name();
            expect(RPAR, ER_E_SP);

            /* Parse the expression being cast */
            inner = parse_expr(OP_PRI_MULT - 1, st);  // Cast has unary precedence

            /* Determine if cast needs runtime operation */
            if (cast_type && inner && inner->type) {
                /* Pointer-to-pointer casts are just type reinterpretation */
                if ((cast_type->flags & TF_POINTER) && (inner->type->flags & TF_POINTER)) {
                    inner->type = cast_type;
                    e = inner;
                }
                /* Scalar casts: determine which operation needed */
                else if (!(cast_type->flags & (TF_POINTER|TF_ARRAY|TF_FUNC)) &&
                         !(inner->type->flags & (TF_POINTER|TF_ARRAY|TF_FUNC))) {
                    int src_size = inner->type->size;
                    int tgt_size = cast_type->size;
                    int src_unsigned = inner->type->flags & TF_UNSIGNED;

                    if (tgt_size == src_size) {
                        /* Same size: just reinterpret (e.g., int <-> unsigned int) */
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

                        e = makeexpr_init(cast_op, inner, cast_type, 0, 0);
                    }
                }
                /* Mixed pointer/scalar casts: need conversion */
                else {
                    /* For pointer<->scalar, use NARROW or WIDEN based on sizes */
                    int src_size = inner->type->size;
                    int tgt_size = cast_type->size;
                    token_t cast_op = (tgt_size < src_size) ? NARROW : WIDEN;

                    e = makeexpr_init(cast_op, inner, cast_type, 0, 0);
                }
            } else {
                /* Shouldn't happen, but create NARROW as fallback */
                e = makeexpr_init(NARROW, inner, cast_type, 0, 0);
            }
        } else {
            /* Parenthesized expression: (expr) */
            e = parse_expr(0, st);  // parse inner expression with lowest precedence
            expect(RPAR, ER_E_SP);
        }
        break;

    case MINUS:     // unary minus
        gettoken();
        e = makeexpr(NEG, parse_expr(OP_PRI_MULT - 1, st));  // higher precedence than mult
        if (e->left) {
            unop_set(e);
        }
        e = cfold(e);
        break;

    case TWIDDLE:   // bitwise not
        gettoken();
        e = makeexpr(TWIDDLE, parse_expr(OP_PRI_MULT - 1, st));
        if (e->left) {
            unop_set(e);
        }
        e = cfold(e);
        break;

    case BANG:      // logical not
        gettoken();
        e = makeexpr(NOT, parse_expr(OP_PRI_MULT - 1, st));
        if (e->left) {
            unop_set(e);
        }
        e = cfold(e);
        break;

    case STAR:      // dereference (unary)
        gettoken();
        e = makeexpr(DEREF, parse_expr(OP_PRI_MULT - 1, st));
        if (e->left) {
            e->left->up = e;
            // type will be determined later when we have full type info
            if (e->left->type && (e->left->type->flags & TF_POINTER) && e->left->type->sub) {
                e->type = e->left->type->sub;
            } else {
                e->type = e->left->type;
            }
        }
        break;

    case AND:       // address-of (unary)
        gettoken();
        e = parse_expr(OP_PRI_MULT - 1, st);
        /* Optimize: &(DEREF x) = x, since SYM already gives address */
        if (e && e->op == DEREF) {
            e = e->left;
        } else {
            struct expr *addr = makeexpr(AND, e);
            if (e) {
                e->up = addr;
                if (e->type) {
                    addr->type = get_type(TF_POINTER, e->type, 0);
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
                    t = get_type(TF_POINTER, t, 0);
                }

                expect(RPAR, ER_E_SP);

                // Create constant expression with the size
                e = makeexpr_init(CONST, 0, inttype, t->size, E_CONST);
            } else {
                // It's sizeof(expr) - parse as expression
                e = parse_expr(0, st);
                expect(RPAR, ER_E_SP);

                // Create constant expression with the size of the expression's type
                if (e && e->type) {
                    int size = e->type->size;
                    destroy_expr(e);  // we only needed it for the type
                    e = makeexpr_init(CONST, 0, inttype, size, E_CONST);
                } else {
                    gripe(ER_E_UO);  // couldn't determine type
                    e = makeexpr_init(CONST, 0, inttype, 0, E_CONST);
                }
            }
        } else {
            // sizeof expr (without parentheses)
            struct expr *operand = parse_expr(OP_PRI_MULT - 1, st);
            if (operand && operand->type) {
                int size = operand->type->size;
                destroy_expr(operand);
                e = makeexpr_init(CONST, 0, inttype, size, E_CONST);
            } else {
                gripe(ER_E_UO);
                e = makeexpr_init(CONST, 0, inttype, 0, E_CONST);
            }
        }
        break;

    case INCR:      // prefix increment: ++i
    case DECR: {    // prefix decrement: --i
        unsigned char inc_op = cur.type;
        struct expr *operand;

        gettoken();
        operand = parse_expr(OP_PRI_MULT - 1, st);  // unary precedence

        // Unwrap DEREF to get lvalue address (similar to ASSIGN)
        if (operand && operand->op == DEREF) {
            operand = operand->left;
        } else {
            // Increment/decrement requires an lvalue
            gripe(ER_E_LV);
            operand = NULL;
        }

        // Create increment/decrement node
        e = makeexpr(inc_op, operand);
        if (e->left) {
            e->left->up = e;
            e->type = e->left->type;
        }
        // Note: Do NOT set E_POSTFIX flag for prefix form
        break;
    }

	default:
		gripe(ER_E_UO);
		return 0;
    }

    /*
     * Handle postfix operators: function calls, array subscripts, struct access, increment/decrement
     */
    while (cur.type == LPAR || cur.type == LBRACK || cur.type == DOT || cur.type == ARROW ||
           cur.type == INCR || cur.type == DECR) {
        if (cur.type == LBRACK) {
            // Array subscript: arr[idx] = DEREF(ADD(base, idx * sizeof))
            struct expr *index, *scaled, *addr, *size_expr;
            int elem_size;

            gettoken();  // consume '['
            index = parse_expr(0, st);
            expect(RBRACK, ER_E_SP);

            // Unwrap DEREF to get base address
            if (e && e->op == DEREF) {
                e = e->left;
            }

            // Get element size from type
            elem_size = 2;  // default to short/int size
            if (e && e->type) {
                if (e->type->flags & TF_POINTER && e->type->sub) {
                    elem_size = e->type->sub->size;
                } else if (e->type->flags & TF_ARRAY && e->type->sub) {
                    elem_size = e->type->sub->size;
                }
            }

            // Scale index by element size: idx * sizeof(elem)
            if (elem_size == 1) {
                scaled = index;
            } else {
                size_expr = makeexpr_init(CONST, 0, inttype, elem_size, E_CONST);

                scaled = makeexpr(STAR, index);
                scaled->right = size_expr;
                scaled->left->up = scaled;
                scaled->right->up = scaled;
                scaled->type = inttype;
                scaled = cfold(scaled);
            }

            // Add scaled offset to base: base + (idx * sizeof)
            addr = makeexpr(PLUS, e);
            addr->right = scaled;
            addr->left->up = addr;
            addr->right->up = addr;
            // The ADD result is a pointer to the element type
            if (e->type && (e->type->flags & TF_ARRAY) && e->type->sub) {
                addr->type = get_type(TF_POINTER, e->type->sub, 0);
            } else if (e->type && (e->type->flags & TF_POINTER)) {
                addr->type = e->type;  // pointer + offset = same pointer type
            } else {
                addr->type = e->type;
            }

            // Dereference to get element value
            e = makeexpr(DEREF, addr);
            e->left->up = e;
            if (e->left->type) {
                if ((e->left->type->flags & TF_POINTER) && e->left->type->sub) {
                    e->type = e->left->type->sub;
                } else if ((e->left->type->flags & TF_ARRAY) && e->left->type->sub) {
                    e->type = e->left->type->sub;
                }
            }
        } else if (cur.type == LPAR) {
            // Function call: expr(arg1, arg2, ...)
            struct expr *call, *arg, *lastarg;

            gettoken();  // consume '('

            // Create CALL node with function expression as left operand
            call = makeexpr(CALL, e);
            call->left->up = call;

            // Parse argument list
            lastarg = NULL;
            if (cur.type != RPAR) {
                // Parse first argument
                arg = parse_expr(OP_PRI_COMMA, st);
                if (arg) {
                    arg->flags |= E_FUNARG;
                    call->right = arg;
                    arg->up = call;
                    lastarg = arg;
                }

                // Parse remaining arguments
                while (cur.type == COMMA) {
                    gettoken();
                    arg = parse_expr(OP_PRI_COMMA, st);
                    if (arg) {
                        arg->flags |= E_FUNARG;
                        if (lastarg) {
                            lastarg->next = arg;
                            arg->prev = lastarg;
                        }
                        lastarg = arg;
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
                // For both DOT and ARROW, if base type is pointer, get the pointed-to type
                // (DOT after array subscript produces pointer type)
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
                e = makeexpr_init(CONST, 0, NULL, 0, 0);
                break;
            }

            // Generate: DEREF(ADD(base, offset)) or BFEXTRACT for bitfields
            offset_expr = makeexpr_init(CONST, 0, inttype, member->offset, E_CONST);

            addr = makeexpr(PLUS, base);
            addr->right = offset_expr;
            addr->left->up = addr;
            addr->right->up = addr;
            // addr is pointer to member, not pointer to base struct
            addr->type = get_type(TF_POINTER, member->type, 0);

            // Fold constant offset (e.g., x + 0 becomes x)
            addr = cfold(addr);

            // Check if this is a bitfield access
            if (member->kind == bitfield) {
                // Use BFEXTRACT operator with bitoff and width stored in expr
                e = makeexpr_init(BFEXTRACT, addr, member->type, 0, 0);
                e->left->up = e;
                // Store bitfield info in the var field (repurpose it)
                // We'll encode bitoff and width for the code generator
                e->var = (struct var *)member;  // Keep reference to member for bitoff/width
            } else {
                e = makeexpr_init(DEREF, addr, member->type, 0, 0);
                e->left->up = e;
            }

            gettoken();
        } else if (cur.type == INCR || cur.type == DECR) {
            // Postfix increment/decrement: i++ or i--
            unsigned char inc_op = cur.type;
            struct expr *inc_node;

            gettoken();

            // Unwrap DEREF to get lvalue address (similar to ASSIGN)
            if (e && e->op == DEREF) {
                e = e->left;
            } else {
                // Increment/decrement requires an lvalue
                gripe(ER_E_LV);
                e = NULL;
            }

            // Create increment/decrement node
            inc_node = makeexpr(inc_op, e);
            if (inc_node->left) {
                inc_node->left->up = inc_node;
                inc_node->type = inc_node->left->type;
            }
            inc_node->flags |= E_POSTFIX;  // Mark as postfix form
            e = inc_node;
        }
    }

    /*
     * the recursive nature of this expression parser will have exhausted
     * the unary operators and terminals by this point. now we have postfix
     * and binary operators to deal with
     */
    while (1) { // binary operators
        p = binop_pri(cur.type);
        if (p == 0) {
            // not a binary operator, we're done
            break;
        }
        if (pri != 0 && p >= pri) {
            // operator has same or lower precedence, stop parsing at this level
            // (pri == 0 means PRI_ALL, so we parse all operators regardless of precedence)
            break;
        }

        // we have a binary operator with higher precedence (lower p value)
        op = cur.type;
        gettoken();

        // Special handling for ternary conditional operator: condition ? true : false
        if (op == QUES) {
            struct expr *condition = e;
            struct expr *true_expr, *false_expr, *colon_node;

            // Parse the true expression
            true_expr = parse_expr(0, st);

            // Expect and consume COLON
            expect(COLON, ER_E_SP);

            // Parse the false expression (right-associative, allow another ?: at same level)
            // Use priority 0 to parse everything, including nested ?: operators
            false_expr = parse_expr(0, st);

            // Build tree: QUES(condition, COLON(true_expr, false_expr))
            colon_node = makeexpr(COLON, true_expr);
            colon_node->right = false_expr;
            if (colon_node->left) colon_node->left->up = colon_node;
            if (colon_node->right) colon_node->right->up = colon_node;

            e = makeexpr(QUES, condition);
            e->right = colon_node;
            if (e->left) e->left->up = e;
            if (e->right) e->right->up = e;

            // Type is the type of the result expressions (should check compatibility)
            if (true_expr && true_expr->type) {
                e->type = true_expr->type;
            }

            e = cfold(e);
            continue;  // Skip the rest of the loop and continue with next operator
        }

        // for assignment and compound assignments, unwrap DEREF from left side to get lvalue address
        struct type *assign_type = NULL;  // Track the actual type being assigned
        unsigned char is_assignment = (op == ASSIGN || op == PLUSEQ || op == SUBEQ || op == MULTEQ ||
                            op == DIVEQ || op == MODEQ || op == ANDEQ || op == OREQ ||
                            op == XOREQ || op == LSHIFTEQ || op == RSHIFTEQ ||
                            op == LANDEQ || op == LOREQ);

        if (is_assignment) {
            if (e && e->op == DEREF) {
                assign_type = e->type;  // Save the type before unwrapping
                e = e->left;  // unwrap to get address
            } else if (e && e->op == BFEXTRACT) {
                // Bitfield assignment - change to BFASSIGN
                assign_type = e->type;  // Save the type before unwrapping
                // Keep the var field which has the member info (bitoff, width)
                struct var *member_info = e->var;
                e = e->left;  // unwrap to get address
                // Store member info temporarily - we'll use it when creating BFASSIGN
                e->var = member_info;  // Pass through member info
                // Flag that we need BFASSIGN
                if (op == ASSIGN) {
                    op = BFASSIGN;
                }
            } else {
                // Assignment requires an lvalue (dereference or bitfield)
                gripe(ER_E_LV);
                // Skip this operator: parse and discard right side, then return left side
                parse_expr(p, st);  // Parse and discard right side
                return e;  // Return left side unchanged
            }
        }

        // Check if this is a struct/array assignment requiring memory copy
        // For aggregates (structs, arrays), use COPY instead of ASSIGN
        // Check both direct aggregate type and dereferenced pointer to aggregate
        if (op == ASSIGN && assign_type && (assign_type->flags & TF_AGGREGATE)) {
            op = COPY;  // Change to memory copy operator
        } else if (op == ASSIGN && e && e->type) {
            // Direct aggregate (not dereferenced)
            if (e->type->flags & TF_AGGREGATE) {
                assign_type = e->type;
                op = COPY;
            }
        }

        // Parse right side based on associativity:
        // - For right-associative operators (assignments), use precedence 0 to allow chaining
        // - For left-associative operators, use precedence p to prevent same-precedence from nesting right
        struct var *saved_member_info = (e && e->var) ? e->var : NULL;
        e = makeexpr(op, e);
        e->left->up = e;
        if (is_assignment) {
            // Right-associative: parse at lowest precedence to allow a = b = c
            e->right = parse_expr(0, st);
        } else {
            // Left-associative: parse at same precedence to prevent (a + b) + c from becoming a + (b + c)
            e->right = parse_expr(p, st);
        }
        if (e->right) {
            e->right->up = e;
        }

        // For BFASSIGN, restore member info (bitoff, width)
        if (op == BFASSIGN && saved_member_info) {
            e->var = saved_member_info;
        }

        // For COPY operator, unwrap DEREF from right side to get address
        if (e->op == COPY && e->right && e->right->op == DEREF) {
            e->right = e->right->left;  // unwrap to get address
        }

        // For assignments, insert type conversion if needed
        if (is_assignment && e->left && e->right && e->left->type && e->right->type) {
            struct type *ltype = assign_type ? assign_type : e->left->type;
            struct type *rtype = e->right->type;

            // Only convert scalar types (not pointers, arrays, functions, aggregates)
            int l_scalar = !(ltype->flags & (TF_POINTER|TF_ARRAY|TF_FUNC|TF_AGGREGATE));
            int r_scalar = !(rtype->flags & (TF_POINTER|TF_ARRAY|TF_FUNC|TF_AGGREGATE));

            if (l_scalar && r_scalar && ltype->size != rtype->size) {
                token_t conv_op;
                struct expr *conv;

                if (ltype->size < rtype->size) {
                    // Narrowing conversion
                    conv_op = NARROW;
                } else {
                    // Widening conversion
                    if (rtype->flags & TF_UNSIGNED) {
                        conv_op = WIDEN;  // Zero extend unsigned
                    } else {
                        conv_op = SEXT;   // Sign extend signed
                    }
                }

                // Create conversion node
                conv = makeexpr_init(conv_op, e->right, ltype, 0, 0);
                conv->left->up = conv;
                e->right = conv;
                e->right->up = e;
            }

            // Check pointer type compatibility
            int l_ptr = (ltype->flags & (TF_POINTER|TF_ARRAY));
            int r_ptr = (rtype->flags & (TF_POINTER|TF_ARRAY));

            if (l_ptr && r_ptr) {
                struct type *l_base = ltype->sub;
                struct type *r_base = rtype->sub;

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
                    } else if ((l_base->flags & TF_AGGREGATE) && (r_base->flags & TF_AGGREGATE)) {
                        // Both point to struct/union - must be same type
                        // For now, just check if pointers are equal (type unification)
                        compatible = (l_base == r_base);
                    } else if (!(l_base->flags & TF_AGGREGATE) && !(r_base->flags & TF_AGGREGATE)) {
                        // Both point to non-aggregate types
                        // Check if they have same size and signedness
                        if (l_base->size == r_base->size) {
                            unsigned char l_unsigned = (l_base->flags & TF_UNSIGNED);
                            unsigned char r_unsigned = (r_base->flags & TF_UNSIGNED);
                            compatible = (l_unsigned == r_unsigned);
                        }
                    }

                    if (!compatible) {
                        gripe(ER_E_PT);  // incompatible pointer types
                    }
                }
            }
        }

        // Widen operands of binary expressions if sizes mismatch
        // Apply to arithmetic, bitwise, and comparison operators
        unsigned char is_binary_op = (op == PLUS || op == MINUS || op == STAR || op == DIV || op == MOD ||
                           op == AND || op == OR || op == XOR || op == LSHIFT || op == RSHIFT ||
                           op == LT || op == GT || op == LE || op == GE || op == EQ || op == NEQ);

        if (is_binary_op && e->left && e->right && e->left->type && e->right->type) {
            struct type *ltype = e->left->type;
            struct type *rtype = e->right->type;

            // Only widen scalar types (not pointers, arrays, functions, aggregates)
            int l_scalar = !(ltype->flags & (TF_POINTER|TF_ARRAY|TF_FUNC|TF_AGGREGATE));
            int r_scalar = !(rtype->flags & (TF_POINTER|TF_ARRAY|TF_FUNC|TF_AGGREGATE));

            if (l_scalar && r_scalar && ltype->size != rtype->size) {
                token_t conv_op;
                struct expr *conv;
                struct type *target_type;

                // Widen smaller operand to match larger operand
                if (ltype->size < rtype->size) {
                    // Widen left operand to right's size
                    target_type = rtype;
                    if (ltype->flags & TF_UNSIGNED) {
                        conv_op = WIDEN;  // Zero extend unsigned
                    } else {
                        conv_op = SEXT;   // Sign extend signed
                    }
                    conv = makeexpr_init(conv_op, e->left, target_type, 0, 0);
                    conv->left->up = conv;
                    e->left = conv;
                    e->left->up = e;
                } else {
                    // Widen right operand to left's size
                    target_type = ltype;
                    if (rtype->flags & TF_UNSIGNED) {
                        conv_op = WIDEN;  // Zero extend unsigned
                    } else {
                        conv_op = SEXT;   // Sign extend signed
                    }
                    conv = makeexpr_init(conv_op, e->right, target_type, 0, 0);
                    conv->left->up = conv;
                    e->right = conv;
                    e->right->up = e;
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
        }
        return e;
    case TWIDDLE:
        if (e->left->op == CONST) {
            val = ~e->left->v;
            e = xreplace(e, e->left);
            e->v = val;
        }
        return e;
    case NOT:
        if (e->left->op == CONST) {
            val = e->left->v ? 0 : 1;
            e = xreplace(e, e->left);
            e->v = val;
        }
        return e;
    case QUES:
        // Ternary conditional: condition ? true : false
        // Structure: QUES(condition, COLON(true_expr, false_expr))
        if (e->left && e->left->op == CONST && e->right && e->right->op == COLON) {
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
            // x + 0 = x
            if (vr == 0) {
                return xreplace(e, e->left);
            }
            break;
        case MINUS:
            // x - 0 = x
            if (vr == 0) {
                return xreplace(e, e->left);
            }
            break;
        case STAR:
            // x * 1 = x
            if (vr == 1) {
                return xreplace(e, e->left);
            }
            // x * 0 = 0
            if (vr == 0) {
                return xreplace(e, e->right);
            }
            break;
        case DIV:
        case MOD:
            // x / 1 = x
            if (vr == 1 && e->op == DIV) {
                return xreplace(e, e->left);
            }
            break;
        case LSHIFT:
        case RSHIFT:
            // x << 0 = x, x >> 0 = x
            if (vr == 0) {
                return xreplace(e, e->left);
            }
            break;
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
 * parse an expression that must yeild a constant. 
 * used for array declarations and CPP stuff
 */
int
parse_const(unsigned char token)
{
    struct expr *e;
    int val;

    // Parse constant expression, stopping before comma operator (priority 15)
    // This allows constants in contexts like enum { A = 10, B = 20 }
    // where we want to stop at the comma
    e = parse_expr(15, 0);
    if (!e) {
        gripe(ER_C_CE);
        return 0;
    }
    if (!(e->flags & E_CONST)) {
        gripe(ER_C_CE);
        return 0;
    }
    val = e->v;
    destroy_expr(e);
    return val;
}

/*
 * vim: tabstop=4 shiftwidth=4 expandtab:
 */
