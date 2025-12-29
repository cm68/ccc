/*
 * constant folding and expression normalization
 */
#include "cc1.h"

/*
 * Determine smallest type that can hold a constant value
 */
struct type *
constType(long v)
{
	if (v < 0)
		return (v >= -128) ? chartype : (v >= -32768) ? inttype : longtype;
	if (v <= 255)
		return uchartype;
	if (v <= 65535)
		return ushorttype;
	return (v <= 2147483647L) ? longtype : ulongtype;
}

/*
 * Replace an expression node in the tree with a different node
 *
 * Substitutes 'in' for 'out' in the expression tree, updating all linkages
 * (next, prev, up) to maintain tree connectivity. Preserves E_FUNARG flag
 * if it was set on the original node. Frees the old node and any orphaned
 * children, then returns the new replacement node.
 *
 * This is used during constant folding and optimization passes to replace
 * complex expressions with simpler ones while maintaining the tree structure.
 *
 * Parameters:
 *   out - Expression node to be replaced (will be freed)
 *   in  - Replacement expression node (must be child of out)
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
    /* Free orphaned children - the one not being promoted */
    if (out->left && out->left != in)
        frExp(out->left);
    if (out->right && out->right != in)
        frExp(out->right);
    free(out);
#ifdef DEBUG
    exprCurCnt--;
#endif
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
	struct expr *left, *right;
	unsigned char op, lop, rop;
	long val;
	long vl, vr;

	op = e->op;
	left = e->left;
	right = e->right;
	lop = left ? left->op : 0;
	rop = right ? right->op : 0;

    switch (op) {
    case NEG:
        if (lop == CONST) {
            val = -left->v;
            e = xreplace(e, left);
            e->v = val;
            e->type = constType((long)val);
        }
        return e;
    case TWIDDLE:
        if (lop == CONST) {
            val = ~left->v;
            e = xreplace(e, left);
            e->v = val;
        }
        return e;
    case BANG:
        if (lop == CONST) {
            val = left->v ? 0 : 1;
            e = xreplace(e, left);
            e->v = val;
        }
        return e;
    case QUES:
        /*
         * Ternary conditional: condition ? true : false
         * Structure: QUES(condition, COLON(true_expr, false_expr))
         */
        if (left && lop == CONST && right && rop == COLON) {
            struct expr *result;
            if (left->v) {
                /* Condition is true, use true branch */
                result = right->left;
                right->left = NULL;  /* prevent frExp from freeing result */
            } else {
                /* Condition is false, use false branch */
                result = right->right;
                right->right = NULL;  /* prevent frExp from freeing result */
            }
            if (result) {
                e = xreplace(e, result);
            }
        }
        return e;
    }
    if (!right) {
        gripe(ER_E_CF);
        return e;
    }

    // Algebraic simplifications for identity operations
    // These work even when one operand is not constant
    if (rop == CONST) {
        long vr = right->v;
        switch (op) {
        case PLUS:
        case MINUS:
        case LSHIFT:
        case RSHIFT:
            if (vr == 0)
                return xreplace(e, left);
            break;
        case STAR:
            if (vr == 1)
                return xreplace(e, left);
            if (vr == 0)
                return xreplace(e, right);
            break;
        case DIV:
            if (vr == 1)
                return xreplace(e, left);
            break;
        }
        // Associative folding: (x + C1) + C2 -> x + (C1+C2)
        // This handles nested struct/union member offsets
        if (op == PLUS && lop == PLUS &&
            left->right && left->right->op == CONST) {
            long c1 = left->right->v;
            long c2 = vr;
            right->v = c1 + c2;
            e->left = xreplace(left, left->left);
            e->left->up = e;
            return cfold(e);  // recurse in case of deeper nesting
        }
    }
    if (lop == CONST) {
        long vl = left->v;
        switch (op) {
        case PLUS:
            // 0 + x = x
            if (vl == 0) {
                return xreplace(e, right);
            }
            break;
        case STAR:
            // 1 * x = x
            if (vl == 1) {
                return xreplace(e, right);
            }
            // 0 * x = 0
            if (vl == 0) {
                return xreplace(e, left);
            }
            break;
        }
    }

    if ((lop != CONST) || (rop != CONST)) {
        return e;
    }
    vl = left->v;
    vr = right->v;

    switch (op) {
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
    e = xreplace(e, left);
    e->v = val;  // Store the computed constant value
    return e;
}

/*
 * Normalize expression tree for better code generation:
 * - Commutative ops: put constant on right
 * - Subtraction: x - const -> x + (-const)
 * - Comparisons: convert to (expr) op 0 form
 *
 * All comparisons become subtract + flag check:
 *   a > b  -> (b - a) < 0       -> sub, check C
 *   a >= b -> (b - a - 1) < 0   -> sub, check C
 *   a < b  -> (a - b) < 0       -> sub, check C
 *   a <= b -> (a - b - 1) < 0   -> sub, check C
 *   a == b -> (a - b) == 0      -> sub, check Z
 *   a != b -> (a - b) != 0      -> sub, check NZ
 *
 * Pass2 should recognize that sub already sets flags, avoiding cp 0.
 */
struct expr *
normalize(struct expr *e)
{
    unsigned char op = e->op;
    struct expr *left = e->left;
    struct expr *right = e->right;
    struct expr *sub, *zero;

    if (!left || !right)
        return e;

    switch (op) {
    /* Commutative ops: swap if left is const and right isn't */
    case PLUS:
    case STAR:
    case AND:
    case OR:
    case XOR:
        if (left->op == CONST && right->op != CONST) {
            e->left = right;
            e->right = left;
            e->left->up = e;
            e->right->up = e;
        }
        break;

    /* Subtraction of constant: x - c -> x + (-c) */
    case MINUS:
        if (right->op == CONST) {
            right->v = -(long)right->v;
            e->op = PLUS;
        }
        break;

    /* Comparisons: convert to (a - b) op 0 form */
    /* For C flag: need left < right, so compute left - right */
    /* For NC flag: need left >= right, so compute left - right */

    /* a > b -> (b - a) < 0 */
    case GT:
        sub = mkexpr(MINUS, right);  /* b - a */
        sub->right = left;
        sub->left->up = sub;
        sub->right->up = sub;
        sub->type = left->type;
        zero = mkexprI(CONST, 0, left->type, 0, E_CONST);
        e->op = LT;
        e->left = sub;
        e->right = zero;
        e->left->up = e;
        e->right->up = e;
        break;

    /* a >= b -> (b - a - 1) < 0, fold -1 into constant if possible */
    case GE:
        if (left->op == CONST) {
            /* a is const: (b - (a+1)) < 0 */
            left->v = left->v + 1;
            sub = mkexpr(MINUS, right);
            sub->right = left;
        } else if (right->op == CONST) {
            /* b is const: ((b-1) - a) < 0 */
            right->v = right->v - 1;
            sub = mkexpr(MINUS, right);
            sub->right = left;
        } else {
            /* neither const: (b - a) + (-1) < 0 */
            struct expr *negone, *adjusted;
            sub = mkexpr(MINUS, right);
            sub->right = left;
            sub->left->up = sub;
            sub->right->up = sub;
            sub->type = left->type;
            negone = mkexprI(CONST, 0, left->type, -1, E_CONST);
            adjusted = mkexpr(PLUS, sub);
            adjusted->right = negone;
            adjusted->left->up = adjusted;
            adjusted->right->up = adjusted;
            adjusted->type = left->type;
            sub = adjusted;  /* use adjusted as the subtraction result */
        }
        sub->left->up = sub;
        sub->right->up = sub;
        sub->type = left->type;
        zero = mkexprI(CONST, 0, left->type, 0, E_CONST);
        e->op = LT;
        e->left = sub;
        e->right = zero;
        e->left->up = e;
        e->right->up = e;
        break;

    /* a < b -> (a - b) < 0 */
    case LT:
        sub = mkexpr(MINUS, left);  /* a - b */
        sub->right = right;
        sub->left->up = sub;
        sub->right->up = sub;
        sub->type = left->type;
        zero = mkexprI(CONST, 0, left->type, 0, E_CONST);
        e->left = sub;
        e->right = zero;
        e->left->up = e;
        e->right->up = e;
        break;

    /* a <= b -> (a - b - 1) < 0, fold -1 into constant if possible */
    case LE:
        if (right->op == CONST) {
            /* b is const: (a - (b+1)) < 0 */
            right->v = right->v + 1;
            sub = mkexpr(MINUS, left);
            sub->right = right;
        } else if (left->op == CONST) {
            /* a is const: ((a-1) - b) < 0 */
            left->v = left->v - 1;
            sub = mkexpr(MINUS, left);
            sub->right = right;
        } else {
            /* neither const: (a - b) + (-1) < 0 */
            struct expr *negone, *adjusted;
            sub = mkexpr(MINUS, left);
            sub->right = right;
            sub->left->up = sub;
            sub->right->up = sub;
            sub->type = left->type;
            negone = mkexprI(CONST, 0, left->type, -1, E_CONST);
            adjusted = mkexpr(PLUS, sub);
            adjusted->right = negone;
            adjusted->left->up = adjusted;
            adjusted->right->up = adjusted;
            adjusted->type = left->type;
            sub = adjusted;
        }
        sub->left->up = sub;
        sub->right->up = sub;
        sub->type = left->type;
        zero = mkexprI(CONST, 0, left->type, 0, E_CONST);
        e->op = LT;
        e->left = sub;
        e->right = zero;
        e->left->up = e;
        e->right->up = e;
        break;

    /* a == b -> (a - b) == 0 */
    case EQ:
        sub = mkexpr(MINUS, left);  /* a - b */
        sub->right = right;
        sub->left->up = sub;
        sub->right->up = sub;
        sub->type = left->type;
        zero = mkexprI(CONST, 0, left->type, 0, E_CONST);
        e->left = sub;
        e->right = zero;
        e->left->up = e;
        e->right->up = e;
        break;

    /* a != b -> (a - b) != 0 */
    case NEQ:
        sub = mkexpr(MINUS, left);  /* a - b */
        sub->right = right;
        sub->left->up = sub;
        sub->right->up = sub;
        sub->type = left->type;
        zero = mkexprI(CONST, 0, left->type, 0, E_CONST);
        e->left = sub;
        e->right = zero;
        e->left->up = e;
        e->right->up = e;
        break;
    }

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
    unsigned char save_phase;

    /*
     * Parse constant expression, stopping before comma operator (priority 15)
     * This allows constants in contexts like enum { A = 10, B = 20 }
     * where we want to stop at the comma.
     *
     * We need to evaluate constants even in phase 1 (for case values).
     * Temporarily set phase=2 so parseExpr builds an expression tree.
     */
    save_phase = phase;
    phase = 2;
    e = parseExpr(15, 0);
    phase = save_phase;

    if (!e) {
        gripe(ER_C_CE);
        return 0;
    }
    if (!(e->flags & E_CONST)) {
        gripe(ER_C_CE);
        frExp(e);
        return 0;
    }
    val = e->v;
    frExp(e);
    return val;
}

/*
 * vim: tabstop=4 shiftwidth=4 expandtab:
 */
