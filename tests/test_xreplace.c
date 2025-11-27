/* Standalone test for xreplace() function from expr.c
 *
 * This file extracts the xreplace() function and minimal supporting
 * structures to test the expression node replacement logic independently.
 */

#include <stdlib.h>
#include <stdio.h>

/* Minimal struct expr definition from cc1.h */
struct expr {
    unsigned char flags;
#define E_CONST     0x01
#define E_RESOLVED  0x02
#define E_FUNARG    0x04
#define E_POSTFIX   0x08
    unsigned char op;
    struct expr *left;
    struct expr *right;
    struct expr *up;
    struct expr *prev;
    struct expr *next;

    void *type;  /* Simplified - normally struct type * */
    void *var;   /* Simplified - normally struct var * */

    unsigned long v;
    unsigned char location;
    unsigned char regs;
    void *stmt;  /* Simplified - normally struct stmt * */
    void *inst;  /* Simplified - normally struct inst * */
};

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

/* Helper function to create a new expression node */
struct expr *
makeexpr(unsigned char op)
{
    struct expr *e = (struct expr *)calloc(1, sizeof(struct expr));
    if (!e) {
        fprintf(stderr, "Out of memory\n");
        exit(1);
    }
    e->op = op;
    return e;
}

/* Test function: Basic replacement */
void tBasicRepl(void)
{
    struct expr *old_node, *new_node, *result;

    old_node = makeexpr('+');
    old_node->v = 10;

    new_node = makeexpr('C');  /* Constant */
    new_node->v = 42;

    result = xreplace(old_node, new_node);

    if (result != new_node) {
        printf("FAIL: tBasicRepl - result != new_node\n");
        return;
    }
    if (result->v != 42) {
        printf("FAIL: tBasicRepl - value not preserved\n");
        return;
    }

    free(new_node);
    printf("PASS: tBasicRepl\n");
}

/* Test function: Linkage preservation */
void tLinkage(void)
{
    struct expr *old_node, *new_node, *prev_node, *next_node, *parent_node;
    struct expr *result;

    /* Create a chain: prev -> old -> next */
    prev_node = makeexpr('A');
    old_node = makeexpr('B');
    next_node = makeexpr('C');
    parent_node = makeexpr('P');

    prev_node->next = old_node;
    old_node->prev = prev_node;
    old_node->next = next_node;
    next_node->prev = old_node;
    old_node->up = parent_node;

    /* Create replacement */
    new_node = makeexpr('X');

    result = xreplace(old_node, new_node);

    /* Verify linkages */
    if (prev_node->next != new_node) {
        printf("FAIL: tLinkage - prev->next not updated\n");
        goto cleanup;
    }
    if (next_node->prev != new_node) {
        printf("FAIL: tLinkage - next->prev not updated\n");
        goto cleanup;
    }
    if (new_node->prev != prev_node) {
        printf("FAIL: tLinkage - new->prev incorrect\n");
        goto cleanup;
    }
    if (new_node->next != next_node) {
        printf("FAIL: tLinkage - new->next incorrect\n");
        goto cleanup;
    }
    if (new_node->up != parent_node) {
        printf("FAIL: tLinkage - new->up incorrect\n");
        goto cleanup;
    }

    printf("PASS: tLinkage\n");

cleanup:
    free(prev_node);
    free(new_node);
    free(next_node);
    free(parent_node);
}

/* Test function: E_FUNARG flag preservation */
void tFunarg(void)
{
    struct expr *old_node, *new_node, *result;

    old_node = makeexpr('+');
    old_node->flags = E_FUNARG;

    new_node = makeexpr('C');
    new_node->flags = 0;

    result = xreplace(old_node, new_node);

    if (!(result->flags & E_FUNARG)) {
        printf("FAIL: tFunarg - E_FUNARG not preserved\n");
        free(new_node);
        return;
    }

    printf("PASS: tFunarg\n");
    free(new_node);
}

/* Main test runner */
int main(void)
{
    printf("=== Running xreplace() tests ===\n");

    tBasicRepl();
    tLinkage();
    tFunarg();

    printf("=== All tests complete ===\n");
    return 0;
}
