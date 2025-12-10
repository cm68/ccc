/*
 * regcache.c - Unified register value cache
 *
 * Tracks what expression each register holds to avoid redundant loads.
 * Uses shallow copies of AST expressions for matching.
 */

#include <stdlib.h>
#include <string.h>
#include "cc2.h"

/*
 * Cache entries - shallow copies of expressions held in each register
 * NULL means register contents unknown/invalid
 */
static struct expr *cacheA;     /* Accumulator (byte) */
static struct expr *cacheHL;    /* Primary (word) */
static struct expr *cacheDE;    /* Secondary (word) */
static struct expr *cacheBC;    /* BC register pair (word) - for BC-allocated vars */

/*
 * Check if two expressions match for caching purposes
 * Compares op, size, value, symbol, and recursively compares children
 */
static int
exprMatch(struct expr *a, struct expr *b)
{
    if (!a || !b) return (a == b);
    if (a->op != b->op) return 0;
    if (a->size != b->size) return 0;

    /* For constants, compare values */
    if (a->op == 'C') {
        return a->value == b->value;
    }

    /* For symbols, compare names */
    if (a->op == '$') {
        if (!a->symbol || !b->symbol) return 0;
        return strcmp(a->symbol, b->symbol) == 0;
    }

    /* For DEREF and other ops, compare children */
    if (!exprMatch(a->left, b->left)) return 0;
    if (!exprMatch(a->right, b->right)) return 0;

    return 1;
}

/*
 * Deep copy of expression tree for caching complex expressions
 */
static struct expr *
deepCopy(struct expr *e)
{
    struct expr *c;
    if (!e) return NULL;
    c = malloc(sizeof(struct expr));
    if (!c) return NULL;
    *c = *e;
    if (e->symbol) c->symbol = strdup(e->symbol);
    c->left = deepCopy(e->left);
    c->right = deepCopy(e->right);
    c->cleanup_block = NULL;
    c->cached_var = NULL;
    return c;
}

/*
 * Deep free of cached expression tree
 */
static void
deepFree(struct expr *e)
{
    if (!e) return;
    deepFree(e->left);
    deepFree(e->right);
    xfree(e->symbol);
    free(e);
}

/*
 * Clear cache for register A
 */
void
cacheInvalA(void)
{
    if (cacheA) {
        deepFree(cacheA);
        cacheA = NULL;
    }
}

/*
 * Clear cache for register HL
 */
void
cacheInvalHL(void)
{
    if (cacheHL) {
        deepFree(cacheHL);
        cacheHL = NULL;
    }
}

/*
 * Clear cache for register DE
 */
void
cacheInvalDE(void)
{
    if (cacheDE) {
        deepFree(cacheDE);
        cacheDE = NULL;
    }
}

/*
 * Clear cache for register BC
 */
void
cacheInvalBC(void)
{
    if (cacheBC) {
        deepFree(cacheBC);
        cacheBC = NULL;
    }
}

/*
 * Clear all caches (e.g., after function call)
 */
void
cacheInvalAll(void)
{
    cacheInvalA();
    cacheInvalHL();
    cacheInvalDE();
    cacheInvalBC();
}

/*
 * Set cache: register now holds expression e
 * Makes a deep copy of e for the cache
 */
void
cacheSetA(struct expr *e)
{
    cacheInvalA();
    cacheA = deepCopy(e);
}

void
cacheSetHL(struct expr *e)
{
    cacheInvalHL();
    cacheHL = deepCopy(e);
}

/*
 * Find which register (if any) holds expression e
 * Returns: 'A', 'H' (HL), 'D' (DE), 'B' (BC), or 0 if not cached
 */
int
cacheFindByte(struct expr *e)
{
    if (cacheA && exprMatch(cacheA, e)) return 'A';
    return 0;
}

int
cacheFindWord(struct expr *e)
{
    if (cacheHL && exprMatch(cacheHL, e)) return 'H';
    if (cacheDE && exprMatch(cacheDE, e)) return 'D';
    if (cacheBC && exprMatch(cacheBC, e)) return 'B';
    return 0;
}

/*
 * Push HL to stack - move HL cache to DE (simulating stack behavior)
 */
void
cachePushHL(void)
{
    cacheInvalDE();
    cacheDE = cacheHL;
    cacheHL = NULL;
}
