/*
 * we want a squeaky-clean type system
 * this compiler has an agenda to do operations in as small an integer as 
 * possible. this means that we might even get the wrong answer sometimes.  
 * we don't do the standard thing of doing word arithmetic on bytes just 
 * so we don't get overflows. that's slow and big.  don't be slow and big.
 *
 * there is no redundancy in the type tree, so two variables of the same
 * type have identical type pointers, even if one of them was declared 
 * with a typedef.
 *
 * names go out of scope, but types don't, so we don't need a ref count.
 *
 * we to add the primitive types to the global scope.
 *
 * the purpose of this is to unify basic type and typedef handling
 * example: typedef unsigned char byte 
 *          gets a name entry that points at the primitive
 * 
 * scope is handled by pushing names onto the name stack for open, 
 * and popping for close.  very simple and fast
 *
 * some example types:
 * int f          - short
 * int *f         - pointer -> short
 * int *f[4]      - array(4) -> pointer -> short
 * int f()        - function -> short
 * int *f()       - function -> pointer -> short
 * int (*f)()     - pointer -> function -> short
 * int (*f[4])()  - array(4) -> pointer -> function -> short
 * int *(*f)()    - pointer -> function -> pointer -> short
 *
 * some other interesting cases:
 * int *pi, i;
 * typedef int *pi;  pi *ppi, pi; 
 */


#include "cc1.h"

#ifdef DEBUG
int nameAllocCnt = 0;
int nameCurCnt = 0;
int nameHighWater = 0;
#endif

#ifndef CCC
char *kindname[] = {
    "prim", "etag", "stag", "utag", "vari", "elem", "tdef", "fdef",
    "bitf", "farg", "locl"
};
#endif
/*
 * Static basic types - chained together, never freed
 */
static struct type basictypes[] = {
    { 1, 0, 0, 0, 0,           0 },              // 0 _char_
    { 2, 0, 0, 0, 0,           &basictypes[0] }, // 1 _short_
    { 4, 0, 0, 0, 0,           &basictypes[1] }, // 2 _long_
    { 1, 0, 0, 0, TF_UNSIGNED, &basictypes[2] }, // 3 _uchar_
    { 2, 0, 0, 0, TF_UNSIGNED, &basictypes[3] }, // 4 _ushort_
    { 4, 0, 0, 0, TF_UNSIGNED, &basictypes[4] }, // 5 _ulong_
    { 0, 0, 0, 0, 0,           &basictypes[5] }, // 6 _void_
};
#define N_BASIC (sizeof(basictypes)/sizeof(basictypes[0]))

struct type *types = &basictypes[N_BASIC-1];
struct type *chartype = &basictypes[0];
struct type *inttype = &basictypes[1];
struct type *longtype = &basictypes[2];
struct type *uchartype = &basictypes[3];
struct type *ushorttype = &basictypes[4];
struct type *ulongtype = &basictypes[5];
struct type *voidtype = &basictypes[6];

/*
 * Static name entries for basic types - never freed
 * Chained via 'chain' field: [8]->[7]->...->[0]->NULL
 * Fields: name, type, chain - remaining fields zero by elision
 */
static struct name basicnames[] = {
    { 0, &basictypes[0], 0 },
    { 0, &basictypes[1], &basicnames[0] },
    { 0, &basictypes[2], &basicnames[1] },
    { 0, &basictypes[3], &basicnames[2] },
    { 0, &basictypes[4], &basicnames[3] },
    { 0, &basictypes[5], &basicnames[4] },
    { 0, &basictypes[6], &basicnames[5] },
};

#ifdef DEBUG
/* spellings for typeName - the entries themselves have no names */
static char *basicnm[] = {
    "char", "short", "long", "uchar", "ushort", "ulong", "void",
};
#endif

/*
 * basic types = 0
 * global = 1
 * inner blocks > 1
 */
unsigned char lexlevel;
struct name *names = &basicnames[6];  /* head of chain, most recent first */

/* block numbering, one entry per open lexical level - see pushScope */
static unsigned char blkIdAt[MAXBLKLVL];
static unsigned char blkIdTop;

/*
 * Locals declared in a nested block, waiting to be hoisted into the
 * function that encloses them.  popScope collects them on the way out,
 * because by the time capLocals runs they are off the lookup chain.
 */
struct name *blockLocals;

/*
 * names unlinked by popScope may still be referenced by pending AST
 * (e->var), so they cannot be freed on the spot.  they go on this
 * graveyard chain instead and cleanupParse frees them at exit.
 */
struct name *deadNames;

/*
 * Pop the current lexical scope
 *
 * Removes names at current level by unlinking from the chain.
 * Names are NOT freed (may be referenced by AST).
 */
void
popScope()
{
    struct name **pp, *n, *grabbed, *grabtail;

#ifdef DEBUG
    if (VERBOSE(V_SCOPE)) {
        fdprintf(2, "popScope: %d -> %d (phase=%d)\n", lexlevel, lexlevel - 1, phase);
    }
#endif
    lexlevel--;

    /*
     * In phase 1, skip removal only for file scope (level 1 -> 0).
     * Local function scopes must be cleaned up so that local variables
     * from one function don't conflict with locals from another function.
     * Global names (level 1) are preserved for phase 2 lookup.
     */
    if (phase == 1 && lexlevel == 0)
        return;

    /*
     * Scan entire list and remove names at popped level.
     * Names may be interleaved due to phase 1 preserving globals.
     */
    grabbed = NULL;
    grabtail = NULL;
    pp = &names;
    while ((n = *pp) != NULL) {
        if (n->level > lexlevel) {
#ifdef DEBUG
            if (VERBOSE(V_SYM)) {
                fdprintf(2,"popScope: remove %s%s from lookup\n",
                    n->is_tag ? "tag:":"", nameOf(n->id));
            }
#endif
            /*
             * A local going out of a block inside a function has to be
             * kept: the function still needs a frame slot for it, and
             * capLocals cannot find it once it is off this chain - it
             * only ever sees the level it is called at.  Without this
             * the slot was never allocated, the variable was addressed
             * at (iy+0), and writing to it overwrote the saved IY.
             *
             * Phase 1 only.  Phase 2 re-walks the same declarations and
             * would collect them a second time.
             */
            if (phase == 1 && lexlevel >= 2 && !n->is_tag &&
                (n->kind == kvar || n->kind == klocal)) {
                struct name *copy =
                    (struct name *)galloc(sizeof(struct name));

                memcpy(copy, n, sizeof(struct name));
                copy->next = NULL;
                if (grabtail)
                    grabtail->next = copy;
                else
                    grabbed = copy;
                grabtail = copy;
            }
            *pp = n->chain;  /* Unlink from list */
            n->chain = deadNames;  /* park on graveyard for cleanupParse */
            deadNames = n;
        } else {
            pp = &n->chain;  /* Move to next */
        }
    }
    /*
     * In front of what is already there, not behind it.  Blocks are
     * left innermost first, so appending would put a block's own
     * locals after the ones declared inside it - and the frame
     * allocator reads the list as a walk down the scope tree, where a
     * block has to come before anything nested in it or the two share
     * space they cannot share.  Going in front restores that order.
     */
    if (grabbed) {
        grabtail->next = blockLocals;
        blockLocals = grabbed;
    }
}

/*
 * Push a new lexical scope
 *
 * Increments the lexical level counter to begin a new scope. Names added
 * after this call will be at the new level and will shadow any outer names
 * with the same identifier.
 *
 * Parameters:
 *   n - Scope name for debugging (currently unused)
 */
void
pushScope(char *n)
{
#ifdef DEBUG
    if (VERBOSE(V_SCOPE)) {
        fdprintf(2, "pushScope(%s): %d -> %d (phase=%d)\n", n ? n : "?", lexlevel, lexlevel + 1, phase);
    }
#endif
    lexlevel++;
    /*
     * Number the blocks within a function as they are entered, so a
     * local can say which one declared it.  Level alone cannot: two
     * sibling blocks are both at the same level, and their locals have
     * to share frame space rather than follow one another.
     *
     * Kept per level so that leaving a block returns to the enclosing
     * block's number rather than to the deepest one reached.  The
     * function body is 0 and the count restarts with each function.
     */
    if (lexlevel < MAXBLKLVL)
        blkIdAt[lexlevel] = (lexlevel <= 2) ? (blkIdTop = 0) : ++blkIdTop;
}

/*
 * The block a declaration made now belongs to.
 */
unsigned char
curblk(void)
{
    return lexlevel < MAXBLKLVL ? blkIdAt[lexlevel] : 0;
}

/*
 * Hand the pending block locals back, so the next function starts
 * empty.  Called by capLocals once it has taken the list.
 */
void
clrblklocs(void)
{
    blockLocals = NULL;
}

/*
 * Look up a name in the symbol table chain
 * Traverses from most recent to oldest, first match wins (shadowing)
 */
struct name *
findName(unsigned short id, unsigned char is_tag)
{
    struct name *n;

    for (n = names; n; n = n->chain) {
        if (n->is_tag == is_tag && n->id == id) {
            return n;
        }
    }
    return 0;
}

#ifdef DEBUG
char *typeBitdefs[] = {
		"AGGREGATE", "INCOMPLETE", "UNSIGNED",
        "FUNC", "POINTER", "ARRAY", "?", "OLD"
};
void dumpType(struct type *t, int lv);

/*
 * Recover a printable name for a type.  Types don't carry names;
 * basic types index into basicnames[], and tags/typedefs are found
 * by scanning the symbol chain for an entry that points at t.
 */
static char *
typeName(struct type *t)
{
    struct name *n;

    if (isBasicType(t))
        return basicnm[t - basictypes];
    for (n = names; n; n = n->chain)
        if (n->type == t && (n->is_tag || n->kind == ktdef))
            return nameOf(n->id);
    return "unnamed";
}

void
dumpName(struct name *n)
{
	fdprintf(2,"dumpName: ");
	if (!n) { printf("null\n"); return; }
	fdprintf(2,"%s (%s)", nameOf(n->id), n->is_tag ? "tag" : "decl");
	if (n->sclass) {
		fdprintf(2," sclass=");
		if (n->sclass & SC_EXTERN) printf("extern ");
		if (n->sclass & SC_STATIC) printf("static ");
		if (n->sclass & SC_REGISTER) printf("register ");
		if (n->sclass & SC_AUTO) printf("auto ");
		if (n->sclass & SC_TYPEDEF) printf("typedef ");
	}
	fdprintf(2,"\n");
	dumpType(n->type, 0);
    fdprintf(2,"\toffset: %d bitoff: %d width: %d\n",
        n->w.m.offset, n->w.m.bitoff, n->w.m.width);
}

void
dumpType(struct type *t, int lv)
{
    struct name *param;
    unsigned char param_count = 0;
    int i;

    if (!t) return;
    if (lv > 20) {
        fdprintf(2,"\t... (max depth)\n");
        return;
    }
    for (i = 0; i < lv; i++) fdprintf(2,"\t");

    if (t->flags & TF_FUNC) {
        if (t->count) {
            struct farg *fa;
            for (fa = (struct farg *)t->elem; fa; fa = fa->next)
                param_count++;
        } else {
            for (param = t->elem; param; param = param->next)
                param_count++;
        }
        fdprintf(2,"function: flags %x (%s) params %d\n",
            t->flags, bitdef(t->flags, typeBitdefs), param_count);
        if (t->sub) {
            for (i = 0; i <= lv; i++) fdprintf(2,"\t");
            fdprintf(2,"returns:\n");
            dumpType(t->sub, lv + 2);
        }
    } else {
        fdprintf(2,"name %s flags %x (%s) count %x\n",
            typeName(t),
            t->flags, bitdef(t->flags, typeBitdefs), t->count);
        dumpType(t->sub, ++lv);
    }
}
#endif

/*
 * Find duplicate name at current scope level
 */
static struct name *
findDup(unsigned short id, unsigned char is_tag)
{
    struct name *n = findName(id, is_tag);
    return (n && n->level == lexlevel) ? n : 0;
}

/*
 * Link name at head of symbol table chain
 */
static void
namesAdd(struct name *n)
{
    n->chain = names;
    names = n;
#ifdef DEBUG
    if (VERBOSE(V_SYM)) {
        fdprintf(2,"addName: level:%d %s %s%s\n",
            lexlevel,
            n->kind < sizeof(kindname)/sizeof(kindname[0]) ?
                kindname[n->kind] : "unkn",
            n->is_tag ? "tag:":"", nameOf(n->id));
    }
#endif
}

/*
 * Create and add a new name entry to the symbol table
 */
struct name *
newName(unsigned short id, kind k, struct type *t, unsigned char is_tag)
{
    struct name *n;

    n = findDup(id, is_tag);
    if (n) {
        /* Phase 2: Reuse existing names from phase 1 */
        if (phase == 2)
            return n;
        if (n->sclass & SC_EXTERN)
            return n;
        if (n->kind == ktdef && t == n->type)
            return n;
        gripe(ER_D_DN);
        return 0;
    }

    n = (struct name *)galloc(sizeof(*n));
    /* Initialize in struct field order */
    n->id = id;
    n->type = t;
    /* chain set by namesAdd */
    n->kind = k;
    n->level = lexlevel;
    n->is_tag = is_tag;
    namesAdd(n);
#ifdef DEBUG
    nameAllocCnt++;
    nameCurCnt++;
    if (nameCurCnt > nameHighWater)
        nameHighWater = nameCurCnt;
#endif
    return n;
}

/*
 * Add an existing name entry to the symbol table.
 * Returns the canonical entry: when a duplicate already exists, n is
 * freed and the duplicate returned - the caller must use the returned
 * pointer, never n (the AST holds these pointers and usage tracking
 * writes through them; a stale n would scribble on freed memory).
 */
struct name *
addName(struct name *n)
{
    struct name *dup;

    dup = findDup(n->id, n->is_tag);
    if (dup) {
        /* Phase 2: Name already exists from phase 1 */
        if (phase == 2) {
            free(n);
            return dup;
        }
        if (dup->sclass & SC_EXTERN) {
            dup->sclass = n->sclass;
            if (n->u.init)
                dup->u.init = n->u.init;
            free(n);
            return dup;
        }
        gripe(ER_D_DN);
        return n;
    }

    n->level = lexlevel;
    namesAdd(n);
    return n;
}

/*
 * indexes into the basic type table
 */
#define UN_SIGNED   3
#define OTHERS      6
#define BYTES1     0
#define BYTES2     1
#define BYTES4     2
#define	MISC_BASIC	6

/*
 * Check if a type is a static basic type (not to be freed)
 */
int
isBasicType(struct type *t)
{
    return t >= &basictypes[0] && t <= &basictypes[N_BASIC-1];
}

/*
 * Get parameter type i of a function type, or NULL past the end.
 * Handles both elem shapes: full struct name entries (definitions)
 * and slim farg nodes (declarations, count == 1).
 */
struct type *
fnArgType(struct type *t, unsigned char i)
{
    if (t->count) {
        struct farg *fa = (struct farg *)t->elem;
        while (fa && i--)
            fa = fa->next;
        return fa ? fa->type : NULL;
    } else {
        struct name *p = t->elem;
        while (p && i--)
            p = p->next;
        return p ? p->type : NULL;
    }
}

/*
 * Compare two function types' parameter lists for type equality
 * Returns 1 if equal, 0 if different
 * Parameter names are ignored - only types matter
 */
static int
cmpParamLists(struct type *t1, struct type *t2)
{
    unsigned char i;
    struct type *a1, *a2;

    for (i = 0; ; i++) {
        a1 = fnArgType(t1, i);
        a2 = fnArgType(t2, i);
        if (a1 != a2) {
            return 0;
        }
        if (!a1) {
            // Both lists ended at the same time
            return 1;
        }
    }
}

/*
 * Compare two function types for compatibility
 * Returns 1 if compatible, 0 if different
 * Checks return type and parameter types, ignoring parameter names
 */
char
compatFnTyp(struct type *t1, struct type *t2)
{
    if (!t1 || !t2) return 0;
    if (!(t1->flags & TF_FUNC) || !(t2->flags & TF_FUNC)) return 0;

    // Compare return types
    if (t1->sub != t2->sub) return 0;

    // Compare variadic flag
    if ((t1->flags & TF_VARIADIC) != (t2->flags & TF_VARIADIC)) return 0;

    // Compare parameter lists
    return cmpParamLists(t1, t2);
}

/*
 * Find or create a type in the unified type system
 *
 * Implements type sharing where two variables of the same type have
 * identical type pointers. This ensures zero redundancy in the type tree
 * and enables fast type comparison using pointer equality.
 *
 * Type sharing rules:
 *   - Primitive types (int, char, etc.): Always shared
 *   - Pointers: Shared if flags and subtype match
 *   - Arrays: Shared if flags, subtype, and count match
 *   - Functions: Shared if flags, return type, and parameters match
 *   - Struct/union: NOT shared (TF_AGGREGATE) - each definition is unique
 *
 * Incomplete types:
 *   - Arrays with count=-1 are marked TF_INCOMPLETE
 *   - Forward-declared structs are TF_INCOMPLETE until definition parsed
 *   - Incomplete types may be updated in place when completed
 *
 * Size calculation:
 *   - Arrays: sub->size * count (0 if incomplete)
 *   - Pointers: TS_PTR constant (2 bytes on this architecture)
 *   - Other types: Size set during type creation
 *
 * Parameters:
 *   flags - Type flags (TF_POINTER, TF_ARRAY, TF_FUNC, TF_AGGREGATE, etc.)
 *   sub   - Subtype pointer (element type for array/pointer, return for function)
 *   count - Array element count (-1 for incomplete arrays)
 *
 * Returns:
 *   Pointer to type (existing if found, new if created)
 */
/*
 * How big is this type, as a number rather than as the byte the type
 * node has room for.  struct type keeps size in an unsigned char,
 * which covers everything a register can hold and not an array:
 * "unsigned char buf[512]" records 0, and sizeof answered 0.  The
 * element count survives, so an array is worked out from that.
 *
 * pass1's own lexBuf is 512 bytes, so read(fd, lexBuf, sizeof lexBuf)
 * asked for none and the compiler read an empty file and said nothing
 * - which is what this was found by.
 */
int
typesize(struct type *t)
{
	if (!t)
		return 0;
	/*
	 * An array carries TF_POINTER as well as TF_ARRAY - it decays -
	 * so the pointer bit says nothing here.  A real array is the one
	 * with a count, which an incomplete one ("char buf[]" as a
	 * parameter) does not have.
	 */
	if ((t->flags & TF_ARRAY) && t->sub && t->count > 0)
		return typesize(t->sub) * t->count;
	return t->size;
}

struct type *
getType(
    char flags,             // TF_whatever
    struct type *sub,       // subtype (return type for functions)
    int count)              // if array, length
{
    struct type *t;
#ifdef DEBUG
    int depth = 0;
#endif

    /*
     * search through types to see if we have a permissive match
     * Cycle protection: limit iterations to prevent infinite loop
     *
     * NOTE: Aggregate types (struct/union) are NOT shared because each
     * struct/union definition is unique with its own member list and size.
     * Only share non-aggregate types (pointers, arrays, functions, primitives).
     */
    if (!(flags & TF_AGGREGATE)) {
        for (t = types; t 
#ifdef DEBUG
&& depth < 1000
#endif
; t = t->next
#ifdef DEBUG
, depth++
#endif
) {
            if ((t->flags == flags) && (t->sub == sub)) {
                /*
                 * For arrays, also check count to distinguish different
                 * array sizes
                 */
                if ((flags & TF_ARRAY) && (t->count != count)) {
                    continue;
                }
                /*
                 * For functions, variadic is part of flags so it matches
                 * automatically
                 */
                return t;
            }
        }

#ifdef DEBUG
        if (depth >= 1000) {
            fdprintf(2, "WARNING: type cache search hit depth limit,"
                " possible cycle in types list\n");
        }
#endif
    }

    t = (struct type *)permalloc(sizeof(*t));
    t->sub = sub;
    t->flags = flags;
    t->count = count;
    if (t->count == -1) {
        t->flags |= TF_INCOMPLETE;
    }

    /* Calculate size for arrays and pointers */
    if (flags & TF_ARRAY) {
        if (sub && count > 0) {
            /*
             * Truncated on purpose - size is a byte, which is enough
             * for anything a register holds and not enough for an
             * array.  count is kept, so typesize() below can work the
             * real number out when someone needs it.
             */
            t->size = sub->size * count;
        } else {
            /*
             * Incomplete array - when used in expressions, decays to
             * pointer so size = TS_PTR for argument promotion
             */
            t->size = (flags & TF_POINTER) ? TS_PTR : 0;
        }
    } else if (flags & TF_POINTER) {
        t->size = TS_PTR;  // pointer size constant
    }

    t->next = types;
    types = t;

    return t;
}


/*
 * Parse basic type keywords into a primitive type
 *
 * Handles C's complex type keyword syntax where unsigned, short, and long
 * can be prefixes or standalone type names. Combines multiple keywords to
 * determine the final primitive type.
 *
 * Valid combinations:
 *   - char, short, int, long: Base types with optional unsigned
 *   - unsigned alone: Defaults to unsigned int (2 bytes)
 *   - unsigned char/short/long/int: Unsigned variants
 *   - float, double: Floating point (unsigned not allowed)
 *   - void: No size (unsigned not allowed)
 *
 * Type resolution:
 *   - Tracks unsignedness (0 or UN_SIGNED offset)
 *   - Tracks length (BYTES1/2/4 for char/short/long)
 *   - Tracks misc types (void/float/double offset)
 *   - Computes index into basictype table: unsignedness + length + misc
 *   - Returns corresponding type from names array (installed by initbasictype)
 *
 * Invalid combinations:
 *   - short long (conflicting length)
 *   - unsigned void/float/double (type qualifier mismatch)
 *
 * Type qualifiers:
 *   - const, volatile: Recognized but ignored by this compiler
 *
 * Returns:
 *   Primitive type pointer, or NULL if no type keywords found
 */
struct type *
parsebasic()
{
    char unsignedness = 0;
    char length = 0;
    char misc = 0;

    while (1) {
		switch (cur.type) {

		case CHAR:
			gettoken();
			length = BYTES1 + 1;
			goto done;

		case LONG:
			gettoken();
			if (length) {
				gripe(ER_T_PT);
			}
			length = BYTES4 + 1;
			continue;

		case SHORT:
			gettoken();
			if (length) {
				gripe(ER_T_PT);
			}
			length = BYTES2 + 1;
			continue;

		case INT:
			gettoken();
			if (!length) {
				length = BYTES2 + 1;
			}
			goto done;

        case UNSIGNED:
            gettoken();
            unsignedness = UN_SIGNED;
            continue;


		case VOID:
			gettoken();
			if (length + unsignedness) {
				gripe(ER_T_PT);
				length = 0;
			}
			misc += MISC_BASIC;
			goto done;

		default:
            // no type, no prefixes, unrecognized keyword. stop parsing type.
			if ((length + unsignedness) == 0) {
				return 0;
			}
			goto done;
		}
	}
done:
    if (unsignedness && (length == 0)) {    // naked unsigned
        length = BYTES2 + 1;
    }
    if (length) length--;
    return basicnames[unsignedness + length + misc].type;
}

/*
 * typedef handling and sametype() would go here
 * These require more infrastructure (new_type, normalizetype, etc.)
 * that doesn't exist yet, so they're removed for now.
 */

/*
 * Parse a base type (primitive, typedef, struct, union, or enum)
 *
 * This is the main type parser that handles all base types before
 * declarators add pointers, arrays, and functions. Returns when a complete
 * base type has been parsed.
 *
 * Type forms handled:
 *   - Primitive types: int, char, void, etc. (via parsebasic)
 *   - Typedef names: Previously declared type aliases
 *   - Struct/union: With or without tag, with or without body
 *   - Enum: With or without tag, with or without enumerator list
 *
 * Struct/union processing:
 *   - Forward declarations: struct foo; (creates incomplete type)
 *   - Definitions: struct foo { ... }; (creates complete type)
 *   - Anonymous: struct { ... } x; (no tag)
 *   - Member parsing: Calls declInternal for each member
 *   - Bitfield packing: Packs bitfields into 16-bit words
 *   - Size calculation: Sum of member sizes (struct) or max (union)
 *
 * Enum processing:
 *   - All enums are unsigned char (1 byte)
 *   - Enumerators are named constants in global namespace
 *   - Values default to sequential (0, 1, 2...) or explicit (= expr)
 *   - Tag is optional and ignored (just for documentation)
 *
 * Incomplete types:
 *   - Forward-declared struct/union without body
 *   - Marked TF_INCOMPLETE until definition found
 *   - Updated in place when body parsed later
 *
 * Returns:
 *   Type pointer for parsed base type, or NULL if no type found
 */
struct type *
getbasetype()
{
    struct type *t;
    struct name *n;
    /*
     * unsigned int, not long: offsets and enum values fit in 16 bits,
     * and zc3 miscompiles uchar -> ulong promotions (the loaded byte
     * gets clobbered), which broke every member offset and enum value.
     */
    unsigned int off = 0;
    unsigned short s;	/* tag id, 0 = untagged */
    int bitoff_accum;
    struct type *member_type;
    struct name *member;
    unsigned char is_union;

    /* a typedef? */
    if (cur.type == SYM) {
        n = findName(cur.v.id, 0);
        if (n && (n->kind == ktdef)) {
            gettoken();
            return n->type;
        }
    }
    t = parsebasic();
    if (t) {
        return t;
    }
    if ((cur.type != STRUCT) && (cur.type != UNION)) {
        return 0;
    }

    /*
     * struct or union [name] [ { members } ]
     */
    if (cur.type == STRUCT || cur.type == UNION) {
        is_union = (cur.type == UNION);
        gettoken();
        n = 0;
        s = 0;

        // optional struct/union tag name
        if (cur.type == SYM) {
            s = cur.v.id;
            n = findName(s, 1);  // look for existing tag
            gettoken();

            // if found and already complete, skip body if present and return it
            if (n && !(n->type->flags & TF_INCOMPLETE)) {
                // In phase 2, the body tokens still need to be consumed
                if (cur.type == BEGIN) {
                    unsigned char depth = 1;
                    gettoken();  // consume '{'
                    while (depth > 0 && cur.type != E_O_F) {
                        if (cur.type == BEGIN) depth++;
                        else if (cur.type == END) depth--;
                        gettoken();
                    }
                }
                return n->type;
            }
        }

        // must have a definition if no tag or if forward reference
        if (cur.type != BEGIN) {
            if (n) {
                return n->type;  // forward reference - tag already exists
            }
            // Forward declaration of a new tag (e.g., typedef struct S S_t;)
            // Create an incomplete type
            if (s) {
                t = getType(TF_AGGREGATE | TF_INCOMPLETE, 0, 0);
                t->size = 0;
                n = newName(s, is_union ? kutag : kstag, t, 1);
                return t;
            }
            // No tag name and no definition - error
            gripe(ER_T_ED);
            return 0;
        }

        // create or reuse the struct/union type
        if (n && (n->type->flags & TF_INCOMPLETE)) {
            // Reuse existing incomplete type to maintain pointer identity
            t = n->type;
            t->flags &= ~TF_INCOMPLETE;  // will be completed below
            t->size = 0;
        } else {
            // Create new type
            t = getType(TF_AGGREGATE, 0, 0);
            t->size = 0;
        }

        if (s) {
            // create or update the tag
            if (!n) {
                n = newName(s, is_union ? kutag : kstag, t, 1);
            } else if (n->type != t) {
                // Only update if we created a new type (shouldn't happen now)
                n->type = t;
            }
            /* Stack buffer automatically freed */
        }

        // parse member list: { type name; ... }
        match(BEGIN);
        off = 0;  // offset for struct members
        /*
         * bit offset within current word for bitfield packing
         */
        bitoff_accum = 0;
        while (cur.type != END && cur.type != E_O_F) {
            member_type = 0;

            /*
             * parse member declaration (struct_elem=true to avoid global
             * namespace pollution). Loop handles comma-separated declarators
             * sharing the same base type: int a, *b, c[3];
             */
            do {
                member = declare(&member_type, 1);
                if (!member) {
                    // skip to semicolon or end
                    while (cur.type != SEMI && cur.type != END &&
                           cur.type != E_O_F) {
                        gettoken();
                    }
                    break;
                }

                // add to member list
                member->next = t->elem;
                t->elem = member;

                // Set kind if not already set (bitfields are already marked)
                if (member->kind != kbitfield) {
                    member->kind = kelem;
                }

                // calculate offset and size
                if (is_union) {
                    member->w.m.offset = 0;
                    if (member->type->size > t->size)
                        t->size = member->type->size;
                } else {
                    // Handle bitfield packing
                    if (member->kind == kbitfield) {
                        // Bitfield - pack into current word
                        /*
                         * Check if bitfield fits in current word (assume
                         * 16-bit words for now)
                         */
                        if (bitoff_accum + member->w.m.width > 16) {
                            // Move to next word
                            off += 2;  // Advance to next word (2 bytes)
                            bitoff_accum = 0;
                        }

                        member->w.m.offset = off;
                        member->w.m.bitoff = bitoff_accum;
                        bitoff_accum += member->w.m.width;

                        // Update struct size if we're using a new word
                        if (off + 2 > t->size) {
                            t->size = off + 2;
                        }
                    } else {
                        // Regular member - reset bitfield accumulator
                        if (bitoff_accum > 0) {
                            /*
                             * Finish current bitfield word before adding
                             * regular member
                             */
                            off += 2;
                            bitoff_accum = 0;
                        }

                        member->w.m.offset = off;
                        /* typesize, not the byte-wide size field: a
                         * member array past 255 bytes has a wrapped
                         * size and used to contribute nothing - the
                         * switch stack in pass2 laid out 48 bytes for
                         * 8K and every case value past the tenth
                         * stomped the statics behind it */
                        off += typesize(member->type);
                        t->size = off;
                    }
                }
            } while (cur.type == COMMA && (gettoken(), 1));

            // expect semicolon after member(s)
            if (cur.type == SEMI) {
                gettoken();
            } else if (cur.type != END) {
                gripe(ER_T_ED);
            }
        }
        match(END);

        // mark as complete
        t->flags &= ~TF_INCOMPLETE;

        /* the size field is a byte and member offsets are bytes, so
         * past 255 the layout is silently wrong; past 127 the (ix+d)
         * window is gone.  Check the WIDE accumulator - the wrapped
         * field is exactly what cannot be trusted here. */
        if (off > 127) {
            gripe(ER_T_SB);
        }

        return t;
    }

    gripe(ER_T_UT);
    return 0;
}

/*
 * vim: tabstop=4 shiftwidth=4 expandtab:
 */
