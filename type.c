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

#ifndef CCC
char *kindname[] = {
    "prim", "etag", "stag", "utag", "vari", "elem", "tdef", "fdef",
    "bitf", "farg", "locl"
};
#endif
struct type *types;
struct type *chartype;
struct type *inttype;
struct type *longtype;
struct type *uchartype;
struct type *ushorttype;
struct type *ulongtype;
struct type *voidtype;

/*
 * basic types = 0
 * global = 1
 * inner blocks > 1
 */
unsigned char lexlevel;
int lastname;
struct name **names;

#define ENUM_TYPE   "_uchar_"

/*
 * Pop the current lexical scope
 *
 * Removes all names at the current scope level from the symbol table by
 * clearing their entries in the names array. This implements lexical scoping
 * where inner block names shadow outer ones and become inaccessible when
 * the block exits.
 *
 * The function decrements the lexical level and then removes all names
 * with level > new lexlevel by walking backwards through the names array
 * and clearing entries.
 *
 * Note: Name structures are NOT freed here as they may still be referenced
 * by AST nodes (e.g., SYM expressions in statement trees). Memory will be
 * reclaimed on process exit or by explicit cleanupParse() call.
 *
 * Scope levels:
 *   - 0: Basic types (char, int, void, etc.)
 *   - 1: Global declarations
 *   - 2: Function parameters
 *   - 3+: Local blocks (nested)
 */
void
popScope()
{
	struct name *n;

    if (VERBOSE(V_SCOPE)) {
        fdprintf(2, "popScope: %d -> %d\n", lexlevel, lexlevel - 1);
    }
    lexlevel--;

    while (lastname >= 0) {
        n = names[lastname];
        if (n->level <= lexlevel)
            break;
        names[lastname] = 0;

        if (VERBOSE(V_SYM)) {
            fdprintf(2,"popScope: remove %s%s from lookup\n",
                n->is_tag ? "tag:":"", n->name);
        }

        // Note: We don't free the name structure here because it may still
        // be referenced by the AST (statement trees via SYM expressions).
        // The memory will be reclaimed when the process exits.
        lastname--;
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
    if (VERBOSE(V_SCOPE)) {
        fdprintf(2, "pushScope(%s): %d -> %d\n", n ? n : "?", lexlevel, lexlevel + 1);
    }
    lexlevel++;
}

/*
 * Look up a name in the symbol table
 *
 * Searches for a name entry by identifier and tag/non-tag namespace.
 * Searches from most recent (highest index) to oldest (lowest index),
 * implementing proper shadowing where inner declarations hide outer ones.
 *
 * C has two separate namespaces:
 *   - Tag namespace (is_tag=1): struct/union/enum tags
 *   - Ordinary namespace (is_tag=0): variables, functions, typedefs, enums
 *
 * This allows declarations like: struct foo { int foo; }; where the
 * struct tag "foo" and member variable "foo" coexist without conflict.
 *
 * Optimization: First character is checked before calling strcmp to
 * quickly reject non-matches.
 *
 * Parameters:
 *   name   - Identifier to search for
 *   is_tag - 1 for tag namespace, 0 for ordinary namespace
 *
 * Returns:
 *   Pointer to name entry if found, NULL if not found
 */
struct name *
findName(char *name, unsigned char is_tag)
{
	struct name *n;
    int i;

    for (i = lastname; i >= 0; i--) {
        n = names[i];
        if (!n) continue;  /* Skip NULL entries from popped scopes */
        if ((n->is_tag == is_tag) &&
            (name[0] == n->name[0]) &&
            (strcmp(name, n->name) == 0)) {
	        return (n);
		}
	}
	return 0;
}

#ifndef CCC
char *typeBitdefs[] = {
		"AGGREGATE", "INCOMPLETE", "UNSIGNED",
        "FUNC", "POINTER", "ARRAY", "FLOAT", "OLD"
};
void dumpType(struct type *t, int lv);

void
dumpName(struct name *n)
{
	fdprintf(2,"dumpName: ");
	if (!n) { printf("null\n"); return; }
	fdprintf(2,"%s (%s)", n->name, n->is_tag ? "tag" : "decl");
	if (n->sclass) {
		fdprintf(2," sclass=");
		if (n->sclass & SC_EXTERN) printf("extern ");
		if (n->sclass & SC_STATIC) printf("static ");
		if (n->sclass & SC_REGISTER) printf("register ");
		if (n->sclass & SC_AUTO) printf("auto ");
		if (n->sclass & SC_CONST) printf("const ");
		if (n->sclass & SC_VOLATILE) printf("volatile ");
		if (n->sclass & SC_TYPEDEF) printf("typedef ");
	}
	fdprintf(2,"\n");
	if (n->type) dumpType(n->type, 0);
    fdprintf(2,"\toffset: %d bitoff: %d width: %d\n",
        n->offset, n->bitoff, n->width);
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
        for (param = t->elem; param; param = param->next)
            param_count++;
        fdprintf(2,"function: flags %x (%s) params %d\n",
            t->flags, bitdef(t->flags, typeBitdefs), param_count);
        if (t->sub) {
            for (i = 0; i <= lv; i++) fdprintf(2,"\t");
            fdprintf(2,"returns:\n");
            dumpType(t->sub, lv + 2);
        }
    } else {
        fdprintf(2,"name %s flags %x (%s) count %x\n",
            t->name ? t->name : "unnamed",
            t->flags, bitdef(t->flags, typeBitdefs), t->count);
        dumpType(t->sub, ++lv);
    }
}
#endif

/*
 * Create and add a new name entry to the symbol table
 *
 * Allocates a new name structure, initializes it with the provided
 * information, and adds it to the symbol table at the current lexical level.
 * Performs duplicate name checking within the current scope.
 *
 * Duplicate handling:
 *   - Duplicate declarations at the same level are errors (except extern)
 *   - If existing entry has SC_EXTERN, it's returned for update with definition
 *   - Inner declarations shadow outer ones (different levels) - allowed
 *
 * Memory management:
 *   - Name string is duplicated with strdup()
 *   - Structure is zero-initialized with calloc()
 *   - Entry persists in names array until scope is popped
 *
 * Parameters:
 *   name   - Identifier string (will be duplicated)
 *   k      - Name kind (var, tdef, fdef, elem, funarg, etc.)
 *   t      - Type pointer (can be NULL for incomplete types)
 *   is_tag - 1 for tag namespace, 0 for ordinary namespace
 *
 * Returns:
 *   Pointer to new name entry, or existing extern entry for update,
 *   or NULL on error (duplicate name or table full)
 */
struct name *
newName(char *name, kind k, struct type *t, unsigned char is_tag)
{
	struct name *n;
    int i;

    if (!names) {
        names = malloc(sizeof(n) * MAXNAMES);
        lastname = -1;
    }
    if (lastname == MAXNAMES) {
        gripe(ER_D_OF);
        return (0);
    }

    // if there already is one of the same space, lose
    for (i = lastname; i >= 0; i--) {
        if (names[i]->level < lexlevel) {
            break;
        }
        n = names[i];
        if ((n->is_tag == is_tag) &&
            (name[0] == n->name[0]) &&
            (strcmp(name, n->name) == 0)) {
            // Allow extern declaration followed by definition (or vice versa)
            // This is valid C: extern int x; ... int x = 0;
            if (n->sclass & SC_EXTERN) {
                /*
                 * Existing is extern - return it to be updated with new
                 * definition
                 */
                return n;
            }
            gripe(ER_D_DN);
	        return (0);
		}
    }
	n = calloc(1, sizeof(*n));  // Zero-initialize all fields
	n->name = strdup(name);
	n->type = t;
    n->level = lexlevel;
    n->is_tag = is_tag;
    n->kind = k;
    names[++lastname] = n;
    if (VERBOSE(V_SYM)) {
        fdprintf(2,"newName: level:%d index:%3d %s %s%s\n",
            lexlevel, lastname,
            k < sizeof(kindname)/sizeof(kindname[0]) ? kindname[k] : "unkn",
            is_tag ? "tag:":"", name);
    }

	return (n);
}

/*
 * Add an existing name entry to the symbol table
 *
 * Inserts a pre-allocated name structure into the symbol table at the
 * current lexical level. Unlike newName(), this function doesn't allocate
 * a new structure but uses an existing one.
 *
 * Primary use case:
 *   - Installing function parameters into level 2 scope
 *   - Parameters are stored in function type's elem list
 *   - This function makes them visible in function body scope
 *
 * Duplicate handling:
 *   - Checks for duplicates at current level (same as newName)
 *   - If existing entry has SC_EXTERN, updates it instead of adding duplicate
 *   - Frees the new name structure if updating existing extern entry
 *
 * Level update:
 *   - Updates n->level to match current lexlevel
 *   - Allows parameters defined at level 1 to be visible at level 2
 *
 * Parameters:
 *   n - Pre-allocated name entry to add
 */
void
addName(struct name *n)
{
    int i;

    if (!names) {
        names = malloc(sizeof(struct name *) * MAXNAMES);
        lastname = -1;
    }
    if (lastname == MAXNAMES) {
        gripe(ER_D_OF);
        return;
    }

    // Check for duplicate names at current level
    for (i = lastname; i >= 0; i--) {
        if (names[i]->level < lexlevel) {
            break;
        }
        if ((names[i]->is_tag == n->is_tag) &&
            (n->name[0] == names[i]->name[0]) &&
            (strcmp(n->name, names[i]->name) == 0)) {
            // Allow extern declaration followed by definition
            if (names[i]->sclass & SC_EXTERN) {
                // Existing is extern - update it instead of adding duplicate
                // Update storage class to remove extern flag
                names[i]->sclass = n->sclass;
                if (n->u.init) {
                    names[i]->u.init = n->u.init;
                }
                free(n);  // Don't need the new one
                return;
            }
            gripe(ER_D_DN);
            return;
        }
    }

    // Update the name's level to match current scope
    n->level = lexlevel;
    names[++lastname] = n;

    if (VERBOSE(V_SYM)) {
        fdprintf(2,"addName: level:%d index:%3d %s %s%s\n",
            lexlevel, lastname,
            n->kind < sizeof(kindname)/sizeof(kindname[0]) ?
                kindname[n->kind] : "unkn",
            n->is_tag ? "tag:":"", n->name);
    }
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
 * all the basic types are pre-loaded, and there is some
 * sensitivity to index in this table.
 */
static struct {
        char *name;
        short size;
        unsigned char flags;
} basictype[] = {
	{ "_char_", 1, 0 },						// 0
	{ "_short_", 2, 0 },					
	{ "_long_", 4, 0 },
	{ "_uchar_", 1, TF_UNSIGNED },			// 3
	{ "_ushort_", 2, TF_UNSIGNED },
	{ "_ulong_", 4, TF_UNSIGNED },
	{ "_void_", 0, 0 },						// 6
	/* { "_boolean_", 1, TF_UNSIGNED }, */  /* Removed - not used */
	{ "_float_", 4, TF_FLOAT },
	{ "_double_", 8, TF_FLOAT },
};

/*
 * Compare two function parameter lists for type equality
 * Returns 1 if equal, 0 if different
 * Parameter names are ignored - only types matter
 */
static int
cmpParamLists(struct name *p1, struct name *p2)
{
    while (p1 && p2) {
        // Compare parameter types, not names
        if (p1->type != p2->type) {
            return 0;
        }
        p1 = p1->next;
        p2 = p2->next;
    }
    // Both lists must end at the same time
    return (p1 == NULL && p2 == NULL);
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
    return cmpParamLists(t1->elem, t2->elem);
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
struct type *
getType(
    char flags,             // TF_whatever
    struct type *sub,       // subtype (return type for functions)
    int count)              // if array, length
{
    struct type *t;
    int depth = 0;

    /*
     * search through types to see if we have a permissive match
     * Cycle protection: limit iterations to prevent infinite loop
     *
     * NOTE: Aggregate types (struct/union) are NOT shared because each
     * struct/union definition is unique with its own member list and size.
     * Only share non-aggregate types (pointers, arrays, functions, primitives).
     */
    if (!(flags & TF_AGGREGATE)) {
        for (t = types; t && depth < 1000; t = t->next, depth++) {
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

        if (depth >= 1000) {
            fdprintf(2, "WARNING: type cache search hit depth limit,"
                " possible cycle in types list\n");
        }
    }

    t = calloc(1, sizeof(*t));  // Zero-initialize all fields
    t->sub = sub;
    t->flags = flags;
    t->count = count;
    if (t->count == -1) {
        t->flags |= TF_INCOMPLETE;
    }

    /* Calculate size for arrays and pointers */
    if (flags & TF_ARRAY) {
        if (sub && count > 0) {
            t->size = sub->size * count;
        } else {
            t->size = 0;  // incomplete array
        }
    } else if (flags & TF_POINTER) {
        t->size = TS_PTR;  // pointer size constant
    }

    t->next = types;
    types = t;

    return t;
}

/*
 * Initialize basic types and add them to the global symbol table
 *
 * Creates type structures for all primitive types (char, short, long,
 * unsigned variants, void, float, double) and installs them at level 0
 * (basic types level) in the symbol table.
 *
 * These types are always available and persist for the entire compilation.
 * They serve as building blocks for all derived types (pointers, arrays,
 * functions, structs).
 *
 * Type table layout (indices are significant):
 *   0-2: Signed types (char, short, long)
 *   3-5: Unsigned types (uchar, ushort, ulong)
 *   6+:  Special types (void, float, double)
 *
 * Global type pointers are initialized:
 *   - chartype (1 byte signed)
 *   - inttype (2 bytes signed) - NOTE: int is 2 bytes, not 4!
 *   - longtype (4 bytes signed)
 *   - uchartype, ushorttype, ulongtype (unsigned variants)
 *   - voidtype (0 bytes)
 *
 * Called once at start of parse() before processing any declarations.
 */
void
initbasictype()
{
    int i;
    struct type *t;

    for (i = 0; i < sizeof(basictype)/sizeof(basictype[0]); i++) {
        t = calloc(1, sizeof(*t));  // Zero-initialize all fields
        t->name = basictype[i].name;
        t->flags = basictype[i].flags;
        t->size = basictype[i].size;
        t->next = types;
        types = t;
        newName(basictype[i].name, prim, t, 0);
        if (i == 0) chartype = t;
        if (i == 1) inttype = t;  // int is 2 bytes (short), not 4 bytes (long)
        if (i == 2) longtype = t;
        if (i == 3) uchartype = t;
        if (i == 4) ushorttype = t;
        if (i == 5) ulongtype = t;
        if (i == 6) voidtype = t;
    }
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
	struct name *n;
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

		case CONST:
		case VOLATILE:
			gettoken();
			continue;

		case DOUBLE:
			misc++;
		case FLOAT:
			misc++;
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
    n = names[unsignedness + length + misc];
	return n->type;
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
    unsigned long off = 0;
    char s_buf[64];  /* Stack buffer for tag names */
    char *s;
    struct name *e;
    int bitoff_accum;
    struct type *member_type;
    struct name *member;
    unsigned char is_union;

    /* a typedef? */
    if (cur.type == SYM) {
        n = findName(cur.v.name, 0);
        if (n && (n->kind == tdef)) {
            gettoken();
            return n->type;
        }
    }
    t = parsebasic();
    if (t) {
        return t;
    }
    if ((cur.type != ENUM) && (cur.type != STRUCT) && (cur.type != UNION)) {
        return 0;
    }

    /*
     * enum [name] { tag [= const], ... }
     *
     * Enums are just a way to define named integer constants in the global
     * namespace. All enum variables are simply unsigned char.
     * The tag name (if present) is ignored - it's just for documentation.
     */
    if (cur.type == ENUM) {
        gettoken();

        // optional enum tag name (ignored)
        if (cur.type == SYM) {
            gettoken();
        }

        // If no definition follows, just return unsigned char type
        if (cur.type != BEGIN) {
            return uchartype;
        }

        // parse enum constants: { name [= value], ... }
        match(BEGIN);
        off = 0;
        while (cur.type != END && cur.type != E_O_F) {
            if (cur.type != SYM) {
                gripe(ER_T_ET);
                break;
            }

            // create enum constant as a global name with elem kind
            e = newName(cur.v.name, elem, uchartype, 0);
            gettoken();

            // optional = value
            if (cur.type == ASSIGN) {
                gettoken();
                off = parseConst(PRI_ALL);
            }

            // newName() can return NULL if symbol table full or duplicate name
            if (e) {
                e->offset = off;  // store enum value in offset field
            }
            off++;

            if (cur.type == COMMA) {
                gettoken();
                continue;
            }
            if (cur.type != END) {
                gripe(ER_T_ED);
                break;
            }
        }
        match(END);
        return uchartype;
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
            /* Copy tag name to stack buffer */
            strncpy(s_buf, cur.v.name, sizeof(s_buf) - 1);
            s_buf[sizeof(s_buf) - 1] = '\0';
            s = s_buf;
            n = findName(s, 1);  // look for existing tag
            gettoken();

            // if found and already complete, return it
            if (n && !(n->type->flags & TF_INCOMPLETE)) {
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
                n = newName(s, is_union ? utag : stag, t, 1);
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
                n = newName(s, is_union ? utag : stag, t, 1);
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
             * namespace pollution)
             */
            member = declInternal(&member_type, 1);
            if (!member) {
                // skip to semicolon or end
                while (cur.type != SEMI && cur.type != END &&
                       cur.type != E_O_F) {
                    gettoken();
                }
                if (cur.type == SEMI) gettoken();
                continue;
            }

            // add to member list
            member->next = t->elem;
            t->elem = member;

            // Set kind if not already set (bitfields are already marked)
            if (member->kind != bitfield) {
                member->kind = elem;
            }

            // calculate offset and size
            if (is_union) {
                member->offset = 0;
                if (member->type && member->type->size > t->size) {
                    t->size = member->type->size;
                }
            } else {
                // Handle bitfield packing
                if (member->kind == bitfield) {
                    // Bitfield - pack into current word
                    /*
                     * Check if bitfield fits in current word (assume
                     * 16-bit words for now)
                     */
                    if (bitoff_accum + member->width > 16) {
                        // Move to next word
                        off += 2;  // Advance to next word (2 bytes)
                        bitoff_accum = 0;
                    }

                    member->offset = off;
                    member->bitoff = bitoff_accum;
                    bitoff_accum += member->width;

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

                    member->offset = off;
                    if (member->type) {
                        off += member->type->size;
                        t->size = off;
                    }
                }
            }

            // expect semicolon after member
            if (cur.type == SEMI) {
                gettoken();
            } else if (cur.type != END) {
                gripe(ER_T_ED);
            }
        }
        match(END);

        // mark as complete
        t->flags &= ~TF_INCOMPLETE;

        /* warn if struct exceeds Z80 IX offset limit */
        if (t->size > 127) {
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
