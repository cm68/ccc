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

char *kindname[] = {
    "prim", "etag", "stag", "utag", "vari", "elem", "tdef", "fdef",
    "bitf", "farg", "locl"
};
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
int lexlevel;
int lastname;
struct name **names;

#define ENUM_TYPE   "_uchar_"

/*
 * free up all the names at a higher scope than here
 */
void
pop_scope()
{
	struct name *n;

    lexlevel--;

    while (lastname >= 0) {
        n = names[lastname];
        if (n->level <= lexlevel)
            break;
        names[lastname] = 0;

#ifdef DEBUG
        if (VERBOSE(V_SYM)) {
            fdprintf(2,"pop_scope: remove %s%s from lookup\n",
                n->is_tag ? "tag:":"", n->name);
        }
#endif

        // Note: We don't free the name structure here because it may still
        // be referenced by the AST (statement trees via SYM expressions).
        // The memory will be reclaimed when the process exits.
        lastname--;
    }
}

void
push_scope(char *n)
{
    lexlevel++;
}

/*
 * resolve this name into a name struct
 */
struct name *
lookup_name(char *name, boolean is_tag)
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

/*
 * a more restrictive name lookup that looks through the elements of a type
 * used for struct, union, and enum tag lookups
 */
struct name *
lookup_element(char *name, struct type *t)
{
	struct name *n;
	for (n = t->elem; n; n = n->next) {
		if (strcmp(name, n->name) == 0) {
			return (n);
		}
	}
	return 0;
}

char *type_bitdefs[] = {
		"AGGREGATE", "INCOMPLETE", "UNSIGNED",
        "FUNC", "POINTER", "ARRAY", "FLOAT", "OLD"
};

/*
 * what's in a name
 */
void
dump_name(struct name *n)
{
	fdprintf(2,"dump_name: ");
	if (!n) { printf("null\n"); return; }
	fdprintf(2,"%s (%s)", n->name, n->is_tag ? "tag" : "decl");

	/* Show storage class if set */
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

	if (n->type) {
		dump_type(n->type, 0);
	}
    fdprintf(2,"\toffset: %d bitoff: %d width: %d\n",
        n->offset, n->bitoff, n->width);
}

/*
 * push a name on the symbol list at lexlevel
 */
struct name *
new_name(char *name, kind k, struct type *t, boolean is_tag)
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
                // Existing is extern - return it to be updated with new definition
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
#ifdef DEBUG
    if (VERBOSE(V_SYM)) {
        fdprintf(2,"new_name: level:%d index:%3d %s %s%s\n",
            lexlevel, lastname,
            k < sizeof(kindname)/sizeof(kindname[0]) ? kindname[k] : "unkn",
            is_tag ? "tag:":"", name);
    }
#endif

	return (n);
}

/*
 * add an existing name struct to the symbol table
 * used for installing function parameters into scope
 */
void
add_name(struct name *n)
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

#ifdef DEBUG
    if (VERBOSE(V_SYM)) {
        fdprintf(2,"add_name: level:%d index:%3d %s %s%s\n",
            lexlevel, lastname,
            n->kind < sizeof(kindname)/sizeof(kindname[0]) ? kindname[n->kind] : "unkn",
            n->is_tag ? "tag:":"", n->name);
    }
#endif
}

void
dump_type(struct type *t, int lv)
{
    struct name *param;
    unsigned char param_count = 0;

    if (!t) return;
    if (lv > 20) {  // Cycle detection: prevent infinite recursion
        fdprintf(2,"\t... (max depth reached, possible cycle)\n");
        return;
    }
    if (lv) {
        int i = lv;
        while (i--) {
            fdprintf(2,"\t");
        }
    }

    // Count parameters for functions
    if (t->flags & TF_FUNC) {
        for (param = t->elem; param; param = param->next) {
            param_count++;
        }
    }

    // Display type info - format depends on whether it's a function
    if (t->flags & TF_FUNC) {
        fdprintf(2,"function type: flags %x (%s) params %d\n",
            t->flags, bitdef(t->flags, type_bitdefs), param_count);

        // Show parameter types (without names - function type signature)
        if (param_count > 0) {
            int arg_num = 0;
            for (param = t->elem; param; param = param->next) {
                // Print indentation
                int i;
                for (i = 0; i <= lv; i++)
                    fdprintf(2,"\t");
                fdprintf(2,"arg %d:", arg_num);
                if (param->type && param->type->name) {
                    fdprintf(2," %s", param->type->name);
                } else {
                    fdprintf(2," ?");
                }
                fdprintf(2,"\n");
                arg_num++;
            }
        }

        // Show return type
        if (t->sub) {
            int i;
            for (i = 0; i <= lv; i++)
                fdprintf(2,"\t");
            fdprintf(2,"returntype:\n");
            dump_type(t->sub, lv + 2);
        }
    } else {
        // Non-function types
        fdprintf(2,"name %s flags %x (%s) count %x\n",
            t->name ? t->name : "unnamed",
            t->flags, bitdef(t->flags, type_bitdefs), t->count);
        dump_type(t->sub, ++lv);
    }
}

/*
 * indexes into the basic type table
 */
#define UN_SIGNED   3
#define OTHERS      6
#define BYTES_1     0
#define BYTES_2     1
#define BYTES_4     2
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
	{ "_boolean_", 1, TF_UNSIGNED },
	{ "_float_", 4, TF_FLOAT },
	{ "_double_", 8, TF_FLOAT },
};

/*
 * Compare two function parameter lists for type equality
 * Returns 1 if equal, 0 if different
 * Parameter names are ignored - only types matter
 */
static int
compare_param_lists(struct name *p1, struct name *p2)
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
int
compatible_function_types(struct type *t1, struct type *t2)
{
    if (!t1 || !t2) return 0;
    if (!(t1->flags & TF_FUNC) || !(t2->flags & TF_FUNC)) return 0;

    // Compare return types
    if (t1->sub != t2->sub) return 0;

    // Compare variadic flag
    if ((t1->flags & TF_VARIADIC) != (t2->flags & TF_VARIADIC)) return 0;

    // Compare parameter lists
    return compare_param_lists(t1->elem, t2->elem);
}

/*
 * find this type, or if it does not exist, create it
 * this is a search of all defined types.
 * open question:  how should incomplete types be handled
 *   for structs, it's clear that it gets updated in place.
 *   for arrays, when it becomes concrete, it should be updated
 *   typedefs for arrays can't be incomplete
 */
struct type *
get_type(
    int flags,              // TF_whatever
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
                /* For arrays, also check count to distinguish different array sizes */
                if ((flags & TF_ARRAY) && (t->count != count)) {
                    continue;
                }
                /* For functions, variadic is part of flags so it matches automatically */
                return t;
            }
        }

        if (depth >= 1000) {
            fdprintf(2,"WARNING: type cache search hit depth limit, possible cycle in types list\n");
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
 * we create table of the basic types which we then can parse into.
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
        new_name(basictype[i].name, prim, t, 0);
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
 * parse the basic type
 * these are a little bizarre, since the words 'unsigned', 'short' and 'long'
 * can be prefixes or type names, but short and long can't both exist
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
			length = BYTES_1 + 1;
			goto done;

		case LONG:
			gettoken();
			if (length) {
				gripe(ER_T_PT);
			}
			length = BYTES_4 + 1;
			continue;

		case SHORT:
			gettoken();
			if (length) {
				gripe(ER_T_PT);
			}
			length = BYTES_2 + 1;
			continue;

		case INT:
			gettoken();
			if (!length) {
				length = BYTES_2 + 1;
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
        length = BYTES_2 + 1;
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
 * return a base type
 * this is either a primitive, typedef, struct/union or enum
 * the parse stops when we see a complete type.
 */
struct type *
getbasetype()
{
    struct type *t;
    struct name *n;
    char off = 0;
    char s_buf[64];  /* Stack buffer for tag names */
    char *s;

    /* a typedef? */
    if (cur.type == SYM) {
        n = lookup_name(cur.v.name, 0);
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
     * Enums are just a way to define named integer constants in the global namespace.
     * All enum variables are simply unsigned char.
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
            struct name *e = new_name(cur.v.name, elem, uchartype, 0);
            gettoken();

            // optional = value
            if (cur.type == ASSIGN) {
                gettoken();
                off = parse_const(PRI_ALL);
            }

            // new_name() can return NULL if symbol table full or duplicate name
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
        unsigned char is_union = (cur.type == UNION);
        gettoken();
        n = 0;
        s = 0;

        // optional struct/union tag name
        if (cur.type == SYM) {
            /* Copy tag name to stack buffer */
            strncpy(s_buf, cur.v.name, sizeof(s_buf) - 1);
            s_buf[sizeof(s_buf) - 1] = '\0';
            s = s_buf;
            n = lookup_name(s, 1);  // look for existing tag
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
                t = get_type(TF_AGGREGATE | TF_INCOMPLETE, 0, 0);
                t->size = 0;
                n = new_name(s, is_union ? utag : stag, t, 1);
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
            t = get_type(TF_AGGREGATE, 0, 0);
            t->size = 0;
        }

        if (s) {
            // create or update the tag
            if (!n) {
                n = new_name(s, is_union ? utag : stag, t, 1);
            } else if (n->type != t) {
                // Only update if we created a new type (shouldn't happen now)
                n->type = t;
            }
            /* Stack buffer automatically freed */
        }

        // parse member list: { type name; ... }
        match(BEGIN);
        off = 0;  // offset for struct members
        int bitoff_accumulator = 0;  // bit offset within current word for bitfield packing
        while (cur.type != END && cur.type != E_O_F) {
            struct type *member_type = 0;

            // parse member declaration (struct_elem=true to avoid global namespace pollution)
            struct name *member = declare_internal(&member_type, 1);
            if (!member) {
                // skip to semicolon or end
                while (cur.type != SEMI && cur.type != END && cur.type != E_O_F) {
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
                    // Check if bitfield fits in current word (assume 16-bit words for now)
                    if (bitoff_accumulator + member->width > 16) {
                        // Move to next word
                        off += 2;  // Advance to next word (2 bytes)
                        bitoff_accumulator = 0;
                    }

                    member->offset = off;
                    member->bitoff = bitoff_accumulator;
                    bitoff_accumulator += member->width;

                    // Update struct size if we're using a new word
                    if (off + 2 > t->size) {
                        t->size = off + 2;
                    }
                } else {
                    // Regular member - reset bitfield accumulator
                    if (bitoff_accumulator > 0) {
                        // Finish current bitfield word before adding regular member
                        off += 2;
                        bitoff_accumulator = 0;
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
        return t;
    }

    gripe(ER_T_UT);
    return 0;
}

/*
 * vim: tabstop=4 shiftwidth=4 expandtab:
 */
