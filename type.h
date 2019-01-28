/*
 * this defines a nested name space.
 * scope is essentially a stack, with the global scope at the bottom.
 * we add/subtract names and types to the scope top, and when we create
 * a block, we push one, and when we end the block, we pop one
 *
 * examples:
 * struct foo { int i; } bar;
 * typedef struct foo footype;
 * footype *baz;
 * footype boop[3];
 * struct { int i; } xyzzy;

 * (name "foo" TYPE 
 *  (type STRUCT "foo" 2 0 0 
 *   (name "i" SYMBOL 
 *    (type INT 2 0 0)
 *   )
 *  )
 * )
 * (name "bar" SYMBOL
 *  (type STRUCT "foo")
 * )
 * (name "footype" TYPEDEF
 *  (type STRUCT "foo")
 * )
 * (name "baz" SYMBOL
 *  (type PTR 2 0 0
 *   (type STRUCT "foo")
 *  )
 * )
 * (name "boop" SYMBOL
 *  (type ARRAY 6 3 0
 *   (type STRUCT "foo")
 *  )
 * )
 * (name "xyzzy" SYMBOL 
 *  (type STRUCT "__anonymous0" 2 0 0 
 *   (name "i" SYMBOL 
 *    (type INT 2 0 0)
 *   )
 *  )
 * )
 *
 * synthetic type information like enum, struct, union, etc goes away as soon
 * as the scope does.  that means that by the time we get to expression trees
 * we only have references to primitive types, sizes and offsets.
 */
extern struct scope *scope;

struct scope {
	char *scopename;
	struct name *names;     
	struct scope *next;
};

extern struct scope *global;

/*
 * C data types
 */
#define TK_INT      0
#define TK_PTR      1
#define TK_ARRAY    2
#define TK_STRUCT   3
#define TK_UNION    4
#define TK_ENUM     5
#define TK_FUNC     6
#define TK_TMASK    0x0f

/*
 * namespaces
 */
#define NK_TDEF     0x10
#define NK_SYMBOL   0x20
#define NK_TYPE     0x40

typedef unsigned char kind_t;

/*
 * how big a pointer is 
 */
#define TS_PTR  2

/*
 * this is a handle for types. types are scoped just like names
 * this gets a little gnarly with sub-structures, since they have
 * the same scope as the contained struct.
 */
struct type {
    struct name *name;  // if a named struct/union/enum - not always present
	kind_t kind;
	int size;		    // how big is one of me
	int count;		    // if we are an array, how many
	int offset;
	struct name *elem;
    int flags;
    struct type *sub;   // it's made of wood
};
#define T_AGGREGATE     0x01
#define T_INCOMPLETE    0x02
#define T_UNSIGNED      0x04
#define T_NORMALIZED    0x08

struct name {
	char *name;             // strdup'ed symbol
	kind_t kind;            // different namespaces for each kind
    struct scope *scope;    // where defined
	struct type *type;      // if this is a type
	struct name *next;      // next name in scope/aggregate/enum
};

/*
 * a declaration can have an initializer.
 * if at global scope or static function scope, it's immediate data.
 * otherwise, it's just code.
 */
struct initial {
    int i;
    /* just a placeholder - XXX */
};

/*
 * vim: tabstop=4 shiftwidth=4 expandtab:
 */
