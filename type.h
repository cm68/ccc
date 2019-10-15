/*
 * synthetic type information like enum, struct, union, etc goes away as soon
 * as the scope does.  that means that by the time we get to expression trees
 * we only have references to primitive types, sizes and offsets.
 *
 * there are dialects of C that have polluted the lexical scope, for example
 * where struct elements or tags leak upwards.  this isn't one of them.
 *
 * if you have a c source that depends on this kind of thing, fix the source.
 */
extern struct scope *scope;

struct scope {
	char *scopename;
	struct name *names;
	struct scope *prev;			// all the way up to and including global
};

extern struct scope *global;
extern struct scope	*local;

/*
 * namespaces
 */
typedef enum namespace {
	SYMBOL,
	TYPEDEF,
	ENUMTAG,
	ENUMELEMENT, // only found in sub-types of ENUMTAB
	AGGTAG,
	AGGELEMENT // only found in sub-types of AGGTAG
} namespace_t;

/*
 * how big a pointer is 
 */
#define TS_PTR  2

/*
 * this is a handle for types. types are scoped just like names
 */
struct type {
    struct name *name;  	// if a named struct/union/enum - not always present
	namespace_t space;
	int size;		    	// how big is one of me
	int count;		    	// if we are an array, how many
	int offset;				// if inside a struct
	struct name *elem;		// element list
    struct type *sub;		// pointer to what, array of what, etc.
    unsigned char flags;
};
#define TF_AGGREGATE	0x01
#define TF_INCOMPLETE	0x02
#define TF_UNSIGNED		0x04
#define TF_NORMALIZED	0x08
#define	TF_POINTER		0x10
#define	TF_ARRAY		0x20
#define	TF_FLOAT		0x40

/*
 * this is an instance of a type with a name.
 * a variable.  note that at the same scope, you can have
 * multiple instances of the same name with different namespaces.
 */
struct name {
	namespace_t space;
	char *name;
	struct name *next;		// all names in same container
	struct type *type;
	unsigned char sclass;
};

/*
 * storage classes - many combinations are illogical
 */
#define	SC_GLOBAL	0x01
#define	SC_STATIC	0x02
#define	SC_LOCAL	0x04
#define	SC_REGISTER	0x08
#define	SC_VOLATILE	0x10
#define	SC_CONST	0x20
#define	SC_EXTERN	0x40

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
