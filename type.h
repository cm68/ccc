/*
 * this defines a nested name space.  when we see a open curly, we add one
 * and when we see the close, we drop one.
 * there's a global scope that is the parent of them all, and since there
 * is no open curly for that, it does not go away.
 */
extern struct scope *global;

struct scope {
	char *scopename;
	struct name *names;
	struct scope *parent;
};

extern struct scope *global;

/*
 * the fundamental C data type
 */
enum typekind { 
	integer,	/* char, short, int, long */
	pointer, 
	structure,
	enumeration,	 
	enumconst,
	function 
};

struct type {
	struct name *typename;
	enum typekind kind;
	int size;		/* how big is one of me */
	int count;		/* if we are an array, how many */
	int offset;
	struct name *elem;
};

/*
 * C is lexically scoped, so when names go out of scope, they become
 * undefined, potentially exposing the next scope.
 */
enum namekind { 
	type_def, 
	symbol 
};

struct name {
	char *name;
	enum namekind kind;
	struct type *type;
	struct scope *scope;
	struct name *next;
};

/*
 * a declaration can have an initializer.
 * if at global scope or static function scope, it's immediate data.
 * otherwise, it's just code.
 */
struct initial {
    /* just a placeholder - XXX */
};

/*
 * vim: tabstop=4 shiftwidth=4 expandtab:
 */
