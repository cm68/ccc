/*
 * this is the a brute force recursive descent parser
 */

#include "ccc.h"

/* file-scope globals/forward declarations expected by legacy parser code */
static struct name *global = 0;
struct stmt *makestmt(char op, struct expr *left);
char *blockname(void);
struct stmt *asmblock(void);

void declaration();

/*
 * parse a statement - this is really the heart of the compiler frontend
 * it recursively calls itself
 * there is some hair here having to do with scope
 */
struct stmt*
statement(struct stmt *parent)
{
    struct stmt *st, **pst = 0;
    struct stmt *head = 0;
    // struct name *v = 0;
    static struct name *global = 0; /* symbol list head used by older parser code */
    struct stmt *makestmt(char op, struct expr *left);
    int block = 1;
    struct scope *sc;

    while (block) {
        st = NULL;  // Initialize st to NULL for each iteration
    	switch (cur.type) {

    	case END:   // end a block
    	case E_O_F: // end of file
            block = 0;
            break;

        case BEGIN: // begin a block
            gettoken();
            push_scope(blockname());
            st = makestmt(BEGIN, 0);
            st->parent = parent;
            st->chain = statement(st);
            pop_scope();
            need(END, END, ER_S_CC);
            // If this is a top-level block (function body), return immediately
            if (parent == NULL) {
                return st;
            }
            break;

        case IF:    // if <condition> <statement>
            gettoken();
            need(LPAR, LPAR, ER_S_NP);
            st = makestmt(IF, parse_expr(PRI_ALL, parent));
            need(RPAR, RPAR, ER_S_NP);
            st->chain = statement(st);
            if (cur.type == ELSE) {   // else <statement>
                gettoken();
                st->otherwise = statement(st);
            } 
            break;
        case BREAK:
            gettoken();
            need(SEMI, SEMI, ER_S_SN);
            st = makestmt(BREAK, 0);
            break;
        case CONTINUE:
            gettoken();
            need(SEMI, SEMI, ER_S_SN);
            break;
        case RETURN:
            gettoken();
            st = makestmt(RETURN, 0);
            if (cur.type != SEMI) {
                st->left = parse_expr(PRI_ALL, parent);
            }
            need(SEMI, SEMI, ER_S_SN);
            break;
        
        /* Local declarations - type keywords */
        case INT:
        case CHAR:
        case SHORT:
        case LONG:
        case FLOAT:
        case DOUBLE:
        case VOID:
        case STRUCT:
        case UNION:
        case ENUM:
        case UNSIGNED:
        case CONST:
        case VOLATILE:
        case STATIC:
        case REGISTER:
        case AUTO:
        case EXTERN:
            declaration();
            st = NULL;  /* declaration() doesn't return a statement */
            break;

        case TYPEDEF:
            /* typedef inside function body - scoped to current block */
            declaration();
            st = NULL;  /* declaration() doesn't return a statement */
            break;
        
        case SYM:
            /* Check if it's a label */
            if (next.type == COLON) {
                st = makestmt(LABEL, 0);
                st->label = strdup(cur.v.name);
                st->flags |= S_LABEL;
                gettoken();
                gettoken();
                break;
            }
            /* Check if it's a typedef name used in a declaration */
            {
                struct name *possible_typedef = lookup_name(cur.v.name, 0);
                if (possible_typedef && possible_typedef->kind == tdef) {
                    declaration();
                    st = NULL;  /* declaration() doesn't return a statement */
                    break;
                }
            }
            /* fall through to expression */
        case LPAR:
        case STAR:
        case INCR:
        case DECR:
            st = makestmt(EXPR, parse_expr(PRI_ALL, parent));
            need(SEMI, SEMI, ER_S_SN);
            break;

        case FOR:   // for (<expr>; <expr>; <expr>) <statement> ;
            gettoken();
            need(LPAR, LPAR, ER_S_NP);
            st = makestmt(FOR, parse_expr(PRI_ALL, parent));
            need(SEMI, SEMI, ER_S_SN);
            st->middle = parse_expr(PRI_ALL, parent);
            need(SEMI, SEMI, ER_S_SN);
            st->right = parse_expr(PRI_ALL, parent);
            need(RPAR, RPAR, ER_S_NP);
            st->chain = statement(st);
            break;

        case WHILE:     // while <condition> <statement> ;
            gettoken();
            need(LPAR, LPAR, ER_S_NP);
            st = makestmt(WHILE, parse_expr(PRI_ALL, parent));
            need(RPAR, RPAR, ER_S_NP);
            st->chain = statement(st);
            break;

        case 'E':
            recover(SEMI, ER_S_OE);
            break;

        case SWITCH:    // switch (<expr>) <block> ;
            gettoken();
            need(LPAR, LPAR, ER_S_NP);
            st = makestmt(SWITCH, parse_expr(PRI_ALL, parent));
            need(RPAR, RPAR, ER_S_NP);
            need(BEGIN, BEGIN, ER_S_SB);
            st->chain = statement(st);
            need(END, END, ER_S_CC);
            break;

        case CASE:
            gettoken();
            // Use priority 13 to stop at colon (ternary/colon have priority 13)
            st = makestmt(CASE, parse_expr(13, parent));
            need(COLON, COLON, ER_S_NL);
            break;

        case GOTO:
            gettoken();
            st = makestmt(GOTO, 0);
            if (cur.type != SYM) {
                recover(ER_S_GL, SEMI);
                break;
            } 
                st->label = strdup(cur.v.name);
            gettoken();
            need(SEMI, SEMI, ER_S_SN);
            break;

        case DEFAULT:
            gettoken();
            need(COLON, COLON, ER_S_NL);
            st = makestmt(DEFAULT, 0);
            break;

        case ';':
            gettoken();
            st = makestmt(';', 0);
            break;

        case DO:    // do <statement> while <condition> ;
            gettoken();
            need(BEGIN, SEMI, ER_S_CC);
            st = makestmt(DO, 0);
            st->chain = statement(st);
            if (cur.type != END || next.type != WHILE) {
                err(ER_S_DO);
                break;
            }
            gettoken();  // advance past END (closing brace)
            gettoken();  // advance past WHILE keyword
            need(LPAR, LPAR, ER_S_NP);
            st->left = parse_expr(PRI_ALL, parent);
            need(RPAR, SEMI, ER_S_NP);
            need(SEMI, SEMI, ER_S_SN);
            break;

        case ASM:
            st->chain = asmblock();
            break;

        default:
            err(ER_E_UO);
            break;
        }
        // Skip if no statement was created (e.g., default case, EOF)
        if (!st) {
            continue;
        }
        if (!pst) {
            head = st;
            st->flags |= S_PARENT;
        } else {
            *pst = st;  // Link previous statement to this one
        }
        pst = &st->next;
        // st->function = v;
        st->parent = parent;

        // If we're parsing a single-statement body for a control structure
        // (if/while/for/etc), return after parsing one statement.
        // Don't return for block statements (BEGIN), switch statements (SWITCH),
        // or top-level (parent == NULL/function body)
        if (parent && parent->op != BEGIN && parent->op != 'S' && st) {
            block = 0;  // Exit the while loop
        }
    } // while
    return head;
}

struct stmt*
makestmt(char op, struct expr *left)
{
	struct stmt *st;

	st = calloc(1, sizeof(*st));  // Zero-initialize all fields
	st->op = op;
	st->left = left;
	return st;
}

/* stubs for missing legacy functions */
struct stmt *
asmblock(void)
{
    return (struct stmt *)0;
}



static struct expr *parse_initializer_list(void)
{
    struct expr *head = NULL;
    struct expr *tail = NULL;
    struct expr *item;

    gettoken(); /* consume { */

    while (cur.type != END) {
        if (cur.type == BEGIN) {
            /* Nested initializer for struct/array member */
            item = parse_initializer_list();
        } else {
            item = parse_expr(0, NULL);
        }

        if (item == NULL) {
            err(ER_E_IT); /* Invalid initializer term */
            return NULL;
        }

        /* Link initializer expressions together */
        if (head == NULL) {
            head = item;
            tail = item;
        } else {
            tail->next = item;
            tail = item;
        }

        if (cur.type == END) {
            break;
        } else if (cur.type != COMMA) {
            err(ER_S_SN); /* need semicolon/separator */
            return NULL;
        }
        gettoken(); /* consume , */
    }
    
    gettoken(); /* consume } */
    return head;
}

struct expr *
do_initializer(void)
{
    struct expr *init;

#ifdef MAXTRACE
    fprintf(stderr, "TRACE INIT: do_initializer() entry\n");
#endif
    gettoken(); /* consume = token */
#ifdef MAXTRACE
    fprintf(stderr, "TRACE INIT: after gettoken(), cur.type=%d\n", cur.type);
#endif

    if (cur.type == BEGIN) {
        /* Handle {...} style initializer list */
#ifdef MAXTRACE
        fprintf(stderr, "TRACE INIT: calling parse_initializer_list()\n");
#endif
        init = parse_initializer_list();
    } else {
        /* Handle simple expression initializer */
#ifdef MAXTRACE
        fprintf(stderr, "TRACE INIT: calling parse_expr()\n");
#endif
        init = parse_expr(0, NULL);
#ifdef MAXTRACE
        fprintf(stderr, "TRACE INIT: parse_expr() returned %p\n", (void*)init);
#endif
    }

    if (init == NULL) {
        err(ER_E_IT); /* Invalid initializer term */
        return NULL;
    }

#ifdef MAXTRACE
    fprintf(stderr, "TRACE INIT: do_initializer() returning %p\n", (void*)init);
#endif
    return init;
}

void
parsefunc(struct name *f)
{
	struct name *param;

	// Push a new scope for the function body
	push_scope(f->name);

	// Install function parameters into the scope at level 2
	// Read parameter info from f->type->elem but create NEW entries at level 2
	if (f->type && (f->type->flags & TF_FUNC)) {
		for (param = f->type->elem; param; param = param->next) {
			// Only add parameters with actual names (skip anonymous ones)
			if (param->name && param->name[0] != '\0') {
				// Create a NEW name entry at level 2 (don't reuse type->elem entry)
				struct name *param_entry = new_name(param->name, var, param->type, 0);
				if (param_entry) {
					param_entry->flags |= V_FUNARG | V_LOCAL;
				}
			}
		}
	}

	// Parse the function body
	f->body = statement(0);
	if (f->body) {
		f->body->flags |= S_FUNC;
	}

	// Dump function while parameters are still in scope (level 2)
	dump_function(f);

	// Pop the function scope
	pop_scope();
}

/*
 * storage class clauses - many combinations are illogical
 */
#define	SC_EXTERN	0x01
#define	SC_REGISTER	0x02
#define	SC_STATIC	0x04
#define	SC_CONST	0x88
#define	SC_VOLATILE	0x10
#define	SC_AUTO		0x20
#define	SC_TYPEDEF	0x40

char *sclass_bitdefs[] = { "EXTERN", "REGISTER", "STATIC", "CONST", "VOLATILE",
		"AUTO", "TYPEDEF"
};

/*
 * parse the storage class on a declaration.  this is a muddy concept, since
 * we've got visibility and storage lumped together, and context also contributes into where our
 * thing actually will reside.  we're just interested in the parse part.
 * so, let's just eat extern, auto, register, volatile, static and const
 * in other compilers, bizarre stuff like fortran, far and pascal show up here.
 * gripe about bogus combinations.
 *
 * how this actually resolves into a storage space is a code generator issue
 */
unsigned char
parse_sclass()
{
	int ret = 0;
	int bit;

	while (1) {
		switch (cur.type) {
		case EXTERN:
			bit = SC_EXTERN;
			break;
		case REGISTER:
			bit = SC_REGISTER;
			break;
		case STATIC:
			bit = SC_STATIC;
			break;
		case CONST:
			bit = SC_CONST;
			break;
		case VOLATILE:
			bit = SC_VOLATILE;
			break;
		case AUTO:
			bit = SC_AUTO;
			break;
		case TYPEDEF:
			bit = SC_TYPEDEF;
			break;
		default:
			bit = 0;
			break;
		}
		if (bit) {
			if (ret & bit) {
				err(ER_P_SC);
			}
			ret |= bit;
			gettoken();
		} else {
			break;
		}
	}
	// bogosity checks
	if ((ret & SC_EXTERN)
			& (ret & (SC_CONST | SC_STATIC | SC_AUTO | SC_REGISTER))) {
		err(ER_P_SC);
	}
	if ((ret & SC_REGISTER) & (ret & (SC_CONST | SC_STATIC))) {
		err(ER_P_SC);
	}
	if ((ret & SC_STATIC) & (ret & (SC_CONST | SC_AUTO))) {
		err(ER_P_SC);
	}
	if ((ret & SC_CONST) & (ret & (SC_VOLATILE))) {
		err(ER_P_SC);
	}
	if ((ret & SC_TYPEDEF) & (ret & (SC_EXTERN | SC_STATIC | SC_AUTO | SC_REGISTER))) {
		err(ER_P_SC);
	}
	return ret;
}

/*
 * read a declaration
 */
void
declaration()
{
	struct type *base;
	struct name *n;
	struct initial *i;
	char sclass;
	struct type *basetype;
	struct name *v;

#ifdef MAXTRACE
	fprintf(stderr, "TRACE DECL: declaration() entry\n");
#endif
	while (1) {
#ifdef MAXTRACE
		fprintf(stderr, "TRACE DECL: top of while loop, calling parse_sclass()\n");
#endif
		sclass = parse_sclass();
#ifdef MAXTRACE
		fprintf(stderr, "TRACE DECL: parse_sclass() returned %d\n", sclass);
		fprintf(stderr, "TRACE DECL: setting basetype = 0\n");
#endif
		basetype = 0;
#ifdef MAXTRACE
		fprintf(stderr, "TRACE DECL: calling declare(&basetype)\n");
#endif

        v = declare(&basetype);
#ifdef MAXTRACE
		fprintf(stderr, "TRACE DECL: declare() returned %p\n", (void*)v);
#endif

        /* error recovery: if declare failed, skip to next ; or , */
        if (!v) {
            while (cur.type != SEMI && cur.type != COMMA && cur.type != E_O_F) {
                gettoken();
            }
            if (cur.type == SEMI) {
                gettoken();
                break;
            }
            if (cur.type == COMMA) {
                gettoken();
                continue;
            }
            /* E_O_F */
            break;
        }

        /* handle typedef declarations */
        if (sclass & SC_TYPEDEF) {
            /* change the name kind from var to tdef */
            v->kind = tdef;
            /* typedefs cannot have initializers or function bodies */
            if (cur.type == ASSIGN) {
                err(ER_T_TD);
                /* skip the initializer */
                while (cur.type != SEMI && cur.type != COMMA && cur.type != E_O_F) {
                    gettoken();
                }
            }
            /* continue to next declarator or end of statement */
            if (cur.type == COMMA) {
                gettoken();
                continue;
            }
            if (cur.type == SEMI) {
                gettoken();
                break;
            }
            /* unexpected token */
            break;
        }

#ifdef MAXTRACE
        fprintf(stderr, "TRACE DECL: checking v->type, v=%p, v->name='%s', v->type=%p\n", (void*)v, (v && v->name ? v->name : "(null)"), (v ? (void*)v->type : NULL));
#endif
        if (v->type) {
#ifdef MAXTRACE
            fprintf(stderr, "TRACE DECL: v->type->flags=0x%x\n", v->type->flags);
#endif
        }
        if (v->type && (v->type->flags & TF_FUNC)) {
#ifdef MAXTRACE
            fprintf(stderr, "TRACE DECL: v->type->flags & TF_FUNC is true\n");
#endif
            if (cur.type == BEGIN) {
                parsefunc(v);

                /* Free the statement tree (dumping now happens in parsefunc) */
                if (v->body) {
                    free_stmt(v->body);
                    v->body = 0;  /* Mark as freed */
                }

                if (sclass == STATIC) {
                    v->flags |= V_STATIC;
                }
                v->flags |= V_GLOBAL;
                v->next = global;
                global = v;
                break;
            }
        }
#ifdef MAXTRACE
        fprintf(stderr, "TRACE DECL: checking cur.type, cur.type=%d\n", cur.type);
#endif
        if (cur.type == ASSIGN) {
#ifdef MAXTRACE
            fprintf(stderr, "TRACE DECL: calling do_initializer()\n");
#endif
            v->init = do_initializer();
#ifdef MAXTRACE
            fprintf(stderr, "TRACE DECL: do_initializer() returned\n");
#endif
        }
#ifdef MAXTRACE
		fprintf(stderr, "TRACE DECL: checking for COMMA/SEMI\n");
#endif
		if (cur.type == COMMA) {
			gettoken();
			continue;
		}
		if (cur.type == SEMI) {
			gettoken();
			break;
		}
#ifdef MAXTRACE
		fprintf(stderr, "TRACE DECL: end of while loop, continuing\n");
#endif
	}
}

char bnbuf[20];

char*
blockname()
{
	static int blockid = 0;
	sprintf(bnbuf, "block %d", blockid++);
	return bnbuf;
}

/*
 * global level parse
 */
void
parse()
{
	push_scope("global");
	initbasictype();
	while (cur.type != E_O_F) {
		while (cur.type == NONE) {
			gettoken();
		}
#ifdef MAXTRACE
		fprintf(stderr, "TRACE PARSE: cur.type=%d\n", cur.type);
#endif
		/* Check if current token looks like start of a declaration */
		/* Also check if it's a typedef name (SYM that's a typedef) */
		struct name *possible_typedef = NULL;
		if (cur.type == SYM) {
			possible_typedef = lookup_name(cur.v.name, 0);
		}

		if (cur.type == INT || cur.type == CHAR || cur.type == SHORT ||
			cur.type == LONG || cur.type == FLOAT || cur.type == DOUBLE ||
			cur.type == VOID || cur.type == UNSIGNED || cur.type == STRUCT ||
			cur.type == UNION || cur.type == ENUM || cur.type == CONST ||
			cur.type == VOLATILE || cur.type == TYPEDEF || cur.type == STATIC ||
			cur.type == REGISTER || cur.type == AUTO || cur.type == EXTERN ||
			(possible_typedef && possible_typedef->kind == tdef)) {
#ifdef MAXTRACE
			fprintf(stderr, "TRACE PARSE: calling declaration()\n");
#endif
			declaration();
#ifdef MAXTRACE
			fprintf(stderr, "TRACE PARSE: declaration() returned\n");
#endif
		} else {
#ifdef MAXTRACE
			fprintf(stderr, "TRACE PARSE: skipping token type=%d\n", cur.type);
#endif
			/* Not a declaration - skip this token to avoid getting stuck */
			gettoken();
		}
	}
	pop_scope();
}

/*
 * Free a statement tree recursively
 */
void
free_stmt(struct stmt *st)
{
	if (!st)
		return;

	/* Free child statements */
	if (st->chain)
		free_stmt(st->chain);
	if (st->otherwise)
		free_stmt(st->otherwise);
	if (st->next)
		free_stmt(st->next);

	/* Free label string if present */
	if (st->label)
		free(st->label);

	/* Note: Don't free st->left, st->right, st->middle (expressions)
	 * as they may be shared or owned elsewhere.
	 * A full implementation would need expression reference counting. */

	free(st);
}

/*
 * Clean up all allocated memory after parsing
 */
void
cleanup_parser(void)
{
	int i;
	struct name *n;
	extern struct name **names;
	extern int lastname;

	/* Free all names and their statement trees */
	for (i = 0; i <= lastname; i++) {
		n = names[i];
		if (n) {
			/* Free function body if present */
			if (n->body)
				free_stmt(n->body);

			/* Free name string (except for function parameters which are owned by type) */
			if (!(n->flags & V_FUNARG) && n->name)
				free(n->name);

			/* Free the name structure itself (except function parameters) */
			if (!(n->flags & V_FUNARG))
				free(n);
		}
	}

	/* Free the names array itself */
	if (names)
		free(names);
}

/*
 * vim: tabstop=4 shiftwidth=4 expandtab:
 */
