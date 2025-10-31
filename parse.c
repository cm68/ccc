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
statement(struct stmt *parent) {
    struct stmt *st, **pst = 0;
    struct stmt *head = 0;
    // struct name *v = 0;
    static struct name *global = 0; /* symbol list head used by older parser code */
    struct stmt *makestmt(char op, struct expr *left);
    int block = 1;
    struct scope *sc;

    while (block) {
    	switch (cur.type) {

    	case END:   // end a block
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
        
        case SYM:
            if (next.type == COLON) {
                st = makestmt(LABEL, 0);
                st->label = strdup(cur.v.name);
                st->flags |= S_LABEL;
                gettoken();
                gettoken();
                break;
            }
            /* fall through */
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
            st = makestmt(CASE, parse_expr(PRI_ALL, parent));
            if (!(st->left->flags & E_CONST) || (st->left->type->size != 1)) {
                err(ER_S_NC);
            }
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
            if (cur.type != LPAR) {
                err(ER_S_NP);
            }
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
        if (!pst) {
            if (!st) {
                continue;
            }
            head = st;
            st->flags |= S_PARENT;
        }
        pst = &st->next;
        // st->function = v;
        st->parent = parent;
    } // while
    return head;
}

struct stmt*
makestmt(char op, struct expr *left) {
	struct stmt *st;

	st = malloc(sizeof(*st));
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

    gettoken(); /* consume = token */

    if (cur.type == BEGIN) {
        /* Handle {...} style initializer list */
        init = parse_initializer_list();
    } else {
        /* Handle simple expression initializer */
        init = parse_expr(0, NULL);
    }

    if (init == NULL) {
        err(ER_E_IT); /* Invalid initializer term */
        return NULL;
    }

    return init;
}

void
parsefunc(struct name *f) {
	f->body = statement(0);
	if (f->body) {
		f->body->flags |= S_FUNC;
	}
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
parse_sclass() {
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
declaration() {
	struct type *base;
	struct name *n;
	struct initial *i;
	char sclass;
	struct type *basetype;
	struct name *v;

	while (1) {
		sclass = parse_sclass();
		basetype = 0;

        v = declare(&basetype);

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

        if (v->type && (v->type->flags & TF_FUNC)) {
            if (cur.type == BEGIN) {
                parsefunc(v);
                if (sclass == STATIC) {
                    v->flags |= V_STATIC;
                }
                v->flags |= V_GLOBAL;
                v->next = global;
                global = v;
                break;
            }
        }
        if (cur.type == ASSIGN) {
            v->init = do_initializer();
        }
		if (cur.type == COMMA) {
			gettoken();
			continue;
		}
		if (cur.type == SEMI) {
			gettoken();
			break;
		}
	}
}

char bnbuf[20];

char*
blockname() {
	static int blockid = 0;
	sprintf(bnbuf, "block %d", blockid++);
	return strdup(bnbuf);
}

/*
 * global level parse
 */
void 
parse() {
	push_scope("global");
	initbasictype();
	while (cur.type != E_O_F) {
		while (cur.type == NONE) {
			gettoken();
		}
		/* Check if current token looks like start of a declaration */
		if (cur.type == INT || cur.type == CHAR || cur.type == SHORT ||
			cur.type == LONG || cur.type == FLOAT || cur.type == DOUBLE ||
			cur.type == VOID || cur.type == UNSIGNED || cur.type == STRUCT ||
			cur.type == UNION || cur.type == ENUM || cur.type == CONST ||
			cur.type == VOLATILE || cur.type == TYPEDEF || cur.type == STATIC ||
			cur.type == REGISTER || cur.type == AUTO || cur.type == EXTERN) {
			declaration();
		} else {
			/* Not a declaration - skip this token to avoid getting stuck */
			gettoken();
		}
	}
	pop_scope();
}

/*
 * vim: tabstop=4 shiftwidth=4 expandtab:
 */
