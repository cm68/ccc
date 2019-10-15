/*
 * this is the a brute force recursive descent parser
 */

#include "ccc.h"

/*
 * parse a statement - this is really the heart of the compiler frontend
 * it recursively calls itself
 * there is some hair here having to do with scope
 */
struct stmt *
statement(struct stmt *parent)
{
    struct stmt *st, **pst;
    pst = 0;
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
            st = statement(parent);     /* recurse */
            pop_scope();
            need(END, END, ER_S_CC);
            break;

        case IF:    // if <condition> <statement>
            gettoken();
            need(LPAR, LPAR, ER_S_NP);
            st = makestmt(IF, expr(PRI_PAREN, parent));
            need(RPAR, RPAR, ER_S_NP);
            st->chain = statement(st);
            if (cur.type == ELSE) {   // else <statement>
                gettoken();
                st->otherwise = statement(st);
            } 
            break;
        case BREAK;
            gettoken();
            need(SEMI, SEMI, ER_S_SN);
            st = makestmt(BREAK, 0);
            break;
        case DEFAULT;
            gettoken();
            need(SEMI, SEMI, ER_S_SN);
            break;
        case RETURN:
            gettoken();
            st = makestmt(RETURN, 0);
            if (cur.type != SEMI) {
                st->left = expr(PRI_ALL, parent);
            }
            need(SEMI, SEMI, ER_S_SN);
            break;
        case SYM:
            if (next.type == COLON) {
                st = makestmt(LABEL, 0);
                st->label = strdup(symbuf);
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
            st = makestmt(EXPR, expr(PRI_ALL, parent));
            need(SEMI, SEMI, ER_S_SN);
            break;

        case FOR:   // for (<expr>; <expr>; <expr>) <statement> ;
            gettoken();
            need(LPAR, LPAR, ER_S_NP);
            st = makestmt(FOR, expr(PRI_ALL, parent));
            need(SEMI, SEMI, ER_S_SN);
            st->middle = expr(PRI_ALL, parent);
            need(SEMI, SEMI, ER_S_SN);
            st->right = expr(PRI_ALL, parent);
            need(RPAR, RPAR, ER_S_NP);
            st->chain = statement(st);
            break;

        case WHILE:     // while <condition> <statement> ;
            gettoken();
            need(LPAR, LPAR, ER_S_NP);
            st = makestmt(WHILE, expr(PRI_ALL, parent));
            need(RPAR, RPAR, ER_S_NP);
            st->chain = statement(st);
            break;

        case 'E':
            recover(SEMI, ER_S_OE);
            break;

        case SWITCH:    // switch (<expr>) <block> ;
            gettoken();
            need(LPAR, LPAR, ER_S_NP);
            st = makestmt(SWITCH, expr(PRI_ALL, parent));
            need(RPAR, RPAR, ER_S_NP);
            need(BEGIN, BEGIN, ER_S_SB);
            st->chain = statement(st);
            need(END, END, ER_S_CC);
            break;

        case CASE:
            gettoken();
            st = makestmt(CASE, expr(PRI_ALL, parent));
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
            st->label = strdup(symbuf);
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
            if ((cur.type != END || next.type != WHILE) {
                err(ER_S_DO);
                break;
            }
            if (cur.type != LPAR) {
                err(ER_S_NP);
            }
            st->left = expr(PRI_ALL, parent);
            need(RPAR, SEMI, ER_S_NP);
            need(SEMI, SEMI, ER_S_SN);
            break;

        case ASM:
            st->chain = asmblock();
            break;

        default:
            lose();
        }
        if (!pst) {
            if (!st) {
                continue;
            }
            st->flags |= S_PARENT;
        }
        pst = &st->next;
        st->function = v;
        st->parent = parent;
    } // while
}

struct stmt *
makestmt(char op, struct expr *left)
{
    struct stmt *st;

    st = malloc(sizeof(*st));
    st->op = op;
    st->left = left;
    return st;
}

void
parsefunc(struct var *v)
{
    v->body = stmt(v, 0);
    v->body->flags = S_FUNC;
}

/*
 * read a storage class - return bitmask of relevant flags
 * can have several terms
 */
char
getsclass()
{
    int ret = 0;
    int bit;

    while (1) {
		switch (cur.type) {
		case EXTERN:
			bit = V_EXTERN;
			break;
		case REGISTER:
			bit = V_REGISTER;
			break;
		case STATIC:
			bit = V_STATIC;
			break;
		case CONST:
			bit = V_CONST;
			break;
		case VOLATILE:
			bit = V_VOLATILE;
			break;
		case AUTO:
			bit = V_LOCAL;
			break;
		default:
			bit = 0;
			break;
		}
		if (bit) {
			if (ret & bit) {
				error(ER_P_SC);
			}
			ret |= bit;
			gettoken();
		} else {
			break;
		}
    }
    // bogosity checks
    if ((ret & V_EXTERN) & (ret & (V_CONST|V_STATIC|V_LOCAL|V_REGISTER))) {
    	error(ER_P_SC);
    }
    if ((ret & V_REGISTER) & (ret & (V_CONST|V_STATIC))) {
    	error(ER_P_SC);
    }
    if ((ret & V_STATIC) & (ret & (V_CONST|V_LOCAL))) {
    	error(ER_P_SC);
    }
    if ((ret & V_CONST) & (ret & (V_VOLATILE))) {
    	error(ER_P_SC);
    }
    return ret;
}

/*
 * read a declaration
 */
void
declaration(struct scope *sc)
{
    struct type *base;
    struct name *n;
    struct initial *i;
    char sclass;
    struct type *basetype;
    struct name *v;

    while (1) {
        sclass = getsclass();
        basetype = 0;

        v = declare(&basetype);
        if (v->type & T_FUNC) {
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
        if (sclass == 'p') {
            v->flags |= V_STATIC;
        }
        if (cur.type == ASSIGN) {
            do_initializar();
        }
        if (cur.type == COMMA) {
            gettoken();
            continue;
        }
        if (cur.type == SEMI) {
            break;
        }
    }  
}

void
block()
{
}
#endif

/*
 * global level parse
 */
void
parse()
{
    push_scope("global");
    while (cur.type != EOF) {
        declaration(global);
    }
    pop_scope(global);
}

/*
 * vim: tabstop=4 shiftwidth=4 expandtab:
 */
