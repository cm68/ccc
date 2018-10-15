/*
 * this is the a brute force recursive descent parser
 */

#include "ccc.h"

/*
 * parse a statement - this is really the heart of the compiler frontend
 * it recursively calls itself
 * there is some hair here having to do with
 */
struct stmt *
statement(struct var *f, struct stmt *parent)
{
    struct stmt *st, **pst;
    pst = 0;
    int block = 1;
    struct scope *sc;

    while (block) {
        switch (curtok) {

        case END:   // end a block
            block = 0;
            break;

        case BEGIN: // begin a block
            gettoken();
            cur_block = new_scope(cur_block, blockname()); 
            st = statement(f, parent);
            sc = cur_block;
            cur_block = sc->parent;
            destroy_scope(sc);
            need(END, END, ER_S_CC);
            break;

        case IF:    // if <condition> <statement>
            gettoken();
            need(LPAR, LPAR, ER_S_NP);
            st = makestmt(IF, expr(PRI_PAREN, parent));
            need(RPAR, RPAR, ER_S_NP);
            st->chain = stmt(f, st);
            if (curtok == ELSE) {   // else <statement>
                gettoken();
                st->otherwise = stmt(f, st);
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
            if (curtok != SEMI) {
                st->left = expr(PRI_ALL, parent);
            }
            need(SEMI, SEMI, ER_S_SN);
            break;
        case SYM:
            if (nexttok == COLON) {
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
            need('(','(', ER_S_NP);
            st = makestmt(FOR, expr(PRI_ALL, parent));
            need(';', ';', EN_S_SN);
            st->middle = expr(PRI_ALL, parent);
            need(';', ';', EN_S_SN);
            st->right = expr(PRI_ALL, parent);
            need(')',')', ER_S_NP);
            st->chain = stmt(f, st);
            break;

        case WHILE:     // while <condition> <statement> ;
            gettoken();
            need('(','(', ER_S_NP);
            st = makestmt(WHILE, expr(PRI_ALL, parent));
            need(')',')', ER_S_NP);
            st->chain = stmt(f, st);
            break;

        case 'E':
            recover(';', ER_S_OE);
            break;

        case SWITCH:    // switch (<expr>) <block> ;
            gettoken();
            need('(','(', ER_S_NP);
            st = makestmt(SWITCH, expr(PRI_ALL, parent));
            need(')',')', ER_S_NP);
            need('{','{', ER_S_SB);
            st->chain = stmt(f, st);
            need('}', '}', ER_S_CC);
            break;

        case CASE:
            gettoken();
            st = newstmt(CASE, expr(PRI_ALL, parent));
            if (!(st->left->flags & E_CONST) || (st->left->type->size != 1)) {
                err(ER_S_NC);
            }
            need(':', ':', ER_S_NL);
            break;

        case GOTO:
            gettoken();
            st = makestmt(GOTO, 0);
            if (curtok != SYM) {
                recover(ER_S_GL, ';');
                break;
            } 
            st->label = strdup(symbuf);
            gettoken();
            need(';',';', ER_S_SN);
            break;

        case DEFAULT:
            gettoken();
            need(':', ':', ER_S_NL);
            st = makestmt(DEFAULT, 0);

        case ';':
            gettoken();
            st = makestmt(';', 0);
            break;

        case DO:    // do <statement> while <condition> ;
            gettoken();
            need('{',';', ER_S_CC);
            st = makestmt(DO, 0);
            st->chain = stmt(f, st);
            if ((curtok != '}') || nexttok != WHILE) {
                err(ER_S_DO);
                break;
            }
            if (curtok != '(') {
                err(ER_S_NP);
            }
            st->left = expr(PRI_ALL, parent);
            need(')',';', ER_S_NP);
            need(';', ';', E_S_SN);
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

char
getsclass(char toplevel)
{
    char sc = curtok;

    if ((sc == CONST) || (sc == EXTERN) || (sc == STATIC) || (sc == VOLATILE)) {
        gettoken();
    } else {
        if ((sc == AUTO) || (sc == REGISTER)) {
            if (toplevel) {
                err(ER_E_TA);
            }
            gettoken();
    }
    return sc;
}

void
declaration(struct scope *sc)
{
    struct type *base;
    struct name *n;
    struct initial *i;

    while (1) {
        sclass = getsclass(1);
        basetype = 0;

        v = declare(&basetype);
        if (v->type & T_FUNC) {
            if (curtok == BEGIN) {
                parsefunc(v);
                if (sclass == 'p') {
                    v->flags |= V_STATIC;
                }
                v->flags |= V_GLOBAL;
                v->next = globals;
                globals = v;
                break;
            }
        }
        if (sclass == 'p') {
            v->flags |= V_STATIC;
        }
        if (curtok == ASSIGN) {
            do_initializar();
        }
        if (curtok == COMMA) {
            gettoken();
            continue;
        }
        if (curtok == SEMI) {
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
    global = new_scope((struct scope *)0, "global");
    while (curtok != EOF) {
        declaration(global);
    }
    destroy_scope(global);
}

/*
 * vim: tabstop=4 shiftwidth=4 expandtab:
 */
