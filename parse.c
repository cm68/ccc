/*
 * this is the a brute force recursive descent parser
 */

#include "ccc.h"
#include "lex.h"

void
eat(char match)
{
    if (token != match) {
        lossage("} expected");
    }
}

int
token_p(char match)
{
	if (token == match) {
		advance();
        return 1;
	}
    return 0;
}

/*
 * given a base type, parse a declaration and return the name
 */
struct name *
decl(struct type *base)
{
}

void
declaration(struct scope *sc)
{
    struct type *base;
    struct name *n;
    struct initial *i;

    while (1) {
        base = basetype();
        name = decl(base);
        addname(sc, name);

        if (token_p('=')) {
            initializer();
        }
        if (token_p(';')) {
            break;
        }
        need(',');
    }  
}

void
block()
{
}

/*
 * global level parse
 */
void
parse()
{
    global = new_scope((struct scope *)0, "global");
    while (*inptr) {
        declaration(global);
    }
    destroy_scope(global);
}

/*
 * vim: tabstop=4 shiftwidth=4 expandtab:
 */
