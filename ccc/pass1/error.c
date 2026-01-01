/*
 * errors, messages, and recovery
 */
#include "cc1.h"

#define DEF_ERRMSG
#include "error.h"

int error;
int exitCode = 0;  /* Global exit code: 0=success, 1=errors occurred */

void
gripe(error_t errcode)
{
    int i = errcode;
    if (i > ER_WTF) i = ER_WTF;
    fdprintf(2, "%s:%d: %s\n", filename, lineno, errmsg[i]);
    error = errcode;
    exitCode = 1;
}

/*
 * some errors are too nasty to fix
 */
void
fatal(error_t errcode)
{
    gripe(errcode);
    fdprintf(2, "fatal\n");
    exit(-errcode);
}

/*
 * throw an error message and discard tokens until we see the token we specify
 */
void
recover(error_t errcode, token_t skipto)
{
    gripe(errcode);
    while ((cur.type != skipto) && (cur.type != E_O_F)) {
        gettoken();
    }
}

/*
 * the next token must be 'check'.  if it isn't, gripe about it and skip
 * until we find 'skipto'
 */
void
need(token_t check, token_t skipto, error_t errcode)
{
    if (cur.type == check) {
        gettoken();
        return;
    }
    recover(errcode, skipto);
}

/*
 * expect: simplified token checking - gripe if wrong, advance regardless
 * Used to reduce code duplication where error recovery isn't needed
 */
void
expect(token_t check, error_t errcode)
{
    if (cur.type != check) {
        gripe(errcode);
    }
    gettoken();
}

/*
 * vim: tabstop=4 shiftwidth=4 expandtab:
 */

