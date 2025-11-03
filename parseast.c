/*
 * parseast.c - Table-driven parser for AST S-expressions
 *
 * Reads AST output from cc1 and dispatches to handler functions.
 * Each handler consumes the entire s-expression for its operation.
 */
#include <stdlib.h>
#include <string.h>
#include <fcntl.h>
#include <unistd.h>

/* Forward declaration from util.c */
int fdprintf(int fd, const char *fmt, ...);

#define BUFSIZE 4096

/* Parser state */
static int in_fd;
static char buf[BUFSIZE];
static int buf_pos;
static int buf_valid;
static int line_num = 1;
static char curchar;

/*
 * Read next character from input
 * Returns 0 on EOF
 */
static char
nextchar(void)
{
    if (buf_pos >= buf_valid) {
        buf_valid = read(in_fd, buf, BUFSIZE);
        if (buf_valid <= 0) {
            curchar = 0;
            return 0;
        }
        buf_pos = 0;
    }
    curchar = buf[buf_pos++];
    if (curchar == '\n') {
        line_num++;
    }
    return curchar;
}

/*
 * Skip whitespace
 */
static void
skipwhite(void)
{
    while (curchar == ' ' || curchar == '\t' || curchar == '\n') {
        nextchar();
    }
}

/*
 * Skip comments (lines starting with ;)
 */
static void
skipcomment(void)
{
    if (curchar == ';') {
        while (curchar && curchar != '\n') {
            nextchar();
        }
        if (curchar == '\n') {
            nextchar();
        }
    }
}

/*
 * Skip whitespace and comments
 */
static void
skip(void)
{
    while (1) {
        skipwhite();
        if (curchar == ';') {
            skipcomment();
        } else {
            break;
        }
    }
}

/*
 * Expect and consume a specific character
 */
static int
expect(char c)
{
    skip();
    if (curchar != c) {
        fdprintf(2, "parseast: line %d: expected '%c', got '%c'\n",
                 line_num, c, curchar);
        return 0;
    }
    nextchar();
    return 1;
}

/*
 * Read a symbol name (starting with $ or alphanumeric)
 * Returns pointer to static buffer
 */
static char symbuf[256];
static char *
read_symbol(void)
{
    int i = 0;

    skip();

    /* Symbol can start with $ or letter */
    if (curchar == '$' || (curchar >= 'a' && curchar <= 'z') ||
        (curchar >= 'A' && curchar <= 'Z') || curchar == '_') {
        symbuf[i++] = curchar;
        nextchar();

        /* Continue with alphanumeric or underscore */
        while ((curchar >= 'a' && curchar <= 'z') ||
               (curchar >= 'A' && curchar <= 'Z') ||
               (curchar >= '0' && curchar <= '9') ||
               curchar == '_') {
            if (i < sizeof(symbuf) - 1) {
                symbuf[i++] = curchar;
            }
            nextchar();
        }
    }

    symbuf[i] = '\0';
    return symbuf;
}

/*
 * Read a number (decimal integer or constant)
 */
static long
read_number(void)
{
    long val = 0;
    int sign = 1;

    skip();

    if (curchar == '-') {
        sign = -1;
        nextchar();
    }

    while (curchar >= '0' && curchar <= '9') {
        val = val * 10 + (curchar - '0');
        nextchar();
    }

    return val * sign;
}

/*
 * Read a type name (e.g., _short_, _char_, :ptr, :array:10)
 */
static char typebuf[256];
static char *
read_type(void)
{
    int i = 0;

    skip();

    /* Type can start with _ or : */
    if (curchar == '_' || curchar == ':') {
        while ((curchar >= 'a' && curchar <= 'z') ||
               (curchar >= 'A' && curchar <= 'Z') ||
               (curchar >= '0' && curchar <= '9') ||
               curchar == '_' || curchar == ':') {
            if (i < sizeof(typebuf) - 1) {
                typebuf[i++] = curchar;
            }
            nextchar();
        }
    }

    typebuf[i] = '\0';
    return typebuf;
}

/* Forward declarations for handlers */
static void parse_expr(void);
static void parse_stmt(void);

/*
 * Handler functions for each operation
 * Each handler consumes the entire s-expression including closing paren
 */

/* Expression handlers */

static void
handle_const(void)
{
    long val = read_number();
    fdprintf(1, "CONST %ld", val);
}

static void
handle_symbol(void)
{
    char *sym = read_symbol();
    fdprintf(1, "SYM %s", sym);
}

static void
handle_string(void)
{
    /* String literal: S followed by index */
    long idx = read_number();
    fdprintf(1, "STRING S%ld", idx);
}

static void
handle_binary_op(char op)
{
    fdprintf(1, "BINOP %c (", op);
    skip();
    parse_expr();  /* left operand */
    fdprintf(1, ", ");
    skip();
    parse_expr();  /* right operand */
    fdprintf(1, ")");
    expect(')');
}

static void
handle_unary_op(char op)
{
    fdprintf(1, "UNOP %c (", op);
    skip();
    parse_expr();  /* operand */
    fdprintf(1, ")");
    expect(')');
}

static void
handle_deref(void)
{
    char width = 's';  /* default */

    /* Check for width annotation :b :s :l :p :f :d */
    skip();
    if (curchar == ':') {
        nextchar();
        width = curchar;
        nextchar();
    }

    fdprintf(1, "DEREF:%c (", width);
    skip();
    parse_expr();  /* address expression */
    fdprintf(1, ")");
    expect(')');
}

static void
handle_assign(void)
{
    char width = 's';  /* default */

    /* Check for width annotation */
    skip();
    if (curchar == ':') {
        nextchar();
        width = curchar;
        nextchar();
    }

    fdprintf(1, "ASSIGN:%c (", width);
    skip();
    parse_expr();  /* lvalue */
    fdprintf(1, ", ");
    skip();
    parse_expr();  /* rvalue */
    fdprintf(1, ")");
    expect(')');
}

static void
handle_call(void)
{
    fdprintf(1, "CALL (");
    skip();
    parse_expr();  /* function */

    /* Arguments */
    skip();
    while (curchar != ')') {
        fdprintf(1, ", ");
        parse_expr();
        skip();
    }

    fdprintf(1, ")");
    expect(')');
}

static void
handle_ternary(void)
{
    fdprintf(1, "TERNARY (");
    skip();
    parse_expr();  /* condition */
    fdprintf(1, " ? ");
    skip();

    /* Expect COLON node */
    if (curchar == '(') {
        nextchar();
        skip();
        if (curchar == ':') {
            nextchar();
            skip();
            parse_expr();  /* true expr */
            fdprintf(1, " : ");
            skip();
            parse_expr();  /* false expr */
            expect(')');
        }
    }

    fdprintf(1, ")");
    expect(')');
}

/* Statement handlers */

static void
handle_block(void)
{
    fdprintf(1, "BLOCK {\n");

    skip();
    while (curchar != ')') {
        /* Could be declaration or statement */
        skip();
        if (curchar == '(') {
            nextchar();
            skip();
            if (curchar == 'd') {
                /* Declaration */
                nextchar();
                char *name = read_symbol();
                char *type = read_type();
                fdprintf(1, "  DECL %s %s\n", name, type);
                expect(')');
            } else {
                /* Statement */
                parse_stmt();
                fdprintf(1, "\n");
            }
        }
        skip();
    }

    fdprintf(1, "}");
    expect(')');
}

static void
handle_if(void)
{
    fdprintf(1, "IF (");
    skip();
    parse_expr();  /* condition */
    fdprintf(1, ") ");

    skip();
    parse_stmt();  /* then branch */

    skip();
    if (curchar != ')') {
        fdprintf(1, " ELSE ");
        parse_stmt();  /* else branch */
    }

    expect(')');
}

static void
handle_while(void)
{
    fdprintf(1, "WHILE (");
    skip();
    parse_expr();  /* condition */
    fdprintf(1, ") ");

    skip();
    parse_stmt();  /* body */

    expect(')');
}

static void
handle_do(void)
{
    fdprintf(1, "DO ");
    skip();
    parse_stmt();  /* body */

    fdprintf(1, " WHILE (");
    skip();
    parse_expr();  /* condition */
    fdprintf(1, ")");

    expect(')');
}

static void
handle_for(void)
{
    fdprintf(1, "FOR (");
    skip();
    parse_expr();  /* init */
    fdprintf(1, "; ");
    skip();
    parse_expr();  /* condition */
    fdprintf(1, "; ");
    skip();
    parse_expr();  /* increment */
    fdprintf(1, ") ");

    skip();
    parse_stmt();  /* body */

    expect(')');
}

static void
handle_return(void)
{
    fdprintf(1, "RETURN");
    skip();
    if (curchar != ')') {
        fdprintf(1, " ");
        parse_expr();
    }
    expect(')');
}

static void
handle_expr_stmt(void)
{
    fdprintf(1, "EXPR (");
    skip();
    parse_expr();
    fdprintf(1, ")");
    expect(')');
}

static void
handle_empty_stmt(void)
{
    fdprintf(1, ";");
    expect(')');
}

/* Top-level handlers */

static void
handle_function(void)
{
    char *name;

    /* (f name (params) return_type declarations body) */
    name = read_symbol();
    fdprintf(1, "\nFUNCTION %s\n", name);

    /* Parameters */
    skip();
    expect('(');
    fdprintf(1, "  PARAMS: ");
    skip();
    while (curchar != ')') {
        char *param = read_symbol();
        fdprintf(1, "%s ", param);
        skip();
    }
    expect(')');
    fdprintf(1, "\n");

    /* Return type */
    char *rettype = read_type();
    fdprintf(1, "  RETURNS: %s\n", rettype);

    /* Declarations and body */
    skip();
    while (curchar != ')') {
        skip();
        if (curchar == '(') {
            /* Peek ahead to see if it's a declaration or statement */
            nextchar();
            skip();
            if (curchar == 'd') {
                /* Declaration - we're already past the '(' */
                nextchar();
                char *dname = read_symbol();
                char *dtype = read_type();
                fdprintf(1, "  DECL %s %s\n", dname, dtype);
                expect(')');
            } else {
                /* Body statement - we've already consumed '(', now consume operator */
                char op = curchar;
                nextchar();

                fdprintf(1, "  BODY: ");
                switch (op) {
                case 'B':  /* Block */
                    handle_block();
                    break;
                case 'I':  /* If */
                    handle_if();
                    break;
                case 'W':  /* While */
                    handle_while();
                    break;
                case 'D':  /* Do-while */
                    handle_do();
                    break;
                case 'F':  /* For */
                    handle_for();
                    break;
                case 'R':  /* Return */
                    handle_return();
                    break;
                case 'E':  /* Expression statement */
                    handle_expr_stmt();
                    break;
                case ';':  /* Empty statement */
                    handle_empty_stmt();
                    break;
                default:
                    fdprintf(2, "parseast: line %d: unknown stmt op '%c'\n", line_num, op);
                    /* Skip to closing paren */
                    while (curchar && curchar != ')') {
                        nextchar();
                    }
                    if (curchar == ')') {
                        nextchar();
                    }
                    break;
                }
                fdprintf(1, "\n");
            }
        }
        skip();
    }

    expect(')');
}

static void
handle_global(void)
{
    char *name, *type;

    /* (g name type [init]) */
    name = read_symbol();
    type = read_type();

    fdprintf(1, "\nGLOBAL %s %s", name, type);

    skip();
    if (curchar != ')') {
        fdprintf(1, " = ");
        parse_expr();
    }

    fdprintf(1, "\n");
    expect(')');
}

/*
 * Parse an expression (recursive)
 */
static void
parse_expr(void)
{
    skip();

    if (curchar == '(') {
        nextchar();
        skip();

        char op = curchar;
        nextchar();

        switch (op) {
        case 'M':  /* DEREF */
            handle_deref();
            break;
        case '=':  /* ASSIGN */
            handle_assign();
            break;
        case '@':  /* CALL */
            handle_call();
            break;
        case '?':  /* TERNARY */
            handle_ternary();
            break;
        case '+': case '-': case '*': case '/': case '%':
        case '&': case '|': case '^':
        case '<': case '>':
            handle_binary_op(op);
            break;
        case '!': case '~':
            handle_unary_op(op);
            break;
        default:
            fdprintf(2, "parseast: line %d: unknown expr op '%c'\n", line_num, op);
            /* Skip to closing paren */
            while (curchar && curchar != ')') {
                nextchar();
            }
            if (curchar == ')') {
                nextchar();
            }
            break;
        }
    } else if (curchar == '$') {
        /* Symbol */
        handle_symbol();
    } else if (curchar == 'S') {
        /* String literal */
        nextchar();
        handle_string();
    } else if ((curchar >= '0' && curchar <= '9') || curchar == '-') {
        /* Constant */
        handle_const();
    } else {
        fdprintf(2, "parseast: line %d: unexpected char '%c' in expr\n",
                 line_num, curchar);
        nextchar();
    }
}

/*
 * Parse a statement (recursive)
 */
static void
parse_stmt(void)
{
    skip();

    if (curchar != '(') {
        fdprintf(2, "parseast: line %d: expected '(' at start of statement\n", line_num);
        return;
    }

    nextchar();
    skip();

    char op = curchar;
    nextchar();

    switch (op) {
    case 'B':  /* Block */
        handle_block();
        break;
    case 'I':  /* If */
        handle_if();
        break;
    case 'W':  /* While */
        handle_while();
        break;
    case 'D':  /* Do-while */
        handle_do();
        break;
    case 'F':  /* For */
        handle_for();
        break;
    case 'R':  /* Return */
        handle_return();
        break;
    case 'E':  /* Expression statement */
        handle_expr_stmt();
        break;
    case ';':  /* Empty statement */
        handle_empty_stmt();
        break;
    default:
        fdprintf(2, "parseast: line %d: unknown stmt op '%c'\n", line_num, op);
        /* Skip to closing paren */
        while (curchar && curchar != ')') {
            nextchar();
        }
        if (curchar == ')') {
            nextchar();
        }
        break;
    }
}

/*
 * Parse top-level constructs
 */
static void
parse_toplevel(void)
{
    skip();

    if (curchar != '(') {
        return;
    }

    nextchar();
    skip();

    char op = curchar;
    nextchar();

    switch (op) {
    case 'f':  /* Function */
        handle_function();
        break;
    case 'g':  /* Global variable */
        handle_global();
        break;
    default:
        fdprintf(2, "parseast: line %d: unknown top-level op '%c'\n", line_num, op);
        /* Skip to closing paren */
        while (curchar && curchar != ')') {
            nextchar();
        }
        if (curchar == ')') {
            nextchar();
        }
        break;
    }
}

/*
 * Initialize parser and read AST file
 */
int
parse_ast_file(int fd)
{
    in_fd = fd;
    buf_pos = 0;
    buf_valid = 0;
    line_num = 1;

    /* Prime the input */
    nextchar();

    /* Parse all top-level constructs */
    while (curchar) {
        skip();
        if (curchar) {
            parse_toplevel();
        }
    }

    return 0;
}

/*
 * vim: tabstop=4 shiftwidth=4 expandtab:
 */
