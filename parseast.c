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

#define BUFSIZE 40960  // AST parser read buffer (original: 4096, tested: 40960)

/* Parser state */
static int in_fd;
static int out_fd = 1;  /* Assembly output (default: stdout) */
static char buf[BUFSIZE];
static int buf_pos;
static int buf_valid;
static int line_num = 1;
static unsigned char curchar;

/*
 * Read next character from input
 * Returns 0 on EOF
 */
static unsigned char
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
expect(unsigned char c)
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
    unsigned char i = 0;

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
    unsigned char i = 0;

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

/* Forward declarations for expression handlers */
static void parse_expr(void);
static void parse_stmt(void);
static void handle_deref(void);
static void handle_assign(void);
static void handle_call(void);
static void handle_ternary(void);

/* Handler function type */
typedef void (*handler_fn)(unsigned char op);

/* Generic handler that skips the operator expression */
static void
handle_generic(unsigned char op)
{
    fdprintf(2, "OP_%02x", op);
    skip();

    /* Skip to closing paren - recursively handle any nested expressions */
    int depth = 1;
    while (curchar && depth > 0) {
        if (curchar == '(') {
            depth++;
        } else if (curchar == ')') {
            depth--;
            if (depth == 0) {
                nextchar();  /* consume closing paren */
                return;
            }
        }
        nextchar();
    }
}

/* Expression handlers */

static void
handle_const(void)
{
    long val = read_number();
    fdprintf(2, "CONST %ld", val);
}

static void
handle_symbol(void)
{
    char *sym = read_symbol();
    fdprintf(2, "SYM %s", sym);
}

static void
handle_string(void)
{
    /* String literal: S followed by index */
    long idx = read_number();
    fdprintf(2, "STRING S%ld", idx);
}

static void
handle_binary_op(unsigned char op)
{
    fdprintf(2, "BINOP %c (", op);
    skip();
    parse_expr();  /* left operand */
    fdprintf(2, ", ");
    skip();
    parse_expr();  /* right operand */
    fdprintf(2, ")");
    expect(')');
}

static void
handle_unary_op(unsigned char op)
{
    char width = ' ';

    /* Check for width annotation :b :s :l :p (for type conversion ops) */
    skip();
    if (curchar == ':') {
        nextchar();
        width = curchar;
        nextchar();
        fdprintf(2, "UNOP %c:%c (", op, width);
    } else {
        fdprintf(2, "UNOP %c (", op);
    }

    skip();
    parse_expr();  /* operand */
    fdprintf(2, ")");
    expect(')');
}

/* Wrappers to match handler_fn signature */
static void handle_deref_dispatch(unsigned char op) { handle_deref(); }
static void handle_assign_dispatch(unsigned char op) { handle_assign(); }
static void handle_call_dispatch(unsigned char op) { handle_call(); }
static void handle_ternary_dispatch(unsigned char op) { handle_ternary(); }

/* COLON is only used as part of ternary, but handle it gracefully if standalone */
static void
handle_colon(unsigned char op)
{
    fdprintf(2, "COLON (");
    skip();
    parse_expr();  /* left */
    fdprintf(2, ", ");
    skip();
    parse_expr();  /* right */
    fdprintf(2, ")");
    expect(')');
}

static void handle_colon_dispatch(unsigned char op) { handle_colon(op); }

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

    fdprintf(2, "DEREF:%c (", width);
    skip();
    parse_expr();  /* address expression */
    fdprintf(2, ")");
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

    fdprintf(2, "ASSIGN:%c (", width);
    skip();
    parse_expr();  /* lvalue */
    fdprintf(2, ", ");
    skip();
    parse_expr();  /* rvalue */
    fdprintf(2, ")");
    expect(')');
}

/*
 * Parser state for nested parsing
 */
struct parser_state {
    char saved_buf[BUFSIZE];
    int saved_buf_pos;
    int saved_buf_valid;
    int saved_line_num;
    unsigned char saved_curchar;
    int saved_in_fd;
};

/*
 * Save current parser state
 */
static void
save_parser_state(struct parser_state *state)
{
    memcpy(state->saved_buf, buf, buf_valid);
    state->saved_buf_pos = buf_pos;
    state->saved_buf_valid = buf_valid;
    state->saved_line_num = line_num;
    state->saved_curchar = curchar;
    state->saved_in_fd = in_fd;
}

/*
 * Restore parser state
 */
static void
restore_parser_state(struct parser_state *state)
{
    memcpy(buf, state->saved_buf, state->saved_buf_valid);
    buf_pos = state->saved_buf_pos;
    buf_valid = state->saved_buf_valid;
    line_num = state->saved_line_num;
    curchar = state->saved_curchar;
    in_fd = state->saved_in_fd;
}

/*
 * Set up parser to read from a string buffer
 */
static void
setup_string_input(char *str, int len)
{
    /* Copy string to main buffer */
    if (len > BUFSIZE - 1) {
        len = BUFSIZE - 1;
    }
    memcpy(buf, str, len);
    buf[len] = 0;
    buf_pos = 0;
    buf_valid = len;
    in_fd = -1;  /* Mark as string input */

    /* Initialize curchar */
    if (len > 0) {
        curchar = buf[0];
        buf_pos = 1;
    } else {
        curchar = 0;
    }
}

static void
handle_call(void)
{
    char arg_buf[4096];  /* Buffer to capture argument expressions */
    int arg_start[32];  /* Start position of each argument */
    int arg_len[32];    /* Length of each argument */
    int arg_count = 0;
    int arg_buf_pos = 0;
    int i;
    char func_buf[512];
    int func_len = 0;

    fdprintf(2, "CALL (");

    skip();

    /* Collect function expression into func_buf */
    int depth = 0;

    while (1) {
        if (curchar == 0) break;

        if (curchar == '(') {
            depth++;
        } else if (curchar == ')') {
            if (depth == 0) {
                /* End of function expr, no args */
                break;
            }
            depth--;
        } else if (depth == 0 && (curchar == ' ' || curchar == '\t' || curchar == '\n')) {
            /* Whitespace at depth 0 = separator between function and first arg */
            skip();
            break;
        }

        if (func_len < sizeof(func_buf) - 1) {
            func_buf[func_len++] = curchar;
        }
        nextchar();
    }
    func_buf[func_len] = 0;

    /* Collect arguments into arg_buf */
    while (curchar != ')' && curchar != 0) {
        skip();
        if (curchar == ')') break;

        arg_start[arg_count] = arg_buf_pos;
        depth = 0;

        /* Copy one argument expression */
        while (1) {
            if (curchar == 0) break;

            if (curchar == '(') {
                depth++;
            } else if (curchar == ')') {
                if (depth == 0) {
                    /* End of arguments */
                    break;
                }
                depth--;
            } else if (depth == 0 && (curchar == ' ' || curchar == '\t' || curchar == '\n')) {
                /* Whitespace at depth 0 = end of this arg */
                skip();
                break;
            }

            if (arg_buf_pos < sizeof(arg_buf) - 1) {
                arg_buf[arg_buf_pos++] = curchar;
            }
            nextchar();
        }

        arg_len[arg_count] = arg_buf_pos - arg_start[arg_count];
        arg_count++;

        if (arg_count >= 32) break;  /* Max 32 arguments */
    }

    /* Now recursively parse: arguments in reverse order, then function */
    struct parser_state saved_state;
    save_parser_state(&saved_state);

    /* Arguments in reverse (so first arg ends up on top of stack) */
    for (i = arg_count - 1; i >= 0; i--) {
        fdprintf(2, "\n  PUSH_ARG%d: ", i);

        /* Set up parser to read from this argument string */
        setup_string_input(&arg_buf[arg_start[i]], arg_len[i]);

        /* Recursively parse this argument expression */
        parse_expr();

        /* Restore parser state for next argument */
        restore_parser_state(&saved_state);
    }

    /* Function address last */
    fdprintf(2, "\n  CALL_FUNC: ");

    /* Set up parser to read from function string */
    setup_string_input(func_buf, func_len);

    /* Recursively parse function expression */
    parse_expr();

    /* Restore parser state */
    restore_parser_state(&saved_state);

    fdprintf(2, ")");
    expect(')');
}

static void
handle_ternary(void)
{
    fdprintf(2, "TERNARY (");
    skip();
    parse_expr();  /* condition */
    fdprintf(2, " ? ");
    skip();

    /* Expect COLON node */
    if (curchar == '(') {
        nextchar();
        skip();
        if (curchar == ':') {
            nextchar();
            skip();
            parse_expr();  /* true expr */
            fdprintf(2, " : ");
            skip();
            parse_expr();  /* false expr */
            expect(')');
        }
    }

    fdprintf(2, ")");
    expect(')');
}

/* Statement handlers */

/* Forward declarations */
static void handle_block(void);
static void handle_if(void);
static void handle_while(void);
static void handle_do(void);
static void handle_for(void);
static void handle_return(void);
static void handle_expr_stmt(void);
static void handle_empty_stmt(void);
static void handle_asm(void);
static void handle_label(void);
static void handle_goto(void);
static void handle_switch(void);

static void
handle_block(void)
{
    fdprintf(2, "BLOCK {\n");

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
                fdprintf(2, "  DECL %s %s\n", name, type);
                expect(')');
            } else {
                /* Statement - dispatch based on operator */
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
                case 'A':  /* Asm block */
                    handle_asm();
                    break;
                case 'L':  /* Label */
                    handle_label();
                    break;
                case 'G':  /* Goto */
                    handle_goto();
                    break;
                case 'S':  /* Switch */
                    handle_switch();
                    break;
                default:
                    fdprintf(2, "parseast: line %d: unknown stmt op '%c' in block\n", line_num, op);
                    /* Skip to closing paren */
                    while (curchar && curchar != ')') {
                        nextchar();
                    }
                    if (curchar == ')') {
                        nextchar();
                    }
                    break;
                }
                fdprintf(2, "\n");
            }
        }
        skip();
    }

    fdprintf(2, "}");
    expect(')');
}

static void
handle_if(void)
{
    fdprintf(2, "IF (");
    skip();
    parse_expr();  /* condition */
    fdprintf(2, ") ");

    skip();
    parse_stmt();  /* then branch */

    skip();
    if (curchar != ')') {
        fdprintf(2, " ELSE ");
        parse_stmt();  /* else branch */
    }

    expect(')');
}

static void
handle_while(void)
{
    fdprintf(2, "WHILE (");
    skip();
    parse_expr();  /* condition */
    fdprintf(2, ") ");

    skip();
    parse_stmt();  /* body */

    expect(')');
}

static void
handle_do(void)
{
    fdprintf(2, "DO ");
    skip();
    parse_stmt();  /* body */

    fdprintf(2, " WHILE (");
    skip();
    parse_expr();  /* condition */
    fdprintf(2, ")");

    expect(')');
}

static void
handle_for(void)
{
    fdprintf(2, "FOR (");
    skip();
    parse_expr();  /* init */
    fdprintf(2, "; ");
    skip();
    parse_expr();  /* condition */
    fdprintf(2, "; ");
    skip();
    parse_expr();  /* increment */
    fdprintf(2, ") ");

    skip();
    parse_stmt();  /* body */

    expect(')');
}

static void
handle_return(void)
{
    fdprintf(2, "RETURN");
    skip();
    if (curchar != ')') {
        fdprintf(2, " ");
        parse_expr();
    }
    expect(')');
}

static void
handle_expr_stmt(void)
{
    fdprintf(2, "EXPR (");
    skip();
    parse_expr();
    fdprintf(2, ")");
    expect(')');
}

static void
handle_empty_stmt(void)
{
    fdprintf(2, ";");
    expect(')');
}

/*
 * Check if a line is a label (ends with ':')
 */
static int
is_label(char *line)
{
    int len;

    /* Trim trailing whitespace to find actual end */
    len = strlen(line);
    while (len > 0 && (line[len-1] == ' ' || line[len-1] == '\t')) {
        len--;
    }

    return len > 0 && line[len-1] == ':';
}

/*
 * Trim leading and trailing whitespace from a line
 * Also collapse multiple consecutive spaces into single spaces
 */
static char *
trim_line(char *line)
{
    char *end;
    char *src, *dst;
    int last_was_space;

    /* Trim leading space */
    while (*line == ' ' || *line == '\t') {
        line++;
    }

    /* Collapse multiple spaces into single spaces */
    src = dst = line;
    last_was_space = 0;
    while (*src) {
        if (*src == ' ' || *src == '\t') {
            if (!last_was_space) {
                *dst++ = ' ';
                last_was_space = 1;
            }
            src++;
        } else {
            *dst++ = *src++;
            last_was_space = 0;
        }
    }
    *dst = '\0';

    /* Trim trailing space */
    end = line + strlen(line) - 1;
    while (end >= line && (*end == ' ' || *end == '\t')) {
        *end = '\0';
        end--;
    }

    return line;
}

static void
handle_asm(void)
{
    char asm_buf[4096];
    char *p;
    char *line_start;
    char *trimmed;
    int i;

    /* ASM statement: (A "assembly text") */
    skip();
    if (curchar == '"') {
        /* Read the quoted string into buffer */
        nextchar();
        p = asm_buf;
        while (curchar && curchar != '"' && (p - asm_buf) < sizeof(asm_buf) - 1) {
            *p++ = curchar;
            nextchar();
        }
        *p = '\0';

        if (curchar == '"') {
            nextchar();
        }

        /* Diagnostic output to stderr */
        fdprintf(2, "ASM \"%s\"\n", asm_buf);

        /* Process and emit assembly to stdout */
        /* Split on semicolons and output each line */
        line_start = asm_buf;
        for (i = 0; asm_buf[i]; i++) {
            if (asm_buf[i] == ';') {
                asm_buf[i] = '\0';  /* Terminate this segment */
                trimmed = trim_line(line_start);
                if (*trimmed) {  /* Skip empty lines */
                    if (is_label(trimmed)) {
                        fdprintf(1, "%s\n", trimmed);
                    } else {
                        fdprintf(1, "\t%s\n", trimmed);
                    }
                }
                line_start = &asm_buf[i + 1];
            }
        }
        /* Handle last segment (no trailing semicolon) */
        trimmed = trim_line(line_start);
        if (*trimmed) {
            if (is_label(trimmed)) {
                fdprintf(1, "%s\n", trimmed);
            } else {
                fdprintf(1, "\t%s\n", trimmed);
            }
        }
    }
    expect(')');
}

static void
handle_label(void)
{
    char *label_name;

    /* Label statement: (L label_name) */
    skip();
    label_name = read_symbol();

    /* Diagnostic output to stderr */
    fdprintf(2, "LABEL %s", label_name);

    /* Emit label to stdout with trailing colon */
    fdprintf(1, "%s:\n", label_name);

    expect(')');
}

static void
handle_goto(void)
{
    char *label_name;

    /* Goto statement: (G label_name) */
    skip();
    label_name = read_symbol();

    /* Diagnostic output to stderr */
    fdprintf(2, "GOTO %s", label_name);

    /* TODO: Emit goto instruction when we have code generation */

    expect(')');
}

static void
handle_switch(void)
{
    /* Switch statement: (S expr (C val ...) (C val ...) (O ...) ) */
    fdprintf(2, "SWITCH (");
    skip();
    parse_expr();  /* switch expression */
    fdprintf(2, ") {");

    /* Parse case and default clauses */
    skip();
    while (curchar != ')') {
        skip();
        if (curchar == '(') {
            nextchar();
            skip();
            char clause_type = curchar;
            nextchar();

            if (clause_type == 'C') {
                /* Case clause: (C value ()) - body placeholder is always empty */
                fdprintf(2, "\n  CASE ");
                skip();
                parse_expr();  /* case value */
                fdprintf(2, ": ");
                skip();
                /* Skip empty body placeholder () */
                if (curchar == '(') {
                    nextchar();
                    skip();
                    if (curchar == ')') {
                        nextchar();
                    }
                }
                expect(')');
            } else if (clause_type == 'O') {
                /* Default clause: (O ()) - body placeholder is always empty */
                fdprintf(2, "\n  DEFAULT: ");
                skip();
                /* Skip empty body placeholder () */
                if (curchar == '(') {
                    nextchar();
                    skip();
                    if (curchar == ')') {
                        nextchar();
                    }
                }
                expect(')');
            } else if (clause_type == 'R' || clause_type == 'G' ||
                       clause_type == 'B' || clause_type == 'E') {
                /* Statements inside switch body (between cases) */
                /* Back up - we need to reparse this as a statement */
                /* Unfortunately we already consumed the '(' and type char */
                /* For now, just handle common ones inline */
                fdprintf(2, "\n");
                if (clause_type == 'R') {
                    handle_return();
                } else if (clause_type == 'G') {
                    skip();
                    char *label = read_symbol();
                    fdprintf(2, "GOTO %s", label);
                    expect(')');
                } else if (clause_type == 'B') {
                    /* BREAK statement */
                    fdprintf(2, "BREAK");
                    expect(')');
                } else if (clause_type == 'E') {
                    /* Expression statement */
                    handle_expr_stmt();
                }
            } else {
                /* Unknown - skip it */
                fdprintf(2, "\n  UNKNOWN_%c ", clause_type);
                while (curchar && curchar != ')') {
                    nextchar();
                }
                if (curchar == ')') {
                    nextchar();
                }
            }
        }
        skip();
    }

    fdprintf(2, "\n}");
    expect(')');
}

/* Top-level handlers */

/*
 * Emit function prologue
 * Outputs assembly comment describing function and function label
 */
static void
emit_function_prologue(char *name, char *params, char *rettype)
{
    /* Assembly comment with function signature */
    fdprintf(out_fd, "; Function: %s", name);

    if (params && params[0]) {
        fdprintf(out_fd, "(%s)", params);
    } else {
        fdprintf(out_fd, "()");
    }

    if (rettype && rettype[0]) {
        fdprintf(out_fd, " -> %s", rettype);
    }

    fdprintf(out_fd, "\n");

    /* Function label (standard C naming with underscore prefix) */
    fdprintf(out_fd, "_%s:\n", name);
}

static void
handle_function(void)
{
    char name_buf[64];  /* Stack buffer for function name */
    char *name;
    char params_buf[256];
    char *p;
    int first_param;

    /* (f name (params) return_type declarations body) */
    /* Copy function name to stack buffer before reading parameters */
    strncpy(name_buf, read_symbol(), sizeof(name_buf) - 1);
    name_buf[sizeof(name_buf) - 1] = '\0';
    name = name_buf;
    fdprintf(2, "\nFUNCTION %s\n", name);

    /* Parameters - collect into buffer for prologue */
    skip();
    expect('(');
    fdprintf(2, "  PARAMS: ");
    p = params_buf;
    params_buf[0] = '\0';
    first_param = 1;
    skip();
    while (curchar != ')') {
        char *param = read_symbol();
        char *ptype = NULL;

        fdprintf(2, "%s ", param);

        /* Add to params buffer */
        if (!first_param) {
            if (p < params_buf + sizeof(params_buf) - 2) {
                *p++ = ',';
                *p++ = ' ';
            }
        }
        first_param = 0;

        /* Copy parameter name */
        while (*param && p < params_buf + sizeof(params_buf) - 20) {
            *p++ = *param++;
        }

        skip();
        /* Read type annotation if present (format: name:type) */
        if (curchar == ':') {
            nextchar();
            ptype = read_type();  /* Get type */
            /* Add type to params buffer */
            if (ptype && p < params_buf + sizeof(params_buf) - 20) {
                *p++ = ':';
                while (*ptype && p < params_buf + sizeof(params_buf) - 1) {
                    *p++ = *ptype++;
                }
            }
            skip();
        }
    }
    *p = '\0';
    expect(')');
    fdprintf(2, "\n");

    /* Return type */
    char *rettype = read_type();
    fdprintf(2, "  RETURNS: %s\n", rettype);

    /* Emit assembly function prologue */
    emit_function_prologue(name, params_buf, rettype);

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
                fdprintf(2, "  DECL %s %s\n", dname, dtype);
                expect(')');
            } else {
                /* Body statement - we've already consumed '(', now consume operator */
                char op = curchar;
                nextchar();

                fdprintf(2, "  BODY: ");
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
                case 'A':  /* Asm block */
                    handle_asm();
                    break;
                case 'L':  /* Label */
                    handle_label();
                    break;
                case 'G':  /* Goto */
                    handle_goto();
                    break;
                case 'S':  /* Switch */
                    handle_switch();
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
                fdprintf(2, "\n");
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

    fdprintf(2, "\nGLOBAL %s %s", name, type);

    skip();
    if (curchar != ')') {
        fdprintf(2, " = ");
        parse_expr();
    }

    fdprintf(2, "\n");
    expect(')');
}

/*
 * Operator lookup table
 * Maps each operator character to its handler function
 * Table is sparse - only operators that need handling are listed
 */
static handler_fn expr_handlers[256];

static void
init_expr_handlers(void)
{
    int i;

    /* Initialize all to generic handler */
    for (i = 0; i < 256; i++) {
        expr_handlers[i] = handle_generic;
    }

    /* Register specific handlers */
    expr_handlers['M'] = handle_deref_dispatch;     /* DEREF */
    expr_handlers['='] = handle_assign_dispatch;    /* ASSIGN */
    expr_handlers['@'] = handle_call_dispatch;      /* CALL */
    expr_handlers['?'] = handle_ternary_dispatch;   /* TERNARY */
    expr_handlers[':'] = handle_colon_dispatch;     /* COLON */

    /* Binary operators */
    expr_handlers['+'] = handle_binary_op;
    expr_handlers['-'] = handle_binary_op;
    expr_handlers['*'] = handle_binary_op;
    expr_handlers['/'] = handle_binary_op;
    expr_handlers['%'] = handle_binary_op;
    expr_handlers['&'] = handle_binary_op;
    expr_handlers['|'] = handle_binary_op;
    expr_handlers['^'] = handle_binary_op;
    expr_handlers['<'] = handle_binary_op;
    expr_handlers['>'] = handle_binary_op;
    expr_handlers['Q'] = handle_binary_op;  /* EQ == */
    expr_handlers['n'] = handle_binary_op;  /* NEQ != */
    expr_handlers['L'] = handle_binary_op;  /* LE <= */
    expr_handlers['g'] = handle_binary_op;  /* GE >= */
    expr_handlers['y'] = handle_binary_op;  /* LSHIFT << */
    expr_handlers['w'] = handle_binary_op;  /* RSHIFT >> */
    expr_handlers['h'] = handle_binary_op;  /* LOR || */
    expr_handlers['j'] = handle_binary_op;  /* LAND && */

    /* Compound assignment operators */
    expr_handlers['P'] = handle_binary_op;  /* PLUSEQ += */
    expr_handlers[0xdf] = handle_binary_op; /* SUBEQ -= */
    expr_handlers['T'] = handle_binary_op;  /* MULTEQ *= */
    expr_handlers['2'] = handle_binary_op;  /* DIVEQ /= */
    expr_handlers[0xfe] = handle_binary_op; /* MODEQ %= */
    expr_handlers[0xc6] = handle_binary_op; /* ANDEQ &= */
    expr_handlers['1'] = handle_binary_op;  /* OREQ |= */
    expr_handlers['0'] = handle_binary_op;  /* LSHIFTEQ <<= */
    expr_handlers['6'] = handle_binary_op;  /* RSHIFTEQ >>= */

    /* Unary operators */
    expr_handlers['!'] = handle_unary_op;
    expr_handlers['~'] = handle_unary_op;
    expr_handlers['\\'] = handle_unary_op; /* NEG */
    expr_handlers['\''] = handle_unary_op; /* NOT */

    /* Type conversion operators */
    expr_handlers['N'] = handle_unary_op;  /* NARROW */
    expr_handlers['W'] = handle_unary_op;  /* WIDEN */
    expr_handlers['X'] = handle_unary_op;  /* SEXT (also XOREQ, but context differs) */

    /* Increment/decrement */
    expr_handlers[0xcf] = handle_unary_op; /* PREINC */
    expr_handlers[0xef] = handle_unary_op; /* POSTINC */
    expr_handlers[0xd6] = handle_unary_op; /* PREDEC */
    expr_handlers[0xf6] = handle_unary_op; /* POSTDEC */

    /* COPY operator */
    expr_handlers[0xbb] = handle_binary_op; /* COPY */
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

        unsigned char op = curchar;
        nextchar();

        /* Use lookup table to dispatch to handler */
        expr_handlers[op](op);

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

    /* Handle empty statement: () */
    if (curchar == ')') {
        nextchar();
        return;
    }

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
    case 'A':  /* Asm block */
        handle_asm();
        break;
    case 'L':  /* Label */
        handle_label();
        break;
    case 'G':  /* Goto */
        handle_goto();
        break;
    case 'S':  /* Switch */
        handle_switch();
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
parse_ast_file(int in, int out)
{
    in_fd = in;
    out_fd = out;
    buf_pos = 0;
    buf_valid = 0;
    line_num = 1;

    /* Initialize expression handler lookup table */
    init_expr_handlers();

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
