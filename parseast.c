/*
 * parseast.c - Table-driven parser for AST S-expressions
 *
 * Reads AST output from cc1 and builds parse trees for code generation.
 * Modified to return tree nodes instead of printing directly.
 */
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <fcntl.h>
#include <unistd.h>

#include "cc2.h"

#define BUFSIZE 40960  // AST parser read buffer (original: 4096, tested: 40960)

/* Forward declarations for static helper functions */
static unsigned char nextchar(void);
static void skip(void);
static void skipwhite(void);
static void skipcomment(void);
static long read_number(void);
static int expect(unsigned char c);
static struct expr *parse_expr(void);

/* Parser state */
static int in_fd;
static int out_fd = 1;  /* Assembly output (default: stdout) */
static char buf[BUFSIZE];
static int buf_pos;
static int buf_valid;
static int line_num = 1;
static unsigned char curchar;
static int label_counter = 0;  /* For generating unique labels */

/*
 * Tree node allocation helpers
 */
struct expr *
new_expr(unsigned char op)
{
    struct expr *e = malloc(sizeof(struct expr));
    if (!e) {
        fdprintf(2, "parseast: out of memory allocating expr\n");
        exit(1);
    }
    e->op = op;
    e->left = NULL;
    e->right = NULL;
    e->type_str = NULL;
    e->value = 0;
    e->symbol = NULL;
    e->size = 2;  /* Default to short size */
    e->flags = 0; /* Default to signed */
    e->asm_block = NULL;
    e->label = 0;
    return e;
}

struct stmt *
new_stmt(unsigned char type)
{
    struct stmt *s = malloc(sizeof(struct stmt));
    if (!s) {
        fdprintf(2, "parseast: out of memory allocating stmt\n");
        exit(1);
    }
    s->type = type;
    s->expr = NULL;
    s->expr2 = NULL;
    s->expr3 = NULL;
    s->then_branch = NULL;
    s->else_branch = NULL;
    s->next = NULL;
    s->symbol = NULL;
    s->type_str = NULL;
    s->label = 0;
    s->label2 = 0;
    s->asm_block = NULL;
    return s;
}

/*
 * Extract size in bytes from type annotation string
 * Type strings: ":b" (byte/1), ":s" (short/2), ":l" (long/4), ":p" (pointer/2),
 *               ":f" (float/4), ":d" (double/8)
 * Returns: size in bytes, or 2 (default short size) if no annotation
 */
unsigned char
get_size_from_type_str(const char *type_str)
{
    if (!type_str || *type_str != ':') {
        return 2;  /* Default to short size */
    }

    switch (type_str[1]) {
    case 'b':  /* byte/char */
        return 1;
    case 's':  /* short/int */
    case 'p':  /* pointer */
        return 2;
    case 'l':  /* long */
    case 'f':  /* float */
        return 4;
    case 'd':  /* double */
        return 8;
    default:
        return 2;  /* Default to short size */
    }
}

/*
 * Extract signedness from type annotation string
 * Pointers (:p) are unsigned (addresses)
 * Other types default to signed (signedness comes from operators like WIDEN/SEXT)
 * Returns: E_UNSIGNED flag if unsigned, 0 if signed
 */
unsigned char
get_signedness_from_type_str(const char *type_str)
{
    if (!type_str || *type_str != ':') {
        return 0;  /* Default to signed */
    }

    /* Pointers are unsigned (addresses) */
    if (type_str[1] == 'p') {
        return E_UNSIGNED;
    }

    /* All other types default to signed */
    /* Actual signedness comes from WIDEN (unsigned) vs SEXT (signed) operators */
    return 0;
}

/*
 * Extract size from full type name (used for declarations)
 * Type names: "_char_", "_uchar_", "_short_", "_ushort_", "_long_", "_ulong_",
 *             "_void_", "_boolean_", "_float_", "_double_", "_ptr_"
 * Returns: size in bytes
 */
unsigned char
get_size_from_typename(const char *typename)
{
    if (!typename) return 2;  /* Default to short */

    /* Check for common type names */
    if (strstr(typename, "char")) return 1;
    if (strstr(typename, "short")) return 2;
    if (strstr(typename, "long")) return 4;
    if (strstr(typename, "ptr")) return 2;
    if (strstr(typename, "float")) return 4;
    if (strstr(typename, "double")) return 8;
    if (strstr(typename, "void")) return 0;

    /* Default to short size */
    return 2;
}

/*
 * Create an ASM statement node for a label
 * Label format: "label_name:\n"
 */
static struct stmt *
create_label_asm(const char *label_name)
{
    struct stmt *s = new_stmt('A');
    char buf[128];

    snprintf(buf, sizeof(buf), "%s:", label_name);
    s->asm_block = malloc(strlen(buf) + 1);
    strcpy(s->asm_block, buf);

    return s;
}

/*
 * Create an ASM statement node for an unconditional jump
 * Jump format: "\tjp label_name\n"
 */
static struct stmt *
create_jump_asm(const char *label_name)
{
    struct stmt *s = new_stmt('A');
    char buf[128];

    snprintf(buf, sizeof(buf), "\tjp %s", label_name);
    s->asm_block = malloc(strlen(buf) + 1);
    strcpy(s->asm_block, buf);

    return s;
}

void
free_expr(struct expr *e)
{
    if (!e) return;
    free_expr(e->left);
    free_expr(e->right);
    /* NOTE: type_str and symbol point to static buffers from read_type()/read_symbol()
     * They should NOT be freed. Only asm_block is dynamically allocated. */
    if (e->asm_block) free(e->asm_block);
    free(e);
}

void
free_stmt(struct stmt *s)
{
    if (!s) return;
    free_expr(s->expr);
    free_expr(s->expr2);
    free_expr(s->expr3);
    free_stmt(s->then_branch);
    free_stmt(s->else_branch);
    free_stmt(s->next);
    /* Free dynamically allocated fields */
    if (s->symbol) free(s->symbol);
    if (s->type_str) free(s->type_str);
    if (s->asm_block) free(s->asm_block);
    free(s);
}

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
static char strbuf[1024];

/*
 * Read a quoted string literal with escape sequences
 * Expects curchar to be on the opening quote
 * Returns pointer to static buffer with unescaped string data
 */
static char *
read_quoted_string(void)
{
    unsigned char i = 0;

    skip();

    /* Expect opening quote */
    if (curchar != '"') {
        fdprintf(2, "parseast: line %d: expected '\"' at start of string\n", line_num);
        strbuf[0] = '\0';
        return strbuf;
    }

    nextchar();  /* Skip opening quote */

    /* Read until closing quote */
    while (curchar && curchar != '"') {
        if (curchar == '\\') {
            /* Handle escape sequences */
            nextchar();
            switch (curchar) {
            case 'n':
                if (i < sizeof(strbuf) - 1) strbuf[i++] = '\n';
                break;
            case 't':
                if (i < sizeof(strbuf) - 1) strbuf[i++] = '\t';
                break;
            case 'r':
                if (i < sizeof(strbuf) - 1) strbuf[i++] = '\r';
                break;
            case '\\':
                if (i < sizeof(strbuf) - 1) strbuf[i++] = '\\';
                break;
            case '"':
                if (i < sizeof(strbuf) - 1) strbuf[i++] = '"';
                break;
            case 'x':
                /* Hex escape: \xNN */
                nextchar();
                {
                    unsigned char hex1 = curchar;
                    nextchar();
                    unsigned char hex2 = curchar;
                    unsigned char val = 0;

                    if (hex1 >= '0' && hex1 <= '9') val = (hex1 - '0') << 4;
                    else if (hex1 >= 'a' && hex1 <= 'f') val = (hex1 - 'a' + 10) << 4;
                    else if (hex1 >= 'A' && hex1 <= 'F') val = (hex1 - 'A' + 10) << 4;

                    if (hex2 >= '0' && hex2 <= '9') val |= (hex2 - '0');
                    else if (hex2 >= 'a' && hex2 <= 'f') val |= (hex2 - 'a' + 10);
                    else if (hex2 >= 'A' && hex2 <= 'F') val |= (hex2 - 'A' + 10);

                    if (i < sizeof(strbuf) - 1) strbuf[i++] = val;
                }
                break;
            default:
                /* Unknown escape, just copy the character */
                if (i < sizeof(strbuf) - 1) strbuf[i++] = curchar;
                break;
            }
            nextchar();
        } else {
            /* Regular character */
            if (i < sizeof(strbuf) - 1) {
                strbuf[i++] = curchar;
            }
            nextchar();
        }
    }

    strbuf[i] = '\0';

    /* Expect closing quote */
    if (curchar == '"') {
        nextchar();
    }

    return strbuf;
}

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
               curchar == '_' || curchar == ':' || curchar == '-') {
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
static struct expr *parse_expr(void);
static struct stmt *parse_stmt(void);

/*
 * Handler functions for each operation
 * Each handler consumes the entire s-expression including closing paren
 */

/* Forward declarations for expression handlers */
static struct expr *handle_deref(void);
static struct expr *handle_assign(void);
static struct expr *handle_call(void);
static struct expr *handle_ternary(void);
static struct expr *handle_const(void);
static struct expr *handle_symbol(void);
static struct expr *handle_binary_op(unsigned char op);
static struct expr *handle_unary_op(unsigned char op);

/* Handler function type - now returns expr* */
typedef struct expr* (*handler_fn)(unsigned char op);

/* Generic handler that builds a generic expr node */
static struct expr *
handle_generic(unsigned char op)
{
    struct expr *e = new_expr(op);

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
                return e;
            }
        }
        nextchar();
    }
    return e;
}

/* Expression handlers */

static struct expr *
handle_const(void)
{
    struct expr *e = new_expr('C');  // 'C' for constant
    e->value = read_number();
    fdprintf(2, "CONST %ld", e->value);
    return e;
}

static struct expr *
handle_symbol(void)
{
    struct expr *e = new_expr('$');  // '$' for symbol
    char *sym = read_symbol();
    e->symbol = strdup(sym);
    fdprintf(2, "SYM %s", e->symbol);
    return e;
}

static struct expr *
handle_string(void)
{
    struct expr *e = new_expr('S');  // 'S' for string
    /* String literal: S followed by index */
    e->value = read_number();
    fdprintf(2, "STRING S%ld", e->value);
    return e;
}

static struct expr *
handle_compound_assign(unsigned char op)
{
    struct expr *e = new_expr(op);
    char width = 's';  /* default */

    /* Check for width annotation */
    skip();
    if (curchar == ':') {
        nextchar();
        width = curchar;
        nextchar();
        /* Store width annotation */
        char width_str[3] = {':', width, '\0'};
        e->type_str = strdup(width_str);
        e->size = get_size_from_type_str(e->type_str);
        e->flags = get_signedness_from_type_str(e->type_str);
    }

    fdprintf(2, "COMP_ASSIGN_%02x:%c (", op, width);
    skip();
    e->left = parse_expr();  /* lvalue */
    fdprintf(2, ", ");
    skip();
    e->right = parse_expr();  /* rvalue */
    fdprintf(2, ")");
    expect(')');

    return e;
}

static struct expr *
handle_binary_op(unsigned char op)
{
    struct expr *e = new_expr(op);

    fdprintf(2, "BINOP %c (", op);
    skip();
    e->left = parse_expr();  /* left operand - now returns tree */
    fdprintf(2, ", ");
    skip();
    e->right = parse_expr();  /* right operand - now returns tree */
    fdprintf(2, ")");
    expect(')');

    /* Result size is the larger of the two operand sizes */
    if (e->left && e->right) {
        e->size = (e->left->size > e->right->size) ? e->left->size : e->right->size;
    } else if (e->left) {
        e->size = e->left->size;
    } else if (e->right) {
        e->size = e->right->size;
    }

    /* Result signedness: if either operand is unsigned, result is unsigned */
    if (e->left && e->right) {
        e->flags = (e->left->flags | e->right->flags) & E_UNSIGNED;
    } else if (e->left) {
        e->flags = e->left->flags;
    } else if (e->right) {
        e->flags = e->right->flags;
    }

    return e;
}

/*
 * Short-circuit evaluation for && (LAND)
 * If left operand is false, skip right operand evaluation
 */
static struct expr *
handle_land(unsigned char op)
{
    struct expr *e = new_expr(op);
    e->label = label_counter++;

    fdprintf(2, "LAND_%d (", e->label);
    skip();
    e->left = parse_expr();  /* left operand */
    fdprintf(2, " ? ");

    /* Will emit conditional jump during code generation */
    fdprintf(2, "JZ skip_%d : ", e->label);

    skip();
    e->right = parse_expr();  /* right operand */

    /* Will emit skip label during code generation */
    fdprintf(2, " : skip_%d)", e->label);
    expect(')');
    return e;
}

/*
 * Short-circuit evaluation for || (LOR)
 * If left operand is true, skip right operand evaluation
 */
static struct expr *
handle_lor(unsigned char op)
{
    struct expr *e = new_expr(op);
    e->label = label_counter++;

    fdprintf(2, "LOR_%d (", e->label);
    skip();
    e->left = parse_expr();  /* left operand */
    fdprintf(2, " ? ");

    /* Will emit conditional jump during code generation */
    fdprintf(2, "JNZ skip_%d : ", e->label);

    skip();
    e->right = parse_expr();  /* right operand */

    /* Will emit skip label during code generation */
    fdprintf(2, " : skip_%d)", e->label);
    expect(')');
    return e;
}

static struct expr *
handle_unary_op(unsigned char op)
{
    struct expr *e = new_expr(op);
    char width = ' ';

    /* Check for width annotation :b :s :l :p (for type conversion ops) */
    skip();
    if (curchar == ':') {
        nextchar();
        width = curchar;
        nextchar();
        /* Store width annotation */
        char width_str[3] = {':', width, '\0'};
        e->type_str = strdup(width_str);
        e->size = get_size_from_type_str(e->type_str);
        fdprintf(2, "UNOP %c:%c (", op, width);
    } else {
        fdprintf(2, "UNOP %c (", op);
    }

    skip();
    e->left = parse_expr();  /* operand */
    fdprintf(2, ")");
    expect(')');

    /* If no type annotation, inherit size from operand */
    if (width == ' ' && e->left) {
        e->size = e->left->size;
    }

    /* Set signedness based on operator */
    if (op == 0xb6) {  /* WIDEN - unsigned (zero extend) */
        e->flags = E_UNSIGNED;
    } else if (op == 0xab) {  /* SEXT - signed (sign extend) */
        e->flags = 0;  /* explicitly signed */
    } else if (e->type_str) {
        /* For other operators with type annotation, use annotation */
        e->flags = get_signedness_from_type_str(e->type_str);
    } else if (e->left) {
        /* Otherwise inherit from operand */
        e->flags = e->left->flags;
    }

    return e;
}

static struct expr *
handle_bfextract(unsigned char op)
{
    struct expr *e = new_expr(op);
    /* Bitfield extract: (0xa7:offset:width addr) */
    int offset = 0, width = 0;

    /* Parse offset:width */
    skip();
    if (curchar == ':') {
        nextchar();
        offset = (int)read_number();
        skip();
        if (curchar == ':') {
            nextchar();
            width = (int)read_number();
        }
    }

    /* Store offset and width in value field (pack into long) */
    e->value = (offset << 16) | width;

    fdprintf(2, "BFEXTRACT<%d:%d> (", offset, width);
    skip();
    e->left = parse_expr();  /* address */
    fdprintf(2, ")");
    expect(')');
    return e;
}

static struct expr *
handle_bfassign(unsigned char op)
{
    struct expr *e = new_expr(op);
    /* Bitfield assign: (0xdd:offset:width addr value) */
    int offset = 0, width = 0;

    /* Parse offset:width */
    skip();
    if (curchar == ':') {
        nextchar();
        offset = (int)read_number();
        skip();
        if (curchar == ':') {
            nextchar();
            width = (int)read_number();
        }
    }

    /* Store offset and width in value field (pack into long) */
    e->value = (offset << 16) | width;

    fdprintf(2, "BFASSIGN<%d:%d> (", offset, width);
    skip();
    e->left = parse_expr();  /* address */
    fdprintf(2, ", ");
    skip();
    e->right = parse_expr();  /* value */
    fdprintf(2, ")");
    expect(')');
    return e;
}

/* Wrappers to match handler_fn signature */
static struct expr *handle_deref_dispatch(unsigned char op) { return handle_deref(); }
static struct expr *handle_assign_dispatch(unsigned char op) { return handle_assign(); }
static struct expr *handle_call_dispatch(unsigned char op) { return handle_call(); }
static struct expr *handle_ternary_dispatch(unsigned char op) { return handle_ternary(); }
static struct expr *handle_bfextract_dispatch(unsigned char op) { return handle_bfextract(op); }
static struct expr *handle_bfassign_dispatch(unsigned char op) { return handle_bfassign(op); }

/* COLON is only used as part of ternary, but handle it gracefully if standalone */
static struct expr *
handle_colon(unsigned char op)
{
    struct expr *e = new_expr(op);

    fdprintf(2, "COLON (");
    skip();
    e->left = parse_expr();  /* left */
    fdprintf(2, ", ");
    skip();
    e->right = parse_expr();  /* right */
    fdprintf(2, ")");
    expect(')');
    return e;
}

static struct expr *handle_colon_dispatch(unsigned char op) { return handle_colon(op); }

static struct expr *
handle_deref(void)
{
    struct expr *e = new_expr('M');  // 'M' for memory/deref
    char width = 's';  /* default */

    /* Check for width annotation :b :s :l :p :f :d */
    skip();
    if (curchar == ':') {
        nextchar();
        width = curchar;
        nextchar();
        /* Store width annotation */
        char width_str[3] = {':', width, '\0'};
        e->type_str = strdup(width_str);
        e->size = get_size_from_type_str(e->type_str);
        e->flags = get_signedness_from_type_str(e->type_str);
    }

    fdprintf(2, "DEREF:%c (", width);
    skip();
    e->left = parse_expr();  /* address expression */
    fdprintf(2, ")");
    expect(')');
    return e;
}

static struct expr *
handle_assign(void)
{
    struct expr *e = new_expr('=');  // '=' for assignment
    char width = 's';  /* default */

    /* Check for width annotation */
    skip();
    if (curchar == ':') {
        nextchar();
        width = curchar;
        nextchar();
        /* Store width annotation */
        char width_str[3] = {':', width, '\0'};
        e->type_str = strdup(width_str);
        e->size = get_size_from_type_str(e->type_str);
        e->flags = get_signedness_from_type_str(e->type_str);
    }

    fdprintf(2, "ASSIGN:%c (", width);
    skip();
    e->left = parse_expr();  /* lvalue */
    fdprintf(2, ", ");
    skip();
    e->right = parse_expr();  /* rvalue */
    fdprintf(2, ")");
    expect(')');
    return e;
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

static struct expr *
handle_call(void)
{
    struct expr *e = new_expr('@');  // '@' for call
    char arg_buf[4096];  /* Buffer to capture argument expressions */
    int arg_start[32];  /* Start position of each argument */
    int arg_len[32];    /* Length of each argument */
    struct expr *args[32];  /* Parsed argument trees */
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

    /* Now recursively parse: function and arguments */
    struct parser_state saved_state;
    save_parser_state(&saved_state);

    /* Parse function expression */
    fdprintf(2, "\n  CALL_FUNC: ");
    setup_string_input(func_buf, func_len);
    e->left = parse_expr();  /* function address */
    restore_parser_state(&saved_state);

    /* Parse arguments */
    for (i = 0; i < arg_count; i++) {
        fdprintf(2, "\n  ARG%d: ", i);
        setup_string_input(&arg_buf[arg_start[i]], arg_len[i]);
        args[i] = parse_expr();
        restore_parser_state(&saved_state);
    }

    /* Build argument chain using right pointers */
    /* Store arg_count in value field */
    e->value = arg_count;
    if (arg_count > 0) {
        e->right = args[0];
        struct expr *prev = args[0];
        for (i = 1; i < arg_count; i++) {
            prev->right = args[i];
            prev = args[i];
        }
    }

    fdprintf(2, ")");
    expect(')');
    return e;
}

static struct expr *
handle_ternary(void)
{
    struct expr *e = new_expr('?');  // '?' for ternary

    fdprintf(2, "TERNARY (");
    skip();
    e->left = parse_expr();  /* condition */
    fdprintf(2, " ? ");
    skip();

    /* Expect COLON node - this becomes the right child */
    if (curchar == '(') {
        nextchar();
        skip();
        if (curchar == ':') {
            nextchar();
            skip();
            /* Build COLON node with true/false branches */
            struct expr *colon = new_expr(':');
            colon->left = parse_expr();  /* true expr */
            fdprintf(2, " : ");
            skip();
            colon->right = parse_expr();  /* false expr */
            expect(')');
            e->right = colon;
        }
    }

    fdprintf(2, ")");
    expect(')');
    return e;
}

/* Forward declarations for recursive parsing */
static struct expr *parse_expr(void);
static struct stmt *parse_stmt(void);

/* Forward declarations for code generation and emission */
static void assign_frame_offsets(struct function_ctx *ctx);
static void generate_expr(struct function_ctx *ctx, struct expr *e);
static void generate_stmt(struct function_ctx *ctx, struct stmt *s);
static void emit_expr(struct expr *e);
static void emit_stmt(struct stmt *s);

/* Helper functions for creating ASM nodes */
static struct stmt *create_label_asm(const char *label_name);
static struct stmt *create_jump_asm(const char *label_name);

/* Statement handlers */

/* Forward declarations */
static struct stmt *handle_block(void);
static struct stmt *handle_if(void);
static struct stmt *handle_while(void);
static struct stmt *handle_do(void);
static struct stmt *handle_for(void);
static struct stmt *handle_return(void);
static struct stmt *handle_expr_stmt(void);
static struct stmt *handle_empty_stmt(void);
static struct stmt *handle_asm(void);
static struct stmt *handle_label(void);
static struct stmt *handle_goto(void);
static struct stmt *handle_switch(void);
static struct stmt *handle_case_in_block(void);
static struct stmt *handle_default_in_block(void);

/* Handlers for case/default when they appear in block context */
static struct stmt *
handle_case_in_block(void)
{
    /* Case statement: (C value ()) */
    struct stmt *child = new_stmt('C');

    fdprintf(2, "CASE ");
    skip();
    child->expr = parse_expr();  /* case value */
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

    return child;
}

static struct stmt *
handle_default_in_block(void)
{
    /* Default statement: (O ()) */
    struct stmt *child = new_stmt('O');

    fdprintf(2, "DEFAULT: ");
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

    return child;
}

static struct stmt *
handle_block(void)
{
    struct stmt *s = new_stmt('B');
    struct stmt *first_child = NULL;
    struct stmt *last_child = NULL;
    struct stmt *child;

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

                /* Create declaration statement node */
                child = new_stmt('d');
                child->symbol = strdup(name);  /* Duplicate - symbuf is reused */
                child->type_str = strdup(type);  /* Duplicate - typebuf is reused */

                expect(')');
            } else {
                /* Statement - dispatch based on operator */
                char op = curchar;
                nextchar();

                switch (op) {
                case 'B':  /* Block */
                    child = handle_block();
                    break;
                case 'I':  /* If */
                    child = handle_if();
                    break;
                case 'W':  /* While */
                    child = handle_while();
                    break;
                case 'D':  /* Do-while */
                    child = handle_do();
                    break;
                case 'F':  /* For */
                    child = handle_for();
                    break;
                case 'R':  /* Return */
                    child = handle_return();
                    break;
                case 'E':  /* Expression statement */
                    child = handle_expr_stmt();
                    break;
                case ';':  /* Empty statement */
                    child = handle_empty_stmt();
                    break;
                case 'A':  /* Asm block */
                    child = handle_asm();
                    break;
                case 'L':  /* Label */
                    child = handle_label();
                    break;
                case 'G':  /* Goto */
                    child = handle_goto();
                    break;
                case 'S':  /* Switch */
                    child = handle_switch();
                    break;
                case 'C':  /* Case (inside switch body) */
                    /* Case statements only valid inside switch, but may appear in block */
                    child = handle_case_in_block();
                    break;
                case 'O':  /* Default (inside switch body) */
                    /* Default statements only valid inside switch, but may appear in block */
                    child = handle_default_in_block();
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
                    child = NULL;
                    break;
                }
                fdprintf(2, "\n");
            }

            /* Add child to linked list and update tail pointer */
            if (child) {
                if (!first_child) {
                    first_child = child;
                } else {
                    last_child->next = child;
                }
                /* Update last_child to point to the actual end of the chain */
                last_child = child;
                while (last_child->next) {
                    last_child = last_child->next;
                }
            }
        }
        skip();
    }

    fdprintf(2, "}");
    expect(')');

    s->then_branch = first_child;  /* Use then_branch for block's child list */
    return s;
}

static struct stmt *
handle_if(void)
{
    struct stmt *s = new_stmt('I');
    char label_buf[64];

    s->label = label_counter++;

    fdprintf(2, "IF (");
    skip();
    s->expr = parse_expr();  /* condition */
    fdprintf(2, ") ");

    skip();
    s->then_branch = parse_stmt();  /* then branch */

    skip();
    if (curchar != ')') {
        /* Has else branch - need second label for end of else */
        s->label2 = label_counter++;

        fdprintf(2, " ELSE ");
        s->else_branch = parse_stmt();  /* else branch */

        /* Insert end label after the IF statement */
        snprintf(label_buf, sizeof(label_buf), "_if_end_%d", s->label2);
        s->next = create_label_asm(label_buf);
    } else {
        s->else_branch = NULL;

        /* Insert end label after the IF statement */
        snprintf(label_buf, sizeof(label_buf), "_if_%d", s->label);
        s->next = create_label_asm(label_buf);
    }

    expect(')');
    return s;
}

static struct stmt *
handle_while(void)
{
    struct stmt *s = new_stmt('W');
    char label_buf[64];

    s->label = label_counter++;  /* Loop start label */

    fdprintf(2, "WHILE (");
    skip();
    s->expr = parse_expr();  /* condition */
    fdprintf(2, ") ");

    skip();
    s->then_branch = parse_stmt();  /* body */

    expect(')');

    /* Insert loop start label before the while, and end label after */
    /* Create label for loop start */
    snprintf(label_buf, sizeof(label_buf), "_while_%d", s->label);
    struct stmt *start_label = create_label_asm(label_buf);
    start_label->next = s;

    /* Insert end/break label after the while */
    snprintf(label_buf, sizeof(label_buf), "_while_end_%d", s->label);
    s->next = create_label_asm(label_buf);

    return start_label;  /* Return the label, not the while node */
}

static struct stmt *
handle_do(void)
{
    struct stmt *s = new_stmt('D');
    char label_buf[64];

    s->label = label_counter++;  /* Loop start label */

    fdprintf(2, "DO ");
    skip();
    s->then_branch = parse_stmt();  /* body */

    fdprintf(2, " WHILE (");
    skip();
    s->expr = parse_expr();  /* condition */
    fdprintf(2, ")");

    expect(')');

    /* Insert loop start label before the do, and end label after */
    snprintf(label_buf, sizeof(label_buf), "_do_%d", s->label);
    struct stmt *start_label = create_label_asm(label_buf);
    start_label->next = s;

    /* Insert end/break label after the do-while */
    snprintf(label_buf, sizeof(label_buf), "_do_end_%d", s->label);
    s->next = create_label_asm(label_buf);

    return start_label;
}

static struct stmt *
handle_for(void)
{
    struct stmt *s = new_stmt('F');
    char label_buf[64];

    s->label = label_counter++;  /* Loop start label */

    fdprintf(2, "FOR (");
    skip();
    s->expr = parse_expr();  /* init */
    fdprintf(2, "; ");
    skip();
    s->expr2 = parse_expr();  /* condition */
    fdprintf(2, "; ");
    skip();
    s->expr3 = parse_expr();  /* increment */
    fdprintf(2, ") ");

    skip();
    s->then_branch = parse_stmt();  /* body */

    expect(')');

    /* Insert loop start label before the for, and end label after */
    snprintf(label_buf, sizeof(label_buf), "_for_%d", s->label);
    struct stmt *start_label = create_label_asm(label_buf);
    start_label->next = s;

    /* Insert end/break label after the for */
    snprintf(label_buf, sizeof(label_buf), "_for_end_%d", s->label);
    s->next = create_label_asm(label_buf);

    return start_label;
}

static struct stmt *
handle_return(void)
{
    struct stmt *s = new_stmt('R');

    fdprintf(2, "RETURN");
    skip();
    if (curchar != ')') {
        fdprintf(2, " ");
        s->expr = parse_expr();
    } else {
        s->expr = NULL;
    }
    expect(')');
    return s;
}

static struct stmt *
handle_expr_stmt(void)
{
    struct stmt *s = new_stmt('E');

    fdprintf(2, "EXPR (");
    skip();
    s->expr = parse_expr();
    fdprintf(2, ")");
    expect(')');
    return s;
}

static struct stmt *
handle_empty_stmt(void)
{
    struct stmt *s = new_stmt(';');

    fdprintf(2, ";");
    expect(')');
    return s;
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

static struct stmt *
handle_asm(void)
{
    struct stmt *s = new_stmt('A');
    char asm_buf[4096];
    char *p;

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

        /* Store assembly text in statement node */
        s->asm_block = malloc(strlen(asm_buf) + 1);
        strcpy(s->asm_block, asm_buf);
    }
    expect(')');
    return s;
}

static struct stmt *
handle_label(void)
{
    struct stmt *s = new_stmt('L');
    char *label_name;

    /* Label statement: (L label_name) */
    skip();
    label_name = read_symbol();

    /* Diagnostic output to stderr */
    fdprintf(2, "LABEL %s", label_name);

    /* Store label name in statement node */
    s->symbol = label_name;

    expect(')');
    return s;
}

static struct stmt *
handle_goto(void)
{
    struct stmt *s = new_stmt('G');
    char *label_name;

    /* Goto statement: (G label_name) */
    skip();
    label_name = read_symbol();

    /* Diagnostic output to stderr */
    fdprintf(2, "GOTO %s", label_name);

    /* Store label name in statement node */
    s->symbol = label_name;

    expect(')');
    return s;
}

static struct stmt *
handle_switch(void)
{
    struct stmt *s = new_stmt('S');
    struct stmt *first_child = NULL;
    struct stmt *last_child = NULL;
    struct stmt *child;

    /* Switch statement: (S expr (C val ...) (C val ...) (O ...) ) */
    fdprintf(2, "SWITCH (");
    skip();
    s->expr = parse_expr();  /* switch expression */
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
                child = new_stmt('C');
                skip();
                child->expr = parse_expr();  /* case value */
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
                child = new_stmt('O');
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
                    child = handle_return();
                } else if (clause_type == 'G') {
                    child = new_stmt('G');
                    skip();
                    child->symbol = read_symbol();
                    fdprintf(2, "GOTO %s", child->symbol);
                    expect(')');
                } else if (clause_type == 'B') {
                    /* BREAK statement */
                    fdprintf(2, "BREAK");
                    child = new_stmt('B');
                    expect(')');
                } else if (clause_type == 'E') {
                    child = handle_expr_stmt();
                } else {
                    child = NULL;
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
                child = NULL;
            }

            /* Add child to linked list and update tail pointer */
            if (child) {
                if (!first_child) {
                    first_child = child;
                } else {
                    last_child->next = child;
                }
                /* Update last_child to point to the actual end of the chain */
                last_child = child;
                while (last_child->next) {
                    last_child = last_child->next;
                }
            }
        }
        skip();
    }

    fdprintf(2, "\n}");
    expect(')');

    s->then_branch = first_child;  /* Use then_branch for switch body */
    return s;
}

/* Top-level handlers */

/*
 * Emit function prologue
 * Outputs assembly comment describing function and function label
 */
static void
emit_function_prologue(char *name, char *params, char *rettype, int frame_size,
                       struct local_var *locals)
{
    int has_params = (params && params[0]);
    struct local_var *var;

    /* Assembly comment with function signature */
    fdprintf(out_fd, "; Function: %s", name);

    if (has_params) {
        fdprintf(out_fd, "(%s)", params);
    } else {
        fdprintf(out_fd, "()");
    }

    if (rettype && rettype[0]) {
        fdprintf(out_fd, " -> %s", rettype);
    }

    fdprintf(out_fd, "\n");

    /* Output variable lifetime information as assembly comments */
    if (locals) {
        fdprintf(out_fd, "; Variable lifetimes:\n");
        for (var = locals; var; var = var->next) {
            fdprintf(out_fd, ";   %s: ", var->name);
            if (var->first_label == -1) {
                fdprintf(out_fd, "unused (0 refs)\n");
            } else {
                fdprintf(out_fd, "labels %d-%d (%d refs)\n",
                         var->first_label, var->last_label, var->ref_count);
            }
        }
    }

    /* Function label (standard C naming with underscore prefix) */
    fdprintf(out_fd, "_%s:\n", name);

    /* Emit frame allocation call if we have locals or parameters */
    if (frame_size > 0 || has_params) {
        fdprintf(out_fd, "\tld a, %d\n", frame_size);
        fdprintf(out_fd, "\tcall framealloc\n");
    }
}

static void
handle_function(void)
{
    struct function_ctx ctx;
    char name_buf[64];  /* Stack buffer for function name */
    char params_buf[256];
    char *p;
    int first_param;
    struct stmt *first_child = NULL;
    struct stmt *last_child = NULL;
    struct stmt *child;

    /* (f name (params) return_type declarations body) */
    /* Copy function name to stack buffer before reading parameters */
    strncpy(name_buf, read_symbol(), sizeof(name_buf) - 1);
    name_buf[sizeof(name_buf) - 1] = '\0';
    ctx.name = name_buf;
    fdprintf(2, "\nFUNCTION %s\n", ctx.name);

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
    ctx.params = params_buf;
    expect(')');
    fdprintf(2, "\n");

    /* Return type */
    ctx.rettype = read_type();
    fdprintf(2, "  RETURNS: %s\n", ctx.rettype);

    /* Declarations and body - collect into statement tree */
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

                /* Create declaration statement node */
                child = new_stmt('d');
                child->symbol = strdup(dname);  /* Duplicate - symbuf is reused */
                child->type_str = strdup(dtype);  /* Duplicate - typebuf is reused */

                expect(')');
            } else {
                /* Body statement - we've already consumed '(', now consume operator */
                char op = curchar;
                nextchar();

                fdprintf(2, "  BODY: ");
                switch (op) {
                case 'B':  /* Block */
                    child = handle_block();
                    break;
                case 'I':  /* If */
                    child = handle_if();
                    break;
                case 'W':  /* While */
                    child = handle_while();
                    break;
                case 'D':  /* Do-while */
                    child = handle_do();
                    break;
                case 'F':  /* For */
                    child = handle_for();
                    break;
                case 'R':  /* Return */
                    child = handle_return();
                    break;
                case 'E':  /* Expression statement */
                    child = handle_expr_stmt();
                    break;
                case ';':  /* Empty statement */
                    child = handle_empty_stmt();
                    break;
                case 'A':  /* Asm block */
                    child = handle_asm();
                    break;
                case 'L':  /* Label */
                    child = handle_label();
                    break;
                case 'G':  /* Goto */
                    child = handle_goto();
                    break;
                case 'S':  /* Switch */
                    child = handle_switch();
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
                    child = NULL;
                    break;
                }
                fdprintf(2, "\n");
            }

            /* Add child to linked list and update tail pointer */
            if (child) {
                if (!first_child) {
                    first_child = child;
                } else {
                    last_child->next = child;
                }
                /* Update last_child to point to the actual end of the chain */
                last_child = child;
                while (last_child->next) {
                    last_child = last_child->next;
                }
            }
        }
        skip();
    }

    /* Store body tree in context */
    ctx.body = first_child;
    ctx.label_counter = label_counter;  /* Save current label counter */
    ctx.locals = NULL;  /* No local variables yet */
    ctx.frame_size = 0;  /* No frame size yet */

    expect(')');

    /* Phase 1.5: Assign stack frame offsets to local variables */
    assign_frame_offsets(&ctx);

    /* Phase 2: Generate assembly code blocks for tree nodes */
    generate_code(&ctx);

    /* Phase 3: Emit assembly and free tree nodes */
    emit_assembly(&ctx, out_fd);
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

static void
handle_string_literal(void)
{
    char *name, *data;

    /* (s name "data") */
    name = read_symbol();
    data = read_quoted_string();

    fdprintf(2, "\nSTRING %s \"%s\"\n", name, data);

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
    expr_handlers['h'] = handle_lor;        /* LOR || */
    expr_handlers['j'] = handle_land;       /* LAND && */

    /* Compound assignment operators */
    expr_handlers['P'] = handle_compound_assign;  /* PLUSEQ += */
    expr_handlers[0xdf] = handle_compound_assign; /* SUBEQ -= */
    expr_handlers['T'] = handle_compound_assign;  /* MULTEQ *= */
    expr_handlers['2'] = handle_compound_assign;  /* DIVEQ /= */
    expr_handlers[0xfe] = handle_compound_assign; /* MODEQ %= */
    expr_handlers[0xc6] = handle_compound_assign; /* ANDEQ &= */
    expr_handlers['1'] = handle_compound_assign;  /* OREQ |= */
    expr_handlers['0'] = handle_compound_assign;  /* LSHIFTEQ <<= */
    expr_handlers['6'] = handle_compound_assign;  /* RSHIFTEQ >>= */

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

    /* Bitfield operators */
    expr_handlers[0xa7] = handle_bfextract_dispatch;  /* BFEXTRACT */
    expr_handlers[0xdd] = handle_bfassign_dispatch;   /* BFASSIGN */
}

/*
 * Parse an expression (recursive)
 */
static struct expr *
parse_expr(void)
{
    struct expr *e;

    skip();

    if (curchar == '(') {
        nextchar();
        skip();

        unsigned char op = curchar;
        nextchar();

        /* Use lookup table to dispatch to handler */
        e = expr_handlers[op](op);

    } else if (curchar == '$') {
        /* Symbol */
        e = handle_symbol();
    } else if (curchar == 'S') {
        /* String literal */
        nextchar();
        e = handle_string();
    } else if ((curchar >= '0' && curchar <= '9') || curchar == '-') {
        /* Constant */
        e = handle_const();
    } else {
        fdprintf(2, "parseast: line %d: unexpected char '%c' in expr\n",
                 line_num, curchar);
        nextchar();
        e = NULL;
    }

    return e;
}

/*
 * Parse a statement (recursive)
 */
static struct stmt *
parse_stmt(void)
{
    struct stmt *s;

    skip();

    if (curchar != '(') {
        fdprintf(2, "parseast: line %d: expected '(' at start of statement\n", line_num);
        return NULL;
    }

    nextchar();
    skip();

    /* Handle empty statement: () */
    if (curchar == ')') {
        nextchar();
        return NULL;
    }

    char op = curchar;
    nextchar();

    switch (op) {
    case 'B':  /* Block */
        s = handle_block();
        break;
    case 'I':  /* If */
        s = handle_if();
        break;
    case 'W':  /* While */
        s = handle_while();
        break;
    case 'D':  /* Do-while */
        s = handle_do();
        break;
    case 'F':  /* For */
        s = handle_for();
        break;
    case 'R':  /* Return */
        s = handle_return();
        break;
    case 'E':  /* Expression statement */
        s = handle_expr_stmt();
        break;
    case ';':  /* Empty statement */
        s = handle_empty_stmt();
        break;
    case 'A':  /* Asm block */
        s = handle_asm();
        break;
    case 'L':  /* Label */
        s = handle_label();
        break;
    case 'G':  /* Goto */
        s = handle_goto();
        break;
    case 'S':  /* Switch */
        s = handle_switch();
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
        s = NULL;
        break;
    }

    return s;
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
    case 's':  /* String literal */
        handle_string_literal();
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
 * Helper: Get the original operand size, looking through SEXT/WIDEN conversions
 */
static int
get_original_size(struct expr *e)
{
    if (!e) return 2;  /* default to 16-bit */

    /* Look through SEXT ('X') and WIDEN ('W') to find original size */
    if ((e->op == 'X' || e->op == 'W') && e->left) {
        return e->left->size;
    }

    /* Also check for NARROW ('N') which truncates to smaller size */
    if (e->op == 'N' && e->left) {
        return e->left->size;
    }

    return e->size;
}

/*
 * Helper: Check if operand is unsigned (look through conversions)
 */
static int
is_operand_unsigned(struct expr *e)
{
    if (!e) return 0;

    /* WIDEN ('W') means unsigned (zero extend) */
    if (e->op == 'W') return 1;

    /* Otherwise check the expression's flags */
    return (e->flags & E_UNSIGNED) ? 1 : 0;
}

/*
 * Helper: Generate function name for binary arithmetic operations
 * Format: [u]<op><leftwidth><rightwidth>
 * Examples: mul88, umul816, div1616, add168
 */
static void
make_binop_funcname(char *buf, size_t bufsize, const char *opname,
                    struct expr *e)
{
    int left_bits = get_original_size(e->left) * 8;
    int right_bits = get_original_size(e->right) * 8;

    /* Operation is unsigned if either operand is unsigned */
    int is_unsigned = is_operand_unsigned(e->left) || is_operand_unsigned(e->right);
    const char *prefix = is_unsigned ? "u" : "";

    snprintf(buf, bufsize, "%s%s%d%d", prefix, opname, left_bits, right_bits);
}

/*
 * Helper: Add a parameter to the function context
 * Parameters have positive offsets (above frame pointer)
 */
static void
add_param(struct function_ctx *ctx, const char *name, unsigned char size, int offset)
{
    struct local_var *var = malloc(sizeof(struct local_var));
    if (!var) {
        fdprintf(2, "parseast: out of memory allocating local_var\n");
        exit(1);
    }

    var->name = strdup(name);
    var->size = size;
    var->offset = offset;
    var->is_param = 1;
    var->first_label = -1;  /* Not used yet */
    var->last_label = -1;   /* Not used yet */
    var->ref_count = 0;     /* Not referenced yet */
    var->next = ctx->locals;

    ctx->locals = var;

    fdprintf(2, "  Parameter: %s, size=%d, offset=+%d\n", name, size, offset);
}

/*
 * Helper: Add a local variable to the function context
 * Local variables have negative offsets (below frame pointer)
 */
static void
add_local_var(struct function_ctx *ctx, const char *name, unsigned char size)
{
    struct local_var *var = malloc(sizeof(struct local_var));
    if (!var) {
        fdprintf(2, "parseast: out of memory allocating local_var\n");
        exit(1);
    }

    var->name = strdup(name);
    var->size = size;
    /* Stack grows downward - assign negative offset from frame pointer */
    var->offset = -(ctx->frame_size + size);
    var->is_param = 0;
    var->first_label = -1;  /* Not used yet */
    var->last_label = -1;   /* Not used yet */
    var->ref_count = 0;     /* Not referenced yet */
    var->next = ctx->locals;

    ctx->locals = var;
    ctx->frame_size += size;

    fdprintf(2, "  Local var: %s, size=%d, offset=%d\n", name, size, var->offset);
}

/*
 * Helper: Update variable lifetime tracking
 * Called whenever a variable is used, updates first_label and last_label
 */
static void
update_var_lifetime(struct function_ctx *ctx, const char *name)
{
    struct local_var *var;

    if (!ctx || !name) return;

    /* Find the variable in locals list */
    for (var = ctx->locals; var; var = var->next) {
        if (strcmp(var->name, name) == 0) {
            /* Update first use if not set */
            if (var->first_label == -1) {
                var->first_label = ctx->current_label;
            }
            /* Always update last use (high water mark) */
            if (ctx->current_label > var->last_label) {
                var->last_label = ctx->current_label;
            }
            /* Increment reference count */
            var->ref_count++;
            return;
        }
    }
}

/*
 * Helper: Check if a name is a parameter
 */
static int
is_parameter(struct function_ctx *ctx, const char *name)
{
    struct local_var *var;
    for (var = ctx->locals; var; var = var->next) {
        if (var->is_param && strcmp(var->name, name) == 0) {
            return 1;
        }
    }
    return 0;
}

/*
 * Walk statement tree and assign stack frame offsets to local variables
 */
static void
walk_for_locals(struct function_ctx *ctx, struct stmt *s)
{
    if (!s) return;

    /* If this is a declaration, add it to locals list (unless it's a parameter) */
    if (s->type == 'd' && s->symbol) {
        /* Skip parameter declarations - they already have offsets */
        if (!is_parameter(ctx, s->symbol)) {
            unsigned char size = get_size_from_typename(s->type_str);
            add_local_var(ctx, s->symbol, size);
        }
    }

    /* Recursively walk child statements */
    if (s->then_branch) walk_for_locals(ctx, s->then_branch);
    if (s->else_branch) walk_for_locals(ctx, s->else_branch);
    if (s->next) walk_for_locals(ctx, s->next);
}

/*
 * Phase 1.5: Assign stack frame offsets to all local variables and parameters
 */
static void
assign_frame_offsets(struct function_ctx *ctx)
{
    char *p;
    char name_buf[64];
    char type_buf[64];
    int param_offset;
    int i;

    if (!ctx || !ctx->body) return;

    fdprintf(2, "  Assigning stack frame offsets:\n");

    /* First, assign offsets to parameters (positive offsets above FP) */
    /* Stack layout: FP+0=saved FP, FP+2=return addr, FP+4=first param */
    param_offset = 4;  /* Skip saved FP (2 bytes) + return address (2 bytes) */

    if (ctx->params && ctx->params[0]) {
        p = ctx->params;
        while (*p) {
            /* Skip whitespace and commas */
            while (*p == ' ' || *p == ',') p++;
            if (!*p) break;

            /* Read parameter name */
            i = 0;
            while (*p && *p != ':' && *p != ',' && *p != ' ' && i < sizeof(name_buf) - 1) {
                name_buf[i++] = *p++;
            }
            name_buf[i] = '\0';

            /* Read parameter type if present */
            type_buf[0] = '\0';
            if (*p == ':') {
                p++;  /* Skip ':' */
                i = 0;
                while (*p && *p != ',' && *p != ' ' && i < sizeof(type_buf) - 1) {
                    type_buf[i++] = *p++;
                }
                type_buf[i] = '\0';
            }

            /* Add parameter with positive offset */
            if (name_buf[0]) {
                unsigned char size = type_buf[0] ? get_size_from_typename(type_buf) : 2;
                add_param(ctx, name_buf, size, param_offset);
                param_offset += size;
            }
        }
    }

    /* Then, assign offsets to local variables (negative offsets below FP) */
    walk_for_locals(ctx, ctx->body);
    fdprintf(2, "  Total frame size: %d bytes\n", ctx->frame_size);
}

/*
 * Code generation phase (Phase 2)
 * Walk expression tree and generate assembly code blocks
 */
static void generate_expr(struct function_ctx *ctx, struct expr *e)
{
    char buf[256];
    if (!e) return;

    /* Recursively generate code for children (postorder traversal) */
    if (e->left) generate_expr(ctx, e->left);
    if (e->right) generate_expr(ctx, e->right);

    /* Track variable usage for lifetime analysis */
    if (e->op == '$' && e->symbol) {
        /* Skip the leading $ to match variable names in locals list */
        const char *var_name = e->symbol;
        if (var_name[0] == '$') {
            var_name++;  /* Skip $ prefix */
        }
        /* Also skip 'A' prefix for arguments (e.g., $Ax -> x) */
        if (var_name[0] == 'A') {
            var_name++;  /* Skip A prefix */
        }
        update_var_lifetime(ctx, var_name);
    }

    /* Generate assembly code for this node based on operator */
    switch (e->op) {
    case 'C':  /* CONST - load immediate value */
        if (e->size == 1) {
            snprintf(buf, sizeof(buf), "\tld a, %ld", e->value);
        } else if (e->size == 2) {
            snprintf(buf, sizeof(buf), "\tld hl, %ld", e->value);
        } else {  /* size == 4 */
            snprintf(buf, sizeof(buf), "\t; TODO: load long %ld", e->value);
        }
        e->asm_block = strdup(buf);
        break;

    case 'M':  /* DEREF - load from memory */
        if (e->size == 1) {
            snprintf(buf, sizeof(buf), "\t; load byte from address");
        } else if (e->size == 2) {
            snprintf(buf, sizeof(buf), "\t; load word from address");
        } else {
            snprintf(buf, sizeof(buf), "\t; load long from address");
        }
        e->asm_block = strdup(buf);
        break;

    case '=':  /* ASSIGN - store to memory */
        if (e->size == 1) {
            snprintf(buf, sizeof(buf), "\t; store byte to address");
        } else if (e->size == 2) {
            snprintf(buf, sizeof(buf), "\t; store word to address");
        } else {
            snprintf(buf, sizeof(buf), "\t; store long to address");
        }
        e->asm_block = strdup(buf);
        break;

    case '+':  /* ADD */
        {
            char funcname[32];
            make_binop_funcname(funcname, sizeof(funcname), "add", e);
            snprintf(buf, sizeof(buf), "\tcall %s", funcname);
            e->asm_block = strdup(buf);
        }
        break;

    case '-':  /* SUB */
        {
            char funcname[32];
            make_binop_funcname(funcname, sizeof(funcname), "sub", e);
            snprintf(buf, sizeof(buf), "\tcall %s", funcname);
            e->asm_block = strdup(buf);
        }
        break;

    case '*':  /* MUL */
        {
            char funcname[32];
            make_binop_funcname(funcname, sizeof(funcname), "mul", e);
            snprintf(buf, sizeof(buf), "\tcall %s", funcname);
            e->asm_block = strdup(buf);
        }
        break;

    case '/':  /* DIV */
        {
            char funcname[32];
            make_binop_funcname(funcname, sizeof(funcname), "div", e);
            snprintf(buf, sizeof(buf), "\tcall %s", funcname);
            e->asm_block = strdup(buf);
        }
        break;

    case '%':  /* MOD */
        {
            char funcname[32];
            make_binop_funcname(funcname, sizeof(funcname), "mod", e);
            snprintf(buf, sizeof(buf), "\tcall %s", funcname);
            e->asm_block = strdup(buf);
        }
        break;

    case '&':  /* AND */
        {
            char funcname[32];
            make_binop_funcname(funcname, sizeof(funcname), "and", e);
            snprintf(buf, sizeof(buf), "\tcall %s", funcname);
            e->asm_block = strdup(buf);
        }
        break;

    case '|':  /* OR */
        {
            char funcname[32];
            make_binop_funcname(funcname, sizeof(funcname), "or", e);
            snprintf(buf, sizeof(buf), "\tcall %s", funcname);
            e->asm_block = strdup(buf);
        }
        break;

    case '^':  /* XOR */
        {
            char funcname[32];
            make_binop_funcname(funcname, sizeof(funcname), "xor", e);
            snprintf(buf, sizeof(buf), "\tcall %s", funcname);
            e->asm_block = strdup(buf);
        }
        break;

    case 'y':  /* LSHIFT */
        {
            char funcname[32];
            make_binop_funcname(funcname, sizeof(funcname), "shl", e);
            snprintf(buf, sizeof(buf), "\tcall %s", funcname);
            e->asm_block = strdup(buf);
        }
        break;

    case 'w':  /* RSHIFT */
        {
            char funcname[32];
            make_binop_funcname(funcname, sizeof(funcname), "shr", e);
            snprintf(buf, sizeof(buf), "\tcall %s", funcname);
            e->asm_block = strdup(buf);
        }
        break;

    case '>':  /* GT - greater than comparison */
        {
            char funcname[32];
            make_binop_funcname(funcname, sizeof(funcname), "gt", e);
            snprintf(buf, sizeof(buf), "\tcall %s", funcname);
            e->asm_block = strdup(buf);
        }
        break;

    case '<':  /* LT - less than comparison */
        {
            char funcname[32];
            make_binop_funcname(funcname, sizeof(funcname), "lt", e);
            snprintf(buf, sizeof(buf), "\tcall %s", funcname);
            e->asm_block = strdup(buf);
        }
        break;

    case 'g':  /* GE - greater or equal comparison */
        {
            char funcname[32];
            make_binop_funcname(funcname, sizeof(funcname), "ge", e);
            snprintf(buf, sizeof(buf), "\tcall %s", funcname);
            e->asm_block = strdup(buf);
        }
        break;

    case 'L':  /* LE - less or equal comparison */
        {
            char funcname[32];
            make_binop_funcname(funcname, sizeof(funcname), "le", e);
            snprintf(buf, sizeof(buf), "\tcall %s", funcname);
            e->asm_block = strdup(buf);
        }
        break;

    case 'Q':  /* EQ - equality comparison */
        {
            char funcname[32];
            make_binop_funcname(funcname, sizeof(funcname), "eq", e);
            snprintf(buf, sizeof(buf), "\tcall %s", funcname);
            e->asm_block = strdup(buf);
        }
        break;

    case 'n':  /* NEQ - not equal comparison */
        {
            char funcname[32];
            make_binop_funcname(funcname, sizeof(funcname), "ne", e);
            snprintf(buf, sizeof(buf), "\tcall %s", funcname);
            e->asm_block = strdup(buf);
        }
        break;

    case 0xab:  /* SEXT - sign extend */
        snprintf(buf, sizeof(buf), "\t; sign extend to size %d", e->size);
        e->asm_block = strdup(buf);
        break;

    case 0xb6:  /* WIDEN - zero extend */
        snprintf(buf, sizeof(buf), "\t; zero extend to size %d", e->size);
        e->asm_block = strdup(buf);
        break;

    case '$':  /* SYM - symbol reference (address) */
        snprintf(buf, sizeof(buf), "\t; load address of %s", e->symbol ? e->symbol : "?");
        e->asm_block = strdup(buf);
        break;

    default:
        /* For now, generate placeholder comment for other operators */
        snprintf(buf, sizeof(buf), "\t; op %c (0x%02x) size=%d%s",
                 e->op >= ' ' && e->op <= '~' ? e->op : '?',
                 e->op, e->size,
                 (e->flags & E_UNSIGNED) ? " unsigned" : "");
        e->asm_block = strdup(buf);
        break;
    }
}

/*
 * Walk statement tree and generate assembly code blocks
 */
static void generate_stmt(struct function_ctx *ctx, struct stmt *s)
{
    if (!s) return;

    /* Update current_label for statements that have labels */
    /* This tracks the current position in the control flow for lifetime analysis */
    if (s->label > 0) {
        ctx->current_label = s->label;
    }

    /* Recursively generate code for expressions */
    if (s->expr) generate_expr(ctx, s->expr);
    if (s->expr2) generate_expr(ctx, s->expr2);
    if (s->expr3) generate_expr(ctx, s->expr3);

    /* Recursively generate code for child statements */
    if (s->then_branch) generate_stmt(ctx, s->then_branch);
    if (s->else_branch) generate_stmt(ctx, s->else_branch);
    if (s->next) generate_stmt(ctx, s->next);

    /* TODO: Generate assembly code for this statement */
    /* s->asm_block = malloc(...); strcpy(s->asm_block, "..."); */
}

/*
 * Generate assembly code for entire function
 */
void generate_code(struct function_ctx *ctx)
{
    if (!ctx || !ctx->body) return;

    fdprintf(2, "=== Phase 2: Generating assembly code blocks ===\n");

    /* Initialize lifetime tracking */
    ctx->current_label = 0;

    generate_stmt(ctx, ctx->body);
}

/*
 * Emission phase (Phase 3)
 * Walk expression tree, emit assembly, and free nodes
 */
static void emit_expr(struct expr *e)
{
    if (!e) return;

    /* Emit children first (postorder traversal) */
    if (e->left) emit_expr(e->left);
    if (e->right) emit_expr(e->right);

    /* Emit assembly block for this node */
    if (e->asm_block) {
        fdprintf(out_fd, "%s\n", e->asm_block);
    }

    /* Free this node (children already freed by recursive emit calls above) */
    if (e->asm_block) free(e->asm_block);
    free(e);
}

/*
 * Walk statement tree, emit assembly, and free nodes
 */
static void emit_stmt(struct stmt *s)
{
    if (!s) return;

    /* For ASM nodes, emit the assembly block directly */
    if (s->type == 'A' && s->asm_block) {
        fdprintf(out_fd, "%s\n", s->asm_block);
    }

    /* Emit expressions (this frees them) */
    if (s->expr) emit_expr(s->expr);
    if (s->expr2) emit_expr(s->expr2);
    if (s->expr3) emit_expr(s->expr3);

    /* Emit child statements (this frees them) */
    if (s->then_branch) emit_stmt(s->then_branch);
    if (s->else_branch) emit_stmt(s->else_branch);

    /* Emit next statement in chain (this frees it) */
    if (s->next) emit_stmt(s->next);

    /* Free this node only (children already freed by recursive emit calls above) */
    if (s->asm_block) free(s->asm_block);
    free(s);
}

/*
 * Emit assembly for entire function and free tree
 */
void emit_assembly(struct function_ctx *ctx, int fd)
{
    struct local_var *var, *next;
    int has_params;

    if (!ctx || !ctx->body) return;

    fdprintf(2, "=== Phase 3: Emitting assembly and freeing tree ===\n");

    /* Check if function has parameters */
    has_params = (ctx->params && ctx->params[0]);

    /* Emit function prologue with frame allocation and lifetime info */
    emit_function_prologue(ctx->name, ctx->params, ctx->rettype, ctx->frame_size,
                          ctx->locals);

    /* Emit function body */
    emit_stmt(ctx->body);

    /* Emit function epilogue with frame deallocation */
    /* Emit framefree if we have locals or parameters */
    if (ctx->frame_size > 0 || has_params) {
        fdprintf(out_fd, "\tcall framefree\n");
    }

    /* Free local variables list */
    var = ctx->locals;
    while (var) {
        next = var->next;
        free(var->name);
        free(var);
        var = next;
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
