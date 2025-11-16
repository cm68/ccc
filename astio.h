/*
 * astio.h - Low-level I/O and token parsing for AST parser
 *
 * Handles buffered input, whitespace/comment skipping, and basic token reading.
 */
#ifndef ASTIO_H
#define ASTIO_H

#define BUFSIZE 4096  /* AST parser read buffer */

/*
 * Parser state for nested parsing (save/restore)
 */
struct parser_state {
    char saved_buf[BUFSIZE];
    int saved_buf_pos;
    int saved_buf_valid;
    int saved_line_num;
    unsigned char saved_curchar;
    int saved_in_fd;
};

/* Global parser state (managed by astio.c, accessible to parser) */
extern unsigned char curchar;
extern int line_num;

/* Buffer initialization */
void init_astio(int fd);

/* Character-level I/O */
unsigned char nextchar(void);
void skip(void);
int expect(unsigned char c);

/* Token reading (return pointers to static buffers) */
char *read_symbol(void);
long read_number(void);
char *read_type(void);
char *read_quoted_string(void);

/* Parser state management for nested parsing */
void save_parser_state(struct parser_state *state);
void restore_parser_state(struct parser_state *state);
void setup_string_input(char *str, int len);

/* String utilities */
int is_label(char *line);
char *trim_line(char *line);

#endif /* ASTIO_H */
