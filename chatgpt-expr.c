#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <string.h>
#include <math.h> // For power operator

// Define a structure for expression tree nodes
typedef struct Node {
    char op; // Operator or '\0' if it's a number, variable, or pointer
    int value; // Used if it's a number
    char var_name; // Used if it's a variable
    char member_name[64]; // For struct member access
    struct Node *left;
    struct Node *right;
} Node;

// Define a structure for variable storage
typedef struct Variable {
    char name;
    int value;
    void *struct_value; // Pointer to a structure (for struct support)
    void *pointer_value; // Pointer for handling pointers
} Variable;

Variable variables[26]; // Support for 26 single-letter variables (a-z)

// Forward declarations
Node *expression();
Node *term();
Node *factor();
Node *unary();
Node *logical();
Node *boolean_expr();

const char *input; // Input string for the parser

// Function to consume whitespace
void skip_whitespace() {
    while (isspace(*input)) {
        input++;
    }
}

// Function to match a specific character
void match(char expected) {
    skip_whitespace();
    if (*input == expected) {
        input++;
    } else {
        fprintf(stderr, "Error: Expected '%c'\n", expected);
        exit(1);
    }
}

// Create a new tree node
Node *new_node(char op, int value, Node *left, Node *right) {
    Node *node = (Node *)malloc(sizeof(Node));
    if (!node) {
        fprintf(stderr, "Error: Memory allocation failed\n");
        exit(1);
    }
    node->op = op;
    node->value = value;
    node->var_name = '\0';
    node->member_name[0] = '\0';
    node->left = left;
    node->right = right;
    return node;
}

// Create a new variable node
Node *new_var_node(char name) {
    Node *node = (Node *)malloc(sizeof(Node));
    if (!node) {
        fprintf(stderr, "Error: Memory allocation failed\n");
        exit(1);
    }
    node->op = '\0';
    node->value = 0;
    node->var_name = name;
    node->member_name[0] = '\0';
    node->left = NULL;
    node->right = NULL;
    return node;
}

// Create a new struct member access or pointer dereference node
Node *new_member_or_pointer_node(char op, Node *left, const char *member_name) {
    Node *node = (Node *)malloc(sizeof(Node));
    if (!node) {
        fprintf(stderr, "Error: Memory allocation failed\n");
        exit(1);
    }
    node->op = op;
    node->value = 0;
    node->var_name = '\0';
    strncpy(node->member_name, member_name, sizeof(node->member_name) - 1);
    node->member_name[sizeof(node->member_name) - 1] = '\0';
    node->left = left;
    node->right = NULL;
    return node;
}

// Parse a number, variable, or pointer
Node *number_or_variable() {
    skip_whitespace();
    if (isdigit(*input)) {
        int result = 0;
        while (isdigit(*input)) {
            result = result * 10 + (*input - '0');
            input++;
        }
        return new_node('\0', result, NULL, NULL);
    } else if (isalpha(*input)) {
        char var_name = *input;
        input++;
        Node *var_node = new_var_node(var_name);
        while (*input == '.' || (*input == '-' && *(input + 1) == '>') || *input == '*') {
            if (*input == '*') {
                input++;
                var_node = new_node('*', 0, var_node, NULL);
            } else {
                char op = *input;
                input += (op == '-') ? 2 : 1; // Skip '.' or '->'
                char member_name[64];
                int i = 0;
                while (isalnum(*input) || *input == '_') {
                    if (i < sizeof(member_name) - 1) {
                        member_name[i++] = *input;
                    }
                    input++;
                }
                member_name[i] = '\0';
                var_node = new_member_or_pointer_node(op, var_node, member_name);
            }
        }
        return var_node;
    } else {
        fprintf(stderr, "Error: Expected a number or variable\n");
        exit(1);
    }
}

// Parse a factor (a number, variable, or parenthesized expression or a power operation)
Node *factor() {
    skip_whitespace();
    if (*input == '(') {
        match('(');
        Node *result = expression();
        match(')');
        return result;
    } else {
        Node *base = number_or_variable();
        if (*input == '^') {
            input++;
            Node *exponent = factor();
            return new_node('^', 0, base, exponent);
        }
        return base;
    }
}

// Parse a unary operator
Node *unary() {
    skip_whitespace();
    if (*input == '-' || *input == '+' || *input == '*') {
        char op = *input;
        input++;
        Node *operand = unary();
        return new_node(op, 0, operand, NULL);
    }
    return factor();
}

// Parse a term (a unary optionally followed by '*', '/', or '%')
Node *term() {
    Node *result = unary();
    while (*input == '*' || *input == '/' || *input == '%') {
        char op = *input;
        input++;
        Node *right = unary();
        result = new_node(op, 0, result, right);
    }
    return result;
}

// Parse a boolean expression (a term optionally followed by relational operators)
Node *boolean_expr() {
    Node *result = term();
    while (*input == '<' || *input == '>' || *input == '=' || (*input == '!' && *(input + 1) == '=')) {
        char op = *input;
        input++;
        if (op == '=' || op == '!') {
            match('=');
            op = (op == '=') ? 'E' : 'N'; // 'E' for '==' and 'N' for '!='
        }
        Node *right = term();
        result = new_node(op, 0, result, right);
    }
    return result;
}

// Parse a logical expression (a boolean expression optionally followed by '&&' or '||')
Node *logical() {
    Node *result = boolean_expr();
    while ((*input == '&' && *(input + 1) == '&') || (*input == '|' && *(input + 1) == '|')) {
        char op = (*input == '&') ? 'A' : 'O'; // 'A' for '&&', 'O' for '||'
        input += 2;
        Node *right = boolean_expr();
        result = new_node(op, 0, result, right);
    }
    return result;
}

// Parse an expression (a logical expression optionally followed by '+' or '-')
Node *expression() {
    return logical();
}

// Evaluate the expression tree
int evaluate(Node *node) {
    if (!node) return 0;
    if (node->op == '\0') {
        if (node->var_name != '\0') {
            int idx = node->var_name - 'a';
            Variable *var = &variables[idx];
            if (node->left && node->left->op == '*') { // Dereference pointer
                return *((int *)var->pointer_value);
            }
            return var->value;
        }
        return node->value;
    }

    int left_val = node->left ? evaluate(node->left) : 0;
    int right_val = node->right ? evaluate(node->right) : 0;

    switch (node->op) {


      int evaluate(Node *node) {
    if (!node) return 0;

    if (node->op == '\0') {
        if (node->var_name != '\0') {
            int idx = node->var_name - 'a';
            Variable *var = &variables[idx];
            if (node->left && node->left->op == '*') { // Dereference pointer
                if (!var->pointer_value) {
                    fprintf(stderr, "Error: Null pointer dereference\n");
                    exit(1);
                }
                return *((int *)var->pointer_value);
            }
            return var->value;
        }
        return node->value;
    }

    int left_val = node->left ? evaluate(node->left) : 0;
    int right_val = node->right ? evaluate(node->right) : 0;

    switch (node->op) {
        case '+': return left_val + right_val;
        case '-': return left_val - right_val;
        case '*': return left_val * right_val;
        case '/': 
            if (right_val == 0) {
                fprintf(stderr, "Error: Division by zero\n");
                exit(1);
            }
            return left_val / right_val;
        case '%': 
            if (right_val == 0) {
                fprintf(stderr, "Error: Division by zero\n");
                exit(1);
            }
            return left_val % right_val;
        case '^': return pow(left_val, right_val);
        case '<': return left_val < right_val;
        case '>': return left_val > right_val;
        case 'E': return left_val == right_val; // '==' operator
        case 'N': return left_val != right_val; // '!=' operator
        case 'A': return left_val && right_val; // '&&' operator
        case 'O': return left_val || right_val; // '||' operator
        case '.':
        case '-': // Struct member access
            if (node->op == '-' && node->left && node->left->op == '*') {
                void *ptr = evaluate(node->left);
                return *(int *)((char *)ptr + offsetof(struct_type, node->member_name)); // Adjust as needed
            }
            fprintf(stderr, "Error: Unsupported struct member access\n");
            exit(1);
        default:
            fprintf(stderr, "Error: Unknown operator '%c'\n", node->op);
            exit(1);
    }
    return 0; // Should never reach here
}
