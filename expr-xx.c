// Define operator precedence
typedef struct {
    char op;
    int precedence;
    int is_right_associative; // 1 for right-associative, 0 otherwise
} Operator;

Operator operators[] = {
    {'^', 4, 1}, // Right-associative
    {'*', 3, 0},
    {'/', 3, 0},
    {'%', 3, 0},
    {'+', 2, 0},
    {'-', 2, 0},
    {'<', 1, 0},
    {'>', 1, 0},
    {'E', 1, 0}, // '==' operator
    {'N', 1, 0}, // '!=' operator
    {'A', 0, 0}, // '&&' operator
    {'O', 0, 0}, // '||' operator
    {0, 0, 0}    // Sentinel value
};

// Get precedence of an operator
int get_precedence(char op) {
    for (int i = 0; operators[i].op; i++) {
        if (operators[i].op == op) {
            return operators[i].precedence;
        }
    }
    return -1; // Not an operator
}

// Check if an operator is right-associative
int is_right_associative(char op) {
    for (int i = 0; operators[i].op; i++) {
        if (operators[i].op == op) {
            return operators[i].is_right_associative;
        }
    }
    return 0; // Default is left-associative
}

// Parse an expression using precedence
Node *parse_expression(int min_precedence) {
    Node *left = unary(); // Start with a unary expression

    while (*input) {
        skip_whitespace();
        char op = *input;

        int precedence = get_precedence(op);
        if (precedence < min_precedence) {
            break; // Operator has lower precedence, stop parsing
        }

        input++; // Consume the operator
        Node *right = parse_expression(precedence + (is_right_associative(op) ? 0 : 1));
        left = new_node(op, 0, left, right); // Create a new operator node
    }

    return left;
}

// Adjust the `expression` function to start with the precedence-based parsing
Node *expression() {
    return parse_expression(0); // Start with the lowest precedence
}

Node *parse_expression(int min_precedence);

// Parse a factor (handles numbers, variables, and parenthesized expressions)
Node *factor() {
    skip_whitespace();

    if (*input == '(') {
        input++; // Consume '('
        Node *result = parse_expression(0); // Parse the inner expression
        skip_whitespace();
        if (*input == ')') {
            input++; // Consume ')'
        } else {
            fprintf(stderr, "Error: Expected ')'\n");
            exit(1);
        }
        return result;
    }

    return number_or_variable(); // Parse a number or variable
}

// Parse an expression using precedence, now with parentheses handling
Node *parse_expression(int min_precedence) {
    Node *left = factor(); // Start with a factor

    while (*input) {
        skip_whitespace();
        char op = *input;

        int precedence = get_precedence(op);
        if (precedence < min_precedence) {
            break; // Operator has lower precedence, stop parsing
        }

        input++; // Consume the operator
        Node *right = parse_expression(precedence + (is_right_associative(op) ? 0 : 1));
        left = new_node(op, 0, left, right); // Create a new operator node
    }

    return left;
}

// Adjust the `expression` function to start with the precedence-based parsing
Node *expression() {
    return parse_expression(0); // Start with the lowest precedence
}


