#!/usr/bin/env python3
"""
validate_i.py - Validate preprocessed .i files

Checks that:
1. No for, while, do loops remain (should be lowered by filtctrl)
2. Every if() body is braced
3. Every else body is braced
"""

import sys
import re

def tokenize(text):
    """Simple tokenizer for C-like syntax."""
    # Remove comments first
    text = re.sub(r'/\*.*?\*/', ' ', text, flags=re.DOTALL)
    text = re.sub(r'//.*$', ' ', text, flags=re.MULTILINE)

    # Token patterns
    patterns = [
        ('STRING', r'"(?:[^"\\]|\\.)*"'),
        ('CHAR', r"'(?:[^'\\]|\\.)*'"),
        ('IDENT', r'[a-zA-Z_][a-zA-Z0-9_]*'),
        ('NUMBER', r'[0-9]+(?:\.[0-9]*)?(?:[eE][+-]?[0-9]+)?[fFlLuU]*'),
        ('LBRACE', r'\{'),
        ('RBRACE', r'\}'),
        ('LPAREN', r'\('),
        ('RPAREN', r'\)'),
        ('SEMI', r';'),
        ('HASH', r'#'),
        ('NEWLINE', r'\n'),
        ('WS', r'[ \t]+'),
        ('OP', r'->|[+\-*/%&|^~!<>=.,?:]'),
        ('OTHER', r'.'),
    ]

    combined = '|'.join(f'(?P<{name}>{pat})' for name, pat in patterns)

    tokens = []
    line = 1
    for m in re.finditer(combined, text):
        kind = m.lastgroup
        value = m.group()
        if kind == 'NEWLINE':
            line += 1
            continue
        if kind == 'WS':
            continue
        # Handle # line directives
        if kind == 'HASH':
            continue
        tokens.append((kind, value, line))

    return tokens

def validate(filename):
    """Validate a .i file. Returns list of errors."""
    errors = []

    with open(filename, 'r') as f:
        text = f.read()

    tokens = tokenize(text)

    i = 0
    n = len(tokens)

    def peek(offset=0):
        if i + offset < n:
            return tokens[i + offset]
        return ('EOF', '', -1)

    def current():
        return peek(0)

    def advance():
        nonlocal i
        i += 1

    def skip_parens():
        """Skip balanced parentheses, return True if successful."""
        if current()[1] != '(':
            return False
        depth = 0
        while i < n:
            tok = current()
            if tok[1] == '(':
                depth += 1
            elif tok[1] == ')':
                depth -= 1
                if depth == 0:
                    advance()
                    return True
            advance()
        return False

    while i < n:
        tok = current()
        kind, value, line = tok

        # Check for forbidden loops
        if kind == 'IDENT' and value in ('for', 'while', 'do'):
            # Check it's actually a keyword usage, not part of identifier
            next_tok = peek(1)
            if value == 'for' and next_tok[1] == '(':
                errors.append(f"{filename}:{line}: 'for' loop not lowered")
            elif value == 'while' and next_tok[1] == '(':
                # Could be do-while, check context
                errors.append(f"{filename}:{line}: 'while' loop not lowered")
            elif value == 'do' and next_tok[1] == '{':
                errors.append(f"{filename}:{line}: 'do' loop not lowered")
            advance()
            continue

        # Check if() has braced body
        if kind == 'IDENT' and value == 'if':
            if_line = line
            advance()

            # Skip condition
            if current()[1] != '(':
                errors.append(f"{filename}:{if_line}: 'if' without condition")
                continue

            if not skip_parens():
                errors.append(f"{filename}:{if_line}: 'if' with unbalanced parens")
                continue

            # Check for brace
            if current()[1] != '{':
                errors.append(f"{filename}:{if_line}: 'if' body not braced (got '{current()[1]}')")
            continue

        # Check else has braced body
        if kind == 'IDENT' and value == 'else':
            else_line = line
            advance()

            # else if is OK - will be checked as if
            if current()[0] == 'IDENT' and current()[1] == 'if':
                continue

            # Check for brace
            if current()[1] != '{':
                errors.append(f"{filename}:{else_line}: 'else' body not braced (got '{current()[1]}')")
            continue

        advance()

    return errors

def main():
    if len(sys.argv) < 2:
        print("Usage: validate_i.py <file.i> [file2.i ...]")
        sys.exit(1)

    all_errors = []
    for filename in sys.argv[1:]:
        errors = validate(filename)
        all_errors.extend(errors)

    if all_errors:
        for err in all_errors:
            print(err)
        sys.exit(1)
    else:
        print(f"OK: {len(sys.argv)-1} file(s) validated")
        sys.exit(0)

if __name__ == '__main__':
    main()
