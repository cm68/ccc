#!/usr/bin/env python3
"""
validate_src.py - Lint C sources against the ccc input language (INPUT.md)

This is the input-language half of the self-compile check: every source
must stay inside the dialect cpp itself accepts and the Z80 backend can
compile (see RESTRICTIONS.md).

Hard errors (exit 1):
  1. `const` qualifier anywhere
  2. `signed` qualifier anywhere
  3. Auto aggregate initializer: `= {` inside a function body
     (file-scope and `static` initializers are fine - they live in data)

Warnings (reported, exit 0):
  4. Function that appears to return a struct/union by value
  5. Identifier longer than 14 characters (object format limit is 15,
     minus C's leading underscore)

Comments, string literals, and char constants are stripped (newlines
preserved) before any check runs.
"""

import sys
import re

IDENT_MAX = 14


def strip_noise(text):
    """Remove comments and string/char literal bodies, keeping newlines
    so line numbers stay accurate."""
    out = []
    i = 0
    n = len(text)
    while i < n:
        c = text[i]
        if c == '/' and i + 1 < n and text[i + 1] == '*':
            j = text.find('*/', i + 2)
            if j < 0:
                j = n - 2
            out.append(re.sub(r'[^\n]', ' ', text[i:j + 2]))
            i = j + 2
        elif c == '/' and i + 1 < n and text[i + 1] == '/':
            j = text.find('\n', i)
            if j < 0:
                j = n
            out.append(' ' * (j - i))
            i = j
        elif c == '"' or c == "'":
            quote = c
            j = i + 1
            while j < n and text[j] != quote:
                if text[j] == '\\':
                    j += 1
                j += 1
            j = min(j + 1, n)
            out.append(quote + ' ' * max(0, j - i - 2) + quote)
            i = j
        else:
            out.append(c)
            i += 1
    return ''.join(out)


TOKEN_RE = re.compile(r'[A-Za-z_][A-Za-z0-9_]*|[{};=#*(),]|\n|[^\sA-Za-z0-9_]')


def tokenize(text):
    """Return list of (value, line). '#'-lines are dropped entirely
    (preprocessor directives are cpp's own input, not C to compile)."""
    toks = []
    line = 1
    at_bol = True
    skipping = False
    i = 0
    n = len(text)
    for m in TOKEN_RE.finditer(text):
        v = m.group()
        if v == '\n':
            line += 1
            # a directive ends at an unescaped newline
            if skipping and text[m.start() - 1:m.start()] != '\\':
                skipping = False
            at_bol = True
            continue
        if skipping:
            continue
        if v == '#' and at_bol:
            skipping = True
            continue
        at_bol = False
        toks.append((v, line))
    return toks


def is_ident(v):
    return re.match(r'[A-Za-z_]', v) is not None


def validate(filename):
    errors = []
    warnings = []

    with open(filename, 'r') as f:
        text = strip_noise(f.read())

    toks = tokenize(text)
    n = len(toks)
    depth = 0
    stmt_start = 0          # index of first token of current statement/decl
    long_seen = set()

    for i in range(n):
        v, line = toks[i]

        if v == '{':
            depth += 1
        elif v == '}':
            depth = max(0, depth - 1)

        if v in ('{', '}', ';'):
            stmt_start = i + 1
            continue

        if v == 'const' or v == 'signed':
            errors.append("%s:%d: '%s' qualifier not allowed"
                          % (filename, line, v))
            continue

        # auto aggregate initializer: `= {` inside a function body,
        # unless the declaration is static (initialized at link time)
        if v == '=' and depth > 0 and i + 1 < n and toks[i + 1][0] == '{':
            stmt = [t[0] for t in toks[stmt_start:i]]
            if 'static' not in stmt:
                errors.append("%s:%d: auto aggregate initializer"
                              % (filename, line))
            continue

        # struct/union return by value: at file scope,
        # `struct tag name (` with no `*` in between
        if v in ('struct', 'union') and depth == 0 and i + 3 < n:
            a, b, c = toks[i + 1][0], toks[i + 2][0], toks[i + 3][0]
            if is_ident(a) and is_ident(b) and c == '(':
                warnings.append("%s:%d: function '%s' may return %s by value"
                                % (filename, toks[i + 2][1], b, v))
            continue

        if is_ident(v) and len(v) > IDENT_MAX and v not in long_seen:
            long_seen.add(v)
            warnings.append("%s:%d: identifier '%s' longer than %d chars"
                            % (filename, line, v, IDENT_MAX))

    return errors, warnings


def main():
    if len(sys.argv) < 2:
        print("Usage: validate_src.py <file.c> [file2.c ...]")
        sys.exit(1)

    all_errors = []
    for filename in sys.argv[1:]:
        errors, warnings = validate(filename)
        for w in warnings:
            print("warning: %s" % w)
        all_errors.extend(errors)

    if all_errors:
        for e in all_errors:
            print(e)
        sys.exit(1)
    print("OK: %d file(s) validated" % (len(sys.argv) - 1))
    sys.exit(0)


if __name__ == '__main__':
    main()
