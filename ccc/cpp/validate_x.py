#!/usr/bin/env python3
"""
validate_x.py - Validate binary .x lexeme streams against OUTPUT.md

This is the output-language half of the self-compile check: pass1 must be
able to consume this stream, so everything OUTPUT.md promises is verified.

Wire format (OUTPUT.md section 1-4):
  1. Every token code is one cpp can emit
  2. Payloads are complete (no truncation)
  3. Stream ends with exactly one E_O_F, no trailing bytes
  4. Braces balance (never negative, zero at EOF)

Normalized grammar (OUTPUT.md section 5, what the filters guarantee):
  5. No WHILE/FOR/DO/BREAK/CONTINUE tokens (filtctrl lowers loops)
  6. No CONST/VOLATILE tokens (dropped by emit.c)
  7. No SIZEOF_KW (normalized to SIZEOF)
  8. Every if/switch condition is followed by a braced body
  9. Every else body is braced (else-if allowed)

Line markers (LINENO/NEWLINE) are decoded for error reporting and skipped
when checking token adjacency.
"""

import sys

# Token codes from lexeme.h
E_O_F   = 0
SEMI    = 1
BEGIN   = 2
END     = 3
LPAR    = 6
RPAR    = 7
SYM     = 20
NUMBER  = 21
STRING  = 22
FNUMBER = 23
LNUMBER = 25
SYMID   = 26        # 2-byte interned id; spelling in the .n sidecar
LABELID = 27
SIZEOF  = 91
LABEL   = 112
LINENO  = 116
NEWLINE = 117
ASMSTR  = 118

IF       = 147
WHILE    = 148
ELSE     = 149
SWITCH   = 150
BREAK    = 152
CONTINUE = 153
DO       = 154
FOR      = 156
CONST    = 158
VOLATILE = 159
SIZEOF_KW = 160

KW_FIRST = 128
KW_LAST  = 160

# Simple one-byte tokens cpp can emit (besides keywords)
SIMPLE = set(range(1, 10)) | {
    30, 31, 34, 35, 36, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50,
    53, 54,                                       # unary/binary operators
    60, 61, 62, 63, 64, 65,                       # relational
    70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80,   # assignment
    90, 91, 92,                                   # QUES SIZEOF ELLIPSIS
} | set(range(KW_FIRST, KW_LAST + 1))

FORBIDDEN = {
    WHILE: 'WHILE', FOR: 'FOR', DO: 'DO',
    BREAK: 'BREAK', CONTINUE: 'CONTINUE',
    CONST: 'CONST', VOLATILE: 'VOLATILE', SIZEOF_KW: 'SIZEOF_KW',
}

TOKNAME = {
    SEMI: ';', BEGIN: '{', END: '}', LPAR: '(', RPAR: ')',
    SYM: 'SYM', NUMBER: 'NUMBER', STRING: 'STRING', LABEL: 'LABEL',
    SYMID: 'SYMID', LABELID: 'LABELID',
    IF: 'if', ELSE: 'else', SWITCH: 'switch',
}


def tokname(code):
    return TOKNAME.get(code, str(code))


def decode(data, filename, errors):
    """Decode the wire layer. Returns list of (code, line, srcfile) for
    grammar-relevant tokens (line markers consumed, not returned)."""
    toks = []
    pos = 0
    n = len(data)
    line = 0
    srcfile = '?'
    depth = 0
    ended = False

    def err(msg):
        errors.append("%s: offset 0x%x (%s:%d): %s"
                      % (filename, pos, srcfile, line, msg))

    while pos < n:
        start = pos
        code = data[pos]
        pos += 1

        if code == E_O_F:
            ended = True
            if pos != n:
                errors.append("%s: %d trailing byte(s) after E_O_F"
                              % (filename, n - pos))
            break

        if code == NEWLINE:
            line += 1
            continue

        if code == LINENO:
            if pos + 3 > n:
                err("truncated LINENO header")
                return toks
            line = data[pos] | (data[pos + 1] << 8)
            namelen = data[pos + 2]
            pos += 3
            if pos + namelen > n:
                err("truncated LINENO filename")
                return toks
            srcfile = data[pos:pos + namelen].decode('latin-1')
            pos += namelen
            continue

        if code in (SYMID, LABELID):
            if pos + 2 > n:
                err("truncated %s payload" % tokname(code))
                return toks
            pos += 2
            # the parser below treats these as their named forms
            toks.append((SYM if code == SYMID else LABEL, line, srcfile))
            continue

        if code in (SYM, LABEL):
            if pos >= n:
                err("truncated %s length" % tokname(code))
                return toks
            namelen = data[pos]
            pos += 1
            if pos + namelen > n:
                err("truncated %s payload" % tokname(code))
                return toks
            pos += namelen
        elif code in (NUMBER, FNUMBER, LNUMBER):
            if pos + 4 > n:
                err("truncated %s payload" % tokname(code))
                return toks
            pos += 4
        elif code in (STRING, ASMSTR):
            if pos + 2 > n:
                err("truncated %s length" % tokname(code))
                return toks
            slen = data[pos] | (data[pos + 1] << 8)
            pos += 2
            if pos + slen > n:
                err("truncated %s payload" % tokname(code))
                return toks
            pos += slen
        elif code in SIMPLE:
            pass
        else:
            pos = start  # report at the bad byte
            err("invalid token code %d (0x%02x)" % (code, code))
            return toks

        if code == BEGIN:
            depth += 1
        elif code == END:
            depth -= 1
            if depth < 0:
                err("unmatched '}'")
                depth = 0

        toks.append((code, line, srcfile))

    if not ended:
        errors.append("%s: stream does not end with E_O_F" % filename)
    if depth > 0:
        errors.append("%s: %d unmatched '{' at EOF" % (filename, depth))

    return toks


def check_grammar(toks, filename, errors):
    n = len(toks)

    def err(i, msg):
        if i < n:
            _, line, srcfile = toks[i]
        elif toks:
            _, line, srcfile = toks[-1]
        else:
            line, srcfile = 0, '?'
        errors.append("%s (%s:%d): %s" % (filename, srcfile, line, msg))

    def skip_parens(i):
        """toks[i] must be LPAR; return index past matching RPAR or -1."""
        depth = 0
        while i < n:
            c = toks[i][0]
            if c == LPAR:
                depth += 1
            elif c == RPAR:
                depth -= 1
                if depth == 0:
                    return i + 1
            i += 1
        return -1

    i = 0
    while i < n:
        code = toks[i][0]

        if code in FORBIDDEN:
            err(i, "forbidden token %s in output" % FORBIDDEN[code])
            i += 1
            continue

        if code in (IF, SWITCH):
            kw = tokname(code)
            if i + 1 >= n or toks[i + 1][0] != LPAR:
                err(i, "'%s' not followed by '('" % kw)
                i += 1
                continue
            j = skip_parens(i + 1)
            if j < 0:
                err(i, "'%s' with unbalanced parens" % kw)
                break
            if j >= n or toks[j][0] != BEGIN:
                got = tokname(toks[j][0]) if j < n else 'EOF'
                err(i, "'%s' body not braced (got '%s')" % (kw, got))
            i = j
            continue

        if code == ELSE:
            if i + 1 >= n or toks[i + 1][0] not in (BEGIN, IF):
                got = tokname(toks[i + 1][0]) if i + 1 < n else 'EOF'
                err(i, "'else' body not braced (got '%s')" % got)
            i += 1
            continue

        i += 1


def validate(filename):
    errors = []
    with open(filename, 'rb') as f:
        data = f.read()
    if len(data) == 0:
        return ["%s: empty file" % filename]
    toks = decode(data, filename, errors)
    check_grammar(toks, filename, errors)
    return errors


def main():
    if len(sys.argv) < 2:
        print("Usage: validate_x.py <file.x> [file2.x ...]")
        sys.exit(1)

    all_errors = []
    for filename in sys.argv[1:]:
        all_errors.extend(validate(filename))

    if all_errors:
        for e in all_errors:
            print(e)
        sys.exit(1)
    print("OK: %d file(s) validated" % (len(sys.argv) - 1))
    sys.exit(0)


if __name__ == '__main__':
    main()
