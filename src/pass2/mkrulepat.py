#!/usr/bin/env python3
#
# Regenerate rulepat[] - the debug spellings of the rewrite rules -
# from the rules[] table itself.
#
# The two used to be maintained by hand, in parallel, and drifted the
# first time rules were added without names: every trace line and
# every coverage report after the insertion point named the WRONG
# rule.  A parallel array nobody checks is a lie waiting to happen,
# so this derives the names and rewrites the block in place; the
# Makefile runs it before every build of rules.c.
#
# Spelling: op(left,right)[:width][dest], the same shapes the hand
# list used.  A DEREF child spells its own child inside D(..).
#
import re, sys, os

SRC = os.path.join(os.path.dirname(os.path.abspath(__file__)), 'rules.c')

OPCH = {
    'ASSIGN': '=', 'PLUS': '+', 'MINUS': '-', 'STAR': '*', 'DIV': '/',
    'MOD': '%', 'AND': '&', 'OR': '|', 'XOR': '^',
    'LSHIFT': 'y', 'RSHIFT': 'w', 'EQ': 'e', 'NEQ': 'n', 'LT': '<',
    'GT': '>', 'LE': 'q', 'GE': 'p', 'LAND': 'a', 'LOR': 'o',
    'BANG': '!', 'NEG': 'm', 'NOT': '~', 'DEREF': 'D', 'WIDEN': 'W',
    'SEXT': 'X', 'NARROW': 'Z', 'CALL': 'c', 'COMMA': ';',
    'PREINC': 'i', 'POSTINC': 'j', 'PREDEC': 'd', 'POSTDEC': 'k',
    'QUES': 'Q', 'TERNBRANCH': 'T', 'REGVAR': 'V', 'LOCALVAR': 'L',
    'INDEX': 'I', 'SYMREF': 'O', 'SYM': 'S', 'NUMBER': 'G',
    'INHL': 'H', 'INDE': 'E', 'INBC': 'B', 'INA': 'A', 'INE': 'K',
    'CODE': 'C', 'ARGNODE': 'R', 'BFEXTRACT': 'F', 'BFASSIGN': 'f',
    'PLUSEQ': 'U', 'LABEL': 'l',
}

def opch(name):
    if name in OPCH:
        return OPCH[name]
    if name.startswith('P_'):
        return name[2:].lower()[:4]
    return '?' + name

def operand(name):
    return opch(name) if name and name != '0' else ''

def spell(args):
    # R(o, lo, ro, llo, rlo, sfx, ...)
    o, lo, ro, llo, rlo, sfx = args[:6]
    l = operand(lo)
    if l and llo != '0':
        l = '%s(%s)' % (l, operand(llo))
    r = operand(ro)
    if r and rlo != '0':
        r = '%s(%s)' % (r, operand(rlo))
    s = opch(o)
    if l or r:
        s += '(' + l + (',' + r if r else '') + ')'
    try:
        n = int(sfx, 0)
    except ValueError:
        n = 0
    w = n & 7
    d = (n >> 3) & 3
    if w:
        s += ':' + 'bsl'[w - 1] if w <= 3 else ':?'
    s += ['', 'F', 'V', 'S'][d]
    return s

def rargs(text):
    # split top-level commas of R( ... ) up to the sixth argument
    depth = 0
    args, cur = [], ''
    for ch in text:
        if ch == '(':
            depth += 1
        elif ch == ')':
            if depth == 0:
                break
            depth -= 1
        if ch == ',' and depth == 0:
            args.append(cur.strip())
            cur = ''
            if len(args) == 6:
                return args
        else:
            cur += ch
    args.append(cur.strip())
    return args[:6]

def main():
    src = open(SRC).read()
    # every R( entry inside rules[]
    start = src.index('struct rule rules[] = {')
    names = []
    for m in re.finditer(r'^\s*R\((.*)$', src[start:], re.M):
        line = m.group(1)
        names.append(spell(rargs(line)))

    block = 'char *rulepat[] = {\n'
    for n in names:
        block += '\t"%s",\n' % n
    block += '};\n'

    a = src.index('char *rulepat[] = {')
    b = src.index('};', a) + 3
    out = src[:a] + block + src[b:]
    if out != src:
        open(SRC, 'w').write(out)
        print('rulepat: %d names regenerated' % len(names))
    else:
        print('rulepat: %d names, unchanged' % len(names))

if __name__ == '__main__':
    main()
