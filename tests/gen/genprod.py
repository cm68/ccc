#!/usr/bin/env python3
#
# Generate the production-coverage corpus: C programs that walk the
# operator x width x operand-residence matrix and check every answer.
#
# Two directions, one corpus:
#
#   completeness - every shape the language can put in front of pass2
#       must have a rule.  The build leg compiles each generated file
#       with -O -s and fails on any XXXXXX marker, which is the
#       comment pass2 leaves where it emitted NOTHING.
#
#   coverage - every rule in pass2's table should be reachable by some
#       program.  rulecover.py counts which rules fire over this
#       corpus (plus the tree); the blessed list of never-fired rules
#       is tests/gen/unfired.ok, and the regression is that the list
#       must not grow.
#
# Correctness rides along: every check's expected value is computed
# here, in Python, with C's arithmetic modelled exactly at each width.
# The same programs run native (gcc -m32, the reference) and under the
# simulator (ccc), so a shape that compiles to the wrong answer fails
# even though it compiled cleanly.
#
# Everything is deterministic - fixed seed - so a corpus regenerated
# anywhere is byte-identical and check numbers in a failure report
# mean the same thing everywhere.
#
# The tree's restrictions apply to what is emitted: K&R declarations,
# no auto aggregate initialisers, shorts where int width would differ,
# check numbers <= 250 because the answer travels in an 8-bit exit
# status.
#
import random, os, sys

OUT = os.path.join(os.path.dirname(os.path.abspath(__file__)), '..', 'run')

# ---------------------------------------------------------------- widths
# letter -> (C type, bits, signed)
WIDTHS = {
    'b': ('char', 8, True),
    'B': ('unsigned char', 8, False),
    's': ('short', 16, True),
    'S': ('unsigned short', 16, False),
    'l': ('long', 32, True),
    'L': ('unsigned long', 32, False),
}

def mask(w):
    return (1 << WIDTHS[w][1]) - 1

def wrap(v, w):
    bits, signed = WIDTHS[w][1], WIDTHS[w][2]
    v &= (1 << bits) - 1
    if signed and v >= (1 << (bits - 1)):
        v -= (1 << bits)
    return v

def lit(v, w):
    # a negative literal is MINUS number in the tree, which is its own
    # shape - emit some that way on purpose (wrap keeps them exact)
    s = str(v)
    if w in 'lL':
        s += 'L'
    return s

# ------------------------------------------------------ C at each width
def cop(op, a, b, w):
    """The value C gives for a op b with both operands of width w."""
    bits, signed = WIDTHS[w][1], WIDTHS[w][2]
    if op == '+': r = a + b
    elif op == '-': r = a - b
    elif op == '*': r = a * b
    elif op == '&': r = a & b
    elif op == '|': r = a | b
    elif op == '^': r = a ^ b
    elif op == '<<': r = a << b
    elif op == '>>':
        # arithmetic for signed (gcc and the Z80 helpers agree),
        # logical for unsigned
        r = a >> b
    elif op == '/':
        q = abs(a) // abs(b)
        r = -q if (a < 0) != (b < 0) else q
    elif op == '%':
        r = a - cop('/', a, b, w) * b
    else:
        raise Exception(op)
    return wrap(r, w)

# ------------------------------------------------------------- residences
# Each residence knows how to spell an operand of width w and what
# setup its value needs.  The point is that each one reduces to a
# different shape in pass2: a constant, a SYMREF, an INDEX, a REGVAR,
# a DEREF chain, an INDEX into an array.
#
#   k  constant literal
#   g  global scalar
#   l  local scalar (loaded from a global so nothing folds)
#   r  register local (word/byte widths only - longs have no home)
#   p  through a global pointer
#   a  global array element, constant subscript
#   x  global array element, variable subscript
RES = 'kglrpax'

def res_ok(res, w):
    if res == 'r' and w in 'lL':
        return False
    return True

class Fn:
    """One check function being assembled."""
    def __init__(self, name):
        self.name = name
        self.decl = []          # local declarations
        self.body = []
        self.used = set()

    def local(self, name, w):
        t = WIDTHS[w][0]
        key = name + w
        if key in self.used:
            return
        self.used.add(key)
        if name.startswith('r'):
            self.decl.append('\tregister %s %s_%s;' % (t, name, w))
        else:
            self.decl.append('\t%s %s_%s;' % (t, name, w))

    def operand(self, res, w, v, side):
        """The C spelling of an operand of width w holding v.  side is
        1 or 2 - each side owns its storage, so two operands are never
        the same variable and neither setup can disturb the other."""
        if res == 'k':
            return lit(v, w)
        if res == 'g':
            self.body.append('\tg%d_%s = %s;' % (side, w, lit(v, w)))
            return 'g%d_%s' % (side, w)
        if res == 'l':
            self.local('l%d' % side, w)
            self.body.append('\tg%d_%s = %s; l%d_%s = g%d_%s;'
                             % (side, w, lit(v, w), side, w, side, w))
            return 'l%d_%s' % (side, w)
        if res == 'r':
            self.local('r%d' % side, w)
            self.body.append('\tg%d_%s = %s; r%d_%s = g%d_%s;'
                             % (side, w, lit(v, w), side, w, side, w))
            return 'r%d_%s' % (side, w)
        if res == 'p':
            self.body.append('\tc%d_%s = %s; p%d_%s = &c%d_%s;'
                             % (side, w, lit(v, w), side, w, side, w))
            return '*p%d_%s' % (side, w)
        if res == 'a':
            self.body.append('\tv_%s[%d] = %s;' % (w, side, lit(v, w)))
            return 'v_%s[%d]' % (w, side)
        if res == 'x':
            iv = 'i%d' % side
            self.body.append('\t%s = %d; v_%s[%s] = %s;'
                             % (iv, 3 + side, w, iv, lit(v, w)))
            return 'v_%s[%s]' % (w, iv)
        raise Exception(res)

    def lvalue(self, res, w, side):
        """A place to store into, and its C spelling for reading back.
        Uses slot 0 storage, disjoint from either operand's."""
        if res == 'g':
            return 'g0_%s' % w
        if res == 'l':
            self.local('l0', w)
            return 'l0_%s' % w
        if res == 'r':
            self.local('r0', w)
            return 'r0_%s' % w
        if res == 'p':
            self.body.append('\tp0_%s = &c0_%s;' % (w, w))
            return '*p0_%s' % w
        if res == 'a':
            return 'v_%s[0]' % w
        if res == 'x':
            self.body.append('\ti0 = 3;')
            return 'v_%s[i0]' % w
        raise Exception(res)


class File:
    def __init__(self, name, title):
        self.name = name
        self.title = title
        self.fns = []
        self.fn = None
        self.nfn = 0
        self.check = 0
        self.perfn = 0

    def need_fn(self):
        if self.fn is None or self.perfn >= 25:
            self.nfn += 1
            self.fn = Fn('c%d' % self.nfn)
            self.fns.append(self.fn)
            self.perfn = 0
        return self.fn

    def add_check(self, lines):
        """lines produce a value in t_<w>; caller appends the test."""
        self.check += 1
        self.perfn += 1
        if self.check > 250:
            raise Exception('%s: past 250 checks - split the file'
                            % self.name)
        return self.check

    def emit(self):
        out = []
        out.append('/* generated by tests/gen/genprod.py - DO NOT EDIT')
        out.append(' * %s' % self.title)
        out.append(' * regenerate: make -C tests/gen corpus */')
        out.append('#include "rt.h"')
        out.append('')
        for w, (t, bits, signed) in WIDTHS.items():
            for slot in (0, 1, 2):
                out.append('%s g%d_%s; %s c%d_%s; %s *p%d_%s;'
                           % (t, slot, w, t, slot, w, t, slot, w))
            out.append('%s v_%s[7]; %s t_%s;' % (t, w, t, w))
        out.append('short i0, i1, i2;')
        out.append('')
        for fn in self.fns:
            out.append('short')
            out.append('%s()' % fn.name)
            out.append('{')
            for d in sorted(fn.decl):
                out.append(d)
            out.extend(fn.body)
            out.append('\treturn 0;')
            out.append('}')
            out.append('')
        out.append('main()')
        out.append('{')
        out.append('\tshort r;')
        out.append('')
        for fn in self.fns:
            out.append('\tif ((r = %s()) != 0) return r;' % fn.name)
        out.append('\treturn 0;')
        out.append('}')
        path = os.path.join(OUT, self.name + '.c')
        open(path, 'w').write('\n'.join(out) + '\n')
        return self.check


rng = random.Random(1789)

def pick(w, op, side):
    """An operand value that keeps every op well-defined at width w
    in both C dialects (no signed overflow, no wild division)."""
    bits, signed = WIDTHS[w][1], WIDTHS[w][2]
    if op == '*':
        top = {8: 9, 16: 150, 32: 40000}[bits]
        v = rng.randint(1, top)
        if signed and rng.random() < 0.4:
            v = -v
        return v
    if op in ('/', '%'):
        if side == 'r':
            v = rng.randint(1, (1 << (bits - 2)) - 1)
        else:
            v = rng.randint(0, (1 << (bits - 2)) - 1)
        return v
    if op == '<<':
        if side == 'r':
            return rng.randint(0, bits - 2)
        hi = (1 << (bits - 2)) - 1 if signed else (1 << (bits - 1)) - 1
        return rng.randint(0, hi)
    if op == '>>':
        if side == 'r':
            return rng.randint(0, bits - 2)
        v = rng.randint(0, (1 << (bits - 1)) - 1)
        if signed and rng.random() < 0.3:
            v = -v
        return v
    # + - & | ^ : half range keeps sums inside the width
    hi = (1 << (bits - 2)) - 1
    v = rng.randint(0, hi)
    if signed and rng.random() < 0.4:
        v = -v
    return v


def binary_file(name, ops, widths, pairs, title):
    f = File(name, title)
    for op in ops:
        for w in widths:
            for (lr, rr) in pairs:
                if not (res_ok(lr, w) and res_ok(rr, w)):
                    continue
                fn = f.need_fn()
                a = pick(w, op, 'l')
                b = pick(w, op, 'r')
                exp = cop(op, a, b, w)
                n = f.add_check(None)
                ea = fn.operand(lr, w, a, 1)
                eb = fn.operand(rr, w, b, 2)
                fn.body.append('\tt_%s = %s %s %s;' % (w, ea, op, eb))
                fn.body.append('\tif (t_%s != %s) return %d;'
                               % (w, lit(exp, w), n))
    return f.emit()


def cmp_file(name, widths, pairs, title):
    f = File(name, title)
    for op in ('==', '!=', '<', '<=', '>', '>='):
        for w in widths:
            for (lr, rr) in pairs:
                if not (res_ok(lr, w) and res_ok(rr, w)):
                    continue
                fn = f.need_fn()
                a = pick(w, '+', 'l')
                b = a if rng.random() < 0.25 else pick(w, '+', 'r')
                exp = int(eval('a %s b' % op))
                # as a value
                n = f.add_check(None)
                ea = fn.operand(lr, w, a, 1)
                eb = fn.operand(rr, w, b, 2)
                fn.body.append('\tt_s = %s %s %s;' % (ea, op, eb))
                fn.body.append('\tif (t_s != %d) return %d;' % (exp, n))
                # as a condition, both polarities of branch
                n = f.add_check(None)
                fn.body.append('\tif (%s %s %s) t_s = 5; else t_s = 9;'
                               % (ea, op, eb))
                fn.body.append('\tif (t_s != %d) return %d;'
                               % (5 if exp else 9, n))
    return f.emit()


def compound_file(name, widths, title):
    f = File(name, title)
    OPS = ['+=', '-=', '*=', '&=', '|=', '^=', '<<=', '>>=', '/=', '%=']
    for op in OPS:
        bare = op[:-1]
        for w in widths:
            for res in 'glrpax':
                if not res_ok(res, w):
                    continue
                fn = f.need_fn()
                a = pick(w, bare, 'l')
                b = pick(w, bare, 'r')
                if bare in ('/', '%') and w in 'bsl':
                    a = abs(a)      # keep ancient division semantics moot
                exp = cop(bare, a, b, w)
                n = f.add_check(None)
                lv = fn.lvalue(res, w, 1)
                fn.body.append('\t%s = %s;' % (lv, lit(a, w)))
                fn.body.append('\t%s %s %s;' % (lv, op, lit(b, w)))
                fn.body.append('\tif (%s != %s) return %d;'
                               % (lv, lit(exp, w), n))
    return f.emit()


def incdec_file(name, widths, title):
    f = File(name, title)
    for form in ('++X', 'X++', '--X', 'X--'):
        for w in widths:
            for res in 'glrpax':
                if not res_ok(res, w):
                    continue
                fn = f.need_fn()
                a = pick(w, '+', 'l')
                step = 1
                after = wrap(a + (step if '+' in form else -step), w)
                pre = form[0] in '+-'
                n = f.add_check(None)
                lv = fn.lvalue(res, w, 1)
                fn.body.append('\t%s = %s;' % (lv, lit(a, w)))
                # parenthesised: "*p++" would step the pointer, not
                # what it points at
                expr = ('++(' + lv + ')' if form == '++X' else
                        '(' + lv + ')++' if form == 'X++' else
                        '--(' + lv + ')' if form == '--X' else
                        '(' + lv + ')--')
                # value context
                fn.body.append('\tt_%s = %s;' % (w, expr))
                val = after if pre else a
                fn.body.append('\tif (t_%s != %s) return %d;'
                               % (w, lit(val, w), n))
                n = f.add_check(None)
                fn.body.append('\tif (%s != %s) return %d;'
                               % (lv, lit(after, w), n))
                # statement context
                n = f.add_check(None)
                fn.body.append('\t%s = %s;' % (lv, lit(a, w)))
                fn.body.append('\t%s;' % expr)
                fn.body.append('\tif (%s != %s) return %d;'
                               % (lv, lit(after, w), n))
    return f.emit()


def unary_file(name, widths, title):
    f = File(name, title)
    for op in ('-', '~', '!'):
        for w in widths:
            for res in 'kglrpax':
                if not res_ok(res, w):
                    continue
                fn = f.need_fn()
                a = pick(w, '+', 'l')
                if op == '-':
                    exp = wrap(-a, w)
                elif op == '~':
                    exp = wrap(~a, w)
                else:
                    exp = int(a == 0)
                n = f.add_check(None)
                ea = fn.operand(res, w, a, 1)
                if op == '!':
                    fn.body.append('\tt_s = !%s;' % ea)
                    fn.body.append('\tif (t_s != %d) return %d;' % (exp, n))
                else:
                    fn.body.append('\tt_%s = %s%s;' % (w, op, ea))
                    fn.body.append('\tif (t_%s != %s) return %d;'
                                   % (w, lit(exp, w), n))
    return f.emit()


def assign_file(name, widths, title):
    """Plain stores: every residence to every residence, plus the
    value-producing forms r = (x = y) and chains a = b = c."""
    f = File(name, title)
    for w in widths:
        for dst in 'glrpax':
            for src in 'kglrpax':
                if not (res_ok(dst, w) and res_ok(src, w)):
                    continue
                fn = f.need_fn()
                a = pick(w, '+', 'l')
                n = f.add_check(None)
                ea = fn.operand(src, w, a, 1)
                lv = fn.lvalue(dst, w, 3)
                fn.body.append('\t%s = %s;' % (lv, ea))
                fn.body.append('\tif (%s != %s) return %d;'
                               % (lv, lit(a, w), n))
    # value-of-assignment and chains
    for w in widths:
        fn = f.need_fn()
        a = pick(w, '+', 'l')
        n = f.add_check(None)
        fn.body.append('\tt_%s = (g1_%s = %s);' % (w, w, lit(a, w)))
        fn.body.append('\tif (t_%s != %s || g1_%s != %s) return %d;'
                       % (w, lit(a, w), w, lit(a, w), n))
        n = f.add_check(None)
        fn.body.append('\tc1_%s = 0; p1_%s = &c1_%s;' % (w, w, w))
        fn.body.append('\tg1_%s = *p1_%s = %s;' % (w, w, lit(a, w)))
        fn.body.append('\tif (g1_%s != %s || c1_%s != %s) return %d;'
                       % (w, lit(a, w), w, lit(a, w), n))
        # assignment as a condition
        n = f.add_check(None)
        fn.body.append('\tif ((g1_%s = %s) != %s) return %d;'
                       % (w, lit(a, w), lit(a, w), n))
    return f.emit()


def logic_file(name, title):
    f = File(name, title)
    for w in ('b', 's', 'S', 'l'):
        for op in ('&&', '||'):
            for (av, bv) in ((0, 0), (0, 3), (3, 0), (3, 5)):
                fn = f.need_fn()
                exp = int(eval('bool(av) %s bool(bv)' %
                               ('and' if op == '&&' else 'or')))
                n = f.add_check(None)
                ea = fn.operand('g', w, av, 1)
                eb = fn.operand('p', w, bv, 2)
                fn.body.append('\tt_s = %s %s %s;' % (ea, op, eb))
                fn.body.append('\tif (t_s != %d) return %d;' % (exp, n))
                n = f.add_check(None)
                fn.body.append('\tif (%s %s %s) t_s = 4; else t_s = 8;'
                               % (ea, op, eb))
                fn.body.append('\tif (t_s != %d) return %d;'
                               % (4 if exp else 8, n))
    # ternary at every width, both arms
    for w in WIDTHS:
        fn = f.need_fn()
        a = pick(w, '+', 'l')
        b = pick(w, '+', 'r')
        for cond in (0, 1):
            n = f.add_check(None)
            fn.body.append('\tg1_s = %d;' % cond)
            fn.body.append('\tt_%s = g1_s ? %s : %s;'
                           % (w, lit(a, w), lit(b, w)))
            fn.body.append('\tif (t_%s != %s) return %d;'
                           % (w, lit(a if cond else b, w), n))
    return f.emit()


def widen_file(name, title):
    """Conversions: every width to every width, both directions of
    signedness, through a store - WIDEN, SEXT and NARROW shapes."""
    f = File(name, title)
    for src in WIDTHS:
        for dst in WIDTHS:
            if src == dst:
                continue
            fn = f.need_fn()
            a = pick(src, '+', 'l')
            exp = wrap(a, dst)
            n = f.add_check(None)
            ea = fn.operand('g', src, a, 1)
            fn.body.append('\tt_%s = (%s)%s;'
                           % (dst, WIDTHS[dst][0], ea))
            fn.body.append('\tif (t_%s != %s) return %d;'
                           % (dst, lit(exp, dst), n))
    return f.emit()


def ptr_file(name, title):
    """Pointer shapes: deref chains, member access, address-of,
    pointer arithmetic, array walks."""
    f = File(name, title)
    fn = f.need_fn()
    body = fn.body
    def chk(cond):
        n = f.add_check(None)
        body.append('\tif (!(%s)) return %d;' % (cond, n))
    body.append('\tv_s[0] = 10; v_s[1] = 11; v_s[2] = 12; v_s[3] = 13;')
    body.append('\tp1_s = &v_s[1];')
    chk('*p1_s == 11')
    chk('p1_s[1] == 12')
    chk('*(p1_s + 2) == 13')
    chk('*(p1_s - 1) == 10')
    chk('p1_s - v_s == 1')
    chk('&v_s[3] - p1_s == 2')
    body.append('\tp1_s++;')
    chk('*p1_s == 12')
    body.append('\tp1_s--; p1_s--;')
    chk('*p1_s == 10')
    fn = f.need_fn()
    body = fn.body
    body.append('\tc1_l = 70000L; p1_l = &c1_l;')
    chk('*p1_l == 70000L')
    body.append('\t*p1_l += 1L;')
    chk('c1_l == 70001L')
    body.append('\tv_l[2] = 5L; p1_l = v_l;')
    chk('p1_l[2] == 5L')
    body.append('\tp1_b = &c1_b; c1_b = 7;')
    chk('*p1_b == 7')
    body.append('\t(*p1_b)++;')
    chk('c1_b == 8')
    return f.emit()


def main():
    total = 0
    files = []
    P_ALL = [(a, b) for a in 'kglrpax' for b in 'kglrpax'
             if not (a == 'k' and b == 'k')]
    P_SOME = [('g', 'g'), ('g', 'k'), ('k', 'g'), ('l', 'r'), ('r', 'l'),
              ('p', 'g'), ('g', 'p'), ('a', 'x'), ('x', 'k'), ('r', 'k'),
              ('l', 'k'), ('p', 'k'), ('r', 'r'), ('l', 'l'), ('p', 'p')]

    def one(n, c):
        files.append((n, c))
        return c

    total += one('gp_add', binary_file('gp_add', ['+', '-'],
        'bBsSlL', P_SOME, 'addition and subtraction'))
    total += one('gp_bit', binary_file('gp_bit', ['&', '|', '^'],
        'BSL', P_SOME, 'bitwise, unsigned widths'))
    total += one('gp_bits', binary_file('gp_bits', ['&', '|', '^'],
        'bsl', P_SOME[:8], 'bitwise, signed widths'))
    total += one('gp_mul', binary_file('gp_mul', ['*'],
        'bBsSlL', P_SOME, 'multiplication'))
    total += one('gp_div', binary_file('gp_div', ['/', '%'],
        'BSL', P_SOME[:10], 'unsigned division'))
    total += one('gp_divs', binary_file('gp_divs', ['/', '%'],
        'bsl', [('g','g'),('g','k'),('l','k'),('r','k'),('p','g')],
        'signed division'))
    total += one('gp_shift', binary_file('gp_shift', ['<<', '>>'],
        'bBsSlL', P_SOME[:10], 'shifts'))
    total += one('gp_cmpb', cmp_file('gp_cmpb', 'bB',
        [('g','g'),('g','k'),('k','g'),('l','r'),('r','k'),('p','g')],
        'comparisons, byte'))
    total += one('gp_cmpw', cmp_file('gp_cmpw', 'sS',
        [('g','g'),('g','k'),('k','g'),('l','r'),('r','k'),('p','g')],
        'comparisons, word'))
    total += one('gp_cmpl', cmp_file('gp_cmpl', 'lL',
        [('g','g'),('g','k'),('l','k'),('p','g')],
        'comparisons, long'))
    total += one('gp_asgb', assign_file('gp_asgb', 'bB',
        'stores: every residence to every residence, byte'))
    total += one('gp_asgw', assign_file('gp_asgw', 'sS',
        'stores: every residence to every residence, word'))
    total += one('gp_asgl', assign_file('gp_asgl', 'lL',
        'stores: every residence to every residence, long'))
    total += one('gp_cas', compound_file('gp_cas', 'bsl',
        'compound assignment, signed'))
    total += one('gp_casu', compound_file('gp_casu', 'BSL',
        'compound assignment, unsigned'))
    total += one('gp_incb', incdec_file('gp_incb', 'bB',
        'increment and decrement, byte'))
    total += one('gp_incw', incdec_file('gp_incw', 'sS',
        'increment and decrement, word'))
    total += one('gp_incl', incdec_file('gp_incl', 'lL',
        'increment and decrement, long'))
    total += one('gp_una', unary_file('gp_una', 'bBs',
        'unary minus, complement, not: byte and short'))
    total += one('gp_unal', unary_file('gp_unal', 'SlL',
        'unary minus, complement, not: unsigned and long'))
    total += one('gp_log', logic_file('gp_log',
        'logical operators and the ternary'))
    total += one('gp_cvt', widen_file('gp_cvt',
        'width conversions, all pairs'))
    total += one('gp_ptr', ptr_file('gp_ptr', 'pointer shapes'))

    for n, c in files:
        print('%-10s %4d checks' % (n, c))
    print('%-10s %4d checks in %d files' % ('total', total, len(files)))

if __name__ == '__main__':
    main()
