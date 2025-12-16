#!/usr/bin/env python3
"""
AST Pretty Printer for ccc compiler

Format:
  Names: 2-hex-len + hex-bytes (e.g., 03666f6f = "foo")
  Counts: 2 hex digits (00-ff)
  Sizes: 4 hex digits (0000-ffff)
  Constants: 8 hex digits with optional leading -
  Top-level: F (function), Z (global), U (string)
  Statements: B (block), I (if), E (expr), R (return), etc.
  Expressions: operator + width suffix + operands
"""

import sys

sys.setrecursionlimit(10000)

class ASTParser:
    def __init__(self, data):
        self.data = data
        self.pos = 0
        self.indent = 0
        self.block_cnt = 0

    def cur(self):
        if self.pos < len(self.data):
            return self.data[self.pos]
        return None

    def peek(self, n=1):
        p = self.pos + n
        if p < len(self.data):
            return self.data[p]
        return None

    def advance(self):
        if self.pos < len(self.data):
            self.pos += 1

    def skip_whitespace(self):
        while self.cur() and self.cur() in ' \t\n\r':
            self.advance()

    def hval(self, c):
        """Convert hex char to value"""
        if c and c in '0123456789':
            return int(c)
        if c and c in 'abcdef':
            return 10 + ord(c) - ord('a')
        if c and c in 'ABCDEF':
            return 10 + ord(c) - ord('A')
        return 0

    def read_hex2(self):
        """Read 2 hex chars as a byte (count/flag)"""
        h = self.hval(self.cur())
        self.advance()
        l = self.hval(self.cur())
        self.advance()
        return h * 16 + l

    def read_hex4(self):
        """Read 4 hex chars as 16-bit value (size)"""
        v = 0
        for _ in range(4):
            v = v * 16 + self.hval(self.cur())
            self.advance()
        return v

    def read_hex8(self):
        """Read 8 hex chars as 32-bit value (constant), with optional -"""
        neg = False
        if self.cur() == '-':
            neg = True
            self.advance()
        v = 0
        for _ in range(8):
            v = v * 16 + self.hval(self.cur())
            self.advance()
        return -v if neg else v

    def read_name(self):
        """Read hex-length-prefixed name (length in hex, followed by raw ASCII)"""
        length = self.read_hex2()
        chars = []
        for _ in range(length):
            c = self.cur()
            if c:
                chars.append(c)
            self.advance()
        return ''.join(chars)

    def indent_str(self):
        return '  ' * self.indent

    def prln(self, s):
        print(f"{self.indent_str()}{s}")

    # Width suffix mapping
    WIDTH_NAMES = {
        'b': 'byte', 'B': 'ubyte',
        's': 'short', 'S': 'ushort',
        'l': 'long', 'L': 'ulong',
        'p': 'ptr', 'f': 'float', 'd': 'double', 'v': 'void'
    }

    # Register allocation mapping
    REG_NAMES = {
        0: '-',    # REG_NONE
        1: 'B',    # REG_B
        2: 'C',    # REG_C
        3: 'BC',   # REG_BC
        4: 'IX'    # REG_IX
    }

    # Operator names
    OP_NAMES = {
        'M': 'DEREF', '=': 'ASSIGN', '+': 'ADD', '-': 'SUB',
        '*': 'MUL', '/': 'DIV', '%': 'MOD', '&': 'AND',
        '|': 'OR', '^': 'XOR', '~': 'NOT', 'y': 'LSHIFT',
        'w': 'RSHIFT', '<': 'LT', '>': 'GT', 'L': 'LE',
        'g': 'GE', 'Q': 'EQ', 'n': 'NE', 'j': 'LAND',
        'h': 'LOR', '!': 'LNOT', 'N': 'NARROW', 'W': 'WIDEN',
        'X': 'SEXT', '?': 'TERNARY', '@': 'CALL', 'Y': 'COPY',
        '\\': 'NEG', "'": 'ADDR',
        # Compound assignments
        'P': '+=', 'T': '*=',
        '0': '<<=', '1': '|=', '2': '/=', '6': '>>=',
        ':': 'COLON',
    }

    # Operators that are hex digits but should be treated as operators
    # when followed by a width character
    DIGIT_OPS = set('0126')
    WIDTH_CHARS = set('bBsSlLpfdv')

    def is_width_char(self, c):
        return c in self.WIDTH_CHARS if c else False

    def width_name(self, c):
        return self.WIDTH_NAMES.get(c, c)

    def op_name(self, c):
        # Handle special operators (new ASCII encoding)
        special = {
            '(': '++p', ')': 'p++', '{': '--p', '}': 'p--',
            'o': '-=', 'm': '%=', 'a': '&=',
            'e': 'BFEXT', 'f': 'BFSET', 'x': 'SEXT'
        }
        if c in special:
            return special[c]
        return self.OP_NAMES.get(c, c)

    def op_arity(self, c):
        """Return arity of operator"""
        # Inc/dec operators (new ASCII encoding)
        if c in '(){}':
            return 'special'
        # Bitfield operators
        if c in 'ef':
            return 'special'
        # Sign extend
        if c == 'x':
            return 1
        # Unary operators
        if c in "MNW!~\\'":
            return 1
        # Special operators
        if c in '@?Y':
            return 'special'
        # Binary operators (includes compound assignments o, a, m)
        return 2

    def parse_expr(self):
        """Parse and return expression string"""
        self.skip_whitespace()
        c = self.cur()

        if c is None:
            return "(EOF)"

        # Null expression
        if c == '_':
            self.advance()
            return "()"

        # Symbol reference
        if c == '$':
            self.advance()
            name = self.read_name()
            return f"${name}"

        # Stack offset
        if c == 'S':
            self.advance()
            off = self.read_hex4()
            return f"SP[{off}]"

        # Inline string literal: U hexname len hexdata, followed by $name ref
        if c == 'U':
            self.advance()
            name = self.read_name()
            data_len = self.read_hex2()
            # Skip hex-encoded data (2 chars per byte)
            for _ in range(data_len):
                self.advance()
                self.advance()
            # The value is the following expression (usually $name)
            return self.parse_expr()

        # Numeric constant: # followed by size suffix and 8 hex digits
        if c == '#':
            self.advance()
            w = self.cur()
            self.advance()
            v = self.read_hex8()
            return f"{v}:{self.width_name(w)}"

        # Inc/Dec: op width expr delta (4 hex) - ASCII encoding (){}
        if c in '(){}':
            self.advance()
            w = self.cur()
            self.advance()
            e = self.parse_expr()
            delta = self.read_hex4()
            op_name = {'(': 'PREINC', ')': 'POSTINC',
                      '{': 'PREDEC', '}': 'POSTDEC'}[c]
            return f"({op_name}:{self.width_name(w)} {e} {delta})"

        # Bitfield extract: e off(2) wid(2) expr
        if c == 'e':
            self.advance()
            off = self.read_hex2()
            wid = self.read_hex2()
            e = self.parse_expr()
            return f"(BFEXT {off}:{wid} {e})"

        # Bitfield assign: f off(2) wid(2) dest val
        if c == 'f':
            self.advance()
            off = self.read_hex2()
            wid = self.read_hex2()
            dst = self.parse_expr()
            val = self.parse_expr()
            return f"(BFSET {off}:{wid} {dst} {val})"

        # Sign extend (x) - unary with width
        if c == 'x':
            self.advance()
            w = self.cur()
            self.advance()
            e = self.parse_expr()
            return f"(SEXT:{self.width_name(w)} {e})"

        # Regular operator
        op_char = c
        self.advance()

        # Call: @ ret_type argc(2) func args...
        if op_char == '@':
            ret_type = self.cur()
            self.advance()
            argc = self.read_hex2()
            func = self.parse_expr()
            args = [self.parse_expr() for _ in range(argc)]
            args_str = ' '.join(args)
            if args_str:
                return f"(CALL:{ret_type} {func} {args_str})"
            return f"(CALL:{ret_type} {func})"

        # Ternary: ? width nlabels(2) cond then else
        if op_char == '?':
            w = self.cur()
            self.advance()
            nlabels = self.read_hex2()  # skip intermediate label count
            cond = self.parse_expr()
            then_e = self.parse_expr()
            else_e = self.parse_expr()
            return f"(?:{self.width_name(w)} {cond} {then_e} {else_e})"

        # Copy: Y size(4) dest src
        if op_char == 'Y':
            sz = self.read_hex4()
            dst = self.parse_expr()
            src = self.parse_expr()
            return f"(COPY:{sz} {dst} {src})"

        # Regular operator with width suffix
        w = self.cur()
        self.advance()
        arity = self.op_arity(op_char)

        if arity == 1:
            e1 = self.parse_expr()
            return f"({self.op_name(op_char)}:{self.width_name(w)} {e1})"
        else:  # arity == 2
            e1 = self.parse_expr()
            e2 = self.parse_expr()
            return f"({self.op_name(op_char)}:{self.width_name(w)} {e1} {e2})"

    def parse_stmt(self):
        """Parse and print a statement"""
        self.skip_whitespace()
        c = self.cur()
        if c is None:
            return
        self.advance()

        # Block: B decl_count(2) stmt_count(2) decls... stmts...
        # Declarations: d suffix name reg off (reg and off are 2 hex digits each)
        if c == 'B':
            decl_count = self.read_hex2()
            stmt_count = self.read_hex2()
            block_num = self.block_cnt
            self.block_cnt += 1
            self.prln(f"BLOCK {block_num} {{")
            self.indent += 1
            for _ in range(decl_count):
                self.skip_whitespace()
                if self.cur() == 'd':
                    self.advance()
                    type_char = self.cur()
                    self.advance()
                    name = self.read_name()
                    reg = self.read_hex2()
                    off = self.read_hex2()
                    # Convert unsigned to signed offset
                    if off > 127:
                        off = off - 256
                    reg_str = self.REG_NAMES.get(reg, str(reg))
                    off_str = f"IY{off:+d}" if off else "IY"
                    self.prln(f"DECL {name} : {self.width_name(type_char)} reg={reg_str} off={off_str}")
            for _ in range(stmt_count):
                self.parse_stmt()
            self.indent -= 1
            self.prln("}")

        # If: I has_else(2) nlabels(2) cond then [else]
        elif c == 'I':
            has_else = self.read_hex2()
            nlabels = self.read_hex2()  # skip intermediate label count
            cond = self.parse_expr()
            self.prln(f"IF ({cond})")
            self.indent += 1
            self.parse_stmt()
            self.indent -= 1
            if has_else == 1:
                self.prln("ELSE")
                self.indent += 1
                self.parse_stmt()
                self.indent -= 1

        # Expression: E expr
        elif c == 'E':
            e = self.parse_expr()
            self.prln(f"EXPR {e}")

        # Return: R has_value(2) [expr]
        elif c == 'R':
            has_val = self.read_hex2()
            if has_val == 1:
                e = self.parse_expr()
                self.prln(f"RETURN {e}")
            else:
                self.prln("RETURN")

        # Label: L hexname
        elif c == 'L':
            name = self.read_name()
            self.prln(f"LABEL {name}:")

        # Goto: G hexname
        elif c == 'G':
            name = self.read_name()
            self.prln(f"GOTO {name}")

        # Switch: S has_label(2) [hexlabel] case_count(2) expr cases...
        elif c == 'S':
            has_label = self.read_hex2()
            label = self.read_name() if has_label == 1 else None
            case_count = self.read_hex2()
            expr = self.parse_expr()
            if label:
                self.prln(f"SWITCH [{label}] ({expr}) {{")
            else:
                self.prln(f"SWITCH ({expr}) {{")
            self.indent += 1
            for _ in range(case_count):
                self.parse_stmt()
            self.indent -= 1
            self.prln("}")

        # Case: C stmt_count(2) value stmts...
        elif c == 'C':
            stmt_count = self.read_hex2()
            value = self.parse_expr()
            self.prln(f"CASE {value}:")
            self.indent += 1
            for _ in range(stmt_count):
                self.parse_stmt()
            self.indent -= 1

        # Default: O stmt_count(2) stmts...
        elif c == 'O':
            stmt_count = self.read_hex2()
            self.prln("DEFAULT:")
            self.indent += 1
            for _ in range(stmt_count):
                self.parse_stmt()
            self.indent -= 1

        # Asm: A len(4) hexdata
        elif c == 'A':
            asm_len = self.read_hex4()
            # Read hex-encoded asm data
            asm_bytes = []
            for _ in range(asm_len):
                hi = self.cur()
                self.advance()
                lo = self.cur()
                self.advance()
                asm_bytes.append(int(hi + lo, 16))
            asm_str = bytes(asm_bytes).decode('latin-1')
            lines = asm_str.split('\n')
            if len(lines) == 1:
                self.prln(f"ASM {{ {lines[0]} }}")
            else:
                self.prln("ASM {")
                self.indent += 1
                for line in lines:
                    self.prln(line)
                self.indent -= 1
                self.prln("}")

        # Empty statement
        elif c == ';':
            self.prln(";")

        # Break
        elif c == 'K':
            self.prln("BREAK")

        # Continue
        elif c == 'N':
            self.prln("CONTINUE")

        # While: W nlabels(2) cond body
        elif c == 'W':
            nlabels = self.read_hex2()  # skip intermediate label count
            cond = self.parse_expr()
            self.prln(f"WHILE ({cond})")
            self.indent += 1
            self.parse_stmt()
            self.indent -= 1

        # Do-while: D nlabels(2) body cond
        elif c == 'D':
            nlabels = self.read_hex2()  # skip intermediate label count
            self.prln("DO")
            self.indent += 1
            self.parse_stmt()
            self.indent -= 1
            cond = self.parse_expr()
            self.prln(f"WHILE ({cond})")

        # For: F nlabels(2) init cond incr body
        elif c == 'F':
            nlabels = self.read_hex2()  # skip intermediate label count
            init = self.parse_expr()
            cond = self.parse_expr()
            incr = self.parse_expr()
            self.prln(f"FOR ({init}; {cond}; {incr})")
            self.indent += 1
            self.parse_stmt()
            self.indent -= 1

        else:
            self.prln(f"??? stmt {repr(c)}")

    def parse_function(self):
        """Parse function: F rettype hexname param_count(2) local_count(2) frm_size(2) params... locals... body
        Decl format: d suffix name reg off (reg and off are 2 hex digits each)"""
        ret_type = self.cur()
        self.advance()
        name = self.read_name()
        param_count = self.read_hex2()
        local_count = self.read_hex2()
        frm_size = self.read_hex2()

        params = []
        for _ in range(param_count):
            self.skip_whitespace()
            if self.cur() == 'd':
                self.advance()
                ptype = self.cur()
                self.advance()
                pname = self.read_name()
                preg = self.read_hex2()
                poff = self.read_hex2()
                # Convert unsigned to signed offset
                if poff > 127:
                    poff = poff - 256
                reg_str = self.REG_NAMES.get(preg, str(preg))
                off_str = f"IY{poff:+d}" if poff else "IY"
                if preg:
                    params.append(f"{pname}:{self.width_name(ptype)}@{reg_str}")
                else:
                    params.append(f"{pname}:{self.width_name(ptype)}@{off_str}")

        locals_list = []
        for _ in range(local_count):
            self.skip_whitespace()
            if self.cur() == 'd':
                self.advance()
                ltype = self.cur()
                self.advance()
                lname = self.read_name()
                lreg = self.read_hex2()
                loff = self.read_hex2()
                if loff > 127:
                    loff = loff - 256
                reg_str = self.REG_NAMES.get(lreg, str(lreg))
                off_str = f"IY{loff:+d}" if loff else "IY"
                if lreg:
                    locals_list.append(f"{lname}:{self.width_name(ltype)}@{reg_str}")
                else:
                    locals_list.append(f"{lname}:{self.width_name(ltype)}@{off_str}")

        params_str = ', '.join(params)
        print(f"\nFUNCTION {name}({params_str}) -> {self.width_name(ret_type)} [frame={frm_size}]")
        if locals_list:
            print(f"  LOCALS: {', '.join(locals_list)}")
        print("{")
        self.indent = 1
        self.skip_whitespace()
        self.parse_stmt()
        print("}")

    def parse_global(self):
        """Parse global: Z $hexname type has_init(2) [init]"""
        self.skip_whitespace()
        if self.cur() == '$':
            self.advance()
        name = self.read_name()
        type_char = self.cur()
        self.advance()

        # Array: a count(4) elemsize(4) has_init(2) [init]
        if type_char == 'a':
            count = self.read_hex4()
            elemsize = self.read_hex4()
            has_init = self.read_hex2()
            print(f"GLOBAL {name} : array[{count}] of {elemsize}-byte", end='')
            if has_init == 1:
                init_str = self.parse_init()
                print(f" = {init_str}")
            else:
                print()

        # Struct: r size(4) has_init(2) [init]
        elif type_char == 'r':
            size = self.read_hex4()
            has_init = self.read_hex2()
            print(f"GLOBAL {name} : struct[{size}]", end='')
            if has_init == 1:
                init_str = self.parse_init()
                print(f" = {init_str}")
            else:
                print()

        # Pointer or primitive
        else:
            has_init = self.read_hex2()
            print(f"GLOBAL {name} : {self.width_name(type_char)}", end='')
            if has_init == 1:
                init_str = self.parse_init()
                print(f" = {init_str}")
            else:
                print()

    def parse_init(self):
        """Parse a single initializer (recursive)

        Format:
          '[' type count init*  - array
          '{' count init*       - aggregate/struct
          '#' type value        - scalar constant
          '$' name              - symbol reference
          'W' p init            - widened constant
        """
        self.skip_whitespace()
        c = self.cur()

        if c == '[':
            # Array: '[' type count init*
            self.advance()
            elem_type = self.cur()
            self.advance()
            count = self.read_hex2()
            items = [self.parse_init() for _ in range(count)]
            return "[" + ", ".join(items) + "]"

        elif c == '{':
            # Aggregate: '{' count init*
            self.advance()
            count = self.read_hex2()
            fields = [self.parse_init() for _ in range(count)]
            if self.cur() == '}':
                self.advance()
            return "{" + ", ".join(fields) + "}"

        elif c == '#':
            # Scalar: '#' type value
            self.advance()
            w = self.cur()
            self.advance()
            val = self.read_hex8()
            return f"{val}:{self.width_name(w)}"

        elif c == '$':
            # Symbol reference
            self.advance()
            sym = self.read_name()
            return f"${sym}"

        elif c == 'W':
            # Widened constant: 'W' target_type init
            self.advance()
            self.advance()  # skip target type (p)
            inner = self.parse_init()
            return f"(W {inner})"

        else:
            # Unknown - skip one char
            self.advance()
            return "?"

    def read_hex_string(self):
        """Read hex-length-prefixed hex-encoded string data"""
        length = self.read_hex2()  # length in hex pairs
        chars = []
        for _ in range(length):
            byte = self.read_hex2()  # read hex pair as byte
            if 32 <= byte < 127:
                chars.append(chr(byte))
            else:
                chars.append(f'\\x{byte:02x}')
        return ''.join(chars)

    def parse_string(self):
        """Parse string: U name hexdata"""
        name = self.read_name()
        data = self.read_hex_string()
        print(f'STRING _{name} = "{data}"')

    def parse_toplevel(self):
        """Parse one top-level item, return True if more to parse"""
        self.skip_whitespace()
        c = self.cur()
        if c is None:
            return False
        self.advance()

        if c == 'F':
            self.parse_function()
        elif c == 'Z':
            self.parse_global()
        elif c == 'U':
            self.parse_string()
        elif c == '\n':
            pass  # skip blank lines
        else:
            print(f"??? top-level: {repr(c)}")
        return True

    def parse(self):
        print("=" * 40)
        print("AST Pretty Printer Output")
        print("=" * 40)
        while self.parse_toplevel():
            pass
        print()
        print("=" * 40)


def main():
    if len(sys.argv) > 1:
        with open(sys.argv[1], 'rb') as f:
            data = f.read().decode('latin-1')
    else:
        data = sys.stdin.buffer.read().decode('latin-1')

    parser = ASTParser(data)
    parser.parse()


if __name__ == '__main__':
    main()
