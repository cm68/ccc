# CPP Normalizer Architecture

cpp simplifies the C that pass1 has to parse.  What used to be a
six-stage pull-filter pipeline is one module, `norm.c`: a
recursive-descent walker that pulls raw tokens from the lexer and
pushes the normalized lexeme stream straight to emit.  The C call
stack carries the nesting that the filters kept in hand-rolled
continuations (context stacks, saved tokens, redispatch states).

## Structure

```
lex_get -> [ source layer: enum lowering -> typedef dissolution ]
        -> [ walker: file scope | statements ]
        -> emitStructTok
```

### Source layer (srcget)

What the walker pulls is already free of ENUM, TYPEDEF, and every
typedef name:

- **Enum lowering** (`epull`): enum constants become macro-table
  entries (globally visible, exactly like #define); the enum type
  rewrites to `unsigned char`; a bare declaration vanishes.
- **Typedef dissolution**: a typedef is a declarator with a hole
  where the name sits; a use composes the use-site declarator into
  the hole.  Expansion output goes through the one pending queue
  left in cpp (`tdq`); typedef'd struct bodies stream one member
  per pull so the queue stays small.

### Walker

- **File scope** (`norm_run`): everything streams through untouched
  except struct/union bodies (skipped verbatim - members are not
  statements) and K&R function normalization (`knr`):

      int foo(a, b)  int a;  char *b;  {   ->   int foo(int a, char *b) {

  with implicit int synthesized where c0 requires a return type.

- **Statements** (`stmt`, at brace depth > 0):
  - local declaration initializers split into assignments
    (`int x = 5, y = f();` -> `int x, y; x = 5; y = f();`);
    statics and arrays keep theirs inline
  - unbraced control-structure bodies gain synthetic braces; an
    if-body's `}` waits until the next token shows whether an
    `else` follows
  - loops lower to labels and gotos, switch gains its break label,
    break/continue become gotos to the innermost matching target:

        while (c) body   ->  __WnT: ; if (!(c)) { goto __WnB ; } body
                             goto __WnT ; __WnB: ;
        for (i;c;n) body ->  i ; __FnT: ; if (!(c)) { goto __FnB ; } body
                             __FnC: ; n ; goto __FnT ; __FnB: ;
        do body while(c) ->  __DnT: ; body __DnC: ; if (c) { goto __DnT ; }
                             __DnB: ;

  Label naming: `__<W|F|D|S><n><T|B|C>` - top, bottom/break,
  continue.  `__DnC` precedes the do test so continue re-tests it.

After the source layer, a declaration is recognisable from its
leading token: pass1 parses without a symbol table.

## Output-byte discipline

.x line markers derive from per-token line stamps at emit, and a
synthesized token stamps the lexer's current position.  So the
walker synthesizes at fixed stream offsets: a synthetic `{` only
after the body's first token is pulled, a loop header only after the
condition's closing paren, the deferred if-`}` only after the
else-check token.  `abdiff.sh save|check` holds the whole tree to
byte-identical .x/.n/.s across changes; anything that moves a
synthesis point shows up there first.

## Memory

- One pending queue (typedef expansions), a handful of tokarrays,
  no filter stacks; buffers grow by GROWSTEP, never doubling.
- Only a for's increment clause outlives a body, and it lives in
  `do_for`'s frame - nested loops save and restore nothing.
- Recursion depth is statement nesting; the frames are smaller than
  the context stacks they replaced.

## Testing

- `make test` - test/runtest.sh (loop lowering vs blessed .expected,
  through the cpp binary) and test/sweep.sh (512 input phases must
  be byte-identical).
- `make langtest` - every cpp source through cpp + validators.
- `abdiff.sh` - tree-wide byte-identity gate (see above).
