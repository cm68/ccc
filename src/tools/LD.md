# wsld: how an object is loaded and linked

Two words that get used loosely, and this linker keeps them apart:

- **loading** is giving an object's segments addresses.  Nothing in the
  object is touched; the object is simply told where it will live.
- **linking** is the fixup that follows, once every symbol has an
  address: going back over the code and filling in the numbers that
  could not be known until then.

The order matters, because the second cannot start until the first has
finished for *every* object.  A reference to `printf` cannot be patched
until `printf` has an address, and `printf` has no address until the
objects ahead of it in the image have been measured.

The machine this runs on has 64K of address space, all in, and the
linker itself is most of 26K of that.  So the third thing this document
is about is what is allowed to stay in memory, which is very little: the
name list, and nothing else that grows with the size of the program.


## The object file

A Whitesmiths object is a 16-byte header followed by four regions:

	 0  magic          0x99
	 1  config         byte order, symbol length, "no relocation"
	 2  symtab size    bytes, not entries
	 4  text size
	 6  data size
	 8  bss size
	10  heap size
	12  text offset
	14  data offset
	16  text        )  the two segments, back to back
	    data        )
	    symbol table
	    text relocations
	    data relocations

bss has no bytes in the file - it is a size and nothing more, and the
loader zeroes it.

The symbol table is fixed-size entries of `symlen + 3` bytes: a two-byte
value, a type byte, and the name padded with nulls.  `symlen` comes out
of the config byte, and is 15 for everything this compiler produces.
That is why the number of symbols is `symtab_size / (symlen + 3)` rather
than a count in the header.

The two relocation streams follow the symbol table, one after the other,
and neither is length-prefixed: each is terminated by a zero byte, so
the only way to find the data relocations is to walk the text ones to
their end.  That is exactly what `read_ar_obj` does at the point it
records `textRelocOff` and `dataRelocOff` - it walks the first stream
once, purely to find where the second begins, and remembers both file
offsets so that pass 2 can seek straight to them.


## Reading an object

`read_object` (and `read_ar_obj`, for a member inside an archive) reads
the header, then the symbol table, and stops.

What it keeps:

- a `struct object`: the sizes, the file, the base offset of this object
  within its file, and the two relocation offsets it just found.
- one `struct symbol` per name, in a single global list.
- `obj->symtab[]`, an array mapping this object's symbol *index* to the
  entry in that global list.  The relocation stream refers to symbols by
  index, so this is how index 6 in `printf.o` becomes the global
  `_printf`.

What it does **not** keep is the text and the data.  They stay in the
file, and are read once, in pass 2, on their way to the output.  This is
the whole reason a program larger than the linker can be linked at all.

A symbol whose type says "external" is a *reference*, not a definition.
It goes in the list with segment `SEG_EXT` and no value.  When some
later object defines the same name, `sym_define` finds the existing
entry and fills it in.  So the list is both the symbol table and the
undefined list, and `has_undefined` is a walk over it looking for
anything still marked `SEG_EXT`.


## Archives

An archive is a two-byte magic and then, for each member, a 14-byte
name, a two-byte length, and the object.

Members are not read on sight.  `ar_needed` seeks to the member,
reads its header and symbol table, and asks a single question: does this
member *define* a name that is currently undefined?  Only if the answer
is yes does `read_ar_obj` run and the member join the link.  This is what
keeps a program from dragging in the whole of libc.

Because including a member can introduce new undefined names of its own,
the archives are scanned repeatedly:

	do {
		added = 0;
		for (each archive) added += read_archive(...);
	} while (added > 0 && has_undefined());

A pass that adds nothing ends it, as does having nothing left undefined.
The archive file is deliberately left open when anything was taken from
it, because the objects inside it are read again in pass 2 and they
refer to it by `FILE *`.


## Loading: assigning addresses

`pass1_layout` runs once, after all the reading.

It walks the objects in order and hands each one a running offset within
each of the three segments:

	obj->text_off = text_pos;   text_pos += obj->text_size;
	obj->data_off = data_pos;   data_pos += obj->data_size;
	obj->bss_off  = bss_pos;    bss_pos  += obj->bss_size;

Segments are concatenated by kind, not by object: all the text of every
object, then all the data, then all the bss.  So the final address of a
thing depends on the totals as well as the offsets, and the totals are
not known until the walk is done.  That is why this is a separate pass.

With the totals in hand, every symbol gets its address.  A symbol's
value in the object is an offset from the start of that object's *file
image*, which is why data and bss subtract what came before them:

	text:  text_base + obj->text_off + value
	data:  data_base + total_text + obj->data_off
	                 + (value - obj->text_size)
	bss:   bss_base + total_text + total_data + obj->bss_off
	                 + (value - obj->text_size - obj->data_size)

Anything still `SEG_EXT` at this point is an undefined symbol and the
link fails, unless `-r` was given and the output is itself relocatable.

There are six names the linker defines rather than finds - `__Ltext`,
`__Htext`, `__Ldata`, `__Hdata`, `__Lbss`, `__Hbss` - the low and high
end of each segment.  crt0 uses the bss pair to clear bss before main.
They live in an object's data segment as ordinary words, so the linker
records where they sit *before* resolving addresses (their pre-resolution
value is their offset) and patches those words in pass 2.


## Linking: the relocation stream

A relocation stream is a walk along the segment from offset zero.  It is
a byte stream of two kinds of item:

- a **bump**, which advances the position without changing anything.
  A byte under 32 is that many bytes; 32 to 63 introduces a two-byte
  form, `((b - 32) << 8) + next + 32`, for longer gaps.
- a **control byte**, 64 or over, which says "there is a fixup here".

The position only ever moves forward.  There is no seeking within the
stream and no going back, and that single property is what makes the
streaming output below possible.

A control byte carries two things.  The low two bits are the *width*:

	0	a whole word - two bytes, little endian
	1	the low byte only
	2	the high byte only

and the rest says what to add:

	0x40	absolute - nothing to add
	0x44	this object's text base
	0x48	this object's data base
	0x4c	this object's bss base
	0x50..	a symbol, index (b - 0x50) >> 2

The fixup itself is an *addition*, not a store.  The compiler has
already put a partial value in the segment - the offset of the thing
within its own object - and the linker adds the base that offset is
relative to:

	val  = the two bytes at the position
	val += add
	store val back

For a hi or lo relocation only one byte is read, added to, and stored,
which is how an address gets built into a pair of `ld a,` immediates.

The `add` for a segment relocation is the base this object's segment
landed at.  For a symbol relocation it is the symbol's resolved value -
with one wrinkle: a hi/lo pair on a symbol *defined in the same object*
uses the object's segment base instead, because in that case the partial
value already in the code is the symbol's own offset and adding the full
resolved address would count it twice.


## Streaming the output

The output is written once, front to back:

	16-byte header
	every object's text, in order
	every object's data, in order
	the symbol table          (unless -s)
	the relocation streams    (only with -r)

The header can be written first because `pass1_layout` has already
worked out the totals it needs.

For each segment of each object, `copy_segment`:

1. remembers where the output file is now - `obase = ftell(outfp)`.
   That is the address this segment is about to occupy in the file.
2. seeks the *input* to the segment and copies it to the output in
   fixed 512-byte chunks.  The bytes go out exactly as they came in,
   with none of the fixups applied yet.
3. walks the relocation stream and applies each fixup **to the output
   file**, at `obase + pos`.
4. flushes and puts the output position back to `obase + seg_size`, so
   the next segment appends where this one ended.

The fixups in step 3 go through a single 512-byte window over the output
file: a buffer, the file offset it covers, and a dirty flag.  Asking for
a byte outside the window writes the window back if it was modified and
reads in the one containing the byte wanted.  The window is clamped to
the segment, so a short last window cannot write past what was copied.

One window is enough, and is never asked to go backwards, because
relocation positions only move forward.  That is not an accident of the
data - it is what a stream of bumps means.

The output is therefore opened `"w+b"` rather than `"wb"`: it has to be
readable, because a fixup adds to what is already there.

### why it is done this way

The obvious implementation reads a segment into memory, relocates it
there, and writes it out.  That is what this linker used to do, and it
cannot work here.  The name list has to stay resident for the whole
link, the linker itself is 26K, and a single `malloc` of the largest
object's text is enough on its own to leave no room.  Worse, it is the
*big* programs - the ones with the biggest segments - that need the
memory for everything else too.

Copying first and patching afterwards costs a second pass over each
segment's bytes and some seeking on the output, and in exchange the
memory used by the output path is a constant: two 512-byte buffers, one
to copy through and one to patch through, whatever the size of the
program being linked.

The result is byte-for-byte the same file as the in-memory version
produced.  That is worth checking after any change here, and it is easy
to check: link something twice, once with each, and `cmp`.


## What stays in memory

Per link:

- one `struct symbol` for every name, in one list.  The name is a
  variable-length tail on the structure rather than a fixed 16-byte
  array, so a symbol costs what its name costs.  These come out of
  `permalloc`, the permanent arena, because none of them is ever freed
  and they have no business paying for malloc's per-block header or its
  free list.
- one `struct object` per object taken, plus its `symtab[]` index map.
- the display name of each archive member, `libc.a(printf.o)`.

Everything else - text, data, relocation streams - is read from the file
at the moment it is needed and not kept.  Nothing in the resident set
scales with the *size* of the objects, only with how many names and
objects there are, and that is the property that has to be preserved by
anything added here.
