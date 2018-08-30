ccc - full native C compiler

i wrote this about a dozen years ago and lost the source.  I am typing
it in from a paper printout with a few missing chunks, so it will be a
while before it works.

this is a 2 pass compiler.

the first pass is a recursive descent parse with embedded CPP.
it writes an ascii parse tree file.

the second pass is a code generator and assembler.
it writes an object file
