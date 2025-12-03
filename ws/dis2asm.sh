#!/bin/sh
# Convert wsnm disassembly output to assemblable source
# Usage: ./dis2asm.sh file.dis > file.s

if [ -z "$1" ]; then
    echo "Usage: $0 file.dis > file.s" >&2
    exit 1
fi

awk '
BEGIN {
    in_disasm = 0
    in_symtab = 0
    nglobals = 0
    nexterns = 0
    nlines = 0
}

# Store all lines for second pass
{ lines[nlines++] = $0 }

# Collect symbols from symbol table (first pass)
/^Symbol table:/ { in_symtab = 1; next }
/^Text relocations:/ { in_symtab = 0; next }
/^Data relocations:/ { in_symtab = 0; next }

in_symtab && /\[.*\]/ {
    # Format: [ N]  XXXX   TT   seg    yes/no   name
    # Fields: $1=[, $2=N], $3=XXXX, $4=TT, $5=seg, $6=yes/no, $7=name
    name = $NF
    if (name ~ /^[a-zA-Z_]/) {
        # Check segment type - ext means external
        if ($5 == "ext") {
            externs[nexterns++] = name
        } else if ($6 == "yes") {
            # Global defined symbol (text, data, or bss)
            globals[nglobals++] = name
        }
    }
}

END {
    # Second pass - output
    in_disasm = 0

    for (i = 0; i < nlines; i++) {
        line = lines[i]

        # Start of disassembly section
        if (line ~ /^Disassembly:/) {
            in_disasm = 1
            # Print header with externals first, then globals
            print "\t.text"
            for (j = 0; j < nexterns; j++) {
                print "\t.extern " externs[j]
            }
            for (j = 0; j < nglobals; j++) {
                print "\t.globl " globals[j]
            }
            print ""
            continue
        }

        # End disassembly at Data segment or Symbol table
        if (line ~ /^Data segment:/ || line ~ /^Symbol table:/) {
            in_disasm = 0
            continue
        }

        if (!in_disasm) continue
        if (line ~ /^$/) continue

        # Label line (no leading whitespace, ends with :)
        if (line ~ /^[a-zA-Z_].*:$/) {
            print line
            continue
        }

        # Instruction line: "  XXXX  XX XX XX XX  mnemonic operands"
        # Columns: 1-6=addr, 7-8=spaces, 9-20=hex bytes, 21+=mnemonic
        if (line ~ /^  [0-9a-f][0-9a-f][0-9a-f][0-9a-f]  /) {
            # Skip first 20 chars (address + hex bytes area)
            mnemonic = substr(line, 21)
            # Remove leading whitespace
            gsub(/^[ \t]+/, "", mnemonic)
            # Remove trailing whitespace
            gsub(/[ \t]+$/, "", mnemonic)

            if (mnemonic != "") {
                # Fix hex numbers starting with a-f (need leading 0)
                # Split on comma, fix each part, rejoin
                n = split(mnemonic, parts, ",")
                result = ""
                for (k = 1; k <= n; k++) {
                    part = parts[k]
                    # For first part, also check operand after space
                    if (k == 1) {
                        m = split(part, subparts, " ")
                        if (m == 2 && subparts[2] ~ /^[a-fA-F][0-9a-fA-F]*h$/) {
                            part = subparts[1] " 0" subparts[2]
                        }
                        # Convert db/dw to .db/.dw
                        if (subparts[1] == "db") part = ".db" substr(part, 3)
                        else if (subparts[1] == "dw") part = ".dw" substr(part, 3)
                    } else {
                        # If part is a hex number starting with a-f, add 0
                        if (part ~ /^[a-fA-F][0-9a-fA-F]*h$/) {
                            part = "0" part
                        }
                    }
                    if (k == 1) result = part
                    else result = result "," part
                }
                print "\t" result
            }
        }
    }
}
' "$1"
