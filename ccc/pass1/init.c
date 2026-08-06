/*
 * static and global initializer parsing, split from decl.c so no
 * single translation unit carries both halves of declaration handling.
 */

#include "cc1.h"

/*
 * Find struct member at given offset (members are linked in reverse order)
 */
struct name *
findMemberOff(struct name *members, int offset)
{
    while (members) {
        if (members->w.m.offset == offset)
            return members;
        members = members->next;
    }
    return NULL;
}

/*
 * Stream an initializer value directly to assembly output
 * Used for static/global initializers to avoid building expression trees
 * Returns count of top-level elements (for array size inference)
 */
int
streamInitVal(struct type *type)
{
    int size = type ? type->size : 2;
    int count = 0;
    struct type *elem_type;
    struct name *member;       /* also used as np in expr branch */
    /*
     * member_offset walks a struct, and a struct is at most 255
     * bytes - type->size is itself a byte.  is_struct is a truth
     * value.  Byte locals cost one load where an int costs two,
     * and this function tests both in loops.
     */
    unsigned char member_offset;
    unsigned char is_struct;
    cstring str;
    int slen, arrlen, i, b;
    char buf[20];              /* used as strname in STRING, buf in expr */
    struct expr *e;
    long val;

    if (cur.type == BEGIN) {
        /* Nested initializer list */
        elem_type = NULL;
        member = NULL;
        member_offset = 0;
        is_struct = 0;
        if (type && (type->flags & TF_AGGREGATE)) {
            is_struct = 1;
            member = findMemberOff(type->elem, 0);  /* First member at offset 0 */
            elem_type = member ? member->type : NULL;
        } else if (type && (type->flags & TF_ARRAY)) {
            elem_type = type->sub;  /* array - pass element type */
        }
        gettoken();  /* consume { */
        while (cur.type != END) {
            streamInitVal(elem_type);
            count++;
            /* Advance to next struct member by offset */
            if (is_struct && member) {
                member_offset += member->type->size;
                member = findMemberOff(type->elem, member_offset);
                elem_type = member ? member->type : NULL;
            }
            if (cur.type == COMMA)
                gettoken();
            else if (cur.type != END) {
                gripe(ER_S_SN);
                return count;
            }
        }
        gettoken();  /* consume } */
        /*
         * Pad out the rest of the struct.  An initializer that names
         * fewer members than the struct has still occupies all of it,
         * and the next element of an array of these begins at sizeof
         * - not where the initializers happened to stop.
         *
         * Without this, pass1's own basicnames[] - seven entries of
         * "{ name, type, chain }" out of a struct with a dozen fields
         * - sat 20 bytes apart in memory while every subscript was
         * computed against sizeof, which is 37.  basicnames[1] read
         * the middle of basicnames[0]'s name and came back null, so
         * the c0 that ccc built could not name a basic type: "int"
         * and "long" and "unsigned char" as parameters all answered
         * "fn array".  Index 0 worked, having no stride to get wrong.
         */
        if (is_struct && type) {
            while (member_offset < type->size) {
                asmDb(0);
                member_offset++;
            }
        } else if (type && (type->flags & TF_ARRAY) && type->count > 0 &&
                   count < type->count) {
            /*
             * And the rest of an array, for the same reason one
             * level up.  An initializer that names fewer elements
             * than the array has still occupies all of it, and what
             * is declared after it begins at the end of the array,
             * not where the initializers stopped.
             *
             * Without this, libc's "FILE _iob[_NFILE]" - three
             * entries initialised out of six - emitted 24 bytes of a
             * 48 byte array, and stdin/stdout/stderr, declared next,
             * were laid down on top of _iob[3..5].  Their pointer
             * values read back as those entries' flags, so fopen
             * found no free slot and every fopen in the system
             * returned null while freopen on a named entry worked.
             */
            b = typesize(type->sub) * (type->count - count);
            while (b-- > 0)
                asmDb(0);
        }
    } else if (cur.type == STRING) {
        /* String literal - check if initializing char array inline */
        str = cur.v.str;
        if (type && (type->flags & TF_ARRAY) && type->sub &&
            type->sub->size == 1 && str) {
            /* Emit string bytes inline for char array */
            slen = str[0];
            arrlen = type->count > 0 ? type->count : slen + 1;
            for (i = 0; i < arrlen; i++) {
                b = (i < slen) ? (unsigned char)str[i + 1] : 0;
                asmDb(b);
            }
            /* Skip the strN label emitted in phase 1 - keep counter in sync */
            globalStrCtr++;
            count = slen + 1;  /* Array size for char[] = "..." */
        } else {
            /* Pointer to string - just emit reference (data emitted in phase 1) */
            fmtstr(buf, "str%d", globalStrCtr++);
            asmDwSym(buf);
            count = 1;
        }
        cur.v.str = NULL;
        gettoken();
    } else {
        /*
         * Parse single expression, emit, free.
         *
         * Folded first.  This streams straight to assembly instead of
         * building an AST, so it never reached foldTree, and the test
         * just below asks for a CONST node - which an unfolded "a | b"
         * is not.  Every constant expression in a static initialiser
         * took the unsupported branch and was written as zero, at both
         * widths, while the same expression anywhere else folded fine.
         *
         * cpp's keyword tables are built out of "c | HI" entries, so
         * every skip byte in its trie was nought and only the one
         * directive spelled without them still matched.
         */
        e = foldTree(parseExpr(OP_PRI_COMMA));
        if (e) {
            if (e->op == CONST) {
                val = (long)e->v;  /* e->v IS the value, not a pointer */
                if (size == 1)
                    asmDb((int)val);
                else if (size == 4) {
                    /*
                     * Four bytes, low word first, which is how everything
                     * that reads one expects to find it: "ld de,(x)" then
                     * "ld hl,(x+2)".  A word was emitted for anything that
                     * was not a byte, so an initialised long global was
                     * half a long - the next variable sat where its high
                     * word belonged, and reading it back gave that
                     * variable in the top half.
                     */
                    asmDw((int)(val & 0xffffL));
                    asmDw((int)((val >> 16) & 0xffffL));
                } else
                    asmDw((int)val);
            } else if (e->op == SYM) {
                member = (struct name *)e->var;
                if (member->sclass & SC_STATIC)
                    fmtstr(buf, "S%d", member->static_id - 1);
                else
                    fmtstr(buf, "_%s", nameOf(member->id));
                asmDwSym(buf);
            } else if (e->op == PLUS && e->left && e->left->op == SYM &&
                       e->right && (e->right->flags & E_CONST)) {
                /*
                 * The address of an element other than the first.
                 * "&arr[0]" folds to the bare symbol and was taken by
                 * the branch above; "&arr[n]" keeps the offset and is
                 * a PLUS, which matched neither and was written as
                 * zero.  The assembler takes label+offset, so say it.
                 *
                 * pass1's own type table is built this way:
                 *
                 *	{ 2, 0, 0, 0, 0, &basictypes[0] },
                 *	{ 4, 0, 0, 0, 0, &basictypes[1] },
                 *
                 * so every link past the first was null and inttype,
                 * longtype and voidtype were all zero.  The c0 that
                 * ccc built could not name a type it had not been
                 * given by a typedef.
                 */
                member = (struct name *)e->left->var;
                if (member->sclass & SC_STATIC)
                    fmtstr(buf, "S%d+%d", member->static_id - 1,
                        (int)e->right->v);
                else
                    fmtstr(buf, "_%s+%d", nameOf(member->id),
                        (int)e->right->v);
                asmDwSym(buf);
            } else {
                /* Unsupported initializer - emit zero */
                if (size == 1)
                    asmDb(0);
                else
                    asmDw(0);
            }
            FreeExpr(e);
        }
        count = 1;
    }
    return count;
}

/*
 * Parse a variable initializer for static/global variables
 *
 * Streams assembly directly without building trees. Auto initializers
 * are handled by cpp (transformed to assignment statements).
 */
void
doInitlzr(struct name *v)
{
    char fullname[32];
    char strname[16];

    gettoken(); /* consume = token */

    /* Phase 1: skip tokens, emit string data */
    if (phase == 1) {
        if (cur.type == BEGIN) {
            /* Struct/array init - emit STRING data in phase 1 */
            unsigned char depth = 1;
            gettoken();  /* consume initial { before loop */
            while (depth > 0 && cur.type != E_O_F) {
                if (cur.type == BEGIN)
                    depth++;
                else if (cur.type == END)
                    depth--;
                else if (cur.type == STRING) {
                    /* Emit string data now for pointer-to-string fields */
                    cstring str = cur.v.str;
                    if (str) {
                        unsigned char slen = str[0];
                        fmtstr(strname, "str%d", globalStrCtr++);
                        setSeg(SEG_TEXT);
                        asmLabel(strname);
                        asmDbStr((unsigned char *)str + 1, slen);
                    }
                }
                if (depth > 0)
                    gettoken();
            }
            gettoken();  /* consume final } */
        } else if (cur.type == STRING) {
            /*
             * A string initialising a POINTER needs a literal to point
             * at.  One initialising a char array does not: phase 2
             * streams the bytes into the array itself and burns a
             * counter value where this label would have been.  Emitting
             * it anyway put a second copy of every such string in the
             * text segment with nothing referring to it - six bytes for
             * "hello", and rather more for a table of them.
             *
             * The test is streamInitVal's, spelled the same way, because
             * the two have to agree about which strings get a label.
             */
            cstring str = cur.v.str;
            if (str) {
                unsigned char slen = str[0];
                struct type *t = v ? v->type : (struct type *)0;
                int inarray = t && (t->flags & TF_ARRAY) &&
                    t->sub && t->sub->size == 1;

                fmtstr(strname, "str%d", globalStrCtr++);
                if (!inarray) {
                    setSeg(SEG_TEXT);
                    asmLabel(strname);
                    asmDbStr((unsigned char *)str + 1, slen);
                }
            }
            gettoken();
        } else {
            parseExpr(15);  /* skip expression */
        }
        return;
    }

    /* Phase 2: emit assembly directly */
    setSeg(SEG_DATA);

    /* Build label: globals get ::, statics get : */
    if (v->sclass & SC_STATIC)
        fmtstr(fullname, "S%d:", v->static_id - 1);
    else
        fmtstr(fullname, "_%s::", nameOf(v->id));
    asmLine(fullname);

    /* Stream initializer and fix array size if needed */
    {
        int count = streamInitVal(v->type);
        if ((v->type->flags & TF_ARRAY) && v->type->count == -1)
            v->type = getType(TF_ARRAY|TF_POINTER, v->type->sub, count);
    }
}
