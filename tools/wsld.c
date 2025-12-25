/*
 * wsld - Whitesmith's object file linker
 *
 * Two-pass linker:
 * Pass 1: assign addresses, resolve symbols
 * Pass 2: write output with relocations applied
 */
#include <stdio.h>

#ifdef linux
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#endif

#include "wsobj.h"
#include "hiobj.h"

/* use wsSegNames from wsobj.c */

char verbose;
char Vflag;            /* -V: list object files */
char rflag;            /* -r: emit relocatable output */
char sflag;            /* -s: strip symbols */
char out_symlen;       /* output symbol length (0=15, set by -9) */

/*
 * segment base addresses (command line settable)
 */
unsigned short text_base;
unsigned short data_base;
unsigned short bss_base;

/*
 * running totals for segment layout
 */
unsigned short text_pos;
unsigned short data_pos;
unsigned short bss_pos;

/*
 * final segment sizes
 */
unsigned short total_text;
unsigned short total_data;
unsigned short total_bss;

/*
 * symbol table entry
 */
struct symbol {
    char name[16];          /* max 15 chars + null */
    unsigned short value;   /* final resolved address */
    unsigned char type;     /* original type byte */
    unsigned char seg;      /* decoded segment */
    struct object *obj;     /* defining object (NULL for extern) */
    struct symbol *next;
};

struct symbol *symbols;
int num_globals;

/*
 * object file info
 */
struct object {
    char *name;
    FILE *fp;
    long file_base;                 /* base offset in file (for archives) */
    unsigned char config;
    unsigned char symlen;
    unsigned short symtab_size;
    unsigned short text_size;
    unsigned short data_size;
    unsigned short bss_size;
    unsigned short heap_size;
    unsigned short hdr_text_off;    /* from header */
    unsigned short hdr_data_off;    /* from header */
    unsigned short num_syms;
    /* assigned during pass 1 */
    unsigned short text_off;        /* offset in final text segment */
    unsigned short data_off;        /* offset in final data segment */
    unsigned short bss_off;         /* offset in final bss segment */
    /* relocation table offsets in file */
    long textRelocOff;            /* file offset of text relocs */
    long dataRelocOff;            /* file offset of data relocs */
    /* local symbol table for relocation lookups */
    struct symbol **symtab;         /* array indexed by symbol index */
    /* Hi-Tech specific fields */
    char is_hitech;                 /* 1 if Hi-Tech format */
    unsigned char *ht_text;         /* collected text segment data */
    unsigned char *ht_data;         /* collected data segment data */
    struct ht_reloc *ht_relocs;     /* relocation array */
    int ht_nrelocs;                 /* number of relocations */
    struct object *next;
};

struct object *objects;
struct object *objects_tail;

/*
 * pending relocation for -r output
 */
struct outreloc {
    unsigned short offset;      /* offset in merged segment */
    struct symbol *sym;         /* symbol (for ext) or NULL (for seg) */
    unsigned char seg;          /* segment type if sym is NULL */
    unsigned char hilo;         /* REL_WORD/LO/HI */
    struct outreloc *next;
};

struct outreloc *text_relocs;
struct outreloc *textRelocTl;
struct outreloc *data_relocs;
struct outreloc *dataRelocTl;

/*
 * Hi-Tech relocation entry for linking
 */
struct ht_reloc {
    unsigned short offset;      /* offset within segment */
    unsigned char type;         /* HT_RPSECT/HT_RNAME | HT_RBYTE/HT_RWORD */
    unsigned char seg;          /* SEG_TEXT or SEG_DATA */
    char target[16];            /* target name (symbol or psect) */
};

char *outfile = "a.out";
FILE *outfp;

/* linker-defined symbol patching */
#define LSYM_LTEXT  0
#define LSYM_HTEXT  1
#define LSYM_LDATA  2
#define LSYM_HDATA  3
#define LSYM_LBSS   4
#define LSYM_HBSS   5
#define LSYM_COUNT  6

struct lnksym {
    char *name;
    struct object *obj;
    unsigned short off;
} lnksyms[] = {
    { "__Ltext", 0, 0 },
    { "__Htext", 0, 0 },
    { "__Ldata", 0, 0 },
    { "__Hdata", 0, 0 },
    { "__Lbss",  0, 0 },
    { "__Hbss",  0, 0 },
};

void
usage()
{
    fprintf(stderr, "usage: wsld [-vV9rs] [-o outfile] [-Ttext=addr] [-Tdata=addr] [-Tbss=addr] file...\n");
    fprintf(stderr, "  -V            list object files linked\n");
    fprintf(stderr, "  -r            emit relocatable output (for subsequent links)\n");
    fprintf(stderr, "  -s            strip symbol table from output\n");
    fprintf(stderr, "  -9            use 9-char symbols in output (default 15)\n");
    fprintf(stderr, "  -Ttext=addr   set text segment base address\n");
    fprintf(stderr, "  -Tdata=addr   set data segment base address\n");
    fprintf(stderr, "  -Tbss=addr    set bss segment base address\n");
    exit(1);
}

void
error(msg)
char *msg;
{
    fprintf(stderr, "wsld: %s\n", msg);
    exit(1);
}

void
error2(msg, arg)
char *msg;
char *arg;
{
    fprintf(stderr, "wsld: %s: %s\n", msg, arg);
    exit(1);
}

unsigned short
read_word(fp)
FILE *fp;
{
    unsigned char buf[2];
    if (fread(buf, 1, 2, fp) != 2)
        error("read error");
    return buf[0] | (buf[1] << 8);
}

unsigned char
read_byte(fp)
FILE *fp;
{
    int c;
    c = fgetc(fp);
    if (c == EOF)
        error("read error");
    return c;
}

void
write_byte(b)
unsigned char b;
{
    if (fputc(b, outfp) == EOF)
        error("write error");
}

void
write_word(w)
unsigned short w;
{
    write_byte(w & 0xff);
    write_byte(w >> 8);
}

/*
 * decode segment from type byte
 */
unsigned char
decode_seg(type)
unsigned char type;
{
    switch (type & 0x07) {
    case 4: return SEG_ABS;
    case 5: return SEG_TEXT;
    case 6: return SEG_DATA;
    case 7: return SEG_BSS;
    default: return SEG_EXT;
    }
}

/*
 * lookup symbol by name in global table
 */
struct symbol *
sym_lookup(name)
char *name;
{
    struct symbol *s;
    for (s = symbols; s; s = s->next) {
        if (strcmp(s->name, name) == 0)
            return s;
    }
    return 0;
}

/*
 * add or update symbol in global table
 */
struct symbol *
sym_define(name, value, seg, type, obj)
char *name;
unsigned short value;
unsigned char seg;
unsigned char type;
struct object *obj;
{
    struct symbol *s;

    s = sym_lookup(name);
    if (s) {
        /* already exists */
        if (s->seg != SEG_EXT && seg != SEG_EXT) {
            fprintf(stderr, "wsld: duplicate symbol: %s\n", name);
            fprintf(stderr, "  defined in %s and %s\n",
                    s->obj ? s->obj->name : "?", obj ? obj->name : "?");
            exit(1);
        }
        if (seg != SEG_EXT) {
            /* this is the definition */
            s->value = value;
            s->seg = seg;
            s->type = type;
            s->obj = obj;
        }
        return s;
    }

    /* new symbol */
    s = (struct symbol *)malloc(sizeof(struct symbol));
    strncpy(s->name, name, 15);
    s->name[15] = '\0';
    s->value = value;
    s->seg = seg;
    s->type = type;
    s->obj = obj;
    s->next = symbols;
    symbols = s;
    num_globals++;
    return s;
}

/*
 * read object file header and symbols
 */
void
read_object(name)
char *name;
{
    struct object *obj;
    struct symbol *gsym;
    FILE *fp;
    unsigned char magic;
    unsigned short val;
    unsigned char type, seg;
    char symname[16];
    int i;

    fp = fopen(name, "rb");
    if (fp == NULL)
        error2("cannot open", name);

    magic = read_byte(fp);
    if (magic != MAGIC)
        error2("bad magic", name);

    obj = (struct object *)malloc(sizeof(struct object));
    memset(obj, 0, sizeof(struct object));
    obj->name = name;
    obj->fp = fp;

    obj->config = read_byte(fp);
    obj->symlen = (obj->config & CONF_SYMASK) * 2 + 1;
    obj->symtab_size = read_word(fp);
    obj->text_size = read_word(fp);
    obj->data_size = read_word(fp);
    obj->bss_size = read_word(fp);
    obj->heap_size = read_word(fp);
    obj->hdr_text_off = read_word(fp);
    obj->hdr_data_off = read_word(fp);

    obj->num_syms = obj->symtab_size / (obj->symlen + 3);

    if (Vflag)
        printf("%s\n", name);

    if (verbose) {
        printf("%s: symlen=%d text=%d data=%d bss=%d syms=%d\n",
               name, obj->symlen,
               obj->text_size, obj->data_size, obj->bss_size, obj->num_syms);
    }

    /* add to list */
    if (!objects) {
        objects = objects_tail = obj;
    } else {
        objects_tail->next = obj;
        objects_tail = obj;
    }

    /* allocate local symbol table for relocation lookups */
    obj->symtab = (struct symbol **)malloc(obj->num_syms * sizeof(struct symbol *));

    /* skip to symbol table: header(16) + text + data */
    fseek(fp, 16 + obj->text_size + obj->data_size, SEEK_SET);

    /* read symbols */
    for (i = 0; i < obj->num_syms; i++) {
        val = read_word(fp);
        type = read_byte(fp);
        fread(symname, 1, obj->symlen, fp);
        symname[obj->symlen] = '\0';
        seg = decode_seg(type);

        /* add to global symbol table, get back pointer */
        gsym = sym_define(symname, val, seg, type, obj);
        obj->symtab[i] = gsym;

        if (verbose > 1) {
            printf("  [%d] %s: val=0x%04x seg=%s%s\n",
                   i, symname, val, wsSegNames[seg],
                   (type & 0x08) ? " global" : "");
        }
    }

    /* record relocation table positions */
    obj->textRelocOff = ftell(fp);

    /* skip text relocs to find data relocs */
    while (1) {
        unsigned char b = read_byte(fp);
        if (b == 0) break;
        if (b >= 32 && b < 64) read_byte(fp);  /* extended bump */
        else if (b == 0xfc) {
            b = read_byte(fp);
            if (b >= 0x80) read_byte(fp);  /* extended symbol */
        }
    }
    obj->dataRelocOff = ftell(fp);

    if (verbose > 1) {
        printf("  text_reloc@0x%lx data_reloc@0x%lx\n",
               obj->textRelocOff, obj->dataRelocOff);
    }
}

/*
 * check if symbol name is currently undefined
 */
int
is_undefined(name)
char *name;
{
    struct symbol *s = sym_lookup(name);
    return s && s->seg == SEG_EXT;
}

/*
 * check if there are any undefined symbols
 */
int
has_undefined()
{
    struct symbol *s;
    for (s = symbols; s; s = s->next) {
        if (s->seg == SEG_EXT)
            return 1;
    }
    return 0;
}

/*
 * scan archive member at given file offset to see if it defines
 * any currently undefined symbol. returns 1 if it does.
 */
int
ar_needed(fp, base)
FILE *fp;
long base;
{
    unsigned char magic, config;
    int symlen;
    unsigned short symtab_size, text_size, data_size;
    unsigned char type, seg;
    char symname[16];
    int num_syms, i;
    int needed = 0;

    fseek(fp, base, SEEK_SET);

    magic = read_byte(fp);
    if (magic != MAGIC)
        return 0;

    config = read_byte(fp);
    symlen = (config & CONF_SYMASK) * 2 + 1;
    symtab_size = read_word(fp);
    text_size = read_word(fp);
    data_size = read_word(fp);
    read_word(fp);  /* bss */
    read_word(fp);  /* heap */
    read_word(fp);  /* text_off */
    read_word(fp);  /* data_off */

    num_syms = symtab_size / (symlen + 3);

    /* seek to symbol table */
    fseek(fp, base + 16 + text_size + data_size, SEEK_SET);

    /* scan symbols looking for definitions of undefined symbols */
    for (i = 0; i < num_syms; i++) {
        read_word(fp);  /* skip value */
        type = read_byte(fp);
        fread(symname, 1, symlen, fp);
        symname[symlen] = '\0';
        seg = decode_seg(type);

        /* if this is a definition (not external) and we need it */
        if (seg != SEG_EXT && (type & 0x08) && is_undefined(symname)) {
            if (verbose) {
                printf("  %s satisfies %s\n", symname, symname);
            }
            needed = 1;
            /* don't break - continue scanning to report all */
        }
    }

    return needed;
}

/*
 * read object from archive at given offset
 * name is the archive member name for display
 */
void
read_ar_obj(arname, fp, base, membername)
char *arname;
FILE *fp;
long base;
char *membername;
{
    struct object *obj;
    struct symbol *gsym;
    unsigned char magic;
    unsigned short val;
    unsigned char type, seg;
    char symname[16];
    char *fullname;
    int i;

    fseek(fp, base, SEEK_SET);

    magic = read_byte(fp);
    if (magic != MAGIC) {
        fprintf(stderr, "wsld: %s(%s): bad magic\n", arname, membername);
        return;
    }

    /* create display name "archive(member)" */
    fullname = (char *)malloc(strlen(arname) + strlen(membername) + 3);
    sprintf(fullname, "%s(%s)", arname, membername);

    obj = (struct object *)malloc(sizeof(struct object));
    memset(obj, 0, sizeof(struct object));
    obj->name = fullname;
    obj->fp = fp;
    obj->file_base = base;

    obj->config = read_byte(fp);
    obj->symlen = (obj->config & CONF_SYMASK) * 2 + 1;
    obj->symtab_size = read_word(fp);
    obj->text_size = read_word(fp);
    obj->data_size = read_word(fp);
    obj->bss_size = read_word(fp);
    obj->heap_size = read_word(fp);
    obj->hdr_text_off = read_word(fp);
    obj->hdr_data_off = read_word(fp);

    obj->num_syms = obj->symtab_size / (obj->symlen + 3);

    if (Vflag)
        printf("%s\n", fullname);

    if (verbose) {
        printf("%s: symlen=%d text=%d data=%d bss=%d syms=%d\n",
               fullname, obj->symlen,
               obj->text_size, obj->data_size, obj->bss_size, obj->num_syms);
    }

    /* add to list */
    if (!objects) {
        objects = objects_tail = obj;
    } else {
        objects_tail->next = obj;
        objects_tail = obj;
    }

    /* allocate local symbol table for relocation lookups */
    obj->symtab = (struct symbol **)malloc(obj->num_syms * sizeof(struct symbol *));

    /* seek to symbol table */
    fseek(fp, base + 16 + obj->text_size + obj->data_size, SEEK_SET);

    /* read symbols */
    for (i = 0; i < obj->num_syms; i++) {
        val = read_word(fp);
        type = read_byte(fp);
        fread(symname, 1, obj->symlen, fp);
        symname[obj->symlen] = '\0';
        seg = decode_seg(type);

        gsym = sym_define(symname, val, seg, type, obj);
        obj->symtab[i] = gsym;

        if (verbose > 1) {
            printf("  [%d] %s: val=0x%04x seg=%s%s\n",
                   i, symname, val, wsSegNames[seg],
                   (type & 0x08) ? " global" : "");
        }
    }

    /* record relocation table positions (relative to file, not archive) */
    obj->textRelocOff = ftell(fp);

    /* skip text relocs to find data relocs */
    while (1) {
        unsigned char b = read_byte(fp);
        if (b == 0) break;
        if (b >= 32 && b < 64) read_byte(fp);
        else if (b == 0xfc) {
            b = read_byte(fp);
            if (b >= 0x80) read_byte(fp);
        }
    }
    obj->dataRelocOff = ftell(fp);

    if (verbose > 1) {
        printf("  text_reloc@0x%lx data_reloc@0x%lx\n",
               obj->textRelocOff, obj->dataRelocOff);
    }
}

/*
 * process archive file - include only objects that satisfy undefined symbols
 * returns number of objects added
 */
int
read_archive(name)
char *name;
{
    FILE *fp;
    unsigned char buf[2];
    unsigned short magic16;
    long off;
    char membername[15];
    unsigned short len;
    int count = 0;
    long base;

    fp = fopen(name, "rb");
    if (fp == NULL)
        error2("cannot open", name);

    if (fread(buf, 1, 2, fp) != 2)
        error2("read error", name);

    magic16 = buf[0] | (buf[1] << 8);
    if (magic16 != AR_MAGIC)
        error2("bad archive magic", name);

    if (verbose) {
        printf("scanning archive %s\n", name);
    }

    off = 2;  /* skip magic */
    while (1) {
        /* read 14-byte name */
        fseek(fp, off, SEEK_SET);
        if (fread(membername, 1, 14, fp) != 14)
            break;
        membername[14] = '\0';

        /* null name marks end */
        if (membername[0] == '\0')
            break;

        /* read length */
        if (fread(buf, 1, 2, fp) != 2)
            break;
        len = buf[0] | (buf[1] << 8);

        base = off + 16;  /* object starts after name and length */

        /* check if this member satisfies any undefined symbol */
        if (ar_needed(fp, base)) {
            if (verbose) {
                printf("including %s(%s)\n", name, membername);
            }
            read_ar_obj(name, fp, base, membername);
            count++;
        }

        off = base + len;
    }

    /* note: we don't close fp because objects keep it open for pass 2 */
    if (count == 0) {
        fclose(fp);
    }

    return count;
}

/*
 * map Hi-Tech psect name to Whitesmith segment
 */
unsigned char
map_psect(psect)
char *psect;
{
    if (strcmp(psect, "text") == 0)
        return SEG_TEXT;
    if (strcmp(psect, "data") == 0)
        return SEG_DATA;
    if (strcmp(psect, "bss") == 0)
        return SEG_BSS;
    /* empty or unknown psect maps to absolute */
    return SEG_ABS;
}

/*
 * read string from file (null-terminated, max len-1 chars)
 */
int
read_str(fp, buf, maxlen)
FILE *fp;
char *buf;
int maxlen;
{
    int i;
    int c;
    for (i = 0; i < maxlen - 1; i++) {
        c = fgetc(fp);
        if (c == EOF || c == '\0') {
            buf[i] = '\0';
            return i;
        }
        buf[i] = c;
    }
    buf[i] = '\0';
    /* skip rest of string if truncated */
    while ((c = fgetc(fp)) != EOF && c != '\0')
        ;
    return i;
}

/*
 * read Hi-Tech object file
 */
void
read_ht_object(name)
char *name;
{
    struct object *obj;
    FILE *fp;
    unsigned char hdr[3];
    int reclen, rectype;
    long off, size;
    unsigned char *textbuf = NULL;
    unsigned char *databuf = NULL;
    unsigned short textsize = 0, textalloc = 0;
    unsigned short datasize = 0, dataalloc = 0;
    unsigned short bsssize = 0;
    struct ht_reloc *htrelocs = NULL;
    int htreloc_alloc = 0, nhtrelocs = 0;
    unsigned char cur_seg = SEG_TEXT;
    unsigned short cur_off = 0;

    fp = fopen(name, "rb");
    if (fp == NULL)
        error2("cannot open", name);

    fseek(fp, 0, SEEK_END);
    size = ftell(fp);
    fseek(fp, 0, SEEK_SET);

    obj = (struct object *)malloc(sizeof(struct object));
    memset(obj, 0, sizeof(struct object));
    obj->name = name;
    obj->fp = fp;
    obj->is_hitech = 1;

    if (Vflag)
        printf("%s (HiTech)\n", name);

    /* add to object list */
    if (!objects) {
        objects = objects_tail = obj;
    } else {
        objects_tail->next = obj;
        objects_tail = obj;
    }

    /* parse records */
    off = 0;
    while (off < size - 3) {
        fseek(fp, off, SEEK_SET);
        if (fread(hdr, 1, 3, fp) != 3)
            break;

        reclen = hdr[0] | (hdr[1] << 8);
        rectype = hdr[2];
        off += 3;

        if (off + reclen > size)
            break;

        switch (rectype) {
        case HT_IDENT:
        case HT_PSECT:
        case HT_START:
            /* skip - not needed for linking */
            break;

        case HT_TEXT:
            if (reclen >= 5) {
                /* TEXT: 4-byte offset + psect name + data */
                unsigned long toff;
                char psect[64];
                int plen, dlen;
                unsigned char seg;
                unsigned short endoff;

                toff = read_word(fp);
                toff |= (unsigned long)read_word(fp) << 16;
                plen = read_str(fp, psect, 64);

                dlen = reclen - 4 - plen - 1;
                seg = map_psect(psect);
                cur_seg = seg;
                cur_off = (unsigned short)toff;

                /* collect data into appropriate buffer */
                if (seg == SEG_TEXT && dlen > 0) {
                    endoff = cur_off + dlen;
                    if (endoff > textsize) textsize = endoff;
                    if (endoff > textalloc) {
                        textalloc = endoff + 256;
                        textbuf = realloc(textbuf, textalloc);
                    }
                    fread(textbuf + cur_off, 1, dlen, fp);
                } else if (seg == SEG_DATA && dlen > 0) {
                    endoff = cur_off + dlen;
                    if (endoff > datasize) datasize = endoff;
                    if (endoff > dataalloc) {
                        dataalloc = endoff + 256;
                        databuf = realloc(databuf, dataalloc);
                    }
                    fread(databuf + cur_off, 1, dlen, fp);
                } else if (seg == SEG_BSS) {
                    endoff = cur_off + dlen;
                    if (endoff > bsssize) bsssize = endoff;
                }
            }
            break;

        case HT_RELOC:
            /* RELOC records apply to most recent TEXT segment */
            fseek(fp, off, SEEK_SET);
            {
                long rend = off + reclen;
                while (ftell(fp) < rend) {
                    unsigned short reloff;
                    unsigned char reltype;
                    char target[64];

                    reloff = read_word(fp);
                    reltype = read_byte(fp);
                    read_str(fp, target, 64);

                    /* grow reloc array if needed */
                    if (nhtrelocs >= htreloc_alloc) {
                        htreloc_alloc = htreloc_alloc ? htreloc_alloc * 2 : 32;
                        htrelocs = realloc(htrelocs,
                            htreloc_alloc * sizeof(struct ht_reloc));
                    }

                    /* record relocation */
                    htrelocs[nhtrelocs].offset = cur_off + reloff;
                    htrelocs[nhtrelocs].type = reltype;
                    htrelocs[nhtrelocs].seg = cur_seg;
                    strncpy(htrelocs[nhtrelocs].target, target, 15);
                    htrelocs[nhtrelocs].target[15] = '\0';
                    nhtrelocs++;
                }
            }
            break;

        case HT_SYMBOL:
            /* parse symbol record */
            fseek(fp, off, SEEK_SET);
            {
                long send = off + reclen;
                while (ftell(fp) < send) {
                    unsigned long val;
                    unsigned short flags;
                    char psect[64], symname[64];
                    unsigned char seg, type;

                    val = read_word(fp);
                    val |= (unsigned long)read_word(fp) << 16;
                    flags = read_word(fp);
                    read_str(fp, psect, 64);
                    read_str(fp, symname, 64);

                    if (symname[0] == '\0')
                        continue;

                    /* overflow check for 32->16 bit */
                    if (val > 0xFFFF && verbose) {
                        fprintf(stderr, "wsld: warning: %s value truncated\n",
                                symname);
                    }

                    /* determine segment */
                    if ((flags & 0x0f) == 6) {
                        seg = SEG_EXT;  /* undefined/external */
                    } else if (flags & HT_F_ABS) {
                        seg = SEG_ABS;
                    } else {
                        seg = map_psect(psect);
                    }

                    /* construct type byte for WS compatibility */
                    type = 0;
                    if (flags & HT_F_GLOBAL)
                        type |= 0x08;  /* global flag */
                    switch (seg) {
                    case SEG_ABS:  type |= 4; break;
                    case SEG_TEXT: type |= 5; break;
                    case SEG_DATA: type |= 6; break;
                    case SEG_BSS:  type |= 7; break;
                    }

                    /* register symbol (only globals for now) */
                    if (flags & HT_F_GLOBAL) {
                        sym_define(symname, (unsigned short)val, seg, type, obj);
                    }

                    if (verbose > 1) {
                        printf("  %s: val=0x%04x seg=%s%s\n",
                               symname, (unsigned short)val,
                               wsSegNames[seg],
                               (flags & HT_F_GLOBAL) ? " global" : "");
                    }
                }
            }
            break;

        case HT_END:
            goto done_parsing;
        }

        off += reclen;
    }

done_parsing:
    /* store collected data in object */
    obj->ht_text = textbuf;
    obj->ht_data = databuf;
    obj->text_size = textsize;
    obj->data_size = datasize;
    obj->bss_size = bsssize;
    obj->ht_relocs = htrelocs;
    obj->ht_nrelocs = nhtrelocs;

    /* close file - data is now in memory buffers */
    fclose(fp);
    obj->fp = NULL;

    if (verbose) {
        printf("%s: text=%d data=%d bss=%d relocs=%d\n",
               name, textsize, datasize, bsssize, nhtrelocs);
    }
}

/*
 * read Hi-Tech object from memory buffer (library member)
 */
void
read_ht_ar_obj(arname, fp, base, msize, membername)
char *arname;
FILE *fp;
long base;
long msize;
char *membername;
{
    struct object *obj;
    unsigned char hdr[3];
    int reclen, rectype;
    long off, endpos;
    unsigned char *textbuf = NULL;
    unsigned char *databuf = NULL;
    unsigned short textsize = 0, textalloc = 0;
    unsigned short datasize = 0, dataalloc = 0;
    unsigned short bsssize = 0;
    struct ht_reloc *htrelocs = NULL;
    int htreloc_alloc = 0, nhtrelocs = 0;
    unsigned char cur_seg = SEG_TEXT;
    unsigned short cur_off = 0;
    char *fullname;

    /* create "archive(member)" name */
    fullname = malloc(strlen(arname) + strlen(membername) + 3);
    sprintf(fullname, "%s(%s)", arname, membername);

    obj = (struct object *)malloc(sizeof(struct object));
    memset(obj, 0, sizeof(struct object));
    obj->name = fullname;
    obj->fp = fp;
    obj->file_base = base;
    obj->is_hitech = 1;

    if (Vflag)
        printf("%s (HiTech)\n", fullname);

    /* add to object list */
    if (!objects) {
        objects = objects_tail = obj;
    } else {
        objects_tail->next = obj;
        objects_tail = obj;
    }

    endpos = base + msize;
    off = base;

    /* parse records */
    while (off < endpos - 3) {
        fseek(fp, off, SEEK_SET);
        if (fread(hdr, 1, 3, fp) != 3)
            break;

        reclen = hdr[0] | (hdr[1] << 8);
        rectype = hdr[2];
        off += 3;

        if (off + reclen > endpos)
            break;

        switch (rectype) {
        case HT_IDENT:
        case HT_PSECT:
        case HT_START:
            break;

        case HT_TEXT:
            if (reclen >= 5) {
                unsigned long toff;
                char psect[64];
                int plen, dlen;
                unsigned char seg;
                unsigned short endoff;

                toff = read_word(fp);
                toff |= (unsigned long)read_word(fp) << 16;
                plen = read_str(fp, psect, 64);

                dlen = reclen - 4 - plen - 1;
                seg = map_psect(psect);
                cur_seg = seg;
                cur_off = (unsigned short)toff;

                if (seg == SEG_TEXT && dlen > 0) {
                    endoff = cur_off + dlen;
                    if (endoff > textsize) textsize = endoff;
                    if (endoff > textalloc) {
                        textalloc = endoff + 256;
                        textbuf = realloc(textbuf, textalloc);
                    }
                    fread(textbuf + cur_off, 1, dlen, fp);
                } else if (seg == SEG_DATA && dlen > 0) {
                    endoff = cur_off + dlen;
                    if (endoff > datasize) datasize = endoff;
                    if (endoff > dataalloc) {
                        dataalloc = endoff + 256;
                        databuf = realloc(databuf, dataalloc);
                    }
                    fread(databuf + cur_off, 1, dlen, fp);
                } else if (seg == SEG_BSS) {
                    endoff = cur_off + dlen;
                    if (endoff > bsssize) bsssize = endoff;
                }
            }
            break;

        case HT_RELOC:
            fseek(fp, off, SEEK_SET);
            {
                long rend = off + reclen;
                while (ftell(fp) < rend) {
                    unsigned short reloff;
                    unsigned char reltype;
                    char target[64];

                    reloff = read_word(fp);
                    reltype = read_byte(fp);
                    read_str(fp, target, 64);

                    if (nhtrelocs >= htreloc_alloc) {
                        htreloc_alloc = htreloc_alloc ? htreloc_alloc * 2 : 32;
                        htrelocs = realloc(htrelocs,
                            htreloc_alloc * sizeof(struct ht_reloc));
                    }

                    htrelocs[nhtrelocs].offset = cur_off + reloff;
                    htrelocs[nhtrelocs].type = reltype;
                    htrelocs[nhtrelocs].seg = cur_seg;
                    strncpy(htrelocs[nhtrelocs].target, target, 15);
                    htrelocs[nhtrelocs].target[15] = '\0';
                    nhtrelocs++;
                }
            }
            break;

        case HT_SYMBOL:
            fseek(fp, off, SEEK_SET);
            {
                long send = off + reclen;
                while (ftell(fp) < send) {
                    unsigned long val;
                    unsigned short flags;
                    char psect[64], symname[64];
                    unsigned char seg, type;

                    val = read_word(fp);
                    val |= (unsigned long)read_word(fp) << 16;
                    flags = read_word(fp);
                    read_str(fp, psect, 64);
                    read_str(fp, symname, 64);

                    if (symname[0] == '\0')
                        continue;

                    if ((flags & 0x0f) == 6) {
                        seg = SEG_EXT;
                    } else if (flags & HT_F_ABS) {
                        seg = SEG_ABS;
                    } else {
                        seg = map_psect(psect);
                    }

                    type = 0;
                    if (flags & HT_F_GLOBAL)
                        type |= 0x08;
                    switch (seg) {
                    case SEG_ABS:  type |= 4; break;
                    case SEG_TEXT: type |= 5; break;
                    case SEG_DATA: type |= 6; break;
                    case SEG_BSS:  type |= 7; break;
                    }

                    if (flags & HT_F_GLOBAL) {
                        sym_define(symname, (unsigned short)val, seg, type, obj);
                    }
                }
            }
            break;

        case HT_END:
            goto done_ar_parsing;
        }

        off += reclen;
    }

done_ar_parsing:
    obj->ht_text = textbuf;
    obj->ht_data = databuf;
    obj->text_size = textsize;
    obj->data_size = datasize;
    obj->bss_size = bsssize;
    obj->ht_relocs = htrelocs;
    obj->ht_nrelocs = nhtrelocs;

    /* don't store fp - library manages it, data is in memory */
    obj->fp = NULL;

    if (verbose) {
        printf("%s: text=%d data=%d bss=%d relocs=%d\n",
               fullname, textsize, datasize, bsssize, nhtrelocs);
    }
}

/*
 * check if Hi-Tech library module defines any currently undefined symbol
 * reads from symbol directory in library header
 */
int
ht_ar_needed(fp, symoff, symcnt)
FILE *fp;
long symoff;
int symcnt;
{
    int i;
    unsigned char symflag;
    char symname[64];

    fseek(fp, symoff, SEEK_SET);

    for (i = 0; i < symcnt; i++) {
        symflag = fgetc(fp);

        /* read symbol name */
        read_str(fp, symname, 64);

        /* check if this is a defined symbol that we need */
        if (symflag == HT_SYM_DEF || symflag == HT_SYM_COMMON) {
            if (is_undefined(symname)) {
                if (verbose) {
                    printf("  %s satisfies undefined\n", symname);
                }
                return 1;
            }
        }
    }
    return 0;
}

/*
 * process Hi-Tech library file
 * returns number of modules included
 */
int
read_ht_archive(name)
char *name;
{
    FILE *fp;
    unsigned char buf[16];
    unsigned short sym_size, num_mods;
    long symoff, modoff;
    int i, count = 0;

    fp = fopen(name, "rb");
    if (fp == NULL)
        error2("cannot open", name);

    /* read library header */
    if (fread(buf, 1, 4, fp) != 4)
        error2("read error", name);

    sym_size = buf[0] | (buf[1] << 8);
    num_mods = buf[2] | (buf[3] << 8);

    if (verbose) {
        printf("scanning HiTech library %s: %d modules\n", name, num_mods);
    }

    modoff = 4 + sym_size;  /* module data starts after symbol directory */
    symoff = 4;             /* symbol directory starts after header */

    /* scan each module */
    for (i = 0; i < num_mods; i++) {
#ifdef notdef
        unsigned short symSize;
#endif
		unsigned short symCnt;
        unsigned long moduleSize;
        char moduleName[256];
        long mod_symoff;
        int j;

        fseek(fp, symoff, SEEK_SET);
        if (fread(buf, 1, 12, fp) != 12)
            break;

#ifdef notdef
        symSize = buf[0] | (buf[1] << 8);
#endif
        symCnt = buf[2] | (buf[3] << 8);
        moduleSize = buf[4] | (buf[5] << 8) |
                     ((unsigned long)buf[6] << 16) |
                     ((unsigned long)buf[7] << 24);
        symoff += 12;

        /* read module name */
        read_str(fp, moduleName, 256);
        symoff += strlen(moduleName) + 1;

        mod_symoff = symoff;  /* remember symbol entries offset */

        /* check if this module is needed */
        if (ht_ar_needed(fp, mod_symoff, symCnt)) {
            if (verbose) {
                printf("including %s(%s)\n", name, moduleName);
            }
            read_ht_ar_obj(name, fp, modoff, moduleSize, moduleName);
            count++;
        }

        /* skip past symbol entries */
        fseek(fp, mod_symoff, SEEK_SET);
        for (j = 0; j < symCnt; j++) {
            fgetc(fp);  /* flags */
            while (fgetc(fp) != '\0')  /* name */
                ;
        }
        symoff = ftell(fp);

        modoff += moduleSize;
    }

    /* always close - objects have data in memory buffers */
    fclose(fp);

    return count;
}

/*
 * Pass 1: assign segment addresses to each object
 */
void
pass1_layout()
{
    struct object *obj;
    struct symbol *s;

    /* assign segment offsets to each object */
    for (obj = objects; obj; obj = obj->next) {
        obj->text_off = text_pos;
        obj->data_off = data_pos;
        obj->bss_off = bss_pos;

        text_pos += obj->text_size;
        data_pos += obj->data_size;
        bss_pos += obj->bss_size;

        if (verbose) {
            printf("%s: text@0x%04x data@0x%04x bss@0x%04x\n",
                   obj->name, obj->text_off, obj->data_off, obj->bss_off);
        }
    }

    total_text = text_pos;
    total_data = data_pos;
    total_bss = bss_pos;

    /* find linker-defined symbols and save original offsets BEFORE resolution */
    {
        int i;
        for (i = 0; i < LSYM_COUNT; i++) {
            s = sym_lookup(lnksyms[i].name);
            if (s && s->seg == SEG_DATA && s->obj) {
                lnksyms[i].obj = s->obj;
                /* original value is text_size + data_offset */
                lnksyms[i].off = s->value - s->obj->text_size;
            }
        }
    }

    /* now resolve all symbol addresses */
    {
    int undef_count = 0;
    for (s = symbols; s; s = s->next) {
        if (s->seg == SEG_EXT) {
            fprintf(stderr, "wsld: undefined symbol: %s\n", s->name);
            undef_count++;
            continue;
        }
        if (s->seg == SEG_ABS)
            continue;

        /* adjust value based on object's segment offset */
        if (s->obj) {
            switch (s->seg) {
            case SEG_TEXT:
                s->value = text_base + s->obj->text_off + s->value;
                break;
            case SEG_DATA:
                s->value = data_base + total_text + s->obj->data_off +
                           (s->value - s->obj->text_size);
                break;
            case SEG_BSS:
                s->value = bss_base + total_text + total_data + s->obj->bss_off +
                           (s->value - s->obj->text_size - s->obj->data_size);
                break;
            }
        }

        if (verbose > 1) {
            printf("resolved %s = 0x%04x\n", s->name, s->value);
        }
    }
    if (undef_count && !rflag)
        exit(1);
    }

    /* set linker symbol values and change to absolute */
    {
        unsigned short vals[LSYM_COUNT];
        int i;

        vals[LSYM_LTEXT] = text_base;
        vals[LSYM_HTEXT] = text_base + total_text;
        vals[LSYM_LDATA] = data_base + total_text;
        vals[LSYM_HDATA] = data_base + total_text + total_data;
        vals[LSYM_LBSS]  = bss_base + total_text + total_data;
        vals[LSYM_HBSS]  = bss_base + total_text + total_data + total_bss;

        for (i = 0; i < LSYM_COUNT; i++) {
            s = sym_lookup(lnksyms[i].name);
            if (s) {
                s->value = vals[i];
                s->seg = SEG_ABS;
                if (verbose)
                    printf("%s = 0x%04x\n", lnksyms[i].name, vals[i]);
            }
        }
    }
}

/*
 * get relocation target value for a symbol index
 */
unsigned short
getRelocVal(obj, ctrl)
struct object *obj;
int ctrl;
{
    struct symbol *s;
    int idx;

    /* decode control byte to get segment or symbol */
    switch (ctrl) {
    case 0x40:  /* absolute - no change */
        return 0;
    case 0x44:  /* text segment */
        return text_base + obj->text_off;
    case 0x48:  /* data segment */
        return data_base + total_text + obj->data_off;
    case 0x4c:  /* bss segment */
        return bss_base + total_text + total_data + obj->bss_off;
    default:
        /* symbol reference */
        if (ctrl >= 0x50 && ctrl < 0xfc) {
            idx = ((ctrl - 0x50) >> 2);
        } else {
            /* extended encoding - not handled yet */
            error("extended reloc encoding not supported");
            return 0;
        }
        if (idx >= obj->num_syms) {
            error("reloc symbol index out of range");
        }
        s = obj->symtab[idx];
        return s->value;
    }
}

/*
 * add a pending relocation for -r output
 */
void
add_outreloc(list, tail, offset, sym, seg, hilo)
struct outreloc **list;
struct outreloc **tail;
unsigned short offset;
struct symbol *sym;
unsigned char seg;
unsigned char hilo;
{
    struct outreloc *r = malloc(sizeof(struct outreloc));
    r->offset = offset;
    r->sym = sym;
    r->seg = seg;
    r->hilo = hilo;
    r->next = NULL;
    if (*tail)
        (*tail)->next = r;
    else
        *list = r;
    *tail = r;
}

/*
 * find symbol index in output symbol table
 */
int
findSymIdx(sym)
struct symbol *sym;
{
    struct symbol *s;
    int idx = 0;
    for (s = symbols; s; s = s->next, idx++) {
        if (s == sym)
            return idx;
    }
    return -1;
}

/*
 * write a relocation table using shared wsobj functions
 */
void
write_relocs(rlist)
struct outreloc *rlist;
{
    struct outreloc *r;
    int last = 0;
    int bump;

    for (r = rlist; r; r = r->next) {
        bump = r->offset - last;
        wsEncBump(outfp, bump);

        if (r->sym) {
            /* symbol reference */
            int idx = findSymIdx(r->sym);
            wsEncReloc(outfp, -1, idx, r->hilo);
        } else {
            /* segment reference */
            wsEncReloc(outfp, r->seg, 0, r->hilo);
        }
        last = r->offset + (r->hilo ? 1 : 2);
    }
    wsEndReloc(outfp);
}

/*
 * apply relocations to segment data
 * reloc_off: file offset of relocation table
 * buf: segment data buffer
 * seg_size: size of segment
 * seg_base: base address adjustment for this object's segment
 * is_text: 1 for text segment, 0 for data segment (for -r reloc collection)
 */
void
apply_relocs(obj, reloc_off, buf, seg_size, seg_base, is_text)
struct object *obj;
long reloc_off;
unsigned char *buf;
int seg_size;
unsigned short seg_base;
int is_text;
{
    int pos = 0;
    unsigned char b;
    int bump, idx;
    unsigned short val, add;
    struct symbol *s;
    int need_reloc;     /* for -r: does this reloc need to be preserved? */
    unsigned char outseg;
    int hilo;           /* 0=word, 1=lo, 2=hi */
    int size;           /* relocation size: 2 for word, 1 for lo/hi */

    fseek(obj->fp, reloc_off, SEEK_SET);

    while (1) {
        b = read_byte(obj->fp);
        if (b == 0) break;  /* end of relocs */

        /* decode bump */
        if (b < 32) {
            bump = b;
        } else if (b < 64) {
            bump = ((b - 32) << 8) + read_byte(obj->fp) + 32;
        } else {
            /* control byte - determine relocation type */
            add = 0;
            need_reloc = 0;
            s = NULL;
            outseg = 0;
            hilo = 0;

            /* segment relocations: 0x40-0x4f range */
            if (b >= 0x40 && b < 0x50) {
                hilo = b & 3;
                switch (b & ~3) {
                case 0x40:  /* absolute - no adjustment */
                    add = 0;
                    break;
                case 0x44:  /* text segment */
                    add = text_base + obj->text_off;
                    if (rflag) {
                        need_reloc = 1;
                        outseg = SEG_TEXT;
                    }
                    break;
                case 0x48:  /* data segment */
                    add = data_base + total_text + obj->data_off - obj->text_size;
                    if (rflag) {
                        need_reloc = 1;
                        outseg = SEG_DATA;
                    }
                    break;
                case 0x4c:  /* bss segment */
                    add = bss_base + total_text + total_data + obj->bss_off
                          - obj->text_size - obj->data_size;
                    if (rflag) {
                        need_reloc = 1;
                        outseg = SEG_BSS;
                    }
                    break;
                }
            } else if (b >= 0x50 && b < 0xfc) {
                /* symbol reference - low 2 bits are hilo */
                hilo = b & 3;
                idx = (b - 0x50) >> 2;
                if (idx < obj->num_syms) {
                    s = obj->symtab[idx];
                    if (hilo && s->obj == obj) {
                        /* hi/lo on symbol in same object: use segment base */
                        if (s->seg == SEG_TEXT)
                            add = text_base + obj->text_off;
                        else if (s->seg == SEG_DATA)
                            add = data_base + total_text + obj->data_off - obj->text_size;
                        else if (s->seg == SEG_BSS)
                            add = bss_base + total_text + total_data + obj->bss_off
                                  - obj->text_size - obj->data_size;
                        else
                            add = s->value;
                        /* in -r mode, preserve as symbol relocation */
                        if (rflag && hilo)
                            need_reloc = 1;
                    } else {
                        /* word reloc or external symbol: use full value */
                        add = s->value;
                        /* in -r mode, preserve symbol hi/lo relocations */
                        if (rflag && (s->seg == SEG_EXT || hilo))
                            need_reloc = 1;
                    }
                }
            } else if ((b & ~3) == 0xfc) {
                /* extended symbol encoding - low 2 bits are hilo */
                hilo = b & 3;
                b = read_byte(obj->fp);
                if (b < 0x80) {
                    idx = b + 47 - 4;
                } else {
                    idx = ((b - 0x80) << 8) + read_byte(obj->fp) + 175 - 4;
                }
                if (idx < obj->num_syms) {
                    s = obj->symtab[idx];
                    if (hilo && s->obj == obj) {
                        /* hi/lo on symbol in same object: use segment base */
                        if (s->seg == SEG_TEXT)
                            add = text_base + obj->text_off;
                        else if (s->seg == SEG_DATA)
                            add = data_base + total_text + obj->data_off - obj->text_size;
                        else if (s->seg == SEG_BSS)
                            add = bss_base + total_text + total_data + obj->bss_off
                                  - obj->text_size - obj->data_size;
                        else
                            add = s->value;
                        /* in -r mode, preserve as symbol relocation */
                        if (rflag && hilo)
                            need_reloc = 1;
                    } else {
                        /* word reloc or external symbol: use full value */
                        add = s->value;
                        /* in -r mode, preserve symbol hi/lo relocations */
                        if (rflag && (s->seg == SEG_EXT || hilo))
                            need_reloc = 1;
                    }
                }
            }

            /* determine relocation size */
            size = (hilo == 0) ? 2 : 1;

            /* apply relocation at current position */
            if (pos < seg_size) {
                if (hilo == 0) {
                    /* word relocation */
                    if (pos + 1 < seg_size) {
                        val = buf[pos] | (buf[pos + 1] << 8);
                        val += add;
                        buf[pos] = val & 0xff;
                        buf[pos + 1] = val >> 8;
                    }
                } else if (hilo == 1) {
                    /* lo byte relocation */
                    val = buf[pos] + (add & 0xff);
                    buf[pos] = val & 0xff;
                } else {
                    /* hi byte relocation */
                    val = buf[pos] + (add >> 8);
                    buf[pos] = val & 0xff;
                }

                if (verbose > 2) {
                    printf("    reloc @%04x += %04x -> %02x (%s)\n",
                           seg_base + pos, add,
                           hilo ? buf[pos] : (buf[pos] | (buf[pos+1]<<8)),
                           hilo == 0 ? "word" : hilo == 1 ? "lo" : "hi");
                }
            }

            /* collect reloc for -r output */
            if (need_reloc) {
                if (is_text)
                    add_outreloc(&text_relocs, &textRelocTl,
                                 seg_base + pos, s, outseg, hilo);
                else
                    add_outreloc(&data_relocs, &dataRelocTl,
                                 seg_base + pos, s, outseg, hilo);
            }

            pos += size;
            continue;
        }

        pos += bump;
    }
}

/*
 * apply Hi-Tech relocations to segment data
 */
void
apply_ht_relocs(obj, buf, seg, seg_size, seg_base)
struct object *obj;
unsigned char *buf;
unsigned char seg;           /* SEG_TEXT or SEG_DATA */
unsigned short seg_size;
unsigned short seg_base;
{
    int i;
    struct symbol *s;
    unsigned short target_val;
    unsigned short pos;
    int rsize;

    for (i = 0; i < obj->ht_nrelocs; i++) {
        struct ht_reloc *r = &obj->ht_relocs[i];

        if (r->seg != seg)
            continue;

        pos = r->offset;
        if (pos >= seg_size)
            continue;

        rsize = r->type & HT_RSIZE_MASK;

        /* get target value */
        if (r->type & HT_RPSECT) {
            /* psect-relative: target is segment name */
            unsigned char tseg = map_psect(r->target);
            switch (tseg) {
            case SEG_TEXT:
                target_val = text_base + obj->text_off;
                break;
            case SEG_DATA:
                target_val = data_base + total_text + obj->data_off;
                break;
            case SEG_BSS:
                target_val = bss_base + total_text + total_data + obj->bss_off;
                break;
            default:
                target_val = 0;
            }
        } else {
            /* symbol reference: lookup target name */
            s = sym_lookup(r->target);
            if (s == NULL) {
                fprintf(stderr, "wsld: undefined symbol: %s\n", r->target);
                target_val = 0;
            } else {
                target_val = s->value;
            }
        }

        /* apply relocation */
        if (rsize == HT_RWORD && pos + 1 < seg_size) {
            unsigned short val = buf[pos] | (buf[pos+1] << 8);
            val += target_val;
            buf[pos] = val & 0xff;
            buf[pos+1] = val >> 8;
        } else if (rsize == HT_RBYTE) {
            buf[pos] += target_val & 0xff;
        }

        if (verbose > 2) {
            printf("  ht_reloc @%04x += %04x (%s)\n",
                   seg_base + pos, target_val, r->target);
        }
    }
}

/*
 * patch linker-defined symbols in data segment
 * these are symbols like __Lbss/__Hbss that need their values written
 * to the data location where they are defined
 */
void
patch_linker_syms(obj, buf, seg_size)
struct object *obj;
unsigned char *buf;
int seg_size;
{
    unsigned short vals[LSYM_COUNT];
    int i;

    vals[LSYM_LTEXT] = text_base;
    vals[LSYM_HTEXT] = text_base + total_text;
    vals[LSYM_LDATA] = data_base + total_text;
    vals[LSYM_HDATA] = data_base + total_text + total_data;
    vals[LSYM_LBSS]  = bss_base + total_text + total_data;
    vals[LSYM_HBSS]  = bss_base + total_text + total_data + total_bss;

    for (i = 0; i < LSYM_COUNT; i++) {
        if (lnksyms[i].obj == obj && lnksyms[i].off + 1 < seg_size) {
            buf[lnksyms[i].off] = vals[i] & 0xff;
            buf[lnksyms[i].off + 1] = vals[i] >> 8;
            if (verbose > 1)
                printf("  patched %s at data+0x%04x = 0x%04x\n",
                       lnksyms[i].name, lnksyms[i].off, vals[i]);
        }
    }
}

/*
 * copy segment data with relocations applied
 */
void
copy_segment(obj, seg_start, seg_size, reloc_off, seg_base, is_text)
struct object *obj;
int seg_start;
int seg_size;
long reloc_off;
unsigned short seg_base;
int is_text;
{
    unsigned char *buf;

    if (seg_size == 0)
        return;

    buf = (unsigned char *)malloc(seg_size);
    if (!buf)
        error("out of memory");

    if (obj->is_hitech) {
        /* Hi-Tech: copy from collected segment buffer */
        if (is_text) {
            if (obj->ht_text)
                memcpy(buf, obj->ht_text, seg_size);
            else
                memset(buf, 0, seg_size);
            apply_ht_relocs(obj, buf, SEG_TEXT, seg_size, seg_base);
        } else {
            if (obj->ht_data)
                memcpy(buf, obj->ht_data, seg_size);
            else
                memset(buf, 0, seg_size);
            apply_ht_relocs(obj, buf, SEG_DATA, seg_size, seg_base);
            patch_linker_syms(obj, buf, seg_size);
        }
    } else {
        /* Whitesmith: read from file */
        fseek(obj->fp, obj->file_base + seg_start, SEEK_SET);
        if (fread(buf, 1, seg_size, obj->fp) != seg_size)
            error("read error");

        apply_relocs(obj, reloc_off, buf, seg_size, seg_base, is_text);
        if (!is_text)
            patch_linker_syms(obj, buf, seg_size);
    }

    /* write to output */
    if (fwrite(buf, 1, seg_size, outfp) != seg_size)
        error("write error");

    free(buf);
}

/*
 * Pass 2: write output file
 */
void
pass2_output()
{
    struct object *obj;
    unsigned char config;
    int symlen;

    /* default to 15-char symbols */
    symlen = out_symlen ? out_symlen : 15;

    outfp = fopen(outfile, "wb");
    if (outfp == NULL)
        error2("cannot create", outfile);

    /* write header */
    write_byte(MAGIC);
    config = CONF_LITTLE | ((symlen - 1) / 2);
    if (!rflag)
        config |= CONF_NORELO;
    write_byte(config);
    write_word(sflag ? 0 : num_globals * (symlen + 3));   /* symtab size */
    write_word(total_text);
    write_word(total_data);
    write_word(total_bss);
    write_word(0);              /* heap */
    write_word(text_base);      /* text offset */
    write_word(text_base + total_text);     /* data offset */

    /* write text segments */
    for (obj = objects; obj; obj = obj->next) {
        copy_segment(obj, 16, obj->text_size,
                     obj->textRelocOff,
                     text_base + obj->text_off, 1);
    }

    /* write data segments */
    for (obj = objects; obj; obj = obj->next) {
        copy_segment(obj, 16 + obj->text_size, obj->data_size,
                     obj->dataRelocOff,
                     data_base + total_text + obj->data_off, 0);
    }

    /* write symbol table (after text and data) unless stripped */
    if (!sflag) {
        struct symbol *s;
        int i;
        unsigned char type;

        for (s = symbols; s; s = s->next) {
            /* warn if symbol name will be truncated */
            if (strlen(s->name) > symlen) {
                fprintf(stderr, "wsld: symbol truncated: %s\n", s->name);
            }
            /* symbol entry: value, type, name */
            write_word(s->value);
            /* encode segment in type byte */
            switch (s->seg) {
            case SEG_ABS:  type = 0x04 | 0x08; break;
            case SEG_TEXT: type = 0x05 | 0x08; break;
            case SEG_DATA: type = 0x06 | 0x08; break;
            case SEG_BSS:  type = 0x07 | 0x08; break;
            default:       type = 0x08; break;
            }
            write_byte(type);
            for (i = 0; i < symlen; i++) {
                write_byte(s->name[i]);
            }
        }
    }

    /* write text relocs */
    if (rflag) {
        write_relocs(text_relocs);
    }

    /* write data relocs */
    if (rflag) {
        write_relocs(data_relocs);
    }

    fclose(outfp);
}

/*
 * print load map to stderr
 */
void
print_map()
{
    struct object *obj;
    struct symbol *s;

    fprintf(stderr, "\nLoad map:\n");
    fprintf(stderr, "  text: 0x%04x - 0x%04x (%d bytes)\n",
            text_base, text_base + total_text - 1, total_text);
    fprintf(stderr, "  data: 0x%04x - 0x%04x (%d bytes)\n",
            data_base + total_text,
            data_base + total_text + total_data - 1, total_data);
    fprintf(stderr, "  bss:  0x%04x - 0x%04x (%d bytes)\n",
            bss_base + total_text + total_data,
            bss_base + total_text + total_data + total_bss - 1, total_bss);

    fprintf(stderr, "\nObjects:\n");
    for (obj = objects; obj; obj = obj->next) {
        fprintf(stderr, "  %-20s text@%04x data@%04x bss@%04x\n",
                obj->name,
                text_base + obj->text_off,
                data_base + total_text + obj->data_off,
                bss_base + total_text + total_data + obj->bss_off);
    }

    fprintf(stderr, "\nSymbols:\n");
    for (s = symbols; s; s = s->next) {
        fprintf(stderr, "  %04x %c %s\n",
                s->value,
                s->seg == SEG_TEXT ? 'T' :
                s->seg == SEG_DATA ? 'D' :
                s->seg == SEG_BSS ? 'B' :
                s->seg == SEG_ABS ? 'A' : 'U',
                s->name);
    }
}

unsigned short
parse_addr(s)
char *s;
{
    unsigned short val = 0;
    int base = 10;

    if (s[0] == '0' && (s[1] == 'x' || s[1] == 'X')) {
        base = 16;
        s += 2;
    } else if (s[0] == '0') {
        base = 8;
    }

    while (*s) {
        val *= base;
        if (*s >= '0' && *s <= '9')
            val += *s - '0';
        else if (base == 16 && *s >= 'a' && *s <= 'f')
            val += *s - 'a' + 10;
        else if (base == 16 && *s >= 'A' && *s <= 'F')
            val += *s - 'A' + 10;
        else
            error2("bad address", s);
        s++;
    }
    return val;
}

/*
 * check if file is Hi-Tech object format
 * looks for IDENT record: 0x0A 0x00 0x07 (len=10, type=7)
 */
int
is_hitech_obj(name)
char *name;
{
    FILE *fp;
    unsigned char buf[3];
    int result = 0;

    fp = fopen(name, "rb");
    if (fp == NULL)
        return 0;

    if (fread(buf, 1, 3, fp) == 3) {
        result = HT_IS_HITECH(buf);
    }
    fclose(fp);
    return result;
}

/*
 * check if file is Hi-Tech library
 * libraries have: 4-byte header, then module data starting with IDENT record
 */
int
is_hitech_lib(name)
char *name;
{
    FILE *fp;
    unsigned char buf[20];
    unsigned short sym_size, num_mods;
    long size, mod_off;
    int result = 0;

    fp = fopen(name, "rb");
    if (fp == NULL)
        return 0;

    fseek(fp, 0, SEEK_END);
    size = ftell(fp);
    fseek(fp, 0, SEEK_SET);

    if (fread(buf, 1, 4, fp) == 4) {
        sym_size = buf[0] | (buf[1] << 8);
        num_mods = buf[2] | (buf[3] << 8);
        mod_off = 4 + sym_size;

        /* sanity check and verify first module is HT object */
        if (num_mods > 0 && num_mods < 1000 &&
            mod_off > 4 && mod_off < size) {
            unsigned char ident[3];
            fseek(fp, mod_off, SEEK_SET);
            if (fread(ident, 1, 3, fp) == 3 && HT_IS_HITECH(ident)) {
                result = 1;
            }
        }
    }
    fclose(fp);
    return result;
}

/*
 * check if file is an archive
 * returns: 0 = not archive, 1 = WS archive, 2 = HT library
 */
int
is_archive(name)
char *name;
{
    FILE *fp;
    unsigned char buf[2];
    unsigned short magic16;

    /* check WS archive first (quick magic check) */
    fp = fopen(name, "rb");
    if (fp == NULL)
        return 0;

    if (fread(buf, 1, 2, fp) != 2) {
        fclose(fp);
        return 0;
    }
    fclose(fp);

    magic16 = buf[0] | (buf[1] << 8);
    if (magic16 == AR_MAGIC)
        return 1;

    /* check Hi-Tech library */
    if (is_hitech_lib(name))
        return 2;

    return 0;
}

/*
 * input file list for archive re-processing
 */
struct infile {
    char *name;
    int is_archive;
    struct infile *next;
};

struct infile *infiles;
struct infile *infiles_tail;

void
add_infile(name, is_ar)
char *name;
int is_ar;
{
    struct infile *f = (struct infile *)malloc(sizeof(struct infile));
    f->name = name;
    f->is_archive = is_ar;
    f->next = 0;

    if (!infiles) {
        infiles = infiles_tail = f;
    } else {
        infiles_tail->next = f;
        infiles_tail = f;
    }
}

int
main(argc, argv)
int argc;
char **argv;
{
    int i;
    char *arg;
    int nfiles = 0;
    struct infile *f;
    int added, pass;

    /* first pass: parse options and collect input files */
    for (i = 1; i < argc; i++) {
        arg = argv[i];

        if (arg[0] == '-') {
            switch (arg[1]) {
            case 'v':
                verbose++;
                break;

            case 'V':
                Vflag++;
                break;

            case 'r':
                rflag++;
                break;

            case 's':
                sflag++;
                break;

            case '9':
                out_symlen = 9;
                break;

            case 'o':
                if (arg[2])
                    outfile = &arg[2];
                else if (++i < argc)
                    outfile = argv[i];
                else
                    usage();
                break;

            case 'T':
                /* -Ttext, -Tdata, -Tbss */
                if (strncmp(&arg[2], "text", 4) == 0) {
                    if (arg[6] == '=')
                        text_base = parse_addr(&arg[7]);
                    else if (++i < argc)
                        text_base = parse_addr(argv[i]);
                    else
                        usage();
                } else if (strncmp(&arg[2], "data", 4) == 0) {
                    if (arg[6] == '=')
                        data_base = parse_addr(&arg[7]);
                    else if (++i < argc)
                        data_base = parse_addr(argv[i]);
                    else
                        usage();
                } else if (strncmp(&arg[2], "bss", 3) == 0) {
                    if (arg[5] == '=')
                        bss_base = parse_addr(&arg[6]);
                    else if (++i < argc)
                        bss_base = parse_addr(argv[i]);
                    else
                        usage();
                } else {
                    usage();
                }
                break;

            default:
                usage();
            }
        } else {
            /* input file - check if archive or object */
            add_infile(arg, is_archive(arg));
            nfiles++;
        }
    }

    if (nfiles == 0)
        usage();

    /* process input files, handling archives specially */
    /* first load all .o files unconditionally */
    for (f = infiles; f; f = f->next) {
        if (f->is_archive == 0) {
            /* object file - auto-detect format */
            if (is_hitech_obj(f->name))
                read_ht_object(f->name);
            else
                read_object(f->name);
        }
    }

    /* now process archives, possibly multiple times for circular deps */
    pass = 0;
    do {
        added = 0;
        for (f = infiles; f; f = f->next) {
            if (f->is_archive == 1) {
                /* Whitesmith archive */
                added += read_archive(f->name);
            } else if (f->is_archive == 2) {
                /* Hi-Tech library */
                added += read_ht_archive(f->name);
            }
        }
        pass++;
        if (verbose && added) {
            printf("archive pass %d: added %d objects\n", pass, added);
        }
    } while (added > 0 && has_undefined());

    /* default data/bss base to text_base if not explicitly set */
    if (data_base == 0 && text_base != 0)
        data_base = text_base;
    if (bss_base == 0 && text_base != 0)
        bss_base = text_base;

    /* Pass 1: assign addresses and resolve symbols */
    pass1_layout();

    /* Pass 2: write output */
    pass2_output();

    /* Print load map */
    print_map();

    /* close all object files (avoid double-close for archive members) */
    {
        struct object *obj, *obj2;
        for (obj = objects; obj; obj = obj->next) {
            if (obj->fp) {
                fclose(obj->fp);
                /* clear fp from any other objects sharing the same handle */
                for (obj2 = obj->next; obj2; obj2 = obj2->next) {
                    if (obj2->fp == obj->fp)
                        obj2->fp = NULL;
                }
                obj->fp = NULL;
            }
        }
    }

    return 0;
}

/* vim: set tabstop=4 shiftwidth=4 noexpandtab: */
