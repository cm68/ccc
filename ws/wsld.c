/*
 * wsld - Whitesmith's object file linker
 *
 * Two-pass linker:
 * Pass 1: assign addresses, resolve symbols
 * Pass 2: write output with relocations applied
 */
#ifdef linux
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <fcntl.h>
#define INIT
#else
#include <stdio.h>
#define INIT = 0
#endif

#include "wsobj.h"

/* use wsSegNames from wsobj.c */

char verbose INIT;
char Vflag INIT;            /* -V: list object files */
char rflag INIT;            /* -r: emit relocatable output */
char out_symlen INIT;       /* output symbol length (0=15, set by -9) */

/*
 * segment base addresses (command line settable)
 */
unsigned short text_base INIT;
unsigned short data_base INIT;
unsigned short bss_base INIT;

/*
 * running totals for segment layout
 */
unsigned short text_pos INIT;
unsigned short data_pos INIT;
unsigned short bss_pos INIT;

/*
 * final segment sizes
 */
unsigned short total_text INIT;
unsigned short total_data INIT;
unsigned short total_bss INIT;

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

struct symbol *symbols INIT;
int num_globals INIT;

/*
 * object file info
 */
struct object {
    char *name;
    int fd;
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
    struct object *next;
};

struct object *objects INIT;
struct object *objects_tail INIT;

/*
 * pending relocation for -r output
 */
struct outreloc {
    unsigned short offset;      /* offset in merged segment */
    struct symbol *sym;         /* symbol (for ext) or NULL (for seg) */
    unsigned char seg;          /* segment type if sym is NULL */
    struct outreloc *next;
};

struct outreloc *text_relocs INIT;
struct outreloc *textRelocTl INIT;
struct outreloc *data_relocs INIT;
struct outreloc *dataRelocTl INIT;

char *outfile = "a.out";
int outfd INIT;

void
usage()
{
    fprintf(stderr, "usage: wsld [-vV9] [-r] [-o outfile] [-Ttext addr] [-Tdata addr] [-Tbss addr] file...\n");
    fprintf(stderr, "  -V          list object files linked\n");
    fprintf(stderr, "  -r          emit relocatable output (for subsequent links)\n");
    fprintf(stderr, "  -9          use 9-char symbols in output (default 15)\n");
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
read_word(fd)
int fd;
{
    unsigned char buf[2];
    if (read(fd, buf, 2) != 2)
        error("read error");
    return buf[0] | (buf[1] << 8);
}

unsigned char
read_byte(fd)
int fd;
{
    unsigned char c;
    if (read(fd, &c, 1) != 1)
        error("read error");
    return c;
}

void
write_byte(b)
unsigned char b;
{
    if (write(outfd, &b, 1) != 1)
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
    int fd;
    unsigned char magic;
    unsigned short val;
    unsigned char type, seg;
    char symname[16];
    int i;

    fd = open(name, O_RDONLY);
    if (fd < 0)
        error2("cannot open", name);

    magic = read_byte(fd);
    if (magic != MAGIC)
        error2("bad magic", name);

    obj = (struct object *)malloc(sizeof(struct object));
    memset(obj, 0, sizeof(struct object));
    obj->name = name;
    obj->fd = fd;

    obj->config = read_byte(fd);
    obj->symlen = (obj->config & CONF_SYMLEN) * 2 + 1;
    obj->symtab_size = read_word(fd);
    obj->text_size = read_word(fd);
    obj->data_size = read_word(fd);
    obj->bss_size = read_word(fd);
    obj->heap_size = read_word(fd);
    obj->hdr_text_off = read_word(fd);
    obj->hdr_data_off = read_word(fd);

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
    lseek(fd, 16 + obj->text_size + obj->data_size, SEEK_SET);

    /* read symbols */
    for (i = 0; i < obj->num_syms; i++) {
        val = read_word(fd);
        type = read_byte(fd);
        read(fd, symname, obj->symlen);
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
    obj->textRelocOff = lseek(fd, 0, SEEK_CUR);

    /* skip text relocs to find data relocs */
    while (1) {
        unsigned char b = read_byte(fd);
        if (b == 0) break;
        if (b >= 32 && b < 64) read_byte(fd);  /* extended bump */
        else if (b == 0xfc) {
            b = read_byte(fd);
            if (b >= 0x80) read_byte(fd);  /* extended symbol */
        }
    }
    obj->dataRelocOff = lseek(fd, 0, SEEK_CUR);

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
ar_needed(fd, base)
int fd;
long base;
{
    unsigned char magic, config;
    int symlen;
    unsigned short symtab_size, text_size, data_size;
    unsigned char type, seg;
    char symname[16];
    int num_syms, i;
    int needed = 0;

    lseek(fd, base, SEEK_SET);

    magic = read_byte(fd);
    if (magic != MAGIC)
        return 0;

    config = read_byte(fd);
    symlen = (config & CONF_SYMLEN) * 2 + 1;
    symtab_size = read_word(fd);
    text_size = read_word(fd);
    data_size = read_word(fd);
    read_word(fd);  /* bss */
    read_word(fd);  /* heap */
    read_word(fd);  /* text_off */
    read_word(fd);  /* data_off */

    num_syms = symtab_size / (symlen + 3);

    /* seek to symbol table */
    lseek(fd, base + 16 + text_size + data_size, SEEK_SET);

    /* scan symbols looking for definitions of undefined symbols */
    for (i = 0; i < num_syms; i++) {
        read_word(fd);  /* skip value */
        type = read_byte(fd);
        read(fd, symname, symlen);
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
read_ar_obj(arname, fd, base, membername)
char *arname;
int fd;
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

    lseek(fd, base, SEEK_SET);

    magic = read_byte(fd);
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
    obj->fd = fd;

    obj->config = read_byte(fd);
    obj->symlen = (obj->config & CONF_SYMLEN) * 2 + 1;
    obj->symtab_size = read_word(fd);
    obj->text_size = read_word(fd);
    obj->data_size = read_word(fd);
    obj->bss_size = read_word(fd);
    obj->heap_size = read_word(fd);
    obj->hdr_text_off = read_word(fd);
    obj->hdr_data_off = read_word(fd);

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
    lseek(fd, base + 16 + obj->text_size + obj->data_size, SEEK_SET);

    /* read symbols */
    for (i = 0; i < obj->num_syms; i++) {
        val = read_word(fd);
        type = read_byte(fd);
        read(fd, symname, obj->symlen);
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
    obj->textRelocOff = lseek(fd, 0, SEEK_CUR);

    /* skip text relocs to find data relocs */
    while (1) {
        unsigned char b = read_byte(fd);
        if (b == 0) break;
        if (b >= 32 && b < 64) read_byte(fd);
        else if (b == 0xfc) {
            b = read_byte(fd);
            if (b >= 0x80) read_byte(fd);
        }
    }
    obj->dataRelocOff = lseek(fd, 0, SEEK_CUR);

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
    int fd;
    unsigned char buf[2];
    unsigned short magic16;
    long off;
    char membername[15];
    unsigned short len;
    int count = 0;
    long base;

    fd = open(name, O_RDONLY);
    if (fd < 0)
        error2("cannot open", name);

    if (read(fd, buf, 2) != 2)
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
        lseek(fd, off, SEEK_SET);
        if (read(fd, membername, 14) != 14)
            break;
        membername[14] = '\0';

        /* null name marks end */
        if (membername[0] == '\0')
            break;

        /* read length */
        if (read(fd, buf, 2) != 2)
            break;
        len = buf[0] | (buf[1] << 8);

        base = off + 16;  /* object starts after name and length */

        /* check if this member satisfies any undefined symbol */
        if (ar_needed(fd, base)) {
            if (verbose) {
                printf("including %s(%s)\n", name, membername);
            }
            read_ar_obj(name, fd, base, membername);
            count++;
        }

        off = base + len;
    }

    /* note: we don't close fd because objects keep it open for pass 2 */
    if (count == 0) {
        close(fd);
    }

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
add_outreloc(list, tail, offset, sym, seg)
struct outreloc **list;
struct outreloc **tail;
unsigned short offset;
struct symbol *sym;
unsigned char seg;
{
    struct outreloc *r = malloc(sizeof(struct outreloc));
    r->offset = offset;
    r->sym = sym;
    r->seg = seg;
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
        wsEncBump(outfd, bump);

        if (r->sym) {
            /* symbol reference */
            int idx = findSymIdx(r->sym);
            wsEncReloc(outfd, -1, idx);
        } else {
            /* segment reference */
            wsEncReloc(outfd, r->seg, 0);
        }
        last = r->offset + 2;
    }
    wsEndReloc(outfd);
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

    lseek(obj->fd, reloc_off, SEEK_SET);

    while (1) {
        b = read_byte(obj->fd);
        if (b == 0) break;  /* end of relocs */

        /* decode bump */
        if (b < 32) {
            bump = b;
        } else if (b < 64) {
            bump = ((b - 32) << 8) + read_byte(obj->fd) + 32;
        } else {
            /* control byte - determine relocation type */
            add = 0;
            need_reloc = 0;
            s = NULL;
            outseg = 0;

            if (b == 0x40) {
                /* absolute - no adjustment */
                add = 0;
            } else if (b == 0x44) {
                /* text segment - code has text-relative offset */
                add = text_base + obj->text_off;
                if (rflag) {
                    /* preserve as text segment reloc */
                    need_reloc = 1;
                    outseg = SEG_TEXT;
                }
            } else if (b == 0x48) {
                /* data segment - code has combined offset, adjust */
                add = data_base + total_text + obj->data_off - obj->text_size;
                if (rflag) {
                    /* preserve as data segment reloc */
                    need_reloc = 1;
                    outseg = SEG_DATA;
                }
            } else if (b == 0x4c) {
                /* bss segment - code has combined offset, adjust */
                add = bss_base + total_text + total_data + obj->bss_off
                      - obj->text_size - obj->data_size;
                if (rflag) {
                    /* preserve as bss segment reloc */
                    need_reloc = 1;
                    outseg = SEG_BSS;
                }
            } else if (b >= 0x50 && b < 0xfc) {
                /* symbol reference */
                idx = (b - 0x50) >> 2;
                if (idx < obj->num_syms) {
                    s = obj->symtab[idx];
                    add = s->value;
                    if (rflag && s->seg == SEG_EXT) {
                        /* external symbol - preserve reloc */
                        need_reloc = 1;
                    }
                }
            } else if (b == 0xfc) {
                /* extended symbol encoding */
                b = read_byte(obj->fd);
                if (b < 0x80) {
                    idx = b + 47 - 4;
                } else {
                    idx = ((b - 0x80) << 8) + read_byte(obj->fd) + 175 - 4;
                }
                if (idx < obj->num_syms) {
                    s = obj->symtab[idx];
                    add = s->value;
                    if (rflag && s->seg == SEG_EXT) {
                        /* external symbol - preserve reloc */
                        need_reloc = 1;
                    }
                }
            }

            /* apply relocation at current position */
            if (pos + 1 < seg_size) {
                val = buf[pos] | (buf[pos + 1] << 8);
                val += add;
                buf[pos] = val & 0xff;
                buf[pos + 1] = val >> 8;

                if (verbose > 2) {
                    printf("    reloc @%04x += %04x -> %04x\n",
                           seg_base + pos, add, val);
                }
            }

            /* collect reloc for -r output */
            if (need_reloc) {
                if (is_text)
                    add_outreloc(&text_relocs, &textRelocTl,
                                 seg_base + pos, s, outseg);
                else
                    add_outreloc(&data_relocs, &dataRelocTl,
                                 seg_base + pos, s, outseg);
            }

            pos += 2;
            continue;
        }

        pos += bump;
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

    /* read segment data */
    lseek(obj->fd, seg_start, SEEK_SET);
    if (read(obj->fd, buf, seg_size) != seg_size)
        error("read error");

    /* apply relocations */
    apply_relocs(obj, reloc_off, buf, seg_size, seg_base, is_text);

    /* write to output */
    if (write(outfd, buf, seg_size) != seg_size)
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

    outfd = open(outfile, O_WRONLY | O_CREAT | O_TRUNC, 0666);
    if (outfd < 0)
        error2("cannot create", outfile);

    /* write header */
    write_byte(MAGIC);
    config = CONF_LITTLE | ((symlen - 1) / 2);
    if (!rflag)
        config |= CONF_NORELO;
    write_byte(config);
    write_word(rflag ? num_globals * (symlen + 3) : 0);   /* symtab size */
    write_word(total_text);
    write_word(total_data);
    write_word(total_bss);
    write_word(0);              /* heap */
    write_word(0);              /* text offset */
    write_word(total_text);     /* data offset */

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

    /* write symbol table (after text and data) */
    if (rflag) {
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

    close(outfd);
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
 * check if file is an archive (by reading magic)
 */
int
is_archive(name)
char *name;
{
    int fd;
    unsigned char buf[2];
    unsigned short magic16;

    fd = open(name, O_RDONLY);
    if (fd < 0)
        return 0;

    if (read(fd, buf, 2) != 2) {
        close(fd);
        return 0;
    }
    close(fd);

    magic16 = buf[0] | (buf[1] << 8);
    return magic16 == AR_MAGIC;
}

/*
 * input file list for archive re-processing
 */
struct infile {
    char *name;
    int is_archive;
    struct infile *next;
};

struct infile *infiles INIT;
struct infile *infiles_tail INIT;

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
        if (!f->is_archive) {
            read_object(f->name);
        }
    }

    /* now process archives, possibly multiple times for circular deps */
    pass = 0;
    do {
        added = 0;
        for (f = infiles; f; f = f->next) {
            if (f->is_archive) {
                added += read_archive(f->name);
            }
        }
        pass++;
        if (verbose && added) {
            printf("archive pass %d: added %d objects\n", pass, added);
        }
    } while (added > 0 && has_undefined());

    /* Pass 1: assign addresses and resolve symbols */
    pass1_layout();

    /* Pass 2: write output */
    pass2_output();

    /* Print load map */
    print_map();

    /* close all object files */
    {
        struct object *obj;
        for (obj = objects; obj; obj = obj->next) {
            close(obj->fd);
        }
    }

    return 0;
}

/*
 * vim: tabstop=4 shiftwidth=4 noexpandtab:
 */
