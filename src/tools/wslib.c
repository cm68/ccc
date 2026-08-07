/*
 * wslib - Whitesmith's archive librarian
 *
 * Archive format:
 *   2-byte magic (0xFF75 = 0177565 octal), little-endian
 *   Entries: 14-byte name + 2-byte length + object data
 *   End marker: entry with null name
 *
 * Usage:
 *   wslib -c archive.a obj1.o obj2.o ...   create archive
 *   wslib -x archive.a [obj1.o ...]        extract (all if no names)
 *   wslib -a archive.a obj1.o obj2.o ...   append to archive
 *   wslib -r archive.a obj1.o obj2.o ...   replace in archive
 *   wslib -cr archive.a obj1.o obj2.o ...  create if needed, then replace
 *   wslib -t archive.a                     list contents
 *   wslib -v                               verbose mode
 */
#include <stdio.h>

#if defined(linux) || defined(__linux__)
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#else
#endif

#include "wsobj.h"
#include "hiobj.h"

char *progname;
int verbose;
int hitech_mode;    /* -H flag: create HiTech format library */

/* Symbol table for HiTech library creation */
#define MAX_SYMS 500
struct htsym {
    char name[64];
    unsigned char flags;
};
struct htsym symtab[MAX_SYMS];
int nsyms;

void
usage()
{
    fprintf(stderr, "usage: %s [-crvH] archive [file...]\n", progname);
    fprintf(stderr, "  -c  create archive (with -r: create if not exists)\n");
    fprintf(stderr, "  -r  replace/add files in archive\n");
    fprintf(stderr, "  -v  verbose (list files as processed)\n");
    fprintf(stderr, "  -x  extract files (all if none specified)\n");
    fprintf(stderr, "  -a  append files to archive\n");
    fprintf(stderr, "  -t  list archive contents\n");
    fprintf(stderr, "  -H  create HiTech format library (default: Whitesmith)\n");
    exit(1);
}

void
error(msg)
char *msg;
{
    fprintf(stderr, "%s: %s\n", progname, msg);
    exit(1);
}

void
error2(msg, arg)
char *msg;
char *arg;
{
    fprintf(stderr, "%s: %s: %s\n", progname, msg, arg);
    exit(1);
}

/*
 * extract basename from path, truncate to 14 chars
 */
void
make_arname(dest, path)
char *dest;
char *path;
{
    char *p;
    int i;

    /* find last slash */
    p = path;
    while (*p) {
        if (*p == '/')
            path = p + 1;
        p++;
    }

    /* copy up to 14 chars */
    for (i = 0; i < 14 && path[i]; i++)
        dest[i] = path[i];
    for (; i < 14; i++)
        dest[i] = '\0';
}

/*
 * check if name matches any in list
 */
int
name_in_list(name, list, count)
char *name;
char **list;
int count;
{
    int i;
    char arname[15];

    for (i = 0; i < count; i++) {
        make_arname(arname, list[i]);
        arname[14] = '\0';
        if (strncmp(name, arname, 14) == 0)
            return 1;
    }
    return 0;
}

/*
 * write archive header
 */
void
write_header(fp)
FILE *fp;
{
    unsigned char buf[2];
    buf[0] = AR_MAGIC & 0xFF;
    buf[1] = (AR_MAGIC >> 8) & 0xFF;
    if (fwrite(buf, 1, 2, fp) != 2)
        error("write error");
}

/*
 * write one archive entry
 */
void
write_entry(arfp, name, data, len)
FILE *arfp;
char *name;
unsigned char *data;
unsigned short len;
{
    unsigned char buf[16];
    int i;

    /* 14-byte name */
    for (i = 0; i < 14 && name[i]; i++)
        buf[i] = name[i];
    for (; i < 14; i++)
        buf[i] = '\0';

    /* 2-byte length */
    buf[14] = len & 0xFF;
    buf[15] = (len >> 8) & 0xFF;

    if (fwrite(buf, 1, 16, arfp) != 16)
        error("write error");

    if (len > 0) {
        if (fwrite(data, 1, len, arfp) != len)
            error("write error");
    }
}

/*
 * write end marker (null name entry)
 */
void
write_end(fp)
FILE *fp;
{
    unsigned char buf[16];
    memset(buf, 0, 16);
    if (fwrite(buf, 1, 16, fp) != 16)
        error("write error");
}

/*
 * add object file to archive
 */
void
add_object(arfp, objname)
FILE *arfp;
char *objname;
{
    FILE *fp;
    long size;
    unsigned char *data;
    char arname[15];

    fp = fopen(objname, "rb");
    if (fp == NULL)
        error2("cannot open", objname);

    fseek(fp, 0, SEEK_END);
    size = ftell(fp);
    fseek(fp, 0, SEEK_SET);

    if (size > 65535)
        error2("file too large", objname);

    data = (unsigned char *)malloc(size);
    if (!data)
        error("out of memory");

    if (fread(data, 1, size, fp) != size)
        error2("read error", objname);
    fclose(fp);

    make_arname(arname, objname);
    arname[14] = '\0';

    if (verbose)
        printf("a - %s\n", arname);

    write_entry(arfp, arname, data, (unsigned short)size);
    free(data);
}

/*
 * create archive from list of objects
 */
void
do_create(archive, files, nfiles)
char *archive;
char **files;
int nfiles;
{
    FILE *fp;
    int i;

    if (nfiles == 0)
        error("no files to add");

    fp = fopen(archive, "wb");
    if (fp == NULL)
        error2("cannot create", archive);

    write_header(fp);

    for (i = 0; i < nfiles; i++)
        add_object(fp, files[i]);

    write_end(fp);
    fclose(fp);
}

/*
 * append objects to existing archive
 */
void
do_append(archive, files, nfiles)
char *archive;
char **files;
int nfiles;
{
    FILE *fp;
    int i;
    long size;
    unsigned char buf[16];

    if (nfiles == 0)
        error("no files to add");

    fp = fopen(archive, "r+b");
    if (fp == NULL)
        error2("cannot open", archive);

    /* verify magic */
    if (fread(buf, 1, 2, fp) != 2)
        error2("read error", archive);
    if ((buf[0] | (buf[1] << 8)) != AR_MAGIC)
        error2("not an archive", archive);

    /* find end - look for null entry or EOF */
    fseek(fp, 0, SEEK_END);
    size = ftell(fp);
    fseek(fp, 2, SEEK_SET);

    while (ftell(fp) < size) {
        long pos = ftell(fp);
        unsigned short len;

        if (fread(buf, 1, 16, fp) != 16)
            break;

        if (buf[0] == '\0') {
            /* found end marker - position before it */
            fseek(fp, pos, SEEK_SET);
            break;
        }

        len = buf[14] | (buf[15] << 8);
        fseek(fp, len, SEEK_CUR);
    }

    /* now at position to write new entries */
    for (i = 0; i < nfiles; i++)
        add_object(fp, files[i]);

    write_end(fp);
    fclose(fp);
}

/*
 * list archive contents
 */
void
do_list(archive)
char *archive;
{
    FILE *fp;
    unsigned char buf[16];
    char name[15];
    unsigned short len;
    long total = 0;
    int count = 0;

    fp = fopen(archive, "rb");
    if (fp == NULL)
        error2("cannot open", archive);

    if (fread(buf, 1, 2, fp) != 2)
        error2("read error", archive);
    if ((buf[0] | (buf[1] << 8)) != AR_MAGIC)
        error2("not an archive", archive);

    while (fread(buf, 1, 16, fp) == 16) {
        if (buf[0] == '\0')
            break;

        memcpy(name, buf, 14);
        name[14] = '\0';
        len = buf[14] | (buf[15] << 8);

        if (verbose)
            printf("%6u %s\n", len, name);
        else
            printf("%s\n", name);

        total += len;
        count++;
        fseek(fp, len, SEEK_CUR);
    }

    if (verbose)
        printf("total %ld bytes in %d files\n", total, count);

    fclose(fp);
}

/*
 * extract files from archive
 */
void
do_extract(archive, files, nfiles)
char *archive;
char **files;
int nfiles;
{
    FILE *fp, *outfp;
    unsigned char buf[16];
    unsigned char *data;
    char name[15];
    unsigned short len;
    int extract_all;

    extract_all = (nfiles == 0);

    fp = fopen(archive, "rb");
    if (fp == NULL)
        error2("cannot open", archive);

    if (fread(buf, 1, 2, fp) != 2)
        error2("read error", archive);
    if ((buf[0] | (buf[1] << 8)) != AR_MAGIC)
        error2("not an archive", archive);

    while (fread(buf, 1, 16, fp) == 16) {
        if (buf[0] == '\0')
            break;

        memcpy(name, buf, 14);
        name[14] = '\0';
        len = buf[14] | (buf[15] << 8);

        if (extract_all || name_in_list(name, files, nfiles)) {
            if (verbose)
                printf("x - %s\n", name);

            data = (unsigned char *)malloc(len);
            if (!data)
                error("out of memory");

            if (fread(data, 1, len, fp) != len)
                error("read error");

            outfp = fopen(name, "wb");
            if (outfp == NULL)
                error2("cannot create", name);

            if (fwrite(data, 1, len, outfp) != len)
                error("write error");

            fclose(outfp);
            free(data);
        } else {
            fseek(fp, len, SEEK_CUR);
        }
    }

    fclose(fp);
}

/*
 * replace files in archive (uses temp file)
 */
void
do_replace(archive, files, nfiles)
char *archive;
char **files;
int nfiles;
{
    FILE *fp, *tmpfp;
    int i;
    unsigned char buf[16];
    unsigned char *data;
    char name[15];
    unsigned short len;
    char tmpname[256];
    int replaced[64];  /* track which files were replaced */

    if (nfiles == 0)
        error("no files to replace");
    if (nfiles > 64)
        error("too many files");

    for (i = 0; i < nfiles; i++)
        replaced[i] = 0;

    fp = fopen(archive, "rb");
    if (fp == NULL)
        error2("cannot open", archive);

    if (fread(buf, 1, 2, fp) != 2)
        error2("read error", archive);
    if ((buf[0] | (buf[1] << 8)) != AR_MAGIC)
        error2("not an archive", archive);

    /* create temp file */
    sprintf(tmpname, "%s.tmp", archive);
    tmpfp = fopen(tmpname, "wb");
    if (tmpfp == NULL)
        error2("cannot create temp file", tmpname);

    write_header(tmpfp);

    /* copy archive, replacing matching files */
    while (fread(buf, 1, 16, fp) == 16) {
        if (buf[0] == '\0')
            break;

        memcpy(name, buf, 14);
        name[14] = '\0';
        len = buf[14] | (buf[15] << 8);

        /* check if this file should be replaced */
        for (i = 0; i < nfiles; i++) {
            char arname[15];
            make_arname(arname, files[i]);
            arname[14] = '\0';
            if (strncmp(name, arname, 14) == 0) {
                /* replace with new file */
                if (verbose)
                    printf("r - %s\n", name);
                add_object(tmpfp, files[i]);
                replaced[i] = 1;
                fseek(fp, len, SEEK_CUR);  /* skip old data */
                goto next_entry;
            }
        }

        /* copy existing entry */
        data = (unsigned char *)malloc(len);
        if (!data)
            error("out of memory");

        if (fread(data, 1, len, fp) != len)
            error("read error");

        write_entry(tmpfp, name, data, len);
        free(data);

next_entry:
        ;
    }

    /* add any files that weren't replacements (they're new) */
    for (i = 0; i < nfiles; i++) {
        if (!replaced[i]) {
            if (verbose) {
                char arname[15];
                make_arname(arname, files[i]);
                printf("a - %s\n", arname);
            }
            add_object(tmpfp, files[i]);
        }
    }

    write_end(tmpfp);
    fclose(tmpfp);
    fclose(fp);

    /* rename temp to archive */
    if (unlink(archive) < 0)
        error2("cannot remove", archive);
    if (rename(tmpname, archive) < 0) {
        /* rename failed, try copy */
        error2("cannot rename", tmpname);
    }
}

/*
 * Scan HiTech object file for symbols
 * Returns file size, fills symtab[] and sets nsyms
 */
long
htScanObj(filename)
char *filename;
{
    FILE *fp;
    long size;
#ifdef notdef
	long val;
#endif
    unsigned char *buf;
    long off;
    int reclen, rectype;

    fp = fopen(filename, "rb");
    if (fp == NULL)
        error2("cannot open", filename);

    fseek(fp, 0, SEEK_END);
    size = ftell(fp);
    fseek(fp, 0, SEEK_SET);

    buf = (unsigned char *)malloc(size);
    if (!buf)
        error("out of memory");

    if (fread(buf, 1, size, fp) != size) {
        free(buf);
        error2("read error", filename);
    }
    fclose(fp);

    /* verify HiTech format */
    if (size < 13 || !HT_IS_HITECH(buf)) {
        free(buf);
        error2("not a HiTech object file", filename);
    }

    nsyms = 0;

    /* scan records for SYMBOL record */
    off = 0;
    while (off < size - 3) {
        reclen = buf[off] | (buf[off + 1] << 8);
        rectype = buf[off + 2];
        off += 3;

        if (off + reclen > size)
            break;

        if (rectype == HT_SYMBOL) {
            /* parse symbol record */
            long soff = off;
            long send = off + reclen;

            while (soff < send && nsyms < MAX_SYMS) {
                unsigned short flags;
                int plen, nlen;
                char psect[64], symname[64];

                if (soff + 7 > send) break;

#ifdef notdef
                val = buf[soff] | (buf[soff+1] << 8) |
                      ((unsigned long)buf[soff+2] << 16) |
                      ((unsigned long)buf[soff+3] << 24);
#endif
                flags = buf[soff+4] | (buf[soff+5] << 8);
                soff += 6;

                /* psect name */
                for (plen = 0; plen < 63 && soff + plen < send; plen++) {
                    psect[plen] = buf[soff + plen];
                    if (psect[plen] == '\0') break;
                }
                psect[plen] = '\0';
                soff += plen + 1;

                /* symbol name */
                for (nlen = 0; nlen < 63 && soff + nlen < send; nlen++) {
                    symname[nlen] = buf[soff + nlen];
                    if (symname[nlen] == '\0') break;
                }
                symname[nlen] = '\0';
                soff += nlen + 1;

                /* determine symbol type for library */
                /* Only include: global defined, common, or undefined */
                if ((flags & 0x10) && (flags & 0x0f) == 0) {
                    /* global defined symbol */
                    strcpy(symtab[nsyms].name, symname);
                    symtab[nsyms].flags = HT_SYM_DEF;
                    nsyms++;
                } else if ((flags & 0x0f) == 2) {
                    /* common */
                    strcpy(symtab[nsyms].name, symname);
                    symtab[nsyms].flags = HT_SYM_COMMON;
                    nsyms++;
                } else if ((flags & 0x0f) == 6) {
                    /* undefined/extern */
                    strcpy(symtab[nsyms].name, symname);
                    symtab[nsyms].flags = HT_SYM_UNDEF;
                    nsyms++;
                }
            }
        } else if (rectype == HT_END) {
            break;
        }

        off += reclen;
    }

    free(buf);
    return size;
}

unsigned char iobuf[512];

/*
 * Create HiTech format library
 */
void
ht_create(archive, files, nfiles)
char *archive;
char **files;
int nfiles;
{
    FILE *fp;
    FILE *tmpfp;
    int i, j;
    long *modsizes;
    int *symcounts;
    int *symsizes;
    char **modnames;
    struct htsym **modsyms;
    unsigned char hdr[12];
    long total_symdir;
    int namelen;
    char *basename;

    if (nfiles == 0)
        error("no files to add");

    /* allocate arrays */
    modsizes = (long *)malloc(nfiles * sizeof(long));
    symcounts = (int *)malloc(nfiles * sizeof(int));
    symsizes = (int *)malloc(nfiles * sizeof(int));
    modnames = (char **)malloc(nfiles * sizeof(char *));
    modsyms = (struct htsym **)malloc(nfiles * sizeof(struct htsym *));

    if (!modsizes || !symcounts || !symsizes || !modnames || !modsyms)
        error("out of memory");

    /* scan all object files */
    for (i = 0; i < nfiles; i++) {
        modsizes[i] = htScanObj(files[i]);
        symcounts[i] = nsyms;

        /* calculate symbol size: flag + name + null for each */
        symsizes[i] = 0;
        for (j = 0; j < nsyms; j++) {
            symsizes[i] += 1 + strlen(symtab[j].name) + 1;
        }

        /* save symbols */
        modsyms[i] = (struct htsym *)malloc(nsyms * sizeof(struct htsym));
        if (!modsyms[i])
            error("out of memory");
        memcpy(modsyms[i], symtab, nsyms * sizeof(struct htsym));

        /* extract basename */
        basename = files[i];
        for (j = 0; files[i][j]; j++) {
            if (files[i][j] == '/')
                basename = &files[i][j + 1];
        }
        modnames[i] = (char *)malloc(strlen(basename) + 1);
        strcpy(modnames[i], basename);

        if (verbose)
            printf("a - %s (%d symbols)\n", modnames[i], symcounts[i]);
    }

    /* calculate total symbol directory size */
    total_symdir = 0;
    for (i = 0; i < nfiles; i++) {
        /* 12-byte header + module name + null + symbols */
        total_symdir += 12 + strlen(modnames[i]) + 1 + symsizes[i];
    }

    /* create temp file for module data */
    tmpfp = fopen("wslib.tmp", "wb");
    if (!tmpfp)
        error("cannot create temp file");

    /* write module data to temp file */
    for (i = 0; i < nfiles; i++) {
        FILE *objfp;
        unsigned char *buf;

        objfp = fopen(files[i], "rb");
        if (objfp == NULL)
            error2("cannot open", files[i]);

        buf = (unsigned char *)malloc(modsizes[i]);
        if (!buf)
            error("out of memory");

        if (fread(buf, 1, modsizes[i], objfp) != modsizes[i]) {
            free(buf);
            error2("read error", files[i]);
        }
        fclose(objfp);

        fwrite(buf, 1, modsizes[i], tmpfp);
        free(buf);
    }
    fclose(tmpfp);

    /* create library file */
    fp = fopen(archive, "wb");
    if (!fp)
        error2("cannot create", archive);

    /* write library header */
    hdr[0] = total_symdir & 0xff;
    hdr[1] = (total_symdir >> 8) & 0xff;
    hdr[2] = nfiles & 0xff;
    hdr[3] = (nfiles >> 8) & 0xff;
    fwrite(hdr, 1, 4, fp);

    /* write symbol directory */
    for (i = 0; i < nfiles; i++) {
        /* module header: symsize, symcnt, modsize, unused */
        hdr[0] = symsizes[i] & 0xff;
        hdr[1] = (symsizes[i] >> 8) & 0xff;
        hdr[2] = symcounts[i] & 0xff;
        hdr[3] = (symcounts[i] >> 8) & 0xff;
        hdr[4] = modsizes[i] & 0xff;
        hdr[5] = (modsizes[i] >> 8) & 0xff;
        hdr[6] = (modsizes[i] >> 16) & 0xff;
        hdr[7] = (modsizes[i] >> 24) & 0xff;
        hdr[8] = 0;
        hdr[9] = 0;
        hdr[10] = 0;
        hdr[11] = 0;
        fwrite(hdr, 1, 12, fp);

        /* module name */
        namelen = strlen(modnames[i]) + 1;
        fwrite(modnames[i], 1, namelen, fp);

        /* symbols */
        for (j = 0; j < symcounts[i]; j++) {
            fputc(modsyms[i][j].flags, fp);
            fwrite(modsyms[i][j].name, 1, strlen(modsyms[i][j].name) + 1, fp);
        }
    }

    /* append module data from temp file */
    tmpfp = fopen("wslib.tmp", "rb");
    if (tmpfp) {
        int n;
        while ((n = fread(iobuf, 1, 512, tmpfp)) > 0) {
            fwrite(iobuf, 1, n, fp);
        }
        fclose(tmpfp);
        unlink("wslib.tmp");
    }

    fclose(fp);

    /* cleanup */
    for (i = 0; i < nfiles; i++) {
        free(modsyms[i]);
        free(modnames[i]);
    }
    free(modsizes);
    free(symcounts);
    free(symsizes);
    free(modnames);
    free(modsyms);
}

/*
 * Check if file is HiTech library format
 */
int
is_hitech_lib(buf, size)
unsigned char *buf;
long size;
{
    unsigned short sym_size, num_mods;
    long mod_data_off;

    if (size < 20)
        return 0;

    sym_size = buf[0] | (buf[1] << 8);
    num_mods = buf[2] | (buf[3] << 8);
    mod_data_off = 4 + sym_size;

    /* check sanity and that first module is valid HiTech object */
    if (num_mods > 0 && num_mods < 1000 &&
        mod_data_off > 4 && mod_data_off < size &&
        size >= mod_data_off + 13 &&
        HT_IS_HITECH(buf + mod_data_off)) {
        return 1;
    }
    return 0;
}

char namebuf[256];

/*
 * list HiTech library contents
 */
void
ht_list(archive)
char *archive;
{
    FILE *fp;
    long size;
    unsigned char *buf;
#ifdef notdef
	unsigned short sym_size;
    unsigned short symSize;
#endif
	unsigned short num_mods;
	unsigned short symCnt;
    unsigned long moduleSize;
    long off;
    int i, j, len;
    long total = 0;
    int count = 0;

    fp = fopen(archive, "rb");
    if (fp == NULL)
        error2("cannot open", archive);

    fseek(fp, 0, SEEK_END);
    size = ftell(fp);
    fseek(fp, 0, SEEK_SET);

    buf = (unsigned char *)malloc(size);
    if (!buf)
        error("out of memory");

    if (fread(buf, 1, size, fp) != size)
        error2("read error", archive);
    fclose(fp);

    if (!is_hitech_lib(buf, size)) {
        free(buf);
        error2("not a HiTech library", archive);
    }

#ifdef notdef
    sym_size = buf[0] | (buf[1] << 8);
#endif
    num_mods = buf[2] | (buf[3] << 8);

    off = 4;
    for (i = 0; i < num_mods && off < size; i++) {
        if (off + 12 > size) break;

#ifdef notdef
        symSize = buf[off] | (buf[off+1] << 8);
#endif
        symCnt = buf[off+2] | (buf[off+3] << 8);
        moduleSize = buf[off+4] | (buf[off+5] << 8) |
                     ((unsigned long)buf[off+6] << 16) |
                     ((unsigned long)buf[off+7] << 24);
        off += 12;

        /* read module name */
        for (len = 0; len < 255 && off + len < size; len++) {
            namebuf[len] = buf[off + len];
            if (namebuf[len] == '\0') break;
        }
        namebuf[len] = '\0';
        off += len + 1;

        if (verbose)
            printf("%6lu %s\n", moduleSize, namebuf);
        else
            printf("%s\n", namebuf);

        total += moduleSize;
        count++;

        /* skip symbols */
        for (j = 0; j < symCnt && off < size; j++) {
            off++;  /* skip flags */
            while (off < size && buf[off]) off++;
            off++;  /* skip null */
        }
    }

    if (verbose)
        printf("total %ld bytes in %d modules\n", total, count);

    free(buf);
}

/*
 * extract modules from HiTech library
 */
void
ht_extract(archive, files, nfiles)
char *archive;
char **files;
int nfiles;
{
    FILE *fp, *outfp;
    long size;
    unsigned char *buf;
    unsigned short sym_size, num_mods;
#ifdef notdef
    unsigned short symSize;
#endif
	unsigned short symCnt;
    unsigned long moduleSize;
    long symOff, modDataOff;
    int i, j, len;
    int extract_all;

    extract_all = (nfiles == 0);

    fp = fopen(archive, "rb");
    if (fp == NULL)
        error2("cannot open", archive);

    fseek(fp, 0, SEEK_END);
    size = ftell(fp);
    fseek(fp, 0, SEEK_SET);

    buf = (unsigned char *)malloc(size);
    if (!buf)
        error("out of memory");

    if (fread(buf, 1, size, fp) != size)
        error2("read error", archive);
    fclose(fp);

    if (!is_hitech_lib(buf, size)) {
        free(buf);
        error2("not a HiTech library", archive);
    }

    sym_size = buf[0] | (buf[1] << 8);
    num_mods = buf[2] | (buf[3] << 8);

    modDataOff = 4 + sym_size;
    symOff = 4;

    for (i = 0; i < num_mods && symOff < size; i++) {
        if (symOff + 12 > size) break;

#ifdef notdef
        symSize = buf[symOff] | (buf[symOff+1] << 8);
#endif
        symCnt = buf[symOff+2] | (buf[symOff+3] << 8);
        moduleSize = buf[symOff+4] | (buf[symOff+5] << 8) |
                     ((unsigned long)buf[symOff+6] << 16) |
                     ((unsigned long)buf[symOff+7] << 24);
        symOff += 12;

        /* read module name */
        for (len = 0; len < 255 && symOff + len < size; len++) {
            namebuf[len] = buf[symOff + len];
            if (namebuf[len] == '\0') break;
        }
        namebuf[len] = '\0';
        symOff += len + 1;

        /* skip symbols */
        for (j = 0; j < symCnt && symOff < size; j++) {
            symOff++;
            while (symOff < size && buf[symOff]) symOff++;
            symOff++;
        }

        /* check if we should extract this module */
        if (extract_all || name_in_list(namebuf, files, nfiles)) {
            if (verbose)
                printf("x - %s\n", namebuf);

            if (modDataOff + moduleSize <= size) {
                outfp = fopen(namebuf, "wb");
                if (outfp == NULL)
                    error2("cannot create", namebuf);

                if (fwrite(buf + modDataOff, 1, moduleSize, outfp) != moduleSize)
                    error("write error");

                fclose(outfp);
            }
        }

        modDataOff += moduleSize;
    }

    free(buf);
}

int
main(argc, argv)
int argc;
char **argv;
{
    char *archive;
    char **files;
    int nfiles;
    int mode = 0;  /* c=1, x=2, a=3, r=4, t=5 */
    int create_flag = 0;  /* -c modifier for -r */
    int i;

    progname = argv[0];

    for (i = 1; i < argc; i++) {
        if (argv[i][0] == '-') {
            char *p = &argv[i][1];
            while (*p) {
                switch (*p) {
                case 'c': create_flag = 1; if (!mode) mode = 1; break;
                case 'x': mode = 2; break;
                case 'a': mode = 3; break;
                case 'r': mode = 4; break;
                case 't': mode = 5; break;
                case 'v': verbose = 1; break;
                case 'H': hitech_mode = 1; break;
                default: usage();
                }
                p++;
            }
        } else {
            break;
        }
    }

    if (mode == 0 || i >= argc)
        usage();

    archive = argv[i++];
    files = &argv[i];
    nfiles = argc - i;

    /* detect archive type for read operations */
    if (mode == 2 || mode == 5) {
        FILE *fp;
        unsigned char hdr[20];
        int is_hitech = 0;

        fp = fopen(archive, "rb");
        if (fp != NULL) {
            long size;
            fseek(fp, 0, SEEK_END);
            size = ftell(fp);
            fseek(fp, 0, SEEK_SET);
            if (fread(hdr, 1, 20, fp) == 20) {
                unsigned short sym_size = hdr[0] | (hdr[1] << 8);
                unsigned short num_mods = hdr[2] | (hdr[3] << 8);
                long mod_off = 4 + sym_size;

                /* check for Whitesmith archive first */
                if ((hdr[0] | (hdr[1] << 8)) == AR_MAGIC) {
                    is_hitech = 0;
                }
                /* then check for HiTech library */
                else if (num_mods > 0 && num_mods < 1000 &&
                         mod_off > 4 && mod_off < size) {
                    unsigned char ident[13];
                    fseek(fp, mod_off, SEEK_SET);
                    if (fread(ident, 1, 13, fp) == 13 && HT_IS_HITECH(ident)) {
                        is_hitech = 1;
                    }
                }
            }
            fclose(fp);
        }

        if (is_hitech) {
            switch (mode) {
            case 2: ht_extract(archive, files, nfiles); break;
            case 5: ht_list(archive); break;
            }
            return 0;
        }
    }

    switch (mode) {
    case 1:
        if (hitech_mode)
            ht_create(archive, files, nfiles);
        else
            do_create(archive, files, nfiles);
        break;
    case 2: do_extract(archive, files, nfiles); break;
    case 3: do_append(archive, files, nfiles); break;
    case 4:
        /* -cr: create if not exists, then replace */
        if (create_flag) {
            FILE *fp = fopen(archive, "rb");
            if (fp == NULL) {
                /* archive doesn't exist, create it */
                if (hitech_mode)
                    ht_create(archive, files, nfiles);
                else
                    do_create(archive, files, nfiles);
            } else {
                fclose(fp);
                do_replace(archive, files, nfiles);
            }
        } else {
            do_replace(archive, files, nfiles);
        }
        break;
    case 5: do_list(archive); break;
    }

    return 0;
}

/* vim: set tabstop=4 shiftwidth=4 noexpandtab: */
