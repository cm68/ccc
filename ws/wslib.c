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
 *   wslib -t archive.a                     list contents
 *
 * vim: tabstop=4 shiftwidth=4 noexpandtab:
 */
#ifdef linux
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <fcntl.h>
#else
#include <stdio.h>
#define void int
#endif

#define AR_MAGIC    0xFF75  /* 0177565 octal */

char *progname;
int verbose;

void
usage()
{
    fprintf(stderr, "usage: %s [-v] -c|-x|-a|-r|-t archive [file...]\n", progname);
    fprintf(stderr, "  -c  create archive\n");
    fprintf(stderr, "  -x  extract files (all if none specified)\n");
    fprintf(stderr, "  -a  append files to archive\n");
    fprintf(stderr, "  -r  replace files in archive\n");
    fprintf(stderr, "  -t  list archive contents\n");
    fprintf(stderr, "  -v  verbose\n");
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
write_header(fd)
int fd;
{
    unsigned char buf[2];
    buf[0] = AR_MAGIC & 0xFF;
    buf[1] = (AR_MAGIC >> 8) & 0xFF;
    if (write(fd, buf, 2) != 2)
        error("write error");
}

/*
 * write one archive entry
 */
void
write_entry(arfd, name, data, len)
int arfd;
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

    if (write(arfd, buf, 16) != 16)
        error("write error");

    if (len > 0) {
        if (write(arfd, data, len) != len)
            error("write error");
    }
}

/*
 * write end marker (null name entry)
 */
void
write_end(fd)
int fd;
{
    unsigned char buf[16];
    memset(buf, 0, 16);
    if (write(fd, buf, 16) != 16)
        error("write error");
}

/*
 * add object file to archive
 */
void
add_object(arfd, objname)
int arfd;
char *objname;
{
    int fd;
    long size;
    unsigned char *data;
    char arname[15];

    fd = open(objname, O_RDONLY);
    if (fd < 0)
        error2("cannot open", objname);

    size = lseek(fd, 0, SEEK_END);
    lseek(fd, 0, SEEK_SET);

    if (size > 65535)
        error2("file too large", objname);

    data = (unsigned char *)malloc(size);
    if (!data)
        error("out of memory");

    if (read(fd, data, size) != size)
        error2("read error", objname);
    close(fd);

    make_arname(arname, objname);
    arname[14] = '\0';

    if (verbose)
        printf("a - %s\n", arname);

    write_entry(arfd, arname, data, (unsigned short)size);
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
    int fd, i;

    if (nfiles == 0)
        error("no files to add");

    fd = open(archive, O_WRONLY | O_CREAT | O_TRUNC, 0666);
    if (fd < 0)
        error2("cannot create", archive);

    write_header(fd);

    for (i = 0; i < nfiles; i++)
        add_object(fd, files[i]);

    write_end(fd);
    close(fd);
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
    int fd, i;
    long size;
    unsigned char buf[16];

    if (nfiles == 0)
        error("no files to add");

    fd = open(archive, O_RDWR);
    if (fd < 0)
        error2("cannot open", archive);

    /* verify magic */
    if (read(fd, buf, 2) != 2)
        error2("read error", archive);
    if ((buf[0] | (buf[1] << 8)) != AR_MAGIC)
        error2("not an archive", archive);

    /* find end - look for null entry or EOF */
    size = lseek(fd, 0, SEEK_END);
    lseek(fd, 2, SEEK_SET);

    while (lseek(fd, 0, SEEK_CUR) < size) {
        long pos = lseek(fd, 0, SEEK_CUR);
        unsigned short len;

        if (read(fd, buf, 16) != 16)
            break;

        if (buf[0] == '\0') {
            /* found end marker - position before it */
            lseek(fd, pos, SEEK_SET);
            break;
        }

        len = buf[14] | (buf[15] << 8);
        lseek(fd, len, SEEK_CUR);
    }

    /* now at position to write new entries */
    for (i = 0; i < nfiles; i++)
        add_object(fd, files[i]);

    write_end(fd);
    close(fd);
}

/*
 * list archive contents
 */
void
do_list(archive)
char *archive;
{
    int fd;
    unsigned char buf[16];
    char name[15];
    unsigned short len;
    long total = 0;
    int count = 0;

    fd = open(archive, O_RDONLY);
    if (fd < 0)
        error2("cannot open", archive);

    if (read(fd, buf, 2) != 2)
        error2("read error", archive);
    if ((buf[0] | (buf[1] << 8)) != AR_MAGIC)
        error2("not an archive", archive);

    while (read(fd, buf, 16) == 16) {
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
        lseek(fd, len, SEEK_CUR);
    }

    if (verbose)
        printf("total %ld bytes in %d files\n", total, count);

    close(fd);
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
    int fd, outfd;
    unsigned char buf[16];
    unsigned char *data;
    char name[15];
    unsigned short len;
    int extract_all;

    extract_all = (nfiles == 0);

    fd = open(archive, O_RDONLY);
    if (fd < 0)
        error2("cannot open", archive);

    if (read(fd, buf, 2) != 2)
        error2("read error", archive);
    if ((buf[0] | (buf[1] << 8)) != AR_MAGIC)
        error2("not an archive", archive);

    while (read(fd, buf, 16) == 16) {
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

            if (read(fd, data, len) != len)
                error("read error");

            outfd = open(name, O_WRONLY | O_CREAT | O_TRUNC, 0666);
            if (outfd < 0)
                error2("cannot create", name);

            if (write(outfd, data, len) != len)
                error("write error");

            close(outfd);
            free(data);
        } else {
            lseek(fd, len, SEEK_CUR);
        }
    }

    close(fd);
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
    int fd, tmpfd, i;
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

    fd = open(archive, O_RDONLY);
    if (fd < 0)
        error2("cannot open", archive);

    if (read(fd, buf, 2) != 2)
        error2("read error", archive);
    if ((buf[0] | (buf[1] << 8)) != AR_MAGIC)
        error2("not an archive", archive);

    /* create temp file */
    sprintf(tmpname, "%s.tmp", archive);
    tmpfd = open(tmpname, O_WRONLY | O_CREAT | O_TRUNC, 0666);
    if (tmpfd < 0)
        error2("cannot create temp file", tmpname);

    write_header(tmpfd);

    /* copy archive, replacing matching files */
    while (read(fd, buf, 16) == 16) {
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
                add_object(tmpfd, files[i]);
                replaced[i] = 1;
                lseek(fd, len, SEEK_CUR);  /* skip old data */
                goto next_entry;
            }
        }

        /* copy existing entry */
        data = (unsigned char *)malloc(len);
        if (!data)
            error("out of memory");

        if (read(fd, data, len) != len)
            error("read error");

        write_entry(tmpfd, name, data, len);
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
            add_object(tmpfd, files[i]);
        }
    }

    write_end(tmpfd);
    close(tmpfd);
    close(fd);

    /* rename temp to archive */
    if (unlink(archive) < 0)
        error2("cannot remove", archive);
    if (rename(tmpname, archive) < 0) {
        /* rename failed, try copy */
        error2("cannot rename", tmpname);
    }
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
    int i;

    progname = argv[0];

    for (i = 1; i < argc; i++) {
        if (argv[i][0] == '-') {
            char *p = &argv[i][1];
            while (*p) {
                switch (*p) {
                case 'c': mode = 1; break;
                case 'x': mode = 2; break;
                case 'a': mode = 3; break;
                case 'r': mode = 4; break;
                case 't': mode = 5; break;
                case 'v': verbose = 1; break;
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

    switch (mode) {
    case 1: do_create(archive, files, nfiles); break;
    case 2: do_extract(archive, files, nfiles); break;
    case 3: do_append(archive, files, nfiles); break;
    case 4: do_replace(archive, files, nfiles); break;
    case 5: do_list(archive); break;
    }

    return 0;
}

/*
 * vim: tabstop=4 shiftwidth=4 noexpandtab:
 */
