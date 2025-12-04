/*
 * wssize - display Whitesmith's object file sizes
 *
 * Output format matches linux 'size' command
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

#include "wsobj.h"

char *progname;

void
usage()
{
    fprintf(stderr, "usage: %s file...\n", progname);
    exit(1);
}

/*
 * read 16-bit little-endian value
 */
unsigned short
read16(buf)
unsigned char *buf;
{
    return buf[0] | (buf[1] << 8);
}

/*
 * process a single object file
 */
int
process_object(buf, size, name)
unsigned char *buf;
long size;
char *name;
{
    unsigned short text_size, data_size, bss_size;
    unsigned long total;

    if (size < 16) {
        fprintf(stderr, "%s: %s: file too small\n", progname, name);
        return 1;
    }

    if (buf[0] != MAGIC) {
        fprintf(stderr, "%s: %s: not an object file\n", progname, name);
        return 1;
    }

    text_size = read16(buf + 4);
    data_size = read16(buf + 6);
    bss_size = read16(buf + 8);
    total = text_size + data_size + bss_size;

    printf("%7u\t%7u\t%7u\t%7lu\t%7lx\t%s\n",
           text_size, data_size, bss_size, total, total, name);

    return 0;
}

/*
 * process an archive file
 */
int
process_archive(buf, size, name)
unsigned char *buf;
long size;
char *name;
{
    long pos = 2;  /* skip magic */
    char membername[15];
    unsigned short memberlen;
    int i, ret = 0;
    char fullname[256];

    while (pos < size) {
        /* read 14-byte name */
        if (pos + 16 > size)
            break;

        for (i = 0; i < 14 && buf[pos + i]; i++)
            membername[i] = buf[pos + i];
        membername[i] = '\0';

        /* end marker is null name */
        if (membername[0] == '\0')
            break;

        memberlen = read16(buf + pos + 14);
        pos += 16;

        if (pos + memberlen > size) {
            fprintf(stderr, "%s: %s: truncated archive\n", progname, name);
            return 1;
        }

        sprintf(fullname, "%s(%s)", name, membername);
        ret |= process_object(buf + pos, memberlen, fullname);

        pos += memberlen;
    }

    return ret;
}

int
main(argc, argv)
int argc;
char **argv;
{
    int fd, ret = 0, header_printed = 0;
    unsigned char *buf;
    long size;
    unsigned short magic;

    progname = *argv++;
    argc--;

    if (argc == 0)
        usage();

    while (argc--) {
        char *name = *argv++;

        fd = open(name, 0);
        if (fd < 0) {
            perror(name);
            ret = 1;
            continue;
        }

        size = lseek(fd, 0, 2);
        lseek(fd, 0, 0);

        buf = (unsigned char *)malloc(size);
        if (!buf) {
            fprintf(stderr, "%s: out of memory\n", progname);
            close(fd);
            ret = 1;
            continue;
        }

        if (read(fd, buf, size) != size) {
            perror(name);
            free(buf);
            close(fd);
            ret = 1;
            continue;
        }
        close(fd);

        if (!header_printed) {
            printf("   text\t   data\t    bss\t    dec\t    hex\tfilename\n");
            header_printed = 1;
        }

        if (size >= 2) {
            magic = read16(buf);
            if (magic == AR_MAGIC) {
                ret |= process_archive(buf, size, name);
                free(buf);
                continue;
            }
        }

        ret |= process_object(buf, size, name);
        free(buf);
    }

    return ret;
}
