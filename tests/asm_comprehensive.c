/*
 * Comprehensive test of asm block syntax with labels and jumps
 */

char kwsearch(char *str, char *tbl)
{
    char *s = str;
    char *t = tbl;
    
    asm {
        ld hl,(t)
        ld de,(s)
        jr start
    loop:
        ld a,(hl)
        cp a,0xff
        jr nz,check
        inc hl
        ld a,(de)
        or a,a
        jr z,found
        jr done
    check:
        cp a,(de)
        jr nz,skip
        inc hl
        inc de
        jr loop
    skip:
        inc hl
        jr loop
    found:
        ld a,(hl)
        jr done
    start:
        jr loop
    done:
        ld l,a
        ld h,0
    }
    return 0;
}
