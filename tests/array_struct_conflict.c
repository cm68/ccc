#define MAXPARMS 32

struct macro {
    char pad;
    char parmcount;
    char **parms;
};

void macdefine() {
    unsigned char i;
    struct macro *m;
    char *parms[MAXPARMS];
    
    for (i = 0; i < m->parmcount; i++) {
        m->parms[i] = parms[i];
    }
}
