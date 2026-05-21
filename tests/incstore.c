char lexlevel;

void foo() {
    register char *p;
    p[21] = lexlevel + 1;
}
