struct foo {
    int a;
    char b;
    int c;
};

int off_b = (int)&((struct foo *)0)->b;
int off_c = (int)&((struct foo *)0)->c;

int main() {
    return off_b + off_c;
}
