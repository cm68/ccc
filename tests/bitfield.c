/* Test bitfield operations */
struct flags {
    int enable : 1;
    int mode : 3;
    int count : 4;
};

int main() {
    struct flags f;
    f.enable = 1;
    f.mode = 5;
    f.count = 10;
    return f.enable + f.mode + f.count;
}
