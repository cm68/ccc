/* Test sizeof with bitfield structs */

/* Simple bitfield struct */
struct flags {
    int a : 1;
    int b : 1;
    int c : 1;
    int d : 1;
};

/* Bitfield struct with various sizes */
struct mixed {
    int small : 4;
    int medium : 8;
    int large : 16;
};

/* Bitfield with regular field */
struct combo {
    int flag : 1;
    int padding : 7;
    char byte;
    int value : 16;
};

/* Test sizeof with bitfield struct types */
int size_of_flags = sizeof(struct flags);
int size_of_mixed = sizeof(struct mixed);
int size_of_combo = sizeof(struct combo);

/* Test sizeof with bitfield struct variables */
struct flags f;
int size_of_f = sizeof(f);

struct mixed m;
int size_of_m = sizeof(m);

struct combo c;
int size_of_c = sizeof(c);

int final = 99;
