/* Test sizeof with struct types and variables */

struct point {
    int x;
    int y;
};

struct rect {
    struct point p1;
    struct point p2;
};

/* Test sizeof with struct type */
int size_of_point = sizeof(struct point);
int size_of_rect = sizeof(struct rect);

/* Test sizeof with variables */
int a;
int size_of_a = sizeof(a);

char b;
int size_of_b = sizeof(b);

struct point p;
int size_of_p = sizeof(p);

int final = 99;
