/* Test that sizeof returns a constant expression */

/* Test sizeof in array declarations (requires constant) */
char buffer[sizeof(int)];
char buffer2[sizeof(long)];

struct point {
    int x;
    int y;
};

/* Array sized by struct */
char struct_buffer[sizeof(struct point)];

/* Nested sizeof in constant expressions */
char double_buffer[sizeof(int) * 2];

/* sizeof in initializers */
int size1 = sizeof(char);
int size2 = sizeof(short);
int size3 = sizeof(int);
int size4 = sizeof(long);

/* sizeof with variables should also work in initializers */
int a;
int size_of_a = sizeof(a);

int final = 99;
