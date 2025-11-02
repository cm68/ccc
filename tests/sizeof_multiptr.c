/* Test sizeof with multiple levels of pointers */

int test_ptr = sizeof(int *);
int test_ptr2 = sizeof(int **);
int test_ptr3 = sizeof(int ***);

char test_charptr = sizeof(char *);
char test_charptr2 = sizeof(char **);
