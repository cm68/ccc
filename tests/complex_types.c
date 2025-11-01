/*
 * Tests complex type declarations including multi-level pointers, arrays, and combinations
 */

// Test complex type declarations

// Multi-level pointers
int *ip;
int **dip;
char ***tip;
void ****qp;

// Array of pointers
int *ipa[10];
char *cpa[5];
void **vpa[3];

// Multi-dimensional arrays
int arr2d[10][20];
char arr3d[5][10][15];

// Pointer to pointer arrays
int **dparray[8];

// Functions returning pointers (these work)
int *func1(int a);
char *func2(void);
void *func3(int x, char y);

// Mixed pointer and array declarations
int *mixed[5][10];
char **strtable[100];

// Very deep pointer nesting
int *****pentaptr;

// TODO: Function pointers not yet supported by parser
// int (*fp)(int, int);
// char (*cfp)(void);
// void (*vfp)(char *);

// TODO: Pointer to array syntax not yet supported
// int (*pa)[10];

// TODO: Array of function pointers not yet supported
// int (*fpa[5])(int, int);
// void (*vfpa[3])(char);
