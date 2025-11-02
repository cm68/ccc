/* Test sizeof with variables */

int a;
int test1 = sizeof a;        /* without parens */
int test2 = sizeof(a);        /* with parens */

char b;
int test3 = sizeof b;
int test4 = sizeof(b);
