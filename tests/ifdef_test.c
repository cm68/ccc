/* Test #ifdef directive */

#define DEFINED_MACRO 1

#ifdef DEFINED_MACRO
int shldAppear1 = 1;
#endif

#ifdef UNDEF_MACRO
int shldNotAppr = 2;
#endif

int shldAppear2 = 3;
