/* Test #ifndef directive */

#define DEFINED_MACRO 1

#ifndef UNDEF_MACRO
int shldAppear1 = 1;
#endif

#ifndef DEFINED_MACRO
int shldNotAppr = 2;
#endif

int shldAppear2 = 3;
