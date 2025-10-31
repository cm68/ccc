/* Test #ifndef directive */

#define DEFINED_MACRO 1

#ifndef UNDEFINED_MACRO
int should_appear_1 = 1;
#endif

#ifndef DEFINED_MACRO
int should_not_appear = 2;
#endif

int should_appear_2 = 3;
