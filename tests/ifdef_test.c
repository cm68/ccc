/* Test #ifdef directive */

#define DEFINED_MACRO 1

#ifdef DEFINED_MACRO
int should_appear_1 = 1;
#endif

#ifdef UNDEFINED_MACRO
int should_not_appear = 2;
#endif

int should_appear_2 = 3;
