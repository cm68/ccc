/*
 * Test #undef interaction with #ifdef and #if defined()
 */

/* Test 1: #ifdef before and after #undef */
#define TESTMACRO1 100

#ifdef TESTMACRO1
int before_undef_ifdef = 1;
#else
int before_undef_ifdef = 0;
#endif

#undef TESTMACRO1

#ifdef TESTMACRO1
int after_undef_ifdef = 1;
#else
int after_undef_ifdef = 0;
#endif

/* Test 2: #ifndef before and after #undef */
#define TESTMACRO2 200

#ifndef TESTMACRO2
int before_undef_ifndef = 1;
#else
int before_undef_ifndef = 0;
#endif

#undef TESTMACRO2

#ifndef TESTMACRO2
int after_undef_ifndef = 1;
#else
int after_undef_ifndef = 0;
#endif

/* Test 3: Multiple undef - should be harmless */
#define TESTMACRO3 300
#undef TESTMACRO3
#undef TESTMACRO3

#ifdef TESTMACRO3
int double_undef_test = 1;
#else
int double_undef_test = 0;
#endif

/* Test 4: Redefine after undef */
#define TESTMACRO4 400
#undef TESTMACRO4
#define TESTMACRO4 500

#ifdef TESTMACRO4
int redefine_after_undef = TESTMACRO4;
#endif
