/*
 * Test #undef interaction with #ifdef and #if defined()
 */

/* Test 1: #ifdef before and after #undef */
#define TESTMACRO1 100

#ifdef TESTMACRO1
int befUndefIf = 1;
#else
int befUndefIf = 0;
#endif

#undef TESTMACRO1

#ifdef TESTMACRO1
int aftUndefIf = 1;
#else
int aftUndefIf = 0;
#endif

/* Test 2: #ifndef before and after #undef */
#define TESTMACRO2 200

#ifndef TESTMACRO2
int befUndefNif = 1;
#else
int befUndefNif = 0;
#endif

#undef TESTMACRO2

#ifndef TESTMACRO2
int aftUndefNif = 1;
#else
int aftUndefNif = 0;
#endif

/* Test 3: Multiple undef - should be harmless */
#define TESTMACRO3 300
#undef TESTMACRO3
#undef TESTMACRO3

#ifdef TESTMACRO3
int dblUndefTst = 1;
#else
int dblUndefTst = 0;
#endif

/* Test 4: Redefine after undef */
#define TESTMACRO4 400
#undef TESTMACRO4
#define TESTMACRO4 500

#ifdef TESTMACRO4
int redefAftUnd = TESTMACRO4;
#endif
