/* Test defined() pseudofunction in #if expressions */

#define FOO 1
#define BAR 2
/* BAZ is not defined */

/* Test: defined(FOO) || defined(BAR) && !defined(BAZ) */
/* This should be: 1 || 1 && !0 = 1 || 1 = 1 (true) */
#if defined(FOO) || defined(BAR) && !defined(BAZ)
int test1 = 1;  /* Should appear */
#endif

/* Test: defined(BAZ) */
/* This should be: 0 (false) */
#if defined(BAZ)
int test2 = 2;  /* Should NOT appear */
#endif

/* Test: !defined(BAZ) */
/* This should be: !0 = 1 (true) */
#if !defined(BAZ)
int test3 = 3;  /* Should appear */
#endif

/* Test: defined(FOO) && defined(BAR) */
/* This should be: 1 && 1 = 1 (true) */
#if defined(FOO) && defined(BAR)
int test4 = 4;  /* Should appear */
#endif

/* Test: defined(FOO) && defined(BAZ) */
/* This should be: 1 && 0 = 0 (false) */
#if defined(FOO) && defined(BAZ)
int test5 = 5;  /* Should NOT appear */
#endif

int final = 99;
