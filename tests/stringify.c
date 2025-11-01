/*
 * Tests preprocessor stringify operator (#)
 */

#define stringify(a) #a

char *m1 = stringify(this is another);
