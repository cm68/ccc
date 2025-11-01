/*
 * Tests typedef declarations for various types
 */

typedef int myint;
typedef unsigned char byte;
typedef int *intptr;
typedef int intarray[10];

myint x;
byte b;
intptr p;
intarray arr;

typedef struct {
    int x;
    int y;
} point;

point pt;

typedef enum {
    RED,
    GREEN,
    BLUE
} color;

color c;
