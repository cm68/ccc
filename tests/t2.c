char foo[] = "this is a test string";

#define nullmacro

#define macro1(x) x

#define macro2(x,y)  x = y

#define macros2(x,y) x = #y

#define macroc2(x,y) x##y

vv = nullmacro yy;

vv = macro1(k);

macro2(a,b);

macros2(c,d);

k = macroc2(a,b);

xMy

;
