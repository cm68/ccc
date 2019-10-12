char foo[] = "this is a test string";

#define nullmacro

#define identity(x) x

#define assign(x,y)  x = y

#define assignstringify(x,y) x = #y

#define glom(x,y) x##y

#define	ab xyzzy

vv = nullmacro yy;

vv = identity(k);

assign(a,b);

assignstringify(c,d);

k = glom(a,b);

k = glom(fu,bar);

