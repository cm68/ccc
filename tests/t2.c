/*
 * Tests preprocessor macros including stringify and token pasting operators
 */

char foo[] = "this is a test string";
char bar[] = "this is a another, uglier\0 one\n";

#define nullmacro

#define identity(x) x

#define assign(x,y)  x = y

#define assignstringify(x,y) x = #y

#define stringify(y) #y

#define glom(x,y) x##y

#define	ab xyzzy

vv = nullmacro yy;

vv = identity(k);

assign(a,b);

assignstringify(c,d);

k = glom(a,b);
k = glom(fu,bar);

#define foo(a,b) a##b
#define bar(c,d) c##d

stringify(foo(b,ar(xy,zzy)))

/* while allowing keyword expansion of any adjacent identifiers
 * #define foo(a,b) a##b
 * #define bar(c,d) c##d
 * foo(b,ar(xy,zzy)) generates xyzzy
 */

