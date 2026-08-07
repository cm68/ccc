/*
 * test_c.c - Comprehensive C language test file
 * Exercises C language features without using the C preprocessor
 * Written in mixed K&R and ANSI C89 style
 */

/* Basic data types */
char gc;
signed char gsc;
unsigned char guc;
short gs;
short int gsi;
signed short gssi;
unsigned short gus;
int gi;
signed int gsint;
unsigned int gui;
long gl;
long int gli;
signed long gsl;
unsigned long gul;
float gf;
double gd;

/* Storage classes */
static int static_var;
extern int extern_var;

/* Type qualifiers (C89) */
int const_var = 100;
int vol_var;
int cv_var = 50;

/* Pointers */
int *gip;
char *gcp;
int **gipp;
char *gccp;
char *gcpc = 0;

/* Arrays */
int garr[10];
char gstr[20];
int garr2d[3][4];
int garr3d[2][3][4];

/* Initialized arrays */
int ginit[] = { 1, 2, 3, 4, 5 };
char gsinit[] = "hello";
int ginit2d[2][3] = { {1, 2, 3}, {4, 5, 6} };

/* Enumerations */
enum color { RED, GREEN, BLUE };
enum numbered { FIRST = 1, SECOND, THIRD = 10, FOURTH };
enum color genum;

/* Simple structures */
struct point {
    int x;
    int y;
};

struct size {
    int width;
    int height;
};

/* Nested structures - multiple levels */
struct rect {
    struct point origin;
    struct size dim;
};

struct window {
    struct rect frame;
    struct rect content;
    int visible;
};

struct screen {
    struct window main_win;
    struct window popup;
    struct point cursor;
    int active;
};

/* Deeply nested structure */
struct level1 {
    int a;
    struct {
        int b;
        struct {
            int c;
            struct {
                int d;
            } l4;
        } l3;
    } l2;
};

/* Self-referential struct for linked list */
struct list {
    int data;
    struct list *next;
    struct list *prev;
};

/* Tree node with multiple children pointers */
struct tnode {
    int value;
    struct tnode *left;
    struct tnode *right;
    struct tnode *parent;
};

/* Structure containing pointer to another struct */
struct employee {
    char name[20];
    int id;
    struct employee *mgr;
    struct {
        int street_num;
        char street[30];
        char city[20];
    } address;
};

/* Array of structures */
struct item {
    int id;
    int count;
    struct item *related;
};

/* Structure with bitfields */
struct bitfield {
    unsigned int a : 1;
    unsigned int b : 3;
    unsigned int c : 4;
    int d : 8;
    unsigned int : 0;
    unsigned int e : 5;
};

/* Unions */
union variant {
    char c;
    int i;
    long l;
    float f;
};

/* Mixed struct/union */
struct tagged {
    int tag;
    union {
        int ival;
        float fval;
        char sval[8];
    } u;
};

/* Typedef */
typedef int INT;
typedef int *INTP;
typedef struct point POINT;
typedef struct point *POINTP;
typedef struct window WINDOW;
typedef struct window *WINDOWP;
typedef int ARR10[10];
typedef int (*FUNCP)(int, int);

/* Global struct/union instances */
struct point gpoint = { 10, 20 };
struct rect grect = { {0, 0}, {100, 100} };
struct window gwindow;
struct screen gscreen;
union variant gvar;

/* Pointers to structures */
struct point *gpointp;
struct rect *grectp;
struct window *gwinp;
struct screen *gscreenp;
struct list *glistp;
struct tnode *gtreep;

/* Function pointer */
int (*gfuncp)(int);

/* K&R style function definition */
int kr_add(a, b)
int a;
int b;
{
    return a + b;
}

/* K&R with various types */
long kr_mixed(c, i, l)
char c;
int i;
long l;
{
    return c + i + l;
}

/* K&R with pointers */
int kr_strlen(s)
char *s;
{
    int len;
    len = 0;
    while (*s++) {
        len++;
    }
    return len;
}

/* ANSI C89 style function */
int ansi_add(int a, int b)
{
    return a + b;
}

/* ANSI with various types */
long ansi_mixed(char c, int i, long l)
{
    return c + i + l;
}

/* ANSI with pointers */
int ansi_strlen(char *s)
{
    int len = 0;
    while (*s++) {
        len++;
    }
    return len;
}

/* Function returning pointer */
int *retptr(int *p)
{
    return p;
}

/* Function returning struct */
struct point retstrct(int x, int y)
{
    struct point p;
    p.x = x;
    p.y = y;
    return p;
}

/* Function returning pointer to struct */
struct point *retstrp(struct point *p)
{
    return p;
}

/* Static function */
static int static_fn(int x)
{
    return x * 2;
}

/* Recursive function */
int factorial(int n)
{
    if (n <= 1)
        return 1;
    return n * factorial(n - 1);
}

/* Mutual recursion forward declaration */
int is_even(int n);
int is_odd(int n);

int is_even(int n)
{
    if (n == 0)
        return 1;
    return is_odd(n - 1);
}

int is_odd(int n)
{
    if (n == 0)
        return 0;
    return is_even(n - 1);
}

/* Function with array parameter */
int sum_arr(int arr[], int n)
{
    int i, sum;
    sum = 0;
    for (i = 0; i < n; i++) {
        sum = sum + arr[i];
    }
    return sum;
}

/* Function with 2D array parameter */
int sum_2d(int arr[][4], int rows)
{
    int i, j, sum;
    sum = 0;
    for (i = 0; i < rows; i++) {
        for (j = 0; j < 4; j++) {
            sum = sum + arr[i][j];
        }
    }
    return sum;
}

/* Function with struct parameter */
int point_sum(struct point p)
{
    return p.x + p.y;
}

/* Function with struct pointer */
void point_set(struct point *p, int x, int y)
{
    p->x = x;
    p->y = y;
}

/* Function with nested struct pointer */
void rect_set(struct rect *r, int x, int y, int w, int h)
{
    r->origin.x = x;
    r->origin.y = y;
    r->dim.width = w;
    r->dim.height = h;
}

/* Function with deeply nested struct pointer */
void window_set(struct window *w, int x, int y, int fw, int fh)
{
    w->frame.origin.x = x;
    w->frame.origin.y = y;
    w->frame.dim.width = fw;
    w->frame.dim.height = fh;
    w->content.origin.x = x + 1;
    w->content.origin.y = y + 1;
    w->content.dim.width = fw - 2;
    w->content.dim.height = fh - 2;
    w->visible = 1;
}

/* Function operating on screen via pointer */
void screen_init(struct screen *s)
{
    s->main_win.frame.origin.x = 0;
    s->main_win.frame.origin.y = 0;
    s->main_win.frame.dim.width = 80;
    s->main_win.frame.dim.height = 24;
    s->main_win.content.origin.x = 1;
    s->main_win.content.origin.y = 1;
    s->main_win.content.dim.width = 78;
    s->main_win.content.dim.height = 22;
    s->main_win.visible = 1;

    s->popup.frame.origin.x = 20;
    s->popup.frame.origin.y = 5;
    s->popup.frame.dim.width = 40;
    s->popup.frame.dim.height = 10;
    s->popup.visible = 0;

    s->cursor.x = 0;
    s->cursor.y = 0;
    s->active = 1;
}

/* Function with function pointer parameter */
int apply(int (*fn)(int), int x)
{
    return fn(x);
}

/* Function returning function pointer */
int (*getfn(int sel))(int)
{
    if (sel)
        return static_fn;
    return 0;
}

/* Test all operators */
int test_ops(void)
{
    int a, b, c;
    unsigned int ua, ub;
    int *p;
    int arr[5];

    /* Arithmetic operators */
    a = 10;
    b = 3;
    c = a + b;
    c = a - b;
    c = a * b;
    c = a / b;
    c = a % b;
    c = -a;
    c = +a;

    /* Increment/decrement */
    c = a++;
    c = ++a;
    c = a--;
    c = --a;

    /* Relational operators */
    c = a < b;
    c = a > b;
    c = a <= b;
    c = a >= b;
    c = a == b;
    c = a != b;

    /* Logical operators */
    c = a && b;
    c = a || b;
    c = !a;

    /* Bitwise operators */
    ua = 0x55;
    ub = 0xAA;
    c = ua & ub;
    c = ua | ub;
    c = ua ^ ub;
    c = ~ua;
    c = ua << 2;
    c = ua >> 2;

    /* Assignment operators */
    a = 10;
    a += 5;
    a -= 3;
    a *= 2;
    a /= 4;
    a %= 3;
    ua = 0xFF;
    ua &= 0x0F;
    ua |= 0x80;
    ua ^= 0x55;
    ua <<= 1;
    ua >>= 1;

    /* Conditional operator */
    c = a > b ? a : b;

    /* Comma operator */
    c = (a = 1, b = 2, a + b);

    /* sizeof operator */
    c = sizeof(int);
    c = sizeof a;
    c = sizeof(struct point);
    c = sizeof(struct screen);
    c = sizeof arr;

    /* Pointer operators */
    p = &a;
    c = *p;

    /* Array subscript */
    arr[0] = 1;
    c = arr[0];
    c = 0[arr];

    /* Cast operators */
    a = (int)3.14;
    ua = (unsigned int)a;
    p = (int *)0;

    return c;
}

/* Test control flow */
int test_ctrl(int x)
{
    int i, sum;

    /* if-else */
    if (x > 0) {
        sum = 1;
    } else if (x < 0) {
        sum = -1;
    } else {
        sum = 0;
    }

    /* Nested if */
    if (x > 0)
        if (x > 10)
            sum = 2;
        else
            sum = 1;

    /* switch */
    switch (x) {
    case 0:
        sum = 0;
        break;
    case 1:
    case 2:
        sum = 1;
        break;
    case 3:
        sum = 3;
        /* fall through */
    case 4:
        sum = sum + 4;
        break;
    default:
        sum = -1;
        break;
    }

    /* while */
    i = 0;
    sum = 0;
    while (i < x) {
        sum = sum + i;
        i++;
    }

    /* do-while */
    i = 0;
    do {
        sum = sum + 1;
        i++;
    } while (i < x);

    /* for */
    sum = 0;
    for (i = 0; i < x; i++) {
        sum = sum + i;
    }

    /* for with empty parts */
    i = 0;
    for (;;) {
        if (i >= x)
            break;
        i++;
    }

    /* break and continue */
    for (i = 0; i < 100; i++) {
        if (i == 5)
            continue;
        if (i == 10)
            break;
        sum = sum + i;
    }

    /* goto */
    i = 0;
loop:
    if (i < x) {
        sum = sum + i;
        i++;
        goto loop;
    }

    return sum;
}

/* Test local variables and scopes */
int test_scope(int x)
{
    int a;
    static int count = 0;
    register int r;

    a = 1;
    r = x;
    count++;

    {
        int a;
        a = 2;
        {
            int a;
            a = 3;
            r = r + a;
        }
        r = r + a;
    }

    r = r + a;
    return r;
}

/* Test struct operations with heavy focus on nested structs */
int test_struct(void)
{
    struct point p1, p2;
    struct point *pp;
    struct rect r;
    struct rect *rp;
    struct window w;
    struct window *wp;
    struct screen s;
    struct screen *sp;
    struct list node, node2;
    struct list *lp;
    struct tnode t1, t2, t3;
    struct tnode *tp;
    struct level1 lv;
    struct level1 *lvp;
    struct bitfield bf;
    int val;

    /* Direct member access */
    p1.x = 10;
    p1.y = 20;

    /* Struct assignment */
    p2 = p1;

    /* Pointer member access - simple struct */
    pp = &p1;
    pp->x = 30;
    (*pp).y = 40;
    val = pp->x + pp->y;

    /* Nested struct - direct access */
    r.origin.x = 0;
    r.origin.y = 0;
    r.dim.width = 100;
    r.dim.height = 50;

    /* Nested struct - via pointer */
    rp = &r;
    rp->origin.x = 5;
    rp->origin.y = 10;
    rp->dim.width = 200;
    rp->dim.height = 100;
    val = rp->origin.x + rp->origin.y;
    val = rp->dim.width * rp->dim.height;

    /* Deeply nested struct - direct access */
    w.frame.origin.x = 0;
    w.frame.origin.y = 0;
    w.frame.dim.width = 80;
    w.frame.dim.height = 24;
    w.content.origin.x = 1;
    w.content.origin.y = 1;
    w.content.dim.width = 78;
    w.content.dim.height = 22;
    w.visible = 1;

    /* Deeply nested struct - via pointer */
    wp = &w;
    wp->frame.origin.x = 10;
    wp->frame.origin.y = 5;
    wp->frame.dim.width = 60;
    wp->frame.dim.height = 20;
    wp->content.origin.x = wp->frame.origin.x + 1;
    wp->content.origin.y = wp->frame.origin.y + 1;
    wp->content.dim.width = wp->frame.dim.width - 2;
    wp->content.dim.height = wp->frame.dim.height - 2;
    wp->visible = 1;
    val = wp->frame.origin.x + wp->content.dim.width;

    /* Very deeply nested - screen contains windows */
    s.main_win.frame.origin.x = 0;
    s.main_win.frame.origin.y = 0;
    s.main_win.frame.dim.width = 80;
    s.main_win.frame.dim.height = 24;
    s.main_win.visible = 1;
    s.popup.frame.origin.x = 20;
    s.popup.frame.origin.y = 5;
    s.popup.frame.dim.width = 40;
    s.popup.frame.dim.height = 10;
    s.popup.visible = 0;
    s.cursor.x = 0;
    s.cursor.y = 0;
    s.active = 1;

    /* Very deeply nested - via pointer */
    sp = &s;
    sp->main_win.frame.origin.x = 0;
    sp->main_win.frame.origin.y = 0;
    sp->main_win.frame.dim.width = 80;
    sp->main_win.frame.dim.height = 24;
    sp->main_win.content.origin.x = 1;
    sp->main_win.content.origin.y = 1;
    sp->main_win.content.dim.width = 78;
    sp->main_win.content.dim.height = 22;
    sp->popup.frame.origin.x = sp->main_win.frame.origin.x + 10;
    sp->popup.frame.origin.y = sp->main_win.frame.origin.y + 5;
    sp->popup.frame.dim.width = sp->main_win.frame.dim.width / 2;
    sp->popup.frame.dim.height = sp->main_win.frame.dim.height / 2;
    sp->popup.content.origin.x = sp->popup.frame.origin.x + 1;
    sp->popup.content.origin.y = sp->popup.frame.origin.y + 1;
    sp->popup.content.dim.width = sp->popup.frame.dim.width - 2;
    sp->popup.content.dim.height = sp->popup.frame.dim.height - 2;
    sp->cursor.x = sp->main_win.content.origin.x;
    sp->cursor.y = sp->main_win.content.origin.y;
    sp->active = 1;

    val = sp->main_win.frame.origin.x + sp->main_win.frame.dim.width;
    val = sp->popup.content.dim.width * sp->popup.content.dim.height;

    /* Anonymous nested struct - 4 levels deep */
    lv.a = 1;
    lv.l2.b = 2;
    lv.l2.l3.c = 3;
    lv.l2.l3.l4.d = 4;

    /* Anonymous nested struct - via pointer */
    lvp = &lv;
    lvp->a = 10;
    lvp->l2.b = 20;
    lvp->l2.l3.c = 30;
    lvp->l2.l3.l4.d = 40;
    val = lvp->a + lvp->l2.b + lvp->l2.l3.c + lvp->l2.l3.l4.d;

    /* Self-referential struct - linked list */
    node.data = 1;
    node.next = &node2;
    node.prev = 0;
    node2.data = 2;
    node2.next = 0;
    node2.prev = &node;

    /* Via pointer - chasing links */
    lp = &node;
    val = lp->data;
    lp = lp->next;
    val = lp->data;
    lp = lp->prev;
    val = lp->data;

    /* Multiple pointer chasing */
    lp = &node;
    val = lp->next->data;
    val = lp->next->prev->data;
    lp->next->data = 100;
    lp->next->prev->data = 200;

    /* Tree structure */
    t1.value = 10;
    t1.parent = 0;
    t1.left = &t2;
    t1.right = &t3;

    t2.value = 5;
    t2.parent = &t1;
    t2.left = 0;
    t2.right = 0;

    t3.value = 15;
    t3.parent = &t1;
    t3.left = 0;
    t3.right = 0;

    /* Tree navigation via pointers */
    tp = &t1;
    val = tp->value;
    val = tp->left->value;
    val = tp->right->value;
    val = tp->left->parent->value;
    val = tp->right->parent->value;
    val = tp->left->parent->right->value;
    tp->left->value = 50;
    tp->right->parent->value = 100;

    /* Chained access patterns */
    tp = &t1;
    if (tp->left)
        val = tp->left->value;
    if (tp->right)
        if (tp->right->parent)
            val = tp->right->parent->value;

    /* Pointer to nested struct member */
    pp = &(r.origin);
    pp->x = 999;
    pp->y = 888;

    pp = &(w.frame.origin);
    pp->x = 111;
    pp->y = 222;

    pp = &(s.main_win.frame.origin);
    pp->x = 333;
    pp->y = 444;

    pp = &(sp->popup.content.origin);
    pp->x = 555;
    pp->y = 666;

    /* Assign nested struct through pointer */
    rp = &r;
    rp->origin = p1;

    wp = &w;
    wp->frame.origin = p1;
    wp->content.origin = p2;

    /* Copy entire nested structs */
    w.frame = r;
    w.content = r;

    sp->popup.frame = sp->main_win.frame;
    sp->popup.content = sp->main_win.content;

    /* Bitfields */
    bf.a = 1;
    bf.b = 7;
    bf.c = 15;
    bf.d = -1;
    bf.e = 31;

    return val;
}

/* Test pointers to structs in arrays */
int test_strarrp(void)
{
    struct point pts[5];
    struct point *pp;
    struct rect rects[3];
    struct rect *rp;
    struct window wins[2];
    struct window *wp;
    int i, val;

    /* Initialize array of points via indexing */
    for (i = 0; i < 5; i++) {
        pts[i].x = i * 10;
        pts[i].y = i * 20;
    }

    /* Access via pointer arithmetic */
    pp = pts;
    val = pp->x;
    pp++;
    val = pp->y;
    pp = pp + 2;
    val = pp->x + pp->y;
    val = (pp - 1)->x;
    val = (pp + 1)->y;

    /* Array of nested structs */
    for (i = 0; i < 3; i++) {
        rects[i].origin.x = i;
        rects[i].origin.y = i * 2;
        rects[i].dim.width = 100 + i * 10;
        rects[i].dim.height = 50 + i * 5;
    }

    /* Via pointer */
    rp = rects;
    val = rp->origin.x;
    rp++;
    val = rp->dim.width;
    val = (rp + 1)->origin.y;
    val = rects[2].dim.height;

    /* Array of deeply nested structs */
    for (i = 0; i < 2; i++) {
        wins[i].frame.origin.x = i * 40;
        wins[i].frame.origin.y = i * 12;
        wins[i].frame.dim.width = 40;
        wins[i].frame.dim.height = 12;
        wins[i].content.origin.x = wins[i].frame.origin.x + 1;
        wins[i].content.origin.y = wins[i].frame.origin.y + 1;
        wins[i].content.dim.width = wins[i].frame.dim.width - 2;
        wins[i].content.dim.height = wins[i].frame.dim.height - 2;
        wins[i].visible = 1;
    }

    /* Via pointer */
    wp = wins;
    val = wp->frame.origin.x;
    val = wp->content.dim.width;
    wp++;
    val = wp->frame.origin.x;
    val = wp->content.dim.height;

    /* Complex expressions */
    wp = wins;
    val = wp[0].frame.origin.x + wp[1].frame.origin.x;
    val = (wp + 0)->content.dim.width + (wp + 1)->content.dim.height;

    return val;
}

/* Test passing pointers to nested structs */
int rect_area(struct rect *rp)
{
    return rp->dim.width * rp->dim.height;
}

int window_area(struct window *wp)
{
    return wp->content.dim.width * wp->content.dim.height;
}

int screen_area(struct screen *sp)
{
    int a1, a2;
    a1 = sp->main_win.content.dim.width * sp->main_win.content.dim.height;
    a2 = sp->popup.content.dim.width * sp->popup.content.dim.height;
    return a1 + a2;
}

void move_window(struct window *wp, int dx, int dy)
{
    wp->frame.origin.x += dx;
    wp->frame.origin.y += dy;
    wp->content.origin.x += dx;
    wp->content.origin.y += dy;
}

void copy_rect(struct rect *dst, struct rect *src)
{
    dst->origin.x = src->origin.x;
    dst->origin.y = src->origin.y;
    dst->dim.width = src->dim.width;
    dst->dim.height = src->dim.height;
}

/* Return pointer to nested struct member */
struct point *get_origin(struct rect *rp)
{
    return &rp->origin;
}

struct rect *get_frame(struct window *wp)
{
    return &wp->frame;
}

struct window *get_main(struct screen *sp)
{
    return &sp->main_win;
}

/* Test union operations */
int test_union(void)
{
    union variant v;
    struct tagged t;

    v.i = 0x12345678;
    gc = v.c;

    v.f = 3.14;
    gl = v.l;

    t.tag = 1;
    t.u.ival = 100;

    t.tag = 2;
    t.u.fval = 2.71;

    return v.i;
}

/* Test array operations */
int test_array(void)
{
    int arr[10];
    int arr2d[3][4];
    char str[20];
    int *p;
    int i, j;

    /* Array initialization in code */
    for (i = 0; i < 10; i++) {
        arr[i] = i * i;
    }

    /* 2D array */
    for (i = 0; i < 3; i++) {
        for (j = 0; j < 4; j++) {
            arr2d[i][j] = i * 4 + j;
        }
    }

    /* String operations */
    str[0] = 'H';
    str[1] = 'e';
    str[2] = 'l';
    str[3] = 'l';
    str[4] = 'o';
    str[5] = '\0';

    /* Pointer arithmetic */
    p = arr;
    i = *p;
    p++;
    i = *p;
    p = p + 3;
    i = *p;
    p = p - 2;
    i = p[1];
    i = *(p + 2);

    /* Pointer difference */
    j = &arr[5] - &arr[0];

    /* Pointer comparison */
    if (p < &arr[5]) {
        i = 1;
    }

    return arr[5];
}

/* Test pointer operations */
int test_ptr(void)
{
    int a;
    int *p;
    int **pp;
    int arr[5];
    int (*arrp)[5];
    void *vp;
    struct point pt;
    struct point *ptp;
    struct rect r;
    struct rect *rp;

    a = 42;
    p = &a;
    pp = &p;

    /* Dereference */
    a = *p;
    a = **pp;

    /* Array pointer */
    arrp = &arr;
    (*arrp)[0] = 1;

    /* Void pointer */
    vp = &a;
    p = (int *)vp;
    a = *(int *)vp;

    /* Void pointer to struct */
    vp = &pt;
    ptp = (struct point *)vp;
    ptp->x = 10;
    ptp->y = 20;

    vp = &r;
    rp = (struct rect *)vp;
    rp->origin.x = 1;
    rp->origin.y = 2;
    rp->dim.width = 100;
    rp->dim.height = 50;

    /* Null pointer */
    p = 0;
    p = (int *)0;
    ptp = 0;
    rp = (struct rect *)0;

    /* Pointer to function */
    gfuncp = static_fn;
    a = (*gfuncp)(5);
    a = gfuncp(5);

    return a;
}

/* Test type conversions */
int test_conv(void)
{
    char c;
    short s;
    int i;
    long l;
    unsigned char uc;
    unsigned int ui;
    unsigned long ul;
    float f;
    double d;

    /* Integer promotions */
    c = 'A';
    i = c;
    s = 1000;
    i = s;

    /* Signed/unsigned conversions */
    i = -1;
    ui = i;
    ul = i;

    uc = 255;
    i = uc;
    ui = uc;

    /* Integer/float conversions */
    i = 42;
    f = i;
    d = i;

    f = 3.14;
    i = f;

    d = 2.71828;
    i = d;
    f = d;

    /* Long conversions */
    l = 100000;
    i = l;
    f = l;

    /* Explicit casts */
    i = (int)f;
    l = (long)d;
    c = (char)i;
    ui = (unsigned int)i;
    f = (float)i;
    d = (double)f;

    /* Pointer casts */
    ul = (unsigned long)&i;
    i = *(int *)(unsigned long)&i;

    return i;
}

/* Test string and character literals */
int test_str(void)
{
    char *s;
    char arr[50];
    char c;
    int i;

    /* Character literals */
    c = 'A';
    c = '0';
    c = ' ';

    /* Escape sequences */
    c = '\n';
    c = '\r';
    c = '\t';
    c = '\b';
    c = '\f';
    c = '\\';
    c = '\'';
    c = '\"';
    c = '\0';

    /* Octal and hex escapes */
    c = '\101';
    c = '\077';

    /* String literal */
    s = "Hello, World!";

    /* String with escapes */
    s = "Line1\nLine2\tTabbed";

    /* Adjacent string literals (should concatenate) */
    s = "Hello, " "World!";

    /* String initialization */
    arr[0] = 'T';
    arr[1] = 'e';
    arr[2] = 's';
    arr[3] = 't';
    arr[4] = '\0';

    /* String indexing */
    c = "Hello"[0];
    c = "Hello"[4];

    /* Length via iteration */
    i = 0;
    s = "Testing";
    while (s[i] != '\0') {
        i++;
    }

    return i;
}

/* Test complex declarations */
int test_decl(void)
{
    int a;
    int *p;
    int **pp;
    int arr[10];
    int *parr[10];
    int (*arrp)[10];
    int (*fp)(int);
    int (*fparr[5])(int);
    int *(*fpret)(int);
    struct point pt;
    struct point *ptp;
    struct point **ptpp;
    struct rect r;
    struct rect *rp;
    struct window w;
    struct window *wp;
    struct window **wpp;

    /* Basic pointer */
    p = &a;

    /* Pointer to pointer */
    pp = &p;

    /* Array of pointers */
    parr[0] = &a;

    /* Pointer to array */
    arrp = &arr;

    /* Function pointer */
    fp = static_fn;
    a = fp(5);

    /* Array of function pointers */
    fparr[0] = static_fn;
    fparr[1] = static_fn;
    a = fparr[0](10);

    /* Pointer to function returning pointer */
    fpret = retptr;

    /* Struct pointers */
    ptp = &pt;
    ptpp = &ptp;
    (*ptp).x = 1;
    ptp->y = 2;
    (*ptpp)->x = 3;
    (**ptpp).y = 4;

    /* Nested struct pointers */
    rp = &r;
    rp->origin.x = 10;
    rp->origin.y = 20;

    wp = &w;
    wpp = &wp;
    wp->frame.origin.x = 0;
    (*wpp)->frame.origin.y = 0;
    (*wpp)->frame.dim.width = 80;
    wp->content.dim.height = 22;
    (**wpp).visible = 1;

    return a;
}

/* Test typedef usage */
int test_typedef(void)
{
    INT i;
    INTP p;
    POINT pt;
    POINTP ptp;
    WINDOW w;
    WINDOWP wp;
    ARR10 arr;
    FUNCP fp;

    i = 42;
    p = &i;
    pt.x = 10;
    pt.y = 20;
    ptp = &pt;
    ptp->x = 30;
    ptp->y = 40;

    w.frame.origin.x = 0;
    w.frame.origin.y = 0;
    w.frame.dim.width = 80;
    w.frame.dim.height = 24;
    w.visible = 1;

    wp = &w;
    wp->frame.origin.x = 10;
    wp->content.dim.width = 60;

    arr[0] = 1;
    arr[9] = 10;

    fp = ansi_add;
    i = fp(3, 4);

    return i;
}

/* Test enum usage */
int test_enum(void)
{
    enum color c;
    enum numbered n;
    int i;

    c = RED;
    c = GREEN;
    c = BLUE;

    n = FIRST;
    i = FIRST;
    i = SECOND;
    i = THIRD;
    i = FOURTH;

    /* Enum arithmetic */
    c = RED + 1;
    i = BLUE - RED;

    /* Compare */
    if (c == GREEN) {
        i = 1;
    }

    /* Switch on enum */
    switch (c) {
    case RED:
        i = 0;
        break;
    case GREEN:
        i = 1;
        break;
    case BLUE:
        i = 2;
        break;
    }

    return i;
}

/* Test complex expressions with nested struct pointers */
int test_expr(void)
{
    int a, b, c, d;
    int arr[10];
    struct point p;
    struct point *pp;
    struct rect r;
    struct rect *rp;
    struct window w;
    struct window *wp;
    struct screen s;
    struct screen *sp;

    a = 5;
    b = 3;

    /* Chained assignments */
    a = b = c = d = 10;

    /* Complex arithmetic */
    a = (b + c) * d - (b / c) % d;
    a = b * c + d / b - c % d;
    a = ((b + c) * (d - b)) / (c + 1);

    /* Mixed operators */
    a = b > c ? d++ : --d;
    a = (b > c) && (d < a) || (c == b);
    a = b & c | d ^ a;
    a = (b & c) | (d ^ a);
    a = b << 2 | c >> 1;

    /* Side effects */
    arr[0] = 1;
    arr[1] = 2;
    a = arr[b = 0];
    a = (b++, c++, d++);

    /* Struct in expressions - direct */
    p.x = 10;
    p.y = 20;
    a = p.x + p.y;
    a = p.x * p.y + p.x - p.y;

    /* Struct pointer in expressions */
    pp = &p;
    a = pp->x + pp->y;
    a = pp->x * pp->y;

    /* Nested struct in expressions - direct */
    r.origin.x = 1;
    r.origin.y = 2;
    r.dim.width = 100;
    r.dim.height = 50;
    a = r.origin.x + r.origin.y;
    a = r.dim.width * r.dim.height;
    a = r.origin.x + r.dim.width;

    /* Nested struct pointer in expressions */
    rp = &r;
    a = rp->origin.x + rp->origin.y;
    a = rp->dim.width * rp->dim.height;
    a = rp->origin.x * rp->dim.width + rp->origin.y * rp->dim.height;

    /* Deeply nested in expressions */
    wp = &w;
    wp->frame.origin.x = 10;
    wp->frame.origin.y = 5;
    wp->frame.dim.width = 80;
    wp->frame.dim.height = 24;
    wp->content.origin.x = 11;
    wp->content.origin.y = 6;
    wp->content.dim.width = 78;
    wp->content.dim.height = 22;
    a = wp->frame.origin.x + wp->content.origin.x;
    a = wp->frame.dim.width * wp->frame.dim.height;
    a = (wp->frame.dim.width - 2) * (wp->frame.dim.height - 2);
    a = wp->content.dim.width == wp->frame.dim.width - 2;

    /* Very deeply nested in expressions */
    sp = &s;
    sp->main_win.frame.origin.x = 0;
    sp->main_win.frame.dim.width = 80;
    sp->popup.frame.origin.x = 20;
    sp->popup.frame.dim.width = 40;
    a = sp->main_win.frame.origin.x + sp->popup.frame.origin.x;
    a = sp->main_win.frame.dim.width - sp->popup.frame.dim.width;
    a = sp->main_win.frame.dim.width > sp->popup.frame.dim.width;
    a = sp->main_win.frame.origin.x == 0 && sp->popup.frame.origin.x > 0;

    /* Conditional with nested struct */
    a = rp->dim.width > 50 ? rp->origin.x : rp->origin.y;
    a = wp->visible ? wp->frame.dim.width : 0;
    a = sp->active ? sp->main_win.frame.dim.width : sp->popup.frame.dim.width;

    /* Parenthesized expressions */
    a = ((((b + c))));
    a = (b = c = (d = 5));

    return a;
}

/* Test statement variations */
void test_stmt(void)
{
    int a, b;

    /* Empty statement */
    ;

    /* Expression statement */
    a = 5;

    /* Compound statement */
    {
        int x;
        x = 10;
        a = x;
    }

    /* Null statement in loops */
    for (a = 0; a < 10; a++)
        ;

    while (0)
        ;

    /* Labels */
label1:
    a = 1;
label2:
    b = 2;
    goto label1;
    goto label2;

end:
    return;
}

/* Forward declaration */
int forward_fn(int x);

/* Definition of forward declared function */
int forward_fn(int x)
{
    return x * 2;
}

/* K&R style no parameters */
int kr_noparam()
{
    return 42;
}

/* ANSI void parameters */
int ansi_void(void)
{
    return 42;
}

/* Multiple return paths */
int multi_ret(int x)
{
    if (x < 0)
        return -1;
    if (x == 0)
        return 0;
    if (x > 100)
        return 100;
    return x;
}

/* Void function */
void void_fn(void)
{
    gi = 100;
    return;
}

/* Void function without return */
void void_fn2(void)
{
    gi = 200;
}

/* Main function - exercises everything */
int main()
{
    int result;
    struct point pt;
    struct point *ptp;
    struct rect r;
    struct rect *rp;
    struct window w;
    struct window *wp;
    struct screen s;
    struct screen *sp;

    /* Call K&R functions */
    result = kr_add(3, 4);
    result = kr_mixed('A', 100, 1000);
    result = kr_strlen("hello");
    result = kr_noparam();

    /* Call ANSI functions */
    result = ansi_add(3, 4);
    result = ansi_mixed('A', 100, 1000);
    result = ansi_strlen("hello");
    result = ansi_void();

    /* Call other functions */
    result = factorial(5);
    result = is_even(10);
    result = is_odd(7);
    result = forward_fn(25);
    result = static_fn(10);
    result = multi_ret(50);
    void_fn();
    void_fn2();

    /* Call test functions */
    result = test_ops();
    result = test_ctrl(10);
    result = test_scope(5);
    result = test_struct();
    result = test_strarrp();
    result = test_union();
    result = test_array();
    result = test_ptr();
    result = test_conv();
    result = test_str();
    result = test_decl();
    result = test_typedef();
    result = test_enum();
    result = test_expr();
    test_stmt();

    /* Test struct pointer functions */
    pt.x = 10;
    pt.y = 20;
    point_set(&pt, 30, 40);
    result = point_sum(pt);

    rect_set(&r, 0, 0, 100, 50);
    result = rect_area(&r);

    window_set(&w, 10, 5, 60, 20);
    result = window_area(&w);
    move_window(&w, 5, 3);

    screen_init(&s);
    result = screen_area(&s);

    /* Get pointers to nested members */
    ptp = get_origin(&r);
    ptp->x = 999;

    rp = get_frame(&w);
    rp->origin.x = 888;
    rp->dim.width = 777;

    wp = get_main(&s);
    wp->frame.origin.x = 666;
    wp->content.dim.width = 555;

    /* Copy via function */
    copy_rect(&r, &w.frame);

    /* Use function pointers */
    gfuncp = static_fn;
    result = gfuncp(10);
    result = apply(static_fn, 5);

    /* Test globals */
    gc = 'X';
    gi = 100;
    gf = 3.14;
    gpoint.x = 50;
    genum = BLUE;

    /* Test global struct pointers */
    gpointp = &gpoint;
    gpointp->x = 100;
    gpointp->y = 200;

    grectp = &grect;
    grectp->origin.x = 10;
    grectp->origin.y = 20;
    grectp->dim.width = 80;
    grectp->dim.height = 24;

    gwinp = &gwindow;
    gwinp->frame.origin.x = 0;
    gwinp->frame.origin.y = 0;
    gwinp->frame.dim.width = 80;
    gwinp->frame.dim.height = 24;
    gwinp->content.origin.x = 1;
    gwinp->content.origin.y = 1;
    gwinp->content.dim.width = 78;
    gwinp->content.dim.height = 22;
    gwinp->visible = 1;

    gscreenp = &gscreen;
    gscreenp->main_win.frame.origin.x = 0;
    gscreenp->main_win.frame.dim.width = 80;
    gscreenp->popup.frame.origin.x = 20;
    gscreenp->popup.frame.dim.width = 40;
    gscreenp->cursor.x = 0;
    gscreenp->cursor.y = 0;
    gscreenp->active = 1;

    return 0;
}
