/*
 * Tests constant folding for arithmetic, bitwise, and logical operators with proper precedence
 */

// Test constant folding for all binary operators

int add = 10 + 5;        // 15
int sub = 10 - 5;        // 5
int mul = 10 * 5;        // 50
int div = 10 / 5;        // 2
int mod = 10 % 3;        // 1

int band = 12 & 10;      // 8  (1100 & 1010 = 1000)
int bor = 12 | 10;       // 14 (1100 | 1010 = 1110)
int bxor = 12 ^ 10;      // 6  (1100 ^ 1010 = 0110)

int lshift = 5 << 2;     // 20 (5 * 4)
int rshift = 20 >> 2;    // 5  (20 / 4)

// Unary operators
int neg1 = -10;          // -10
int neg2 = -(-5);        // 5
int bnot = ~12;          // -13 (bitwise NOT of 12)
int lnot1 = !0;          // 1 (logical NOT of 0)
int lnot2 = !5;          // 0 (logical NOT of non-zero)

// Precedence tests (verify C operator precedence)
int prec1 = 2 + 3 * 4;   // 14 (multiplication before addition)
int prec2 = (10 + 5) * 2; // 30 (parentheses first)
int prec3 = 20 - 10 / 2;  // 15 (division before subtraction)
int prec4 = 3 * 4 + 5 * 2; // 22 (both mults first, then additions)
int prec5 = 16 / 4 / 2;   // 2 (left-to-right: 16/4=4, 4/2=2)
int prec6 = 100 - 50 - 10; // 40 (left-to-right: 100-50=50, 50-10=40)
int prec7 = 8 << 1 + 1;   // 32 (addition before shift: 1+1=2, 8<<2=32)
int prec8 = 15 & 7 + 1;   // 8 (addition before AND: 7+1=8, 15&8=8)
