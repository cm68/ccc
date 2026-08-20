/*
 * float.h - software floating point on top of a 32-bit long.
 *
 * ccc has no float/double type, so a FLOAT is an unsigned long holding
 * the bits.  The representation is the one float.s uses:
 *
 *     bit 31          sign
 *     bits 24..30     exponent, bias 64
 *     bits 0..23      24-bit mantissa, bit 23 is an explicit leading 1
 *
 * value = mantissa * 2^(exp - 88), mantissa in [0x800000, 0xffffff].
 * mantissa == 0 is zero, regardless of exponent.
 */
typedef unsigned long FLOAT;

FLOAT	itof(int);
int	ftoi(FLOAT);
FLOAT	fadd(FLOAT, FLOAT);
FLOAT	fsub(FLOAT, FLOAT);
FLOAT	fmul(FLOAT, FLOAT);
FLOAT	fdiv(FLOAT, FLOAT);
int	fcmp(FLOAT, FLOAT);	/* -1, 0, +1 */
FLOAT	fneg(FLOAT);
FLOAT	fmod(FLOAT, FLOAT);
FLOAT	fatof(char *);		/* string -> FLOAT */
char	*ftoa(char *, FLOAT, int);	/* FLOAT -> string */

/* math functions (all FLOAT, all function calls) */
FLOAT	sqrt();
FLOAT	exp();
FLOAT	log();
FLOAT	sin();
FLOAT	cos();
FLOAT	atan();
FLOAT	atan2();
FLOAT	pow();
FLOAT	floor();
FLOAT	fabs();
