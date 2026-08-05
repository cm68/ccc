/* #if arithmetic obeys C precedence - it was left-to-right once */
#if 1 + 2 * 3 == 7
short right_precedence = 1;
#endif
#if 1 + 2 * 3 == 9
short leftright_bug = 1;
#endif
#if (2 + 3) * 4 == 20
short parens = 1;
#endif
#if 1 ? 2 : 3
short ternary = 1;
#endif
#if -3 + 5 == 2
short unary = 1;
#endif
#if 7 / 0 == 0
short divzero = 1;
#endif
#if 1 << 3 | 1 == 9
short shiftor_c0_grammar = 1;
#endif
