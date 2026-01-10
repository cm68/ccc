/*
 * test_filtctrl.c - Worst case test for control flow lowering filter
 *
 * Tests:
 * - While loop lowering to labels/gotos
 * - For loop lowering (init, cond, incr separation)
 * - Do-while loop lowering
 * - Switch statement transformation
 * - Break/continue in loops
 * - Break in switch
 * - Nested loops with break/continue
 * - Very long conditions (>32 tokens - stress test BUF_MAX)
 * - Complex for loop expressions
 * - Empty loop parts
 * - Multiple switches
 * - Switch with fallthrough
 */

/* Global for condition testing */
struct token { int type; };
struct token cur;

void test_simple_while(int n)
{
	while (n > 0) {
		n--;
	}
}

void test_simple_for(void)
{
	int i;
	for (i = 0; i < 10; i++) {
		foo(i);
	}
}

void test_simple_do(int n)
{
	do {
		n--;
	} while (n > 0);
}

void test_while_break(int n)
{
	while (1) {
		if (n <= 0)
			break;
		n--;
	}
}

void test_while_continue(int n)
{
	int i = 0;
	while (i < n) {
		i++;
		if (i % 2 == 0)
			continue;
		process(i);
	}
}

void test_for_break_continue(int n)
{
	int i;
	for (i = 0; i < n; i++) {
		if (i % 3 == 0)
			continue;
		if (i > 100)
			break;
		process(i);
	}
}

void test_nested_loops(int n, int m)
{
	int i, j;
	for (i = 0; i < n; i++) {
		for (j = 0; j < m; j++) {
			if (i == j)
				continue;  /* continues inner loop */
			if (matrix[i][j] < 0)
				break;     /* breaks inner loop */
			process(i, j);
		}
	}
}

void test_nested_break_outer(int n, int m)
{
	int i, j, done = 0;
	for (i = 0; i < n && !done; i++) {
		for (j = 0; j < m; j++) {
			if (found(i, j)) {
				done = 1;
				break;
			}
		}
	}
}

void test_simple_switch(int x)
{
	switch (x) {
	case 1:
		one();
		break;
	case 2:
		two();
		break;
	case 3:
		three();
		break;
	default:
		other();
		break;
	}
}

void test_switch_fallthrough(int x)
{
	switch (x) {
	case 1:
	case 2:
	case 3:
		small();
		break;
	case 4:
	case 5:
		medium();
		/* fallthrough */
	case 6:
		process();
		break;
	default:
		large();
	}
}

void test_switch_no_default(int x)
{
	switch (x) {
	case 0:
		zero();
		break;
	case 1:
		one();
		break;
	}
}

void test_switch_in_loop(int n)
{
	int i;
	for (i = 0; i < n; i++) {
		switch (arr[i]) {
		case 0:
			continue;  /* continues the for loop */
		case 1:
			process();
			break;     /* breaks the switch */
		default:
			skip();
		}
	}
}

void test_loop_in_switch(int x)
{
	switch (x) {
	case 1:
		while (condition()) {
			if (done())
				break;  /* breaks the while */
			process();
		}
		break;          /* breaks the switch */
	case 2:
		{
			int i;
			for (i = 0; i < 10; i++) {
				if (i == 5)
					continue;
				work(i);
			}
		}
		break;
	}
}

void test_do_break_continue(void)
{
	int i = 0;
	do {
		i++;
		if (i % 2 == 0)
			continue;
		if (i > 50)
			break;
		process(i);
	} while (i < 100);
}

/*
 * STRESS TEST: Very long while condition
 * This tests BUF_MAX (was 32, now 64)
 * Count: cur.type == X || (6 tokens each, 6 conditions = 36 tokens)
 */
void test_long_while_condition(void)
{
	while (cur.type == LPAR || cur.type == LBRACK || cur.type == DOT ||
	       cur.type == ARROW || cur.type == INCR || cur.type == DECR) {
		process();
	}
}

/*
 * STRESS TEST: Even longer condition
 * 8 conditions = 48 tokens (tests the increased BUF_MAX)
 */
void test_very_long_condition(void)
{
	while (cur.type == 1 || cur.type == 2 || cur.type == 3 ||
	       cur.type == 4 || cur.type == 5 || cur.type == 6 ||
	       cur.type == 7 || cur.type == 8) {
		handle();
	}
}

/*
 * STRESS TEST: Complex for loop expressions
 */
void test_complex_for(void)
{
	int i, j, k;

	for (i = 0, j = 10, k = 100; i < j && j < k; i++, j--, k -= 2) {
		process(i, j, k);
	}
}

void test_for_missing_parts(void)
{
	int i = 0;

	/* Missing init */
	for (; i < 10; i++) {
		foo();
	}

	/* Missing cond (infinite) */
	for (i = 0;; i++) {
		if (i > 100) break;
		bar();
	}

	/* Missing incr */
	for (i = 0; i < 100;) {
		i += step();
	}

	/* All missing (infinite) */
	for (;;) {
		if (done()) break;
		work();
	}
}

void test_deeply_nested_ctrl(int a, int b, int c)
{
	while (a > 0) {
		for (; b > 0; b--) {
			do {
				switch (c) {
				case 1:
					if (special())
						continue;  /* do-while continue */
					break;         /* switch break */
				case 2:
					break;
				}
				c--;
			} while (c > 0);
		}
		a--;
	}
}

void test_multiple_switches(int x, int y)
{
	switch (x) {
	case 1:
		switch (y) {
		case 10:
			foo();
			break;
		case 20:
			bar();
			break;
		}
		break;
	case 2:
		baz();
		break;
	}
}

void test_switch_many_cases(int x)
{
	switch (x) {
	case 0: zero(); break;
	case 1: one(); break;
	case 2: two(); break;
	case 3: three(); break;
	case 4: four(); break;
	case 5: five(); break;
	case 6: six(); break;
	case 7: seven(); break;
	case 8: eight(); break;
	case 9: nine(); break;
	case 10: ten(); break;
	case 11: eleven(); break;
	case 12: twelve(); break;
	case 13: thirteen(); break;
	case 14: fourteen(); break;
	case 15: fifteen(); break;
	default: other(); break;
	}
}

void test_empty_switch(int x)
{
	switch (x) {
	}
}

void test_switch_only_default(int x)
{
	switch (x) {
	default:
		handle();
	}
}

/* Make sure labels don't conflict */
void test_label_uniqueness(void)
{
	int i;

	for (i = 0; i < 10; i++) {
		foo();
	}

	for (i = 0; i < 10; i++) {
		bar();
	}

	while (i > 0) {
		i--;
	}

	while (i < 100) {
		i++;
	}

	do {
		work();
	} while (--i);

	do {
		more();
	} while (++i < 50);
}
