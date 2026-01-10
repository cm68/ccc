/*
 * test_filtbrace.c - Worst case test for brace insertion filter
 *
 * Tests:
 * - Single-statement if/else without braces
 * - Single-statement while/for/do without braces
 * - Nested control structures without braces
 * - if-else chains (dangling else)
 * - Already braced statements (should pass through)
 * - switch statements (case/default bodies)
 * - Mixed braced and unbraced
 * - Empty statements (just semicolon)
 * - Deeply nested unbraced structures
 */

void test_simple_if(int x)
{
	if (x > 0)
		x = 1;

	if (x < 0)
		x = -1;
	else
		x = 0;
}

void test_simple_while(int n)
{
	while (n > 0)
		n--;

	while (n < 10)
		n++;
}

void test_simple_for(void)
{
	int i;
	for (i = 0; i < 10; i++)
		foo(i);
}

void test_simple_do(int n)
{
	do
		n--;
	while (n > 0);
}

void test_nested_if(int x, int y)
{
	if (x > 0)
		if (y > 0)
			foo();
		else
			bar();
	else
		baz();
}

void test_nested_loops(int n)
{
	int i, j;
	for (i = 0; i < n; i++)
		for (j = 0; j < n; j++)
			matrix[i][j] = i * j;
}

void test_if_in_loop(int n)
{
	int i;
	for (i = 0; i < n; i++)
		if (i % 2 == 0)
			even(i);
		else
			odd(i);
}

void test_loop_in_if(int x)
{
	if (x > 0)
		while (x > 0)
			x--;
	else
		while (x < 0)
			x++;
}

void test_already_braced(int x)
{
	if (x > 0) {
		foo();
		bar();
	} else {
		baz();
	}

	while (x > 0) {
		x--;
	}

	for (;;) {
		break;
	}
}

void test_if_else_chain(int x)
{
	if (x == 1)
		one();
	else if (x == 2)
		two();
	else if (x == 3)
		three();
	else
		other();
}

void test_switch_cases(int x)
{
	switch (x) {
	case 1:
		one();
		break;
	case 2:
		two();
		/* fallthrough */
	case 3:
		three();
		break;
	default:
		other();
	}
}

void test_switch_no_default(int x)
{
	switch (x) {
	case 1:
		one();
		break;
	case 2:
		two();
		break;
	}
}

void test_empty_bodies(int x)
{
	if (x > 0)
		;  /* empty statement */

	while (0)
		;

	for (;;)
		break;
}

void test_deeply_nested(int a, int b, int c, int d)
{
	if (a)
		if (b)
			if (c)
				if (d)
					foo();
				else
					bar();
			else
				baz();
		else
			qux();
	else
		quux();
}

void test_mixed_braces(int x)
{
	if (x > 0) {
		foo();
	} else
		bar();

	if (x < 0)
		baz();
	else {
		qux();
	}
}

void test_for_variants(void)
{
	int i;

	/* All parts present */
	for (i = 0; i < 10; i++)
		foo(i);

	/* Missing init */
	for (; i < 20; i++)
		bar(i);

	/* Missing condition */
	for (i = 0;; i++)
		if (i > 30) break;

	/* Missing increment */
	for (i = 0; i < 40;)
		i += 2;

	/* Infinite loop */
	for (;;)
		if (done()) break;
}

void test_do_while_nested(int x)
{
	do
		if (x > 0)
			x--;
		else
			x++;
	while (x != 0);
}

void test_continue_break(int n)
{
	int i;
	for (i = 0; i < n; i++)
		if (i % 2 == 0)
			continue;
		else if (i > 100)
			break;
		else
			process(i);
}

void test_return_in_if(int x)
{
	if (x < 0)
		return;

	if (x == 0)
		return;
	else
		foo(x);
}

void test_goto_label(int x)
{
	if (x < 0)
		goto error;

	foo(x);
	return;

error:
	handle_error();
}
