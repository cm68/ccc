int sum(n)
int n;
{
	int i, t;

	t = 0;
	for (i = 0; i < n; i++)
		t += i;
	while (n--)
		t -= 1;
	return t;
}
main() { return sum(10); }
