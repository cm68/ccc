extern char *tn[] = {
	"foo",
	"bar",
	"baz"
};

int printf();

void
main()
{
	printf("%s\n", tn[2]);
}

