/*
 * syscall_link test
 */

int creat();
int close();
int link();
int unlink();

int main(void)
{
	int fd;

	fd = creat("/tmp/linktest", 0644);
	if (fd < 0)
		return 1;
	close(fd);

	if (link("/tmp/linktest", "/tmp/linktest2") < 0) {
		unlink("/tmp/linktest");
		return 2;
	}

	unlink("/tmp/linktest2");
	unlink("/tmp/linktest");
	return 0;
}
