/* Test: for loop with large body that spills to disk */
main()
{
	int i;
	int x;
	x = 0;
	for (i = 0; i < 10; i = i + 1) {
		/* Large body to force spill */
		x = x + 1;
		x = x + 2;
		x = x + 3;
		x = x + 4;
		x = x + 5;
		x = x + 6;
		x = x + 7;
		x = x + 8;
		x = x + 9;
		x = x + 10;
		x = x + 11;
		x = x + 12;
		x = x + 13;
		x = x + 14;
		x = x + 15;
		x = x + 16;
		x = x + 17;
		x = x + 18;
		x = x + 19;
		x = x + 20;
		x = x + 21;
		x = x + 22;
		x = x + 23;
		x = x + 24;
		x = x + 25;
		x = x + 26;
		x = x + 27;
		x = x + 28;
		x = x + 29;
		x = x + 30;
		x = x + 31;
		x = x + 32;
		x = x + 33;
		x = x + 34;
		x = x + 35;
		x = x + 36;
		x = x + 37;
		x = x + 38;
		x = x + 39;
		x = x + 40;
		x = x + 41;
		x = x + 42;
		x = x + 43;
		x = x + 44;
		x = x + 45;
		x = x + 46;
		x = x + 47;
		x = x + 48;
		x = x + 49;
		x = x + 50;
	}
	return x;
}
