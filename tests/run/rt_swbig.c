/*
 * A switch with more than 128 cases.
 *
 * pass1 grows the case array by doubling and stored the doubled
 * value in the unsigned char it came from: 8, 16, 32, 64, 128, and
 * then 128*2 = 256, which is 0 in a byte.  The array was realloc'd
 * to nothing, the capacity was set to nothing, and the next case
 * wrote through it.  The field is a byte on purpose - the count is
 * one too - so the fix was to double where 256 still fits and to
 * stop at 255, not to widen the field.
 *
 * The largest switch in the tree is 125 cases (wsnm.c), so this sat
 * one case-batch away from being reached.
 */
#include "rt.h"

int
pick(n)
int n;
{
	int r;

	r = -1;
	switch (n) {
	case 0: r = 0; break;
	case 1: r = 2; break;
	case 2: r = 4; break;
	case 3: r = 6; break;
	case 4: r = 8; break;
	case 5: r = 10; break;
	case 6: r = 12; break;
	case 7: r = 14; break;
	case 8: r = 16; break;
	case 9: r = 18; break;
	case 10: r = 20; break;
	case 11: r = 22; break;
	case 12: r = 24; break;
	case 13: r = 26; break;
	case 14: r = 28; break;
	case 15: r = 30; break;
	case 16: r = 32; break;
	case 17: r = 34; break;
	case 18: r = 36; break;
	case 19: r = 38; break;
	case 20: r = 40; break;
	case 21: r = 42; break;
	case 22: r = 44; break;
	case 23: r = 46; break;
	case 24: r = 48; break;
	case 25: r = 50; break;
	case 26: r = 52; break;
	case 27: r = 54; break;
	case 28: r = 56; break;
	case 29: r = 58; break;
	case 30: r = 60; break;
	case 31: r = 62; break;
	case 32: r = 64; break;
	case 33: r = 66; break;
	case 34: r = 68; break;
	case 35: r = 70; break;
	case 36: r = 72; break;
	case 37: r = 74; break;
	case 38: r = 76; break;
	case 39: r = 78; break;
	case 40: r = 80; break;
	case 41: r = 82; break;
	case 42: r = 84; break;
	case 43: r = 86; break;
	case 44: r = 88; break;
	case 45: r = 90; break;
	case 46: r = 92; break;
	case 47: r = 94; break;
	case 48: r = 96; break;
	case 49: r = 98; break;
	case 50: r = 100; break;
	case 51: r = 102; break;
	case 52: r = 104; break;
	case 53: r = 106; break;
	case 54: r = 108; break;
	case 55: r = 110; break;
	case 56: r = 112; break;
	case 57: r = 114; break;
	case 58: r = 116; break;
	case 59: r = 118; break;
	case 60: r = 120; break;
	case 61: r = 122; break;
	case 62: r = 124; break;
	case 63: r = 126; break;
	case 64: r = 128; break;
	case 65: r = 130; break;
	case 66: r = 132; break;
	case 67: r = 134; break;
	case 68: r = 136; break;
	case 69: r = 138; break;
	case 70: r = 140; break;
	case 71: r = 142; break;
	case 72: r = 144; break;
	case 73: r = 146; break;
	case 74: r = 148; break;
	case 75: r = 150; break;
	case 76: r = 152; break;
	case 77: r = 154; break;
	case 78: r = 156; break;
	case 79: r = 158; break;
	case 80: r = 160; break;
	case 81: r = 162; break;
	case 82: r = 164; break;
	case 83: r = 166; break;
	case 84: r = 168; break;
	case 85: r = 170; break;
	case 86: r = 172; break;
	case 87: r = 174; break;
	case 88: r = 176; break;
	case 89: r = 178; break;
	case 90: r = 180; break;
	case 91: r = 182; break;
	case 92: r = 184; break;
	case 93: r = 186; break;
	case 94: r = 188; break;
	case 95: r = 190; break;
	case 96: r = 192; break;
	case 97: r = 194; break;
	case 98: r = 196; break;
	case 99: r = 198; break;
	case 100: r = 200; break;
	case 101: r = 202; break;
	case 102: r = 204; break;
	case 103: r = 206; break;
	case 104: r = 208; break;
	case 105: r = 210; break;
	case 106: r = 212; break;
	case 107: r = 214; break;
	case 108: r = 216; break;
	case 109: r = 218; break;
	case 110: r = 220; break;
	case 111: r = 222; break;
	case 112: r = 224; break;
	case 113: r = 226; break;
	case 114: r = 228; break;
	case 115: r = 230; break;
	case 116: r = 232; break;
	case 117: r = 234; break;
	case 118: r = 236; break;
	case 119: r = 238; break;
	case 120: r = 240; break;
	case 121: r = 242; break;
	case 122: r = 244; break;
	case 123: r = 246; break;
	case 124: r = 248; break;
	case 125: r = 250; break;
	case 126: r = 1; break;
	case 127: r = 3; break;
	case 128: r = 5; break;
	case 129: r = 7; break;
	case 130: r = 9; break;
	case 131: r = 11; break;
	case 132: r = 13; break;
	case 133: r = 15; break;
	case 134: r = 17; break;
	case 135: r = 19; break;
	case 136: r = 21; break;
	case 137: r = 23; break;
	case 138: r = 25; break;
	case 139: r = 27; break;
	}
	return r;
}

int
main()
{
	CHECK(1, pick(0), 0);
	CHECK(2, pick(63), 126);
	CHECK(3, pick(127), 3);        /* the batch that used to break */
	CHECK(4, pick(128), 5);
	CHECK(5, pick(139), 27);
	CHECK(6, pick(200), -1);       /* no such case */
	return 0;
}

/* vim: set tabstop=4 shiftwidth=4 noexpandtab: */
