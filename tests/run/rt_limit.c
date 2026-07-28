/*
 * Per-file limits in pass1, which are counted in phase 1 and read back
 * by position in phase 2.
 *
 * Running out of one of those tables is not a truncation, it is a loss
 * of place: the count read back is zero, so pass2 believes a body is
 * empty, stops reading its statements, and takes the ones that follow
 * for whatever record they happen to decode as.  Everything after that
 * point in the file is wrong, and nothing says so - the compiler exits
 * cleanly and the assembler is handed code that simply lacks whole
 * functions.  pass2's own rewrite.c lost thirteen of its forty-six,
 * its entry point among them, and every suite here passed.
 *
 * So this file is deliberately shaped rather than interesting: many
 * more functions than the table held, many more nested blocks, and
 * enough string literals to pass the counter that used to be a signed
 * char.  What each one computes matters only in that a wrong answer
 * names the function that got lost.
 */
#include "rt.h"

short g;

short f0(x) short x; { return x + 0; }
short f1(x) short x; { return x + 1; }
short f2(x) short x; { return x + 2; }
short f3(x) short x; { return x + 3; }
short f4(x) short x; { return x + 4; }
short f5(x) short x; { return x + 5; }
short f6(x) short x; { return x + 6; }
short f7(x) short x; { return x + 7; }
short f8(x) short x; { return x + 8; }
short f9(x) short x; { return x + 9; }
short f10(x) short x; { return x + 10; }
short f11(x) short x; { return x + 11; }
short f12(x) short x; { return x + 12; }
short f13(x) short x; { return x + 13; }
short f14(x) short x; { return x + 14; }
short f15(x) short x; { return x + 15; }
short f16(x) short x; { return x + 16; }
short f17(x) short x; { return x + 17; }
short f18(x) short x; { return x + 18; }
short f19(x) short x; { return x + 19; }
short f20(x) short x; { return x + 20; }
short f21(x) short x; { return x + 21; }
short f22(x) short x; { return x + 22; }
short f23(x) short x; { return x + 23; }
short f24(x) short x; { return x + 24; }
short f25(x) short x; { return x + 25; }
short f26(x) short x; { return x + 26; }
short f27(x) short x; { return x + 27; }
short f28(x) short x; { return x + 28; }
short f29(x) short x; { return x + 29; }
short f30(x) short x; { return x + 30; }
short f31(x) short x; { return x + 31; }
short f32(x) short x; { return x + 32; }
short f33(x) short x; { return x + 33; }
short f34(x) short x; { return x + 34; }
short f35(x) short x; { return x + 35; }
short f36(x) short x; { return x + 36; }
short f37(x) short x; { return x + 37; }
short f38(x) short x; { return x + 38; }
short f39(x) short x; { return x + 39; }

/*
 * Blocks are counted in the order they are entered, so a function with
 * many of them is what pushes that table over - the old one held 256
 * for a whole file and four sources here needed more.
 *
 * Nothing is declared inside them.  A local declared in a nested block
 * is a separate bug and still open - it is given no frame slot, so it
 * is written over the saved IY and the function returns into nowhere.
 * This file is about the counting; that wants its own.
 */
short
blocky(n) short n;
{
	short t;

	t = 0;
	{ if (n > 0) { t += 0; } }
	{ if (n > 1) { t += 1; } }
	{ if (n > 2) { t += 2; } }
	{ if (n > 3) { t += 3; } }
	{ if (n > 4) { t += 4; } }
	{ if (n > 5) { t += 5; } }
	{ if (n > 6) { t += 6; } }
	{ if (n > 7) { t += 7; } }
	{ if (n > 8) { t += 8; } }
	{ if (n > 9) { t += 9; } }
	{ if (n > 10) { t += 10; } }
	{ if (n > 11) { t += 11; } }
	{ if (n > 12) { t += 12; } }
	{ if (n > 13) { t += 13; } }
	{ if (n > 14) { t += 14; } }
	{ if (n > 15) { t += 15; } }
	{ if (n > 16) { t += 16; } }
	{ if (n > 17) { t += 17; } }
	{ if (n > 18) { t += 18; } }
	{ if (n > 19) { t += 19; } }
	{ if (n > 20) { t += 20; } }
	{ if (n > 21) { t += 21; } }
	{ if (n > 22) { t += 22; } }
	{ if (n > 23) { t += 23; } }
	{ if (n > 24) { t += 24; } }
	{ if (n > 25) { t += 25; } }
	{ if (n > 26) { t += 26; } }
	{ if (n > 27) { t += 27; } }
	{ if (n > 28) { t += 28; } }
	{ if (n > 29) { t += 29; } }
	{ if (n > 30) { t += 30; } }
	{ if (n > 31) { t += 31; } }
	{ if (n > 32) { t += 32; } }
	{ if (n > 33) { t += 33; } }
	{ if (n > 34) { t += 34; } }
	{ if (n > 35) { t += 35; } }
	{ if (n > 36) { t += 36; } }
	{ if (n > 37) { t += 37; } }
	{ if (n > 38) { t += 38; } }
	{ if (n > 39) { t += 39; } }
	{ if (n > 40) { t += 40; } }
	{ if (n > 41) { t += 41; } }
	{ if (n > 42) { t += 42; } }
	{ if (n > 43) { t += 43; } }
	{ if (n > 44) { t += 44; } }
	{ if (n > 45) { t += 45; } }
	{ if (n > 46) { t += 46; } }
	{ if (n > 47) { t += 47; } }
	{ if (n > 48) { t += 48; } }
	{ if (n > 49) { t += 49; } }
	{ if (n > 50) { t += 50; } }
	{ if (n > 51) { t += 51; } }
	{ if (n > 52) { t += 52; } }
	{ if (n > 53) { t += 53; } }
	{ if (n > 54) { t += 54; } }
	{ if (n > 55) { t += 55; } }
	{ if (n > 56) { t += 56; } }
	{ if (n > 57) { t += 57; } }
	{ if (n > 58) { t += 58; } }
	{ if (n > 59) { t += 59; } }
	return t;
}

/*
 * String literals are named from a counter that was a signed char, so
 * the 129th became "str-128" and the ones after reused earlier names.
 */
char *sp[160];

void
fillsp()
{
	sp[0] = "s0";
	sp[1] = "s1";
	sp[2] = "s2";
	sp[3] = "s3";
	sp[4] = "s4";
	sp[5] = "s5";
	sp[6] = "s6";
	sp[7] = "s7";
	sp[8] = "s8";
	sp[9] = "s9";
	sp[10] = "s10";
	sp[11] = "s11";
	sp[12] = "s12";
	sp[13] = "s13";
	sp[14] = "s14";
	sp[15] = "s15";
	sp[16] = "s16";
	sp[17] = "s17";
	sp[18] = "s18";
	sp[19] = "s19";
	sp[20] = "s20";
	sp[21] = "s21";
	sp[22] = "s22";
	sp[23] = "s23";
	sp[24] = "s24";
	sp[25] = "s25";
	sp[26] = "s26";
	sp[27] = "s27";
	sp[28] = "s28";
	sp[29] = "s29";
	sp[30] = "s30";
	sp[31] = "s31";
	sp[32] = "s32";
	sp[33] = "s33";
	sp[34] = "s34";
	sp[35] = "s35";
	sp[36] = "s36";
	sp[37] = "s37";
	sp[38] = "s38";
	sp[39] = "s39";
	sp[40] = "s40";
	sp[41] = "s41";
	sp[42] = "s42";
	sp[43] = "s43";
	sp[44] = "s44";
	sp[45] = "s45";
	sp[46] = "s46";
	sp[47] = "s47";
	sp[48] = "s48";
	sp[49] = "s49";
	sp[50] = "s50";
	sp[51] = "s51";
	sp[52] = "s52";
	sp[53] = "s53";
	sp[54] = "s54";
	sp[55] = "s55";
	sp[56] = "s56";
	sp[57] = "s57";
	sp[58] = "s58";
	sp[59] = "s59";
	sp[60] = "s60";
	sp[61] = "s61";
	sp[62] = "s62";
	sp[63] = "s63";
	sp[64] = "s64";
	sp[65] = "s65";
	sp[66] = "s66";
	sp[67] = "s67";
	sp[68] = "s68";
	sp[69] = "s69";
	sp[70] = "s70";
	sp[71] = "s71";
	sp[72] = "s72";
	sp[73] = "s73";
	sp[74] = "s74";
	sp[75] = "s75";
	sp[76] = "s76";
	sp[77] = "s77";
	sp[78] = "s78";
	sp[79] = "s79";
	sp[80] = "s80";
	sp[81] = "s81";
	sp[82] = "s82";
	sp[83] = "s83";
	sp[84] = "s84";
	sp[85] = "s85";
	sp[86] = "s86";
	sp[87] = "s87";
	sp[88] = "s88";
	sp[89] = "s89";
	sp[90] = "s90";
	sp[91] = "s91";
	sp[92] = "s92";
	sp[93] = "s93";
	sp[94] = "s94";
	sp[95] = "s95";
	sp[96] = "s96";
	sp[97] = "s97";
	sp[98] = "s98";
	sp[99] = "s99";
	sp[100] = "s100";
	sp[101] = "s101";
	sp[102] = "s102";
	sp[103] = "s103";
	sp[104] = "s104";
	sp[105] = "s105";
	sp[106] = "s106";
	sp[107] = "s107";
	sp[108] = "s108";
	sp[109] = "s109";
	sp[110] = "s110";
	sp[111] = "s111";
	sp[112] = "s112";
	sp[113] = "s113";
	sp[114] = "s114";
	sp[115] = "s115";
	sp[116] = "s116";
	sp[117] = "s117";
	sp[118] = "s118";
	sp[119] = "s119";
	sp[120] = "s120";
	sp[121] = "s121";
	sp[122] = "s122";
	sp[123] = "s123";
	sp[124] = "s124";
	sp[125] = "s125";
	sp[126] = "s126";
	sp[127] = "s127";
	sp[128] = "s128";
	sp[129] = "s129";
	sp[130] = "s130";
	sp[131] = "s131";
	sp[132] = "s132";
	sp[133] = "s133";
	sp[134] = "s134";
	sp[135] = "s135";
	sp[136] = "s136";
	sp[137] = "s137";
	sp[138] = "s138";
	sp[139] = "s139";
	sp[140] = "s140";
	sp[141] = "s141";
	sp[142] = "s142";
	sp[143] = "s143";
	sp[144] = "s144";
	sp[145] = "s145";
	sp[146] = "s146";
	sp[147] = "s147";
	sp[148] = "s148";
	sp[149] = "s149";
	sp[150] = "s150";
	sp[151] = "s151";
	sp[152] = "s152";
	sp[153] = "s153";
	sp[154] = "s154";
	sp[155] = "s155";
	sp[156] = "s156";
	sp[157] = "s157";
	sp[158] = "s158";
	sp[159] = "s159";
}

short
same(a, b) char *a, *b;
{
	while (*a && *a == *b) { a++; b++; }
	return *a == *b;
}

main()
{
	short i;

	/* every function past the old table has to still be there */
	CHECK(1, f0(0), 0);
	CHECK(2, f31(0), 31);		/* the last one that used to fit */
	CHECK(3, f32(0), 32);		/* the first one that did not */
	CHECK(4, f39(1), 40);
	CHECK(5, f35(5), 40);

	/* and all of them, so none is quietly an empty body */
	g = 0;
	g += f0(0);
	g += f1(0);
	g += f2(0);
	g += f3(0);
	g += f4(0);
	g += f5(0);
	g += f6(0);
	g += f7(0);
	g += f8(0);
	g += f9(0);
	g += f10(0);
	g += f11(0);
	g += f12(0);
	g += f13(0);
	g += f14(0);
	g += f15(0);
	g += f16(0);
	g += f17(0);
	g += f18(0);
	g += f19(0);
	g += f20(0);
	g += f21(0);
	g += f22(0);
	g += f23(0);
	g += f24(0);
	g += f25(0);
	g += f26(0);
	g += f27(0);
	g += f28(0);
	g += f29(0);
	g += f30(0);
	g += f31(0);
	g += f32(0);
	g += f33(0);
	g += f34(0);
	g += f35(0);
	g += f36(0);
	g += f37(0);
	g += f38(0);
	g += f39(0);
	CHECK(6, g, 780);		/* 0+1+...+39 */

	/* the block-heavy function, which is where the block table ran out */
	CHECK(7, blocky(0), 0);
	CHECK(8, blocky(100), 1770);	/* 0+1+...+59 */
	CHECK(9, blocky(1), 0);

	/* string literals either side of the old signed-char boundary */
	fillsp();
	CHECK(10, same(sp[0], "s0"), 1);
	CHECK(11, same(sp[127], "s127"), 1);
	CHECK(12, same(sp[128], "s128"), 1);
	CHECK(13, same(sp[159], "s159"), 1);

	/* and none of them collided with an earlier one */
	CHECK(14, same(sp[128], "s0"), 0);
	CHECK(15, same(sp[129], "s1"), 0);
	for (i = 0; i < 159; i++)
		if (same(sp[i], sp[i + 1]))
			return 16;

	return 0;
}
