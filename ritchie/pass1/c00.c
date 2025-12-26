/*
 * C compiler - pass 1 (front end)
 * Called from cc: c0 source.x temp1 temp2 [ profileflag ] 
 * Reads pre-tokenized .x file from cpp. 
 * temp1 gets most of the intermediate code; 
 * strings are put on temp2, which c1 reads after temp1. 
 */

#include "c0.h"

int isn = 1;
char peeksym = -1;
char peekc;
int line = 1;
char eof;
struct tnode funcblk = { NAME };

/*
 * Buffer for string literals from tokenized input 
 */
char strbuf[1024];
int strbuflen;

union tree *cmst[CMSIZ];
union tree **cp = cmst;

char unscflg;
FILE *xfile;
FILE *sbufp;
char sbuf[512];
char proflg;
char strflg;
char symbuf[MAXCPS + 2];
char mossym;
LTYPE lcval;
int nchstr;
char initflg;
char numbuf[64];
extern char Wflag;				/* print warning messages */

main(argc, argv)
int argc;
char *argv[];
{
	char buf2[BUFSIZ];

	if (argc > 1 && strcmp(argv[1], "-u") == 0) {
		argc--;
		argv++;
		unscflg++;
	}
	if (argc < 4)
		fatal("Arg count");
	/*
	 * Open .x file (binary tokenized input from cpp) 
	 */
	if ((xfile = fopen(argv[1], "rb")) == NULL)
		fatal("Can't find %s", argv[1]);
	if (freopen(argv[2], "w", stdout) == NULL
		|| (sbufp = fopen(argv[3], "w")) == NULL)
		fatal("Can't create temp");
	setbuf(stdout, buf2);		/* stdio sbrk problems */
	setbuf(sbufp, sbuf);
	STAUTO = -8;
	while (argc > 4) {
		switch (argv[4][1]) {
		case 'P':
			proflg++;
			break;
		case 'w':
		case 'W':				/* don't print warning messages */
			Wflag++;
			break;
		}
		argc--;
		argv++;
	}
	coremax = locbase = sbrk(0);
	while (!eof)
		extdef();
	emittent();					/* emit deferred tentative definitions */
	outcode("B", EOFC);
	strflg++;
	outcode("B", EOFC);
	blkend();
	fclose(xfile);
	exit(nerror != 0);
}

/*
 * Look up the identifier in symbuf in the symbol table.
 * Return is a ptr to the symbol table entry.
 */
lookup()
{
	unsigned ihash;
	register struct nmlist *rp;

	ihash = hash(symbuf);
	rp = hshtab[ihash];
	while (rp) {
		if (strcmp(symbuf, rp->name) != 0)
			goto no;
		if (mossym != (rp->hflag & FKIND))
			goto no;
		csym = rp;
		return (NAME);
no:
		rp = rp->nextnm;
	}
	rp = (struct nmlist *) Dblock(sizeof(struct nmlist));
	rp->nextnm = hshtab[ihash];
	hshtab[ihash] = rp;
	rp->hclass = 0;
	rp->htype = 0;
	rp->hoffset = 0;
	rp->hsubsp = NULL;
	rp->hstrp = NULL;
	rp->sparent = NULL;
	rp->hblklev = blklev;
	rp->hflag = mossym;
	rp->name = Dblock((strlen(symbuf) + 1 + LNCPW - 1) & ~(LNCPW - 1));
	strcpy(rp->name, symbuf);
	csym = rp;
	return (NAME);
}

/*
 * Read a 16-bit little-endian word from xfile
 */
static int
readword()
{
	int v;

	v = getc(xfile) & 0xff;
	v |= (getc(xfile) & 0xff) << 8;
	return v;
}

/*
 * Read a 32-bit little-endian long from xfile
 */
static long
readlong()
{
	long v;

	v = readword();
	v |= (long) readword() << 16;
	return v;
}

/*
 * Read next token byte, filtering out line markers
 */
int
gettok()
{
	char c;

again:
	c = getc(xfile);
	if (c == EOF)
		return c;
	if (c == NEWLINE) {
		line++;
		goto again;
	}
	if (c == LINENO) {
		char len, i;

		line = readword();
		len = getc(xfile);
		for (i = 0; i < len && i < sizeof(filename) - 1; i++)
			filename[i] = getc(xfile);
		filename[i] = 0;
		while (i++ < len)
			getc(xfile);
		goto again;
	}
	return c;
}

/*
 * Read a symbol name from xfile into symbuf
 */
static void
readsym()
{
	char len, i;

	len = getc(xfile);
	for (i = 0; i < len && i < MAXCPS; i++)
		symbuf[i] = getc(xfile);
	symbuf[i] = '\0';
	/*
	 * skip any remaining bytes if name was truncated 
	 */
	while (i++ < len)
		getc(xfile);
}

/*
 * Return the next symbol from the input.
 * Reads pre-tokenized binary .x file from cpp.
 * peeksym is a pushed-back symbol.
 * mosflg means that the next symbol, if an identifier,
 * is a member of structure or a structure tag or an enum tag
 */
int
symbol()
{
	register char c;

	if (peeksym >= 0) {
		c = peeksym;
		peeksym = -1;
		if (c == NAME)
			mosflg = 0;
		return (c);
	}

	if (eof)
		return (EOFC);

	/*
	 * Check peekc first (set by nextchar lookahead) 
	 */
	if (peekc) {
		c = peekc;
		peekc = 0;
	} else {
		c = gettok();
	}
	if (c == EOF || c == EOFC) {
		eof++;
		return (EOFC);
	}

	switch (c) {
	case NAME:
		/*
		 * NAME: len byte + name bytes 
		 */
		readsym();
		mossym = mosflg;
		mosflg = 0;
		return (lookup());

	case CON:
		/*
		 * CON: 4-byte little-endian value 
		 */
		lcval = readlong();
		cval = lcval;
		if (lcval < 0 || lcval > MAXINT)
			return (LCON);
		return (CON);

	case LCON:
		/*
		 * LCON: 4-byte little-endian value 
		 */
		lcval = readlong();
		cval = lcval;
		return (LCON);

	case STRING:
		/*
		 * STRING: 2-byte len + string bytes Buffer the string for later
		 * output by putstr() or here for pointer initializers.
		 */
		cval = isn++;
		/* FALLTHROUGH */
	case ASMSTR:
		/*
		 * ASMSTR: 2-byte len + string bytes
		 */
		{
			int len = readword();
			int i;

			strbuflen = len;
			for (i = 0; i < len && i < sizeof(strbuf) - 1; i++)
				strbuf[i] = getc(xfile);
			/*
			 * Skip remaining if too long
			 */
			for (; i < len; i++)
				getc(xfile);
			strbuf[i < sizeof(strbuf) ? i : sizeof(strbuf) - 1] = 0;
			if (c == STRING)
				nchstr = len + 1;
		}
		return (c);

	case FCON:
		/*
		 * FCON: 4-byte IEEE754 float 
		 */
		lcval = readlong();
		/*
		 * Store float string representation for fblock 
		 */
		sprintf(numbuf, "%g", *(float *) &lcval);
		cval = strlen(numbuf) + 1;
		return (FCON);

	case KEYW:
		/*
		 * KEYW: 1-byte keyword value 
		 */
		cval = getc(xfile);
		if (cval == SIZEOF)
			return (SIZEOF);
		return (KEYW);

		/*
		 * All other tokens are single bytes - return as-is 
		 */
	default:
		return (c);
	}
}

/*
 * Write out a string, either in-line
 * or in the string temp file labelled by
 * lab.
 */
putstr(lab, max)
register max;
{
	register int i;

	/*
	 * Output buffered string data from symbol().
	 * lab=0 means inline (for arrays), lab>0 means to a label (for pointers).
	 */
	if (lab) {
		strflg++;
		outcode("BNB", LABEL, lab, BDATA);
		max = 10000;
	} else {
		outcode("B", BDATA);
	}

	for (i = 0; i < strbuflen && i < max; i++) {
		if (i % 15 == 0 && i > 0)
			outcode("0B", BDATA);
		outcode("1N", strbuf[i] & CHARMASK);
	}
	/*
	 * null terminator if room 
	 */
	if (i < max) {
		if (i % 15 == 0 && i > 0)
			outcode("0B", BDATA);
		outcode("10");
	}
	outcode("0");
	strflg = 0;
	nchstr = i + 1;
}

/*
 * Read an expression and return a pointer to its tree.
 * It's the classical bottom-up, priority-driven scheme.
 * The initflg prevents the parse from going past
 * "," or ":" because those delimiters are special
 * in initializer (and some other) expressions.
 */
union tree *
tree(eflag)
{
	static unsigned char opst[SSIZE], prst[SSIZE];
	unsigned char *op, *pp;
	register struct nmlist *cs;
	unsigned char p, ps, os, andflg, o;
	char *svtree;
	static struct cnode garbage =
		{ CON, INT, (int *) NULL, (union str *) NULL, 0 };

	svtree = starttree();
	op = opst;
	pp = prst;
	*op = SEOF;
	*pp = 0;
	andflg = 0;

advanc:
	switch (o = symbol()) {

	case NAME:
		cs = csym;
		if (cs->hclass == TYPEDEF)
			goto atype;
		if (cs->hclass == ENUMCON) {
			*cp++ = cblock(cs->hoffset);
			goto tand;
		}
		if (cs->hclass == 0 && cs->htype == 0)
			if (nextchar() == '(') {
				/*
				 * set function 
				 */
				cs->hclass = EXTERN;
				cs->htype = FUNC;
			} else {
				cs->hclass = STATIC;
				error("%s undefined; func. %s", cs->name,
					  funcsym ? funcsym->name : "(none)");
			}
		*cp++ = nblock(cs);
		goto tand;

	case FCON:
		*cp++ = fblock(DOUBLE, copnum(cval));
		goto tand;

	case LCON:
		*cp = (union tree *) Tblock(sizeof(struct lnode));
		(*cp)->l.op = LCON;
		(*cp)->l.type = LONG;
		(*cp)->l.lvalue = lcval;
		cp++;
		goto tand;

	case CON:
		*cp++ = cblock(cval);
		goto tand;

		/*
		 * fake a static char array 
		 */
	case STRING:
		/*
		 * String is buffered by symbol().
		 * Output to .2 file with label for pointer use.
		 */
		putstr(cval, 10000);
		cs = (struct nmlist *) Tblock(sizeof(struct nmlist));
		cs->hclass = STATIC;
		cs->hoffset = cval;
		*cp++ =
			block(NAME, unscflg ? ARRAY + UNCHAR : ARRAY + CHAR, &nchstr,
				  (union str *) NULL, (union tree *) cs, TNULL);

tand:
		if (cp >= cmst + CMSIZ)
			fatal("Expression overflow");
		if (andflg)
			goto syntax;
		andflg = 1;
		goto advanc;

	case KEYW:
atype:
		if (*op != LPARN || andflg)
			goto syntax;
		peeksym = o;
		*cp++ = xprtype();
		if ((o = symbol()) != RPARN)
			goto syntax;
		o = CAST;
		--op;
		--pp;
		if (*op == SIZEOF) {
			andflg = 1;
			*pp = 100;
			goto advanc;
		}
		goto oponst;

	case INCBEF:
	case DECBEF:
		if (andflg)
			o += 2;
		goto oponst;

	case COMPL:
	case EXCLA:
	case SIZEOF:
		if (andflg)
			goto syntax;
		goto oponst;

	case MINUS:
		if (!andflg)
			o = NEG;
		andflg = 0;
		goto oponst;

	case AND:
	case TIMES:
		if (andflg)
			andflg = 0;
		else if (o == AND)
			o = AMPER;
		else
			o = STAR;
		goto oponst;

	case LPARN:
		if (andflg) {
			o = symbol();
			if (o == RPARN)
				o = MCALL;
			else {
				peeksym = o;
				o = CALL;
				andflg = 0;
			}
		}
		goto oponst;

	case RBRACK:
	case RPARN:
		if (!andflg)
			goto syntax;
		goto oponst;

	case DOT:
	case ARROW:
		mosflg = FMOS;
		break;

	case ASSIGN:
		if (andflg == 0 && PLUS <= *op && *op <= EXOR) {
			o = *op-- + ASPLUS - PLUS;
			pp--;
			goto oponst;
		}
		break;

	}
	/*
	 * binaries 
	 */
	if (andflg == 0)
		goto syntax;
	andflg = 0;

oponst:
	p = (opdope[o] >> PREC_POS) & PREC_MASK;
opon1:
	if (o == COLON && op[0] == COLON && op[-1] == QUEST) {
		build(*op--);
		build(*op--);
		pp -= 2;
	}
	ps = *pp;
	if (p > ps || p == ps && (opdope[o] & RASSOC) != 0) {
		switch (o) {

		case INCAFT:
		case DECAFT:
			p = PREC_HIGH;
			break;
		case LPARN:
		case LBRACK:
		case CALL:
			p = PREC_NONE;
		}
		if (initflg) {
			if ((o == COMMA && *op != LPARN && *op != CALL)
				|| (o == COLON && *op != QUEST)) {
				/*
				 * End expression early - force reduction 
				 */
				goto reduce;
			}
		}
		if (op >= &opst[SSIZE - 1])
			fatal("expression overflow");
		*++op = o;
		*++pp = p;
		goto advanc;
	}
reduce:
	--pp;
	os = *op--;
	if (andflg == 0 && p
		&& ((opdope[o] & BINARY) == 0 || o >= INCBEF && o <= DECAFT)
		&& opdope[os] & BINARY)
		goto syntax;
	switch (os) {

	case SEOF:
		peeksym = o;
		build(0);				/* flush conversions */
		if (eflag)
			endtree(svtree);
		return (*--cp);

	case COMMA:
		if (*op != CALL)
			os = SEQNC;
		break;

	case CALL:
		if (o != RPARN)
			goto syntax;
		build(os);
		goto advanc;

	case MCALL:
		*cp++ = block(NULLOP, INT, (int *) NULL,
					  (union str *) NULL, TNULL, TNULL);
		os = CALL;
		break;

	case INCBEF:
	case INCAFT:
	case DECBEF:
	case DECAFT:
		*cp++ = cblock(1);
		break;

	case LPARN:
		if (o != RPARN)
			goto syntax;
		goto advanc;

	case LBRACK:
		if (o != RBRACK)
			goto syntax;
		build(LBRACK);
		goto advanc;
	}
	build(os);
	goto opon1;

syntax:
	error("Expression syntax");
	errflush(o);
	if (eflag)
		endtree(svtree);
	return ((union tree *) &garbage);
}

union tree *
xprtype()
{
	struct nmlist typer, absname;
	char sc;
	register union tree **scp;

	scp = cp;
	sc = DEFXTRN;				/* will cause error if class mentioned */
	getkeywords(&sc, &typer);
	absname.hclass = 0;
	absname.hblklev = blklev;
	absname.hsubsp = NULL;
	absname.hstrp = NULL;
	absname.htype = 0;
	decl1(sc, &typer, 0, &absname);
	cp = scp;
	return (block(ETYPE, absname.htype, absname.hsubsp,
				  absname.hstrp, TNULL, TNULL));
}

char *
copnum(len)
{
	register char *s1;

	s1 = Tblock((len + LNCPW - 1) & ~(LNCPW - 1));
	strcpy(s1, numbuf);
	return (s1);
}
