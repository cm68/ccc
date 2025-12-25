#include "cgen.h"

/*
 * File - code.c
 */
/*
 * emitInitData - Emit data section initialization
 *
 * Recursively emits initialization data for variables:
 *   - Struct initializers: iterates members, handles alignment
 *     padding with prDefb0s, packs bitfields
 *   - Array initializers: recursively processes each element
 *   - Scalar values: optimizes expr and generates via genExprCode
 *
 * Pads with zeros if initializer < target size.
 * Returns total bytes emitted.
 */
int emitInitData(register member_t *symbol, node_t *exprNode) {
    int16_t bytesEmitted;
    int16_t alignPad;
    member_t *typePtr;
    member_t *memberPtr;
    int memberIdx;
    node_t *bitfieldExpr FORCEINIT;

    bytesEmitted = 0;
    if (exprNode->op == COLON_U) {
        freeExprTree(exprNode);
        memberIdx = 0;
        if (symbol->sclass == STRUCT) {
            typePtr = symbol;
            while ((exprNode = parseExpr())->op != DOT_DOT) {
                if ((alignPad = memberAlign(memberPtr = typePtr->u.mlist->vals[memberIdx++])) != 0 && bytesEmitted % ++alignPad != 0) {
                    alignPad -= bytesEmitted % alignPad;
                    bytesEmitted += alignPad;
                    prDefb0s(alignPad); /* emit defb 0 to pad to item boundary */
                }
                if (memberPtr->sflags & B_SLOC_BITFIELD) {
                    exprNode = mkNode(BAND, exprNode, mkConstNode((1L << (int8_t)(memberPtr->bWidth)) - 1L));
                    if (memberPtr->bOffset != 0)
                        /* PMO: bug fix, replaced 0L arg with bit offset memberPtr->bOffset*/
                        bitfieldExpr = mkNode(BOR, bitfieldExpr, mkNode(LSHIFT, exprNode, mkConstNode(memberPtr->bOffset)));
                    else
                        bitfieldExpr = exprNode;

                    if (memberIdx != typePtr->u.mlist->cnt &&
                        (typePtr->u.mlist->vals[memberIdx]->sflags & B_SLOC_BITFIELD) &&
                        typePtr->u.mlist->vals[memberIdx]->offset == memberPtr->offset)
                        continue;
                    exprNode = bitfieldExpr;
                }
                bytesEmitted += emitInitData(memberPtr, exprNode);
            }

            freeExprTree(exprNode);
        } else {
            typePtr = symbol->type;
            for (;;) {
                if ((exprNode = parseExpr())->op == COLON_U)
                    bytesEmitted += emitInitData(typePtr, exprNode);
                else if (exprNode->op == DOT_DOT) {
                    freeExprTree(exprNode);
                    break;
                } else {
                    bytesEmitted += nodesize(exprNode);
                    genExprCode(optimizeExpr(mkNode(ATGIGN, exprNode, 0)));
                }
            }
        }
    } else {
        exprNode = optimizeExpr(mkNode(ATGIGN, exprNode, 0));
        bytesEmitted  = nodesize(exprNode);
        genExprCode(exprNode);
    }
    if (bytesEmitted < symbol->size) {
        prDefb0s(symbol->size - bytesEmitted);
        bytesEmitted = symbol->size;
    } else if (symbol->size < bytesEmitted && symbol->size != 0)
        prError("Too many initializers");

    return bytesEmitted; /* m15: */
}

/*
 * parseInit - Parse and emit variable initialization
 *
 * Handles initialized data declarations:
 *   - Emits data section and variable label
 *   - Processes initializer expression tree
 *   - Computes/updates array element count if needed
 *
 * OK++ PMO
 */
void parseInit() {
    register member_t *symbol;
    int bytesEmitted;
    int elemSize;

    symbol = lookupSymbol(getToken());
    prPsect(P_DATA);
    emitVarLbl(symbol); /* Emit "symbolic_name:" (identifier label) */
    bytesEmitted = emitInitData(symbol, parseExpr());
    elemSize = symbol->refl ? 2 : symbol->type->size;
    if (symbol->nelem == 0) { /* make sure nelem is set */
        symbol->nelem = bytesEmitted / elemSize;
        symbol->size  = varSize(symbol);
    }
    expect(']');
}

/*
 * prFrameHead - Emit function frame setup code
 *
 * Generates function prologue:
 *   - Switches to text section
 *   - Emits global declarations (first call only)
 *   - Emits call to ncsv (stack frame setup)
 *   - Emits frame size reference (defw f<id>)
 *
 * OK++ PMO
 */
void prFrameHead(int fId) {
    static bool frameGlobEmit; /* First call ncsv	   */

    prPsect(P_TEXT);
    if (frameGlobEmit == false) /* if first call	*/
        printf("global\tncsv, cret, indir\n");

    printf("call\tncsv\n");
    frameGlobEmit = true; /* first call is done	*/
    printf("defw\tf%d\n", fId);
}

/*
 * prFrameTail - Emit function frame cleanup code
 *
 * Generates function epilogue:
 *   - Emits jump to cret (common return handler)
 *   - Defines frame size constant (f<id> equ <size>)
 *
 * OK++ PMO
 */
void prFrameTail(int fId, int fSize) {

    prPsect(P_TEXT);
    printf("jp\tcret\n"
           "f%d\tequ\t%d\n",
           fId, fSize);
}

/*
 * prStrcRetCpy - Emit struct-by-value return copy code
 *
 * Emits Z80 code for functions returning structs by value:
 *   ld de,k<id>  ; dest in BSS
 *   ld bc,<size> ; byte count
 *   ldir         ; block copy
 *   ld hl,k<id>  ; return pointer in HL
 * Plus BSS storage: k<id>: defs <size>
 */
void prStrcRetCpy(int kId, int size) {

    prPsect(P_TEXT);
    printf("ld\tde,k%d\n"
           "ld\tbc,%d\n"
           "ldir\n"
           "ld\thl,k%d\n",
           kId, size, kId);
    prPsect(P_BSS);
    printf("k%d:defs\t%d\n", kId, size);
}

/*
 * prGlobalDef - Emit "global name" directive
 */
void prGlobalDef(register member_t *symbol) {

    printf("global\t%s\n", symbol->name);
    symbol->sflags |= B_SLOC_GLOBAL;
}

/*
 * emitBssDef - Emit BSS variable definition
 */
void emitBssDef(register member_t *symbol) {

    if ((symbol->sflags & B_SLOC_EMITTED) == 0 && symbol->nelem != 0 && (symbol->refl & B_REFL_FUNC) == 0) {
        prPsect(P_BSS);
        if (symbol->tflag == 3 && (symbol->sflags & B_SLOC_GLOBAL) == 0)
            prGlobalDef(symbol); /* Emit "global name" */
        emitVarLbl(symbol);      /* Emit "symbolic_name:" (identifier label) */
        printf("\tdefs\t%u\n", symbol->size);
    }
}

/*
 * emitVarLbl - Emit variable label with optional global
 */
void emitVarLbl(register member_t *symbol) {

    if ((symbol->sflags & B_SLOC_GLOBAL) == 0 && symbol->tflag == 3)
        prGlobalDef(symbol); /* Emit "global name" */
    symbol->sflags |= B_SLOC_EMITTED;
    printf("%s:\n", symbol->name);
}

/*
 * prDefb0s OK++ PMO				 Used in: emitInitData
 * Emit "defb 0, ..." (num bytes)
 */
void prDefb0s(int num) {
    char cnt;

    cnt = 0;             /* Reset counter bytes printed	   */
    while (num-- != 0) { /* While data is available	   */
        if (cnt == 0)
            printf("defb\t0"); /* Initially output "defb 0",	   */
        else
            printf(",0");   /* later ",0"			   */
        cnt++;              /* and update number bytes output  */
        if (cnt == 16) {    /* If number bytes in string is 16 */
            cnt = 0;        /* Reset counter and		   */
            fputchar('\n'); /* continue output in next line	   */
        }
    } /* continue processing		   */
    if (cnt != 0)
        fputchar('\n'); /* If line is incomplete, new line */
}

/*
 * prPsect OK++ PMO     Used in: parseStmt, parseData, parseInit,
 *                           prFrameHead, prFrameTail, prStrcRetCpy
 * Select psect
 */
void prPsect(int section) {
    static int curPsect;
    static char *psectNames[] = { "", "bss", "text", "data" };

    if (section != curPsect) /* Only when changing section */
        printf("psect\t%s\n", psectNames[curPsect = section]);
}

/*
 * sortCaseLabels - Sort switch case values and labels
 *
 * Bubble sorts parallel arrays of case values and code labels.
 * Detects duplicate case values and reports error.
 * Used to prepare switch tables for jump table or linear search.
 *
 * OK++ PMO
 */
void sortCaseLabels(int *pCase, int *pLabel, int nCase) {
    bool changed;
    int *pl;
    int cnt;
    int tCase;
    int tLabel;
    register int *pc;

    do {
        changed = false;
        pc      = pCase;
        pl      = pLabel;
        for (cnt = nCase; --cnt > 0; pc++, pl++) {
            if (pc[1] < pc[0]) {
                changed = true;
                tCase   = pc[0];
                pc[0]   = pc[1];
                pc[1]   = tCase;
                tLabel  = pl[0];
                pl[0]   = pl[1];
                pl[1]   = tLabel;
            } else if (pc[0] == pc[1]) {
                prError("Duplicate case label");
                return;
            }
        }
    } while (changed != 0);
    return;
}

/*
 * parseSwitch - Parse and emit switch statement code
 *
 * Generates optimal switch implementation based on case density:
 *   - Dense cases: jump table (hl-indexed indirect jump)
 *   - Sparse cases: linear compare/jump sequence
 * Handles default label, sorts cases, validates uniqueness.
 *
 * OK++ PMO - Minor differences due to char/uint8_t parameters
 * and optimizer detecting zero values in h/l registers.
 */
void parseSwitch() {
    node_t *switchExpr;
    int codeLabel, swTableLabel, caseRange, caseCnt, defaultCodeLabel;
    int16_t andMask, orMask;
    bool loTest, hiTest;
    int caseVals[255], codeLabels[255];
    register node_t *node;

    switchExpr     = parseExpr();
    caseCnt = 0;
    for (;;) {
        node        = optimizeExpr(parseExpr());
        codeLabel = atoi(getToken());
        if (node->op == DOT_DOT) { /* end of switch block */
            defaultCodeLabel = codeLabel;
            freeNode(node);
            expect(']');
            if (caseCnt != 0)
                break;
            prWarning("No case\tlabels");
            freeExprTree(switchExpr);
            printf("jp\tl%d\n", defaultCodeLabel);
            return;
        }
        if (node->op == CONST) { /* only constants allowed */
            caseVals[caseCnt]     = node->info.l;
            codeLabels[caseCnt++] = codeLabel;
        } else
            prError("Non-constant case label");
        freeExprTree(node);
    }
    sortCaseLabels(caseVals, codeLabels, caseCnt); /* m6:  */
    caseRange = caseVals[caseCnt - 1] - caseVals[0];
    if (0 <= caseRange && caseRange < 16000 &&
        caseRange * 2 + 20 < caseCnt * 5) { /* if jmp table is shorter */
        /* jump table option is smaller so use it
           note the alternative cmp/jp option may be slower
           even if it is shorter
        */
        node        = allocNode();
        node->op  = TYPE;
        node->pm    = switchExpr->pm;
        node->tFlags = switchExpr->tFlags;
        node        = mkNode(CONV, mkConstNode(caseVals[0]), node);
        node        = mkNode(SUB, switchExpr, node);
        if (nodesize(node) != 2) {
            switchExpr       = allocNode();
            switchExpr->op = TYPE;
            switchExpr->pm   = lookupSymbol("us");
            node        = mkNode(CONV, node, switchExpr);
        }
        prPsect(P_TEXT);
        emitExpr(mkNode(RPAREN, node, 0));
        swTableLabel = newLocal(); /* swTableCnt++ */
        /*
            with the switch value in hl, the code
            emitted is effectively
            ld  a,.high. caseRange
            cp  h
            jp  c,l{defaultCodeLabel}
            jp  nz,1f
            ld  a,.low. caseRange
            cp  l
            jp  c,l{defaultCodeLabel}
         1: add hl,hl
            ld  de,S{swTableLabel}
            ld  a,(hl)
            inc hl
            ld  h,(hl)
            ld  l,a
            jp  (hl)
            where {xxx} indicates the respective label number of xxx
        */
        printf("ld\ta,%u\n"
               "cp\th\n"
               "jp\tc,l%d\n"
               "jp\tnz,1f\n"
               "ld\ta,%u\n"
               "cp\tl\n"
               "jp\tc,l%d\n"
               "1:add\thl,hl\n"
               "ld\tde,S%u\n"
               "add\thl,de\n"
               "ld\ta,(hl)\n"
               "inc\thl\n"
               "ld\th,(hl)\n"
               "ld\tl,a\n"
               "jp\t(hl)\n",
               caseRange >> 8, defaultCodeLabel, caseRange & LOBYTE_MASK, defaultCodeLabel, swTableLabel);
        prPsect(P_DATA);
        printf("S%d:\n", swTableLabel);
        codeLabel = 0;
        caseRange = caseVals[0];
        do {
            if (caseRange == caseVals[codeLabel]) {
                printf("defw\tl%d\n", codeLabels[codeLabel]);
                codeLabel++;
            } else
                printf("defw\tl%d\n", defaultCodeLabel); /* fill holes with default */

            caseRange++;
        } while (codeLabel < caseCnt);
        return;
    }
    if (nodesize(switchExpr) == 2) {
        prPsect(P_TEXT);
        emitExpr(mkNode(RPAREN, switchExpr, 0));
        andMask = -1;
        orMask = (int)(loTest = hiTest = false);

        for (codeLabel = 0; codeLabel < caseCnt; codeLabel++) {
            andMask &= caseVals[codeLabel];
            orMask |= caseVals[codeLabel];
            if (andMask >> 8 != orMask >> 8 && (andMask & LOBYTE_MASK) != (orMask & LOBYTE_MASK)) {
                if (hiTest | loTest)
                    printf("1:\n"); /* target of previous hi/lo test */
                loTest = hiTest = false;
                andMask = orMask = caseVals[codeLabel];
            }
            if (andMask >> 8 == orMask >> 8) {
                if (!hiTest) {
                    printf("ld\ta,h\n");
                    prCaseCmp(andMask >> 8);
                    printf("jp\tnz,1f\n"
                           "ld\ta,l\n");
                    hiTest = true;
                }
                caseVals[codeLabel] &= LOBYTE_MASK;
            } else {
                if (!loTest) { /* m16: */
                    if (hiTest)
                        printf("1:\n");
                    printf("ld\ta,l\n"); /* m17: */
                    prCaseCmp(andMask & LOBYTE_MASK);
                    printf("jp\tnz,1f\n"
                           "ld\ta,h\n");
                    loTest = true;
                }
                caseVals[codeLabel] >>= 8;
                caseVals[codeLabel] &= LOBYTE_MASK;
            }
            prCaseCmp(caseVals[codeLabel]); /* m19: */
            printf("jp\tz,l%d\n", codeLabels[codeLabel]);
        }
        if (hiTest | loTest)
            printf("1:\n");
        printf("jp\tl%d\n", defaultCodeLabel); /* m21: */
        return;
    }

    prPsect(P_TEXT);
    emitExpr(mkNode(INAREG, switchExpr, 0));
    for (codeLabel = 0; codeLabel < caseCnt; codeLabel++) {
        if (caseVals[codeLabel] < 256 && caseVals[codeLabel] >= -128) {
            prCaseCmp(caseVals[codeLabel]);
            printf("jp\tz,l%d\n", codeLabels[codeLabel]);
        }
    }
    printf("jp\tl%d\n", defaultCodeLabel);
}

/*
 * prCaseCmp - Print case comparison instruction
 *
 * Emits Z80 compare for switch/case value testing:
 *   non-zero: cp <value>
 *   zero:     or a  (tests A==0 without changing flags)
 */
void prCaseCmp(int par) {

    printf((uint16_t)par ? "cp\t%d\n" : "or\ta\n", par);
}

/*
 * prPush - Emit register push instruction
 *
 * Generates Z80 push for given register:
 *   - Converts single regs to pairs (a->af, etc)
 *   - Handles special DEHL case (push hl; push de)
 *
 * OK++ PMO
 */
void prPush(uint8_t reg) {

    if (reg == REG_DEHL) {
        printf("push\thl\n"
               "push\tde\n");
        return;
    }
    if (reg <= REG_H)
        reg = reg / 2 + REG_AF;
    printf("push\t%s\n", regNames[reg]);
}

/*
 * prPop - Emit register pop instruction
 *
 * Generates Z80 pop for given register:
 *   - Converts single regs to pairs (a->af, etc)
 *   - Handles special DEHL case (pop de; pop hl)
 *
 * OK++ PMO
 */
void prPop(uint8_t reg) {

    if (reg == REG_DEHL) {
        printf("pop\tde\n"
               "pop\thl\n");
        return;
    }
    if (reg <= REG_H)
        reg = reg / 2 + REG_AF;
    printf("pop\t%s\n", regNames[reg]);
}

/*
 * prIXnPush - Load IY register from stack frame parameter
 *
 * Emits code to load IY from a parameter stored in the IX-based
 * stack frame. Used for register formal parameters:
 *   ld l,(ix+offset)
 *   ld h,(ix+offset+1)
 *   push hl / pop iy
 *
 * OK++ PMO
 */
void prIXnPush(register member_t *symbol) {

    printf("ld\tl,(ix+%d)\n"
           "ld\th,(ix+%d)\n"
           "push\thl\n"
           "pop\tiy\n",
           symbol->offset, symbol->offset + 1);
}

/*
 * getTypeClass - Get type classification of expression node
 *
 * Returns a code indicating the type category:
 *   0: Special (function pointer, array)
 *   1: Signed integer
 *   2: Unsigned integer (includes pointers)
 *   3: Float
 *
 * Used for code generation to select appropriate instructions
 * and for type compatibility checking.
 */
uint8_t getTypeClass(register node_t *node) {

    if (node->tFlags & T_FUNC)
        return 0;
    if (node->tFlags != 0)
        return 2;
    if (node->pm->nelem > 1)
        return 0;
    return node->pm->tflag;
}

/*
 * prTypeChar - Print type character (a=signed, l=unsigned, f=float)
 */
void prTypeChar(register node_t *node) {

    static char typeChars[] = { 0, 'a', 'l', 'f' };

    fputchar(typeChars[getTypeClass(node)]);
}

/*
 * prDefb - Emit byte data definition
 *
 * Generates "defb" directives from buffer:
 *   - Outputs bytes as unsigned decimal values
 *   - Formats 16 bytes per line maximum
 *   - Used for string literals and byte arrays
 *
 * OK++ PMO
 */
void prDefb(register char *ptr, int num) {
    char cnt;

    cnt = 0;             /* Reset counter bytes printed	   */
    while (num-- != 0) { /* While data is available	   */
        if (cnt == 0)
            printf("defb\t"); /* Initially output "defb",	   */
        else
            fputchar(',');             /* later "," 			   */
        printf("%d", (uint8_t)*ptr++); /* Output byte and advance pointer */
        cnt++;                         /* update number bytes output 	   */
        if (cnt == 16) {               /* max 16 bytes per line */
            fputchar('\n');
            cnt = 0;
        }
    }
    if (cnt != 0)
        fputchar('\n'); /* If line is incomplete, new line */
}

/*
 * prJmpLabel - Emit local jump label
 *
 * Generates label for local jumps: j<number>:
 * Used internally for control flow.
 *
 * OK++ PMO
 */
void prJmpLabel(int p) {
    printf("j%d:\n", p);
}

/*
 * prJump - Emit unconditional jump to local label
 *
 * Generates: jp j<number>
 * Used for control flow to local labels.
 *
 * OK++ PMO
 */
void prJump(int p) {
    printf("jp\tj%d\n", p);
}

/*
 * setEnumSize OK++ PMO			Used in: parseEnum
 *
 * Computes storage size and signedness for an enum type
 * based on the range of its values.
 *
 * Parameters:
 *   sb    - enum symbol to update
 *   hibnd - highest value in enum
 *   lobnd - lowest value in enum
 *
 * Sets:
 *   b_flag: 1 = signed, 2 = unsigned
 *   b_size: 1 = fits in byte, 2 = needs 16-bit
 */
void setEnumSize(register member_t *symbol, int hibnd, int lobnd) {

    if (hibnd >= 0) {
        symbol->tflag = 2;              /* unsigned */
        if (lobnd <= 255)
            symbol->size = 1;          /* uint8_t */
        else
            symbol->size = 2;          /* uint16_t */
    } else {
        symbol->tflag = 1;              /* signed */
        if (lobnd <= 127 && hibnd >= -128)
            symbol->size = 1;          /* int8_t */
        else
            symbol->size = 2;          /* int16_t */
    }
}

/*
 * max OK++ PMO			Used in: layoutStruct
 * Find maximum between two numbers
 */
int max(int num1, int num2) {
    return (num1 > num2) ? num1 : num2;
}

#define NVARS 14

struct tType {
    char *t_str;
    int t_size;
    int t_alig;
    char t_flag;
};

/*
 * initTypes - Initialize built-in type symbols
 */
void initTypes() {
    member_t *symbol;
    int16_t cnt;
    register struct tType *typePtr;

    /*
     *	Initializaion of type pointers
     */
    static struct tType vars[NVARS] = {
                                        { "i", 2, 0, 1 },  { "s", 2, 0, 1 },  { "c", 1, 0, 1 },
                                        { "l", 4, 0, 1 },  { "ui", 2, 0, 2 }, { "us", 2, 0, 2 },
                                        { "uc", 1, 0, 2 }, { "ul", 4, 0, 2 }, { "f", 4, 0, 3 },
                                        { "d", 4, 0, 3 },  { "x", 2, 0, 1 },  { "ux", 2, 0, 2 },
                                        { "b", 0, 0, 0 },  { "v", 0, 0, 0 }
    };

    /* Clear hash table */

    blkclr((char *)hashtab, sizeof(hashtab));

    /* Create a hash table of templates for standard types */

    cnt = NVARS;
    typePtr  = vars;
    do {
        symbol          = lookupSymbol(typePtr->t_str);
        symbol->sclass = TYPE;
        symbol->offset   = typePtr->t_alig;
        symbol->size  = typePtr->t_size;
        symbol->tflag  = typePtr->t_flag;
        typePtr++;
    } while (--cnt != 0);

    /* Additional patterns for types */

    typeLong   = lookupSymbol("l");  /* long	  	*/
    typeDouble = lookupSymbol("d");  /* double 	*/
    typeB      = lookupSymbol("b");  /* b   	  	*/
    typeVar    = lookupSymbol("v");  /* variable 	*/
    typeChar   = lookupSymbol("c");  /* char   	*/
    typeUChar  = lookupSymbol("uc"); /* uint8_t  	*/
    typeX      = lookupSymbol("x");  /* x      	*/
    lvlidx     = -1;
}

/*
 * newLocal - Allocate a new local label number
 */
int newLocal() {
    static int localLabelCnt;
    return ++localLabelCnt;
}

/*
 * declareSymbol OK++ PMO
 *
 * Looks up or creates a symbol table entry for the given token.
 * If a symbol exists at the current scope (nstdpth), returns it.
 * Otherwise creates a new entry that shadows any outer scope symbol.
 *
 * Parameters:
 *   token - the identifier name
 *   cls   - symbol class (VAR, STRUCT, UNION, ENUM, etc.)
 *
 * Returns: pointer to the symbol table entry (member_t)
 */
member_t *declareSymbol(char *token, uint8_t cls) {
    member_t **hashPtr;
    register member_t *symbol;

    symbol = lookupSymbol(token);                     /* Look up existing symbol                 */
    if (symbol->depth != nstdpth) {                 /* Not at current scope - create new entry */
        hashPtr        = gethashptr(token);           /* Get hash bucket pointer                 */
        symbol         = allocMem(sizeof(member_t));  /* Allocate new symbol entry               */
        symbol->next = *hashPtr;                        /* Link into hash chain                    */
        *hashPtr = symbol;                                /* Insert at head of bucket                */
        symbol->name = allocMem(strlen(token) + 1); /* Allocate and copy name                  */
        strcpy(symbol->name, token);
        symbol->depth = (uint8_t)nstdpth;           /* Set scope nesting depth                 */
    }
    symbol->sclass = cls;
    return symbol;
}
/* end of code.c */

/* vim: set tabstop=4 shiftwidth=4 noexpandtab: */
