/*
 *
 * The stmt.c file is part of the restored P1.COM program
 * from the Hi-Tech CP/M Z80 C v3.09
 *
 * Not a commercial goal of this laborious work is to popularize among
 * potential fans of 8-bit computers the old HI-TECH Z80 C compiler V3.09
 * (HI-TECH Software) and extend its life, outside of the CP/M environment
 * for full operation in windows 32/64 and Unix-like operating systems
 *
 * The HI-TECH Z80 C cross compiler V3.09 is provided free of charge for any use,
 * private or commercial, strictly as-is. No warranty or product support
 * is offered or implied including merchantability, fitness for a particular
 * purpose, or non-infringement. In no event will HI-TECH Software or its
 * corporate affiliates be liable for any direct or indirect damages.
 *
 * You may use this software for whatever you like, providing you acknowledge
 * that the copyright to this software remains with HI-TECH Software and its
 * corporate affiliates.
 *
 * All copyrights to the algorithms used, binary code, trademarks, etc.
 * belong to the legal owner - Microchip Technology Inc. and its subsidiaries.
 * Commercial use and distribution of recreated source codes without permission
 * from the copyright holderis strictly prohibited.
 *
 *
 * See the readme.md file for additional commentary
 *
 * Mark Ogden
 * 09-Jul-2022
 */
#include "p1.h"

void parseStmt(int16_t p1, int16_t p2, register case_t *p3, int16_t *p4);
void parseCompound(int16_t p1, int16_t p2, case_t *p3, int16_t *p4);
void parseAsmStmt(void);
void parseWhileStmt(case_t *p3);
void parseDoStmt(case_t *p3);
void parseIfStmt(int16_t p1, int16_t p2, case_t *p3, int16_t *p4);
void parseSwitch(int16_t p1);
void parseForStmt(case_t *p1);
void parseBreak(int16_t label);
void parseDefault(int16_t p1, int16_t p2, register case_t *p3, int16_t *p4);
void parseCastStmt(int16_t p1, int16_t p2, register case_t *p3, int16_t *p4);
void parseReturn(void);
void parseGotoStmt(void);
void parseStmtLabel(register sym_t *ps, int16_t p1, int16_t p2, case_t *p3, int16_t *p4);
sym_t *resolveLabel(register sym_t *ps);
void emitJumpLabel(int16_t n);
void emitCondBranch(int16_t n, register expr_t *st, char c);
void emitReturnExpr(register expr_t *st);

/**************************************************
 * 84: 409B PMO +++
 * use of uint8_t parameter
 **************************************************/
void parseFunction(void) {
    uint8_t tok;

    enterScope();
    parseLocalDecls(D_STACK);
    defineFuncSig();
    if ((tok = yylex()) != T_LBRACE) {
        expectErr("{");
        skipStmt(tok);
    }
    emitLabelDef(curFuncNode);
    unreachable = false;
    parseLocalDecls(0x14);
    returnLabel = newTmpLabel();
    while ((tok = yylex()) != T_RBRACE) {
        ungetTok = tok;
        parseStmt(0, 0, 0, 0);
    }
    if (!unreachable && !voidReturn)
        prWarning("implicit return at end of non-void function");
    emitLocLabDef(returnLabel);
    exitScope();
}

/**************************************************
 * 85: 4126 PMO +++
 * trivial optimiser differences, use of uint8_t param
 * and addition of dummy parameters
 **************************************************/
void parseStmt(int16_t p1, int16_t p2, register case_t *p3, int16_t *p4) {
    uint8_t tok;
    expr_t *exprResult;

    tok = yylex();
    if (unreachable && tok != T_SEMI && tok != T_LBRACE) {
        if (tok != T_CASE && tok != T_DEFAULT && (tok != T_ID || peekCh() != ':'))
            prWarning("Unreachable code");
        unreachable = false;
    }
    switch (tok) {
    case T_SEMI:
        break;
    case T_LBRACE:
        parseCompound(p1, p2, p3, p4);
        break;
    case T_ASM:
        parseAsmStmt();
        /* FALLTHRU */
    case T_WHILE:
        parseWhileStmt(p3);
        break;
    case T_DO:
        parseDoStmt(p3);
        break;
    case T_FOR:
        parseForStmt(p3);
        break;
    case T_IF:
        parseIfStmt(p1, p2, p3, p4);
        break;
    case T_SWITCH:
        parseSwitch(p1);
        break;
    case T_CASE:
        parseCastStmt(p1, p2, p3, p4);
        break;
    case T_DEFAULT:
        parseDefault(p1, p2, p3, p4);
        break;
    case T_BREAK:
        if (p4)
            *p4 = 1;
        parseBreak(p2);
        break;
    case T_CONTINUE:
        parseBreak(p1);
        break;
    case T_RETURN:
        parseReturn();
        break;
    case T_GOTO:
        parseGotoStmt();
        break;
    case T_ELSE:
        prError("inappropriate 'else'");
        break;
    case T_ID:
        if (peekCh() == ':') {
            tok = yylex();
            parseStmtLabel(yylval.ySym, p1, p2, p3, p4);
            break;
        }
        /* FALLTHRU */
    default:
        ungetTok   = tok;
        exprResult = parseExpr(T_EROOT, parsePrimaryExpr(), 0); /* dummy 3rd arg added */
        emitCast(exprResult);
        freeExpr(exprResult);
        expect(T_SEMI, ";");
        break;
    }
}

/**************************************************
 * 86: 4300 PMO +++
 * trivial optimiser differences and  use of uint8_t param
 **************************************************/
void parseCompound(int16_t p1, int16_t p2, case_t *p3, int16_t *p4) {
    bool haveDecl;
    uint8_t tok;

    haveDecl = (tok = yylex()) == S_CLASS || tok == S_TYPE ||
               (tok == T_ID && yylval.ySym->sclass == T_TYPEDEF);
    if (haveDecl) {
        ungetTok = tok;
        enterScope();
        parseLocalDecls(T_AUTO);
        tok = yylex();
    }
    while (tok != T_RBRACE) {
        ungetTok = tok;
        parseStmt(p1, p2, p3, p4);
        tok = yylex();
    }
    if (haveDecl)
        exitScope();
}

/**************************************************
 * 87: 4390 PMO +++
 * use of uint8_t and the code fix
 **************************************************/
void parseAsmStmt(void) {
    uint8_t tok;
    if ((tok = yylex()) != T_LPAREN) {
        expectErr("(");
        ungetTok = tok;
    }
    if ((tok = yylex()) != T_SCONST) {
        expectErr("string");
        ungetTok = tok;
    } else {
        printf(";; %s\n", yylval.yStr);
        free(yylval.yStr);
    }
#ifdef BUGGY
    if ((tok = yylex()) != ')') {
#else
    /* fix to check against token for ) i.e. T_RPAREN */
    if ((tok = yylex()) != T_RPAREN) {
#endif
        expectErr(")");
        ungetTok = tok;
    }
    expect(T_SEMI, ";");
}
/**************************************************
 * 88: 4406 PMO +++
 * trivial optimiser differences and  use of uint8_t param
 **************************************************/
void parseWhileStmt(case_t *p3) {
    uint8_t tok;
    int16_t continueLabel;
    int16_t breakLabel;
    int16_t loopLabel;
    register expr_t *pe;

    if ((tok = yylex()) != T_LPAREN) {
        expectErr("(");
        ungetTok = tok;
    }
    emitJumpLabel(continueLabel = newTmpLabel());
    emitLocLabDef(loopLabel = newTmpLabel());
    pe = parsePrimaryExpr();
    if ((tok = yylex()) != T_RPAREN) {
        expectErr(")");
        ungetTok = tok;
    }
    parseStmt(continueLabel, breakLabel = newTmpLabel(), p3, 0);
    emitLocLabDef(continueLabel);
    emitCondBranch(loopLabel, pe, 1);
    emitLocLabDef(breakLabel);
    unreachable = false;
}

/**************************************************
 * 89: 44AF PMO +++
 * trivial optimiser differences and  use of uint8_t param
 **************************************************/
void parseDoStmt(case_t *p3) {
    uint8_t tok;
    int16_t continueLabel;
    int16_t breakLabel;
    int16_t loopLabel;
    register expr_t *pe;

    continueLabel = newTmpLabel();
    breakLabel    = newTmpLabel();
    emitLocLabDef(loopLabel = newTmpLabel());
    parseStmt(continueLabel, breakLabel, p3, 0);
    emitLocLabDef(continueLabel);
    if ((tok = yylex()) != T_WHILE)
        expectErr("while");
    if (tok == T_WHILE || tok == T_FOR)
        tok = yylex();
    else if (tok != T_LPAREN) {
        skipStmt(tok);
        return;
    }
    if (tok != T_LPAREN) {
        expectErr("(");
        ungetTok = tok;
    }
    pe = parsePrimaryExpr();
    expect(T_RPAREN, ")");
    emitCondBranch(loopLabel, pe, 1);
    emitLocLabDef(breakLabel);
    unreachable = false;
    if ((tok = yylex()) != T_SEMI)
        expectErr(";");
}

/**************************************************
 * 90: 4595 PMO +++
 * trivial optimiser differences and  use of uint8_t param
 **************************************************/
void parseIfStmt(int16_t p1, int16_t p2, case_t *p3, int16_t *p4) {
    uint8_t tok;
    uint16_t endElseLabel;
    uint16_t endIfLabel;
    uint8_t endifUnreachable;
    register expr_t *pe;

    if ((tok = yylex()) != T_LPAREN) {
        expectErr("(");
        ungetTok = tok;
    }
    pe = parsePrimaryExpr();
    if ((tok = yylex()) != T_RPAREN) {
        expectErr(")");
        ungetTok = tok;
    }
    endIfLabel = newTmpLabel();
    emitCondBranch(endIfLabel, pe, 0);
    parseStmt(p1, p2, p3, p4);
    endifUnreachable = unreachable;
    unreachable      = false;
    if ((tok = yylex()) == T_ELSE) {
        emitJumpLabel(endElseLabel = newTmpLabel());
        emitLocLabDef(endIfLabel);
        parseStmt(p1, p2, p3, p4);
        unreachable = unreachable && endifUnreachable;
        emitLocLabDef(endElseLabel);
    } else {
        ungetTok = tok;
        emitLocLabDef(endIfLabel);
    }
}

/**************************************************
 * 91: 469B PMO +++
 * trivial optimiser differences, use of uint8_t param
 * and addition of dummy paramaters
 **************************************************/
void parseSwitch(int16_t p1) {
    uint8_t tok;
    int16_t endLabel;
    int16_t switchLabel;
    int16_t haveBreak;
    int16_t cnt;
    case_t caseInfo;
    register attr_t *ps;

    if ((tok = yylex()) != T_LPAREN) {
        expectErr("(");
        ungetTok = tok;
    }
    haveBreak         = 0;
    caseInfo.defLabel = 0;
    caseInfo.caseCnt  = 0;
    if ((caseInfo.switchExpr = parseExpr(T_EROOT, parsePrimaryExpr(), 0))) {
        ps = &caseInfo.switchExpr->attr;
        if (!isVarOfType(ps, DT_ENUM) && (!isIntType(ps) || ps->dataType >= DT_LONG))
            prError("illegal type for switch expression");
    }

    if ((tok = yylex()) != T_RPAREN) {
        expectErr(")");
        ungetTok = tok;
    }
    endLabel    = newTmpLabel();
    switchLabel = newTmpLabel();
    emitJumpLabel(switchLabel);
    unreachable = true;
    parseStmt(p1, endLabel, &caseInfo, &haveBreak);
    if (caseInfo.defLabel == 0) {
        caseInfo.defLabel = endLabel;
        haveBreak         = true;
    }
    emitJumpLabel(endLabel);
    emitLocLabDef(switchLabel);
    emitCase(&caseInfo);
    freeExpr(caseInfo.switchExpr);
    cnt = caseInfo.caseCnt;
    while (--cnt >= 0)
        freeExpr(caseInfo.caseOptions[cnt].caseVal);
    emitLocLabDef(endLabel);
    unreachable = !haveBreak;
}

/**************************************************
 * 92: 4838 PMO +++
 * trivial optimiser differences, use of uint8_t param
 * and addition of dummy paramaters
 **************************************************/
void parseForStmt(case_t *p1) {
    int16_t continueLabel;
    int16_t breakLabel;
    int16_t bodyLabel;
    int16_t condLabel;
    int16_t haveCond;
    uint8_t tok;
    expr_t *condExpr;
    expr_t *stepExpr;
    register expr_t *st;

    haveCond = false;
    tok      = yylex();
    if (tok != T_LPAREN)
        expectErr("(");

    if ((tok = yylex()) != T_SEMI) {
        ungetTok = tok;
        st       = parseExpr(T_EROOT, parsePrimaryExpr(), 0);
        emitCast(st);
        freeExpr(st);
        expect(T_SEMI, ";");
    }
    if ((tok = yylex()) != T_SEMI) {
        haveCond = true;
        ungetTok = tok;
        condExpr = parsePrimaryExpr();
        expect(T_SEMI, ";");
    } else
        condExpr = NULL;
    if ((tok = yylex()) != T_RPAREN) {
        ungetTok = tok;
        stepExpr = parseExpr(T_EROOT, parsePrimaryExpr(), 0);
        tok      = yylex();
        if (tok != T_RPAREN) {
            expectErr(")");
            ungetTok = tok;
        }
    } else
        stepExpr = NULL;
    bodyLabel     = newTmpLabel();
    breakLabel    = newTmpLabel();
    continueLabel = newTmpLabel();
    if (condExpr)
        emitJumpLabel(condLabel = newTmpLabel());
    emitLocLabDef(bodyLabel);
    parseStmt(continueLabel, breakLabel, p1, &haveCond);
    emitLocLabDef(continueLabel);
    if (stepExpr) {
        emitCast(stepExpr);
        freeExpr(stepExpr);
    }
    if (condExpr) {
        emitLocLabDef(condLabel);
        emitCondBranch(bodyLabel, condExpr, 1);
    } else
        emitJumpLabel(bodyLabel);
    emitLocLabDef(breakLabel);
    unreachable = !haveCond;
}

/**************************************************
 * 93: 49E1 PMO +++
 * trivial optimiser differences
 **************************************************/
void parseBreak(int16_t label) {
    uint8_t tok;
    if (label) {
        emitJumpLabel(label);
        unreachable = true;
    } else
        prError("inappropriate break/continue");
    if ((tok = yylex()) != T_SEMI)
        expectErr(";");
}

/**************************************************
 * 94: 4A1E PMO +++
 **************************************************/
void parseDefault(int16_t p1, int16_t p2, register case_t *p3, int16_t *p4) {
    uint8_t tok;

    if ((tok = yylex()) != T_COLON)
        expectErr(":");
    if (p3)
        if (p3->defLabel)
            prError("default case redefined");
        else
            emitLocLabDef(p3->defLabel = newTmpLabel());
    else
        prError("'default' not in switch");
    unreachable = false;
    parseStmt(p1, p2, p3, p4);
}

/**************************************************
 * 95: 4A90 PMO +++
 * trivial optimiser differences and  use of uint8_t param
 **************************************************/
void parseCastStmt(int16_t p1, int16_t p2, register case_t *p3, int16_t *p4) {
    uint8_t tok;
    expr_t *caseExpr;
    int16_t caseLabel;
    int16_t caseIdx;
    s4_t *caseOption;

    caseExpr = parseConstExpr(1);
    if ((tok = yylex()) != T_COLON) {
        expectErr(":");
        ungetTok = tok;
    }
    emitLocLabDef(caseLabel = newTmpLabel());
    if (p3) {
        if ((caseIdx = p3->caseCnt++) == 255)
            fatalErr("Too many cases in switch");
        caseOption            = &p3->caseOptions[caseIdx];
        caseOption->caseLabel = caseLabel;
        if (caseExpr && p3->switchExpr) {
            caseExpr            = parseExpr(0x7d, newSTypeLeaf(&p3->switchExpr->attr), caseExpr);
            caseOption->caseVal = caseExpr->t_rhs;
            caseExpr->t_rhs     = NULL;
            freeExpr(caseExpr);
        }
    } else
        prError("'case' not in switch");
    unreachable = false;
    parseStmt(p1, p2, p3, p4);
}

/**************************************************
 * 96: 4BAA PMO +++
 * trivial optimiser differences and  use of uint8_t param
 **************************************************/
void parseReturn(void) {
    uint8_t tok;
    if ((tok = yylex()) != T_SEMI) {
        ungetTok = tok;
        emitReturnExpr(parseExpr(T_EROOT, parsePrimaryExpr(), 0));
        if (yylex() != T_SEMI) {
            expectErr(";");
            ungetTok = tok;
        }
    } else if (!voidReturn)
        prWarning("non-void function returns no value");
    emitJumpLabel(returnLabel);
    unreachable = true;
}

/**************************************************
 * 97: 4C03 PMO +++
 * trivial optimiser differences and  use of uint8_t param
 **************************************************/
void parseGotoStmt(void) {
    uint8_t tok;
    register sym_t *ps;

    if ((tok = yylex()) != T_ID)
        expectErr("label identifier");
    else {
        ps = resolveLabel(yylval.ySym);
        if (ps) {
            emitJumpLabel(ps->a_labelId);
            ps->flags |= 2;
        }
        unreachable = true;
        tok         = yylex();
        if (tok != T_SEMI)
            expectErr(";");
    }
}

/**************************************************
 * 98: 4C57 PMO +++
 **************************************************/
void parseStmtLabel(register sym_t *ps, int16_t p1, int16_t p2, case_t *p3, int16_t *p4) {
    ps = resolveLabel(ps);
    if (ps) {
        emitLocLabDef(ps->a_labelId);
        ps->flags |= 1;
    }
    unreachable = false;
    parseStmt(p1, p2, p3, p4);
}

/**************************************************
 * 99: 4CA4 PMO +++
 * resolveLabel - Resolve or create label symbol
 **************************************************/
sym_t *resolveLabel(register sym_t *ps) {
    if (ps->sclass) {
        if (ps->sclass != D_LABEL) {
            prError("not a label identifier: %s", ps->nVName);
            return NULL;
        }
    } else {
        ps        = declareSym(ps, D_LABEL, 0, 0);
        ps->level = 1;
    }
    return ps;
}

/**************************************************
 * 100: 4CE8 PMO +++
 * emitJumpLabel - Emit unconditional jump to label
 **************************************************/
void emitJumpLabel(int16_t n) {
    register expr_t *st;
    st = parseExpr(P1_COND, newIConstLeaf(n), 0);
    emitCast(st);
    freeExpr(st);
}

/**************************************************
 * 101: 4D15 PMO +++
 * emitCondBranch - Emit conditional branch to label
 **************************************************/
void emitCondBranch(int16_t n, register expr_t *st, char c) {

    if (st) {
        if (c == 0)
            st = parseExpr(T_LNOT, st, 0);

        st = parseExpr(T_123, st, newIConstLeaf(n));
        emitCast(st);
        freeExpr(st);
    }
}

/**************************************************
 * 102: 4D67 PMO +++
 * emitReturnExpr - Emit return with expression
 **************************************************/
void emitReturnExpr(register expr_t *st) {

    if (st) {
        st = parseExpr(T_121, st, 0);
        emitCast(st);
        freeExpr(st);
    }
}
