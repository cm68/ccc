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

void parseStmt(int16_t p1, int16_t p2, register case_t * p3, int16_t * p4);
void parseCompound(int16_t p1, int16_t p2, case_t * p3, int16_t * p4);
void parseAsmStmt(void);
void parseWhileStmt(case_t * p3);
void parseDoStmt(case_t * p3);
void parseIfStmt(int16_t p1, int16_t p2, case_t * p3, int16_t * p4);
void parseSwitch(int16_t p1);
void parseForStmt(case_t * p1);
void parseBreak(int16_t label);
void parseDefault(int16_t p1, int16_t p2, register case_t * p3,
				  int16_t * p4);
void parseCastStmt(int16_t p1, int16_t p2, register case_t * p3,
				   int16_t * p4);
void parseReturn(void);
void parseGotoStmt(void);
void parseStmtLabel(register sym_t * ps, int16_t p1, int16_t p2,
					case_t * p3, int16_t * p4);
sym_t *resolveLabel(register sym_t * ps);
void emitJumpLabel(int16_t n);
void emitCondBranch(int16_t n, register expr_t * st, char c);
void emitReturnExpr(register expr_t * st);

/*
 * 84: 409B PMO +++
 * use of uint8_t parameter
 *
 * Parses function body after parameter declarations. Enters new scope,
 * parses local declarations and statements until closing brace. Emits
 * function prologue and epilogue, handles implicit return warnings.
 */
void
parseFunction(void)
{
	uint8_t tok;

	enterScope();
	parseLocDecls(D_STACK);
	defineFuncSig();
	if ((tok = yylex()) != T_LBRACE) {
		expectErr("{");
		skipStmt(tok);
	}
	emitLabelDef(curFuncNode);
	unreachable = 0;
	parseLocDecls(0x14);
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

/*
 * 85: 4126 PMO +++
 * trivial optimiser differences, use of uint8_t param
 * and addition of dummy parameters
 *
 * Main statement parser. Dispatches to specific handlers based on
 * token type. Handles: blocks, loops, conditionals, switch/case,
 * break/continue, return, goto, labels, and expression statements.
 * Tracks unreachable code warnings.
 */
void
parseStmt(int16_t p1, int16_t p2, register case_t * p3, int16_t * p4)
{
	uint8_t tok;
	expr_t *exprResult;

	tok = yylex();
	if (unreachable && tok != T_SEMI && tok != T_LBRACE) {
		if (tok != T_CASE && tok != T_DEFAULT
			&& (tok != T_ID || peekCh() != ':'))
			prWarning("Unreachable code");
		unreachable = 0;
	}
	switch (tok) {
	case T_SEMI:
		break;
	case T_LBRACE:
		parseCompound(p1, p2, p3, p4);
		break;
	case T_ASM:
		parseAsmStmt();
		/*
		 * FALLTHRU 
		 */
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
		/*
		 * FALLTHRU 
		 */
	default:
		ungetTok = tok;
		exprResult = parseExpr(T_EROOT, parsePrimExpr(), 0); /* dummy 3rd
															  * arg added */
		emitCast(exprResult);
		freeExpr(exprResult);
		expect(T_SEMI, ";");
		break;
	}
}

/*
 * 86: 4300 PMO +++
 * trivial optimiser differences and  use of uint8_t param
 *
 * Parses compound statement (brace-enclosed block). Creates new scope
 * if local declarations are present. Parses statements until closing
 * brace.
 */
void
parseCompound(int16_t p1, int16_t p2, case_t * p3, int16_t * p4)
{
	bool haveDecl;
	uint8_t tok;

	haveDecl = (tok = yylex()) == S_CLASS || tok == S_TYPE ||
		(tok == T_ID && yylval.ySym->sclass == T_TYPEDEF);
	if (haveDecl) {
		ungetTok = tok;
		enterScope();
		parseLocDecls(T_AUTO);
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

/*
 * 87: 4390 PMO +++
 * use of uint8_t and the code fix
 *
 * Parses asm("string") statement. Outputs the string as an assembly
 * comment. Expects parenthesized string literal followed by semicolon.
 */
void
parseAsmStmt(void)
{
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
	/*
	 * fix to check against token for ) i.e. T_RPAREN 
	 */
	if ((tok = yylex()) != T_RPAREN) {
#endif
		expectErr(")");
		ungetTok = tok;
	}
	expect(T_SEMI, ";");
}

/*
 * 88: 4406 PMO +++
 * trivial optimiser differences and  use of uint8_t param
 *
 * Parses while statement. Generates labels for loop body, continue
 * point, and break point. Condition is tested at end of loop (jump
 * to condition, then conditional branch back to body).
 */
void
parseWhileStmt(case_t * p3)
{
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
	pe = parsePrimExpr();
	if ((tok = yylex()) != T_RPAREN) {
		expectErr(")");
		ungetTok = tok;
	}
	parseStmt(continueLabel, breakLabel = newTmpLabel(), p3, 0);
	emitLocLabDef(continueLabel);
	emitCondBranch(loopLabel, pe, 1);
	emitLocLabDef(breakLabel);
	unreachable = 0;
}

/*
 * 89: 44AF PMO +++
 * trivial optimiser differences and  use of uint8_t param
 *
 * Parses do-while statement. Body executes first, then condition is
 * tested. Generates labels for loop top, continue point (before
 * condition), and break point.
 */
void
parseDoStmt(case_t * p3)
{
	uint8_t tok;
	int16_t continueLabel;
	int16_t breakLabel;
	int16_t loopLabel;
	register expr_t *pe;

	continueLabel = newTmpLabel();
	breakLabel = newTmpLabel();
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
	pe = parsePrimExpr();
	expect(T_RPAREN, ")");
	emitCondBranch(loopLabel, pe, 1);
	emitLocLabDef(breakLabel);
	unreachable = 0;
	if ((tok = yylex()) != T_SEMI)
		expectErr(";");
}

/*
 * 90: 4595 PMO +++
 * trivial optimiser differences and  use of uint8_t param
 *
 * Parses if statement with optional else clause. Emits conditional
 * branch to skip if-body when condition is false. Handles else by
 * jumping over else-body after if-body. Tracks reachability.
 */
void
parseIfStmt(int16_t p1, int16_t p2, case_t * p3, int16_t * p4)
{
	uint8_t tok;
	uint16_t endElseLabel;
	uint16_t endIfLabel;
	uint8_t endifUnreachable;
	register expr_t *pe;

	if ((tok = yylex()) != T_LPAREN) {
		expectErr("(");
		ungetTok = tok;
	}
	pe = parsePrimExpr();
	if ((tok = yylex()) != T_RPAREN) {
		expectErr(")");
		ungetTok = tok;
	}
	endIfLabel = newTmpLabel();
	emitCondBranch(endIfLabel, pe, 0);
	parseStmt(p1, p2, p3, p4);
	endifUnreachable = unreachable;
	unreachable = 0;
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

/*
 * 91: 469B PMO +++
 * trivial optimiser differences, use of uint8_t param
 * and addition of dummy paramaters
 *
 * Parses switch statement. Validates switch expression is int/enum.
 * Collects case values and labels during body parse. Emits jump table
 * dispatch code after body. Handles default case and break detection.
 */
void
parseSwitch(int16_t p1)
{
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
	haveBreak = 0;
	caseInfo.defLabel = 0;
	caseInfo.caseCnt = 0;
	if ((caseInfo.switchExpr = parseExpr(T_EROOT, parsePrimExpr(), 0))) {
		ps = &caseInfo.switchExpr->attr;
		if (!isVarOfType(ps, DT_ENUM)
			&& (!isIntType(ps) || ps->dataType >= DT_LONG))
			prError("illegal type for switch expression");
	}

	if ((tok = yylex()) != T_RPAREN) {
		expectErr(")");
		ungetTok = tok;
	}
	endLabel = newTmpLabel();
	switchLabel = newTmpLabel();
	emitJumpLabel(switchLabel);
	unreachable = 1;
	parseStmt(p1, endLabel, &caseInfo, &haveBreak);
	if (caseInfo.defLabel == 0) {
		caseInfo.defLabel = endLabel;
		haveBreak = 1;
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

/*
 * 92: 4838 PMO +++
 * trivial optimiser differences, use of uint8_t param
 * and addition of dummy paramaters
 *
 * Parses for statement with init, condition, and step expressions.
 * Any part can be omitted. Generates labels for body, continue (before
 * step), condition test, and break point. Infinite loop if no condition.
 */
void
parseForStmt(case_t * p1)
{
	int16_t continueLabel;
	int16_t breakLabel;
	int16_t bodyLabel;
	int16_t condLabel;
	int16_t haveCond;
	uint8_t tok;
	expr_t *condExpr;
	expr_t *stepExpr;
	register expr_t *st;

	haveCond = 0;
	tok = yylex();
	if (tok != T_LPAREN)
		expectErr("(");

	if ((tok = yylex()) != T_SEMI) {
		ungetTok = tok;
		st = parseExpr(T_EROOT, parsePrimExpr(), 0);
		emitCast(st);
		freeExpr(st);
		expect(T_SEMI, ";");
	}
	if ((tok = yylex()) != T_SEMI) {
		haveCond = 1;
		ungetTok = tok;
		condExpr = parsePrimExpr();
		expect(T_SEMI, ";");
	} else
		condExpr = NULL;
	if ((tok = yylex()) != T_RPAREN) {
		ungetTok = tok;
		stepExpr = parseExpr(T_EROOT, parsePrimExpr(), 0);
		tok = yylex();
		if (tok != T_RPAREN) {
			expectErr(")");
			ungetTok = tok;
		}
	} else
		stepExpr = NULL;
	bodyLabel = newTmpLabel();
	breakLabel = newTmpLabel();
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

/*
 * 93: 49E1 PMO +++
 * trivial optimiser differences
 *
 * Parses break or continue statement. Emits jump to provided label
 * (break label for break, continue label for continue). Reports error
 * if used outside loop/switch context. Expects semicolon terminator.
 */
void
parseBreak(int16_t label)
{
	uint8_t tok;

	if (label) {
		emitJumpLabel(label);
		unreachable = 1;
	} else
		prError("inappropriate break/continue");
	if ((tok = yylex()) != T_SEMI)
		expectErr(";");
}

/*
 * 94: 4A1E PMO +++
 *
 * Parses default label in switch statement. Creates label if first
 * occurrence, reports error if duplicate or outside switch. Continues
 * parsing following statement.
 */
void
parseDefault(int16_t p1, int16_t p2, register case_t * p3, int16_t * p4)
{
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
	unreachable = 0;
	parseStmt(p1, p2, p3, p4);
}

/*
 * 95: 4A90 PMO +++
 * trivial optimiser differences and  use of uint8_t param
 *
 * Parses case label in switch statement. Evaluates constant expression,
 * converts to switch expression type, and records case value with label.
 * Limits to 255 cases. Reports error if outside switch context.
 */
void
parseCastStmt(int16_t p1, int16_t p2, register case_t * p3, int16_t * p4)
{
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
		caseOption = &p3->caseOptions[caseIdx];
		caseOption->caseLabel = caseLabel;
		if (caseExpr && p3->switchExpr) {
			caseExpr =
				parseExpr(0x7d, newSTypeLeaf(&p3->switchExpr->attr),
						  caseExpr);
			caseOption->caseVal = caseExpr->t_rhs;
			caseExpr->t_rhs = NULL;
			freeExpr(caseExpr);
		}
	} else
		prError("'case' not in switch");
	unreachable = 0;
	parseStmt(p1, p2, p3, p4);
}

/*
 * 96: 4BAA PMO +++
 * trivial optimiser differences and  use of uint8_t param
 *
 * Parses return statement with optional expression. Validates return
 * value matches function type. Emits return expression and jump to
 * function epilogue. Warns if non-void function returns without value.
 */
void
parseReturn(void)
{
	uint8_t tok;

	if ((tok = yylex()) != T_SEMI) {
		ungetTok = tok;
		emitReturnExpr(parseExpr(T_EROOT, parsePrimExpr(), 0));
		if (yylex() != T_SEMI) {
			expectErr(";");
			ungetTok = tok;
		}
	} else if (!voidReturn)
		prWarning("non-void function returns no value");
	emitJumpLabel(returnLabel);
	unreachable = 1;
}

/*
 * 97: 4C03 PMO +++
 * trivial optimiser differences and  use of uint8_t param
 *
 * Parses goto statement. Resolves or creates label symbol, emits
 * unconditional jump, and marks code as unreachable. Label must be
 * identifier.
 */
void
parseGotoStmt(void)
{
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
		unreachable = 1;
		tok = yylex();
		if (tok != T_SEMI)
			expectErr(";");
	}
}

/*
 * 98: 4C57 PMO +++
 *
 * Parses labeled statement (label: stmt). Resolves or creates label
 * symbol, emits label definition, marks as defined, then parses the
 * following statement.
 */
void
parseStmtLabel(register sym_t * ps, int16_t p1, int16_t p2, case_t * p3,
			   int16_t * p4)
{
	ps = resolveLabel(ps);
	if (ps) {
		emitLocLabDef(ps->a_labelId);
		ps->flags |= 1;
	}
	unreachable = 0;
	parseStmt(p1, p2, p3, p4);
}

/*
 * 99: 4CA4 PMO +++
 *
 * Resolves label identifier. If already declared, validates it's a
 * label (not other identifier type). If undeclared, creates new label
 * symbol at function scope level.
 */
sym_t *
resolveLabel(register sym_t * ps)
{
	if (ps->sclass) {
		if (ps->sclass != D_LABEL) {
			prError("not a label identifier: %s", ps->nVName);
			return NULL;
		}
	} else {
		ps = declareSym(ps, D_LABEL, 0, 0);
		ps->level = 1;
	}
	return ps;
}

/*
 * 100: 4CE8 PMO +++
 *
 * Emits unconditional jump to local label. Creates P1_COND expression
 * with label number and emits it.
 */
void
emitJumpLabel(int16_t n)
{
	register expr_t *st;

	st = parseExpr(P1_COND, newIConstLeaf(n), 0);
	emitCast(st);
	freeExpr(st);
}

/*
 * 101: 4D15 PMO +++
 *
 * Emits conditional branch to label. If c is 0, negates condition
 * (branch if false). Creates T_123 conditional jump expression.
 */
void
emitCondBranch(int16_t n, register expr_t * st, char c)
{

	if (st) {
		if (c == 0)
			st = parseExpr(T_LNOT, st, 0);

		st = parseExpr(T_123, st, newIConstLeaf(n));
		emitCast(st);
		freeExpr(st);
	}
}

/*
 * 102: 4D67 PMO +++
 *
 * Emits return statement with value. Wraps expression in T_121
 * (return value) node and emits it.
 */
void
emitReturnExpr(register expr_t * st)
{

	if (st) {
		st = parseExpr(T_121, st, 0);
		emitCast(st);
		freeExpr(st);
	}
}

/*
 * vim: tabstop=4 shiftwidth=4 noexpandtab: 
 */
