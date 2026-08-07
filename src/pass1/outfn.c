/*
 * function- and file-level emitters, split from outast.c so no
 * single translation unit carries them alongside the expression
 * emitter.
 */

#include "cc1.h"

extern struct local *curFuncLocals;
extern int analyzeFunc(struct name *func);  /* regalloc.c */
extern int frameSaveBase;                   /* regalloc.c */

/* Emit function parameter declarations */
void
emitPrmDecls(struct type *functype, struct local *locals)
{
	struct name *param;
	struct local *local, *found;

	if (!functype || !(functype->flags & TF_FUNC))
		return;
	for (param = functype->elem; param; param = param->next) {
		if (param->type->size == 0)
			continue;
		found = NULL;
		if (param->id)
			for (local = locals; local; local = local->next)
				if (local->id == param->id) {
					found = local;
					break;
				}
		emit1(AST_DECL);
		emit1(typeSfx(param->type));
		emitS(param->id ? nameOf(param->id) : "_");
		emit1(found ? found->reg : 0);
		emit1(found ? (unsigned char)found->frm_off : 0);
	}
}

/* Emit local variable declarations */
void
emitLocals(struct local *locals)
{
	struct local *local;
	char lbuf[32];

	for (local = locals; local; local = local->next) {
		if (local->kind == kfunarg)
			continue;
		if (local->sclass & SC_STATIC)
			fmtstr(lbuf, "S%d", local->static_id - 1);
		else if (local->static_id)
			fmtstr(lbuf, "L%d", local->static_id - 1);
		else
			fmtstr(lbuf, "%s", nameOf(local->id));
		emit1(AST_DECL);
		emit1(typeSfx(local->type));
		emitS(lbuf);
		emit1(local->reg);
		emit2((unsigned short)local->frm_off);
	}
}

/*
 * Output a global asm block - write directly to assembly file
 */
void
emitGlobalAsm(char *text)
{
	/* Phase 1: don't emit */
	if (phase == 1)
		return;

	if (!text)
		return;
	setSeg(SEG_TEXT);
	asmLine(text);
}

/*
 * Output an asm block inside a function body.  Unlike a global asm
 * block this must stay in place relative to the generated code, so it
 * rides in the AST stream and pass2 copies it to the output.
 * Format: ASM len(2) text
 */
void
emitAsmStmt(char *text)
{
	unsigned short len;

	/* Phase 1: don't emit */
	if (phase == 1)
		return;

	len = text ? strlen(text) : 0;
	emit1(ASM);
	emit2(len);
	emitRaw(text, len);
}

/*
 * Output function header in AST format
 * Format: F rettype name param_count local_count frm_size(2) savebase
 *         params... locals...
 * savebase = scalar area size; the callee-save slots sit at
 * (iy-savebase-2)/(iy-savebase-4), with arrays below them.
 */
void
emitFuncPre(struct name *func)
{
	char func_name[32];
	int frm_size;
	char param_count, local_count;
	struct name *n;
	struct local *l;

	if (!func)
		return;
#ifdef DEBUG
	if (VERBOSE(V_EMIT))
		fdprintf(2, "EMIT func %s\n", nameOf(func->id));
#endif

	frm_size = analyzeFunc(func);
	curFuncLocals = func->u.locals;

	/* Count params and locals first */
	param_count = local_count = 0;
	if (func->type->flags & TF_FUNC)
		for (n = func->type->elem; n; n = n->next)
			if (n->type->size > 0)
				param_count++;
	for (l = func->u.locals; l; l = l->next)
		if (l->kind != kfunarg)
			local_count++;

	/* Emit function header */
	if (func->sclass & SC_STATIC)
		fmtstr(func_name, "S%d", func->static_id - 1);
	else
		fmtstr(func_name, "_%s", nameOf(func->id));
	emit1(AST_FUNC);
	emit1(func->type->sub ? typeSfx(func->type->sub) : 'v');
	emitS(func_name);
	emit1(param_count);
	emit1(local_count);
	emit2((unsigned short)frm_size);
	emit1((unsigned char)frameSaveBase);

	/* Emit declarations */
	emitPrmDecls(func->type, func->u.locals);
	emitLocals(func->u.locals);

	/* Block header */
	emit1(AST_BLOCK);
	emit1(0);
	emit1(popFuncCnt());
}

/*
 * Output an uninitialized global variable declaration
 * Initialized globals are handled by streaming in doInitlzr()
 */
void
emitGv(struct name *var)
{
	char fullname[32];
	int size;

	/* Phase 1: don't emit, just build symbol table */
	if (phase == 1)
		return;

	if (!var)
		return;

	/* Build label: globals get ::, statics get : */
	if (var->sclass & SC_STATIC)
		fmtstr(fullname, "S%d:", var->static_id - 1);
	else
		fmtstr(fullname, "_%s::", nameOf(var->id));

	/* Calculate total size */
	if (var->type->flags & TF_ARRAY)
		size = var->type->count * var->type->sub->size;
	else
		size = var->type->size;

	/* Uninitialized variable - use .bss */
	setSeg(SEG_BSS);
	asmLine(fullname);
	asmDs(size);
}

/*
 * vim: tabstop=4 shiftwidth=4 expandtab:
 */
