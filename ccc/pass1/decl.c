/*
 * declaration parsing and top-level parse driver
 */

#include "cc1.h"

/* List of global declarations built during parsing */
static struct name *global = 0;

/*
 * Find struct member at given offset (members are linked in reverse order)
 */
struct name *
findMemberOff(struct name *members, int offset)
{
    while (members) {
        if (members->w.m.offset == offset)
            return members;
        members = members->next;
    }
    return NULL;
}

/*
 * Stream an initializer value directly to assembly output
 * Used for static/global initializers to avoid building expression trees
 * Returns count of top-level elements (for array size inference)
 */
int
streamInitVal(struct type *type)
{
    int size = type ? type->size : 2;
    int count = 0;
    struct type *elem_type;
    struct name *member;       /* also used as np in expr branch */
    /*
     * member_offset walks a struct, and a struct is at most 255
     * bytes - type->size is itself a byte.  is_struct is a truth
     * value.  Byte locals cost one load where an int costs two,
     * and this function tests both in loops.
     */
    unsigned char member_offset;
    unsigned char is_struct;
    cstring str;
    int slen, arrlen, i, b;
    char buf[20];              /* used as strname in STRING, buf in expr */
    struct expr *e;
    long val;

    if (cur.type == BEGIN) {
        /* Nested initializer list */
        elem_type = NULL;
        member = NULL;
        member_offset = 0;
        is_struct = 0;
        if (type && (type->flags & TF_AGGREGATE)) {
            is_struct = 1;
            member = findMemberOff(type->elem, 0);  /* First member at offset 0 */
            elem_type = member ? member->type : NULL;
        } else if (type && (type->flags & TF_ARRAY)) {
            elem_type = type->sub;  /* array - pass element type */
        }
        gettoken();  /* consume { */
        while (cur.type != END) {
            streamInitVal(elem_type);
            count++;
            /* Advance to next struct member by offset */
            if (is_struct && member) {
                member_offset += member->type->size;
                member = findMemberOff(type->elem, member_offset);
                elem_type = member ? member->type : NULL;
            }
            if (cur.type == COMMA)
                gettoken();
            else if (cur.type != END) {
                gripe(ER_S_SN);
                return count;
            }
        }
        gettoken();  /* consume } */
        /*
         * Pad out the rest of the struct.  An initializer that names
         * fewer members than the struct has still occupies all of it,
         * and the next element of an array of these begins at sizeof
         * - not where the initializers happened to stop.
         *
         * Without this, pass1's own basicnames[] - seven entries of
         * "{ name, type, chain }" out of a struct with a dozen fields
         * - sat 20 bytes apart in memory while every subscript was
         * computed against sizeof, which is 37.  basicnames[1] read
         * the middle of basicnames[0]'s name and came back null, so
         * the c0 that ccc built could not name a basic type: "int"
         * and "long" and "unsigned char" as parameters all answered
         * "fn array".  Index 0 worked, having no stride to get wrong.
         */
        if (is_struct && type) {
            while (member_offset < type->size) {
                asmDb(0);
                member_offset++;
            }
        }
    } else if (cur.type == STRING) {
        /* String literal - check if initializing char array inline */
        str = cur.v.str;
        if (type && (type->flags & TF_ARRAY) && type->sub &&
            type->sub->size == 1 && str) {
            /* Emit string bytes inline for char array */
            slen = str[0];
            arrlen = type->count > 0 ? type->count : slen + 1;
            for (i = 0; i < arrlen; i++) {
                b = (i < slen) ? (unsigned char)str[i + 1] : 0;
                asmDb(b);
            }
            /* Skip the strN label emitted in phase 1 - keep counter in sync */
            globalStrCtr++;
            count = slen + 1;  /* Array size for char[] = "..." */
        } else {
            /* Pointer to string - just emit reference (data emitted in phase 1) */
            fmtstr(buf, "str%d", globalStrCtr++);
            asmDwSym(buf);
            count = 1;
        }
        cur.v.str = NULL;
        gettoken();
    } else {
        /*
         * Parse single expression, emit, free.
         *
         * Folded first.  This streams straight to assembly instead of
         * building an AST, so it never reached foldTree, and the test
         * just below asks for a CONST node - which an unfolded "a | b"
         * is not.  Every constant expression in a static initialiser
         * took the unsupported branch and was written as zero, at both
         * widths, while the same expression anywhere else folded fine.
         *
         * cpp's keyword tables are built out of "c | HI" entries, so
         * every skip byte in its trie was nought and only the one
         * directive spelled without them still matched.
         */
        e = foldTree(parseExpr(OP_PRI_COMMA));
        if (e) {
            if (e->op == CONST) {
                val = (long)e->v;  /* e->v IS the value, not a pointer */
                if (size == 1)
                    asmDb((int)val);
                else
                    asmDw((int)val);
            } else if (e->op == SYM) {
                member = (struct name *)e->var;
                if (member->sclass & SC_STATIC)
                    fmtstr(buf, "S%d", member->static_id - 1);
                else
                    fmtstr(buf, "_%s", nameOf(member->id));
                asmDwSym(buf);
            } else if (e->op == PLUS && e->left && e->left->op == SYM &&
                       e->right && (e->right->flags & E_CONST)) {
                /*
                 * The address of an element other than the first.
                 * "&arr[0]" folds to the bare symbol and was taken by
                 * the branch above; "&arr[n]" keeps the offset and is
                 * a PLUS, which matched neither and was written as
                 * zero.  The assembler takes label+offset, so say it.
                 *
                 * pass1's own type table is built this way:
                 *
                 *	{ 2, 0, 0, 0, 0, &basictypes[0] },
                 *	{ 4, 0, 0, 0, 0, &basictypes[1] },
                 *
                 * so every link past the first was null and inttype,
                 * longtype and voidtype were all zero.  The c0 that
                 * ccc built could not name a type it had not been
                 * given by a typedef.
                 */
                member = (struct name *)e->left->var;
                if (member->sclass & SC_STATIC)
                    fmtstr(buf, "S%d+%d", member->static_id - 1,
                        (int)e->right->v);
                else
                    fmtstr(buf, "_%s+%d", nameOf(member->id),
                        (int)e->right->v);
                asmDwSym(buf);
            } else {
                /* Unsupported initializer - emit zero */
                if (size == 1)
                    asmDb(0);
                else
                    asmDw(0);
            }
            FreeExpr(e);
        }
        count = 1;
    }
    return count;
}

/*
 * Parse a variable initializer for static/global variables
 *
 * Streams assembly directly without building trees. Auto initializers
 * are handled by cpp (transformed to assignment statements).
 */
void
doInitlzr(struct name *v)
{
    char fullname[32];
    char strname[16];

    gettoken(); /* consume = token */

    /* Phase 1: skip tokens, emit string data */
    if (phase == 1) {
        if (cur.type == BEGIN) {
            /* Struct/array init - emit STRING data in phase 1 */
            unsigned char depth = 1;
            gettoken();  /* consume initial { before loop */
            while (depth > 0 && cur.type != E_O_F) {
                if (cur.type == BEGIN)
                    depth++;
                else if (cur.type == END)
                    depth--;
                else if (cur.type == STRING) {
                    /* Emit string data now for pointer-to-string fields */
                    cstring str = cur.v.str;
                    if (str) {
                        unsigned char slen = str[0];
                        fmtstr(strname, "str%d", globalStrCtr++);
                        setSeg(SEG_TEXT);
                        asmLabel(strname);
                        asmDbStr((unsigned char *)str + 1, slen);
                    }
                }
                if (depth > 0)
                    gettoken();
            }
            gettoken();  /* consume final } */
        } else if (cur.type == STRING) {
            /*
             * A string initialising a POINTER needs a literal to point
             * at.  One initialising a char array does not: phase 2
             * streams the bytes into the array itself and burns a
             * counter value where this label would have been.  Emitting
             * it anyway put a second copy of every such string in the
             * text segment with nothing referring to it - six bytes for
             * "hello", and rather more for a table of them.
             *
             * The test is streamInitVal's, spelled the same way, because
             * the two have to agree about which strings get a label.
             */
            cstring str = cur.v.str;
            if (str) {
                unsigned char slen = str[0];
                struct type *t = v ? v->type : (struct type *)0;
                int inarray = t && (t->flags & TF_ARRAY) &&
                    t->sub && t->sub->size == 1;

                fmtstr(strname, "str%d", globalStrCtr++);
                if (!inarray) {
                    setSeg(SEG_TEXT);
                    asmLabel(strname);
                    asmDbStr((unsigned char *)str + 1, slen);
                }
            }
            gettoken();
        } else {
            parseExpr(15);  /* skip expression */
        }
        return;
    }

    /* Phase 2: emit assembly directly */
    setSeg(SEG_DATA);

    /* Build label: globals get ::, statics get : */
    if (v->sclass & SC_STATIC)
        fmtstr(fullname, "S%d:", v->static_id - 1);
    else
        fmtstr(fullname, "_%s::", nameOf(v->id));
    asmLine(fullname);

    /* Stream initializer and fix array size if needed */
    {
        int count = streamInitVal(v->type);
        if ((v->type->flags & TF_ARRAY) && v->type->count == -1)
            v->type = getType(TF_ARRAY|TF_POINTER, v->type->sub, count);
    }
}

/*
 * Parse a function definition
 *
 * Processes the function body following a function declarator. This function:
 *   1. Sets up function context for static variable mangling
 *   2. Pushes a new scope for the function body
 *   3. Installs function parameters into level 2 scope
 *   4. Parses the function body statement tree
 *   5. Emits the AST for pass 2 (code generation)
 *   6. Pops the function scope and cleans up
 *
 * Two-phase operation:
 *   Phase 1: Parse body to discover locals, don't build expr trees, don't emit
 *   Phase 2: Parse body, build trees, emit AST
 *
 * Parameter handling:
 *   - Parameters are read from f->type->elem (function type signature)
 *   - New name entries are created at level 2 with funarg kind
 *   - This separates type signature (normalized, name-independent) from
 *     actual parameter symbols (visible in function body)
 *
 * Function-scoped static variables:
 *   - Mangled name format: <file>_<func>_<var>_<counter>
 *   - Allocated in global data section with proper scoping
 *
 * Debug assertions:
 *   - Verifies lexlevel returns to 1 (global) after parsing
 *   - Verifies no local names remain in symbol table
 *
 * Parameters:
 *   f - Function name entry with type signature in f->type
 */
void freeLocals(struct name *local);

void
parsefunc(struct name *f)
{
	struct name *param;
	struct name *phase1_func = NULL;
	struct name *gravemark;

	/* In phase 2, lookup the function definition from phase 1 to get
	 * its u.locals which contains locals for register allocation.
	 * The 'f' from declare() might be a new entry without u.locals. */
	if (phase == 2) {
		struct name *n;
		for (n = names; n; n = n->chain) {
			if (n->kind == kfdef && n->id == f->id) {
				phase1_func = n;
				break;
			}
		}
		if (phase1_func) {
			f = phase1_func;  /* Use the phase 1 version with u.locals */
		}
	}

#ifdef DEBUG
	if (phase == 2)
		fdprintf(2, "func %s: exprs=%d\n", nameOf(f->id), exprCurCnt);
#endif

	// Set current function context for static variable name mangling
	curFunc = f;
	// staticCtr is file-global, not reset per function
	shadowCtr = 0;

	/* Snapshot the graveyard: everything popScope parks above this
	 * point belongs to this function's scopes and dies with it. */
	gravemark = deadNames;

	// Push a new scope for the function body
	pushScope(nameOf(f->id));

	// Install function parameters into the scope at level 2
	// Read parameter info from f->type->elem but create NEW entries at level 2
	if (f->type->flags & TF_FUNC) {
		for (param = f->type->elem; param; param = param->next) {
			// Only add parameters with actual names (skip anonymous ones)
			if (param->id) {
				// Create a NEW name entry at level 2 (don't reuse type->elem)
				struct name *pn;

				pn = newName(param->id, kfunarg, param->type, 0);
				/*
				 * register survives the copy.  The declarator kept it
				 * on the prototype entry; this is the entry the
				 * allocator walks, and dropping it here is where the
				 * keyword used to silently die.
				 */
				if (pn && (param->sclass & SC_REGISTER))
					pn->sclass = SC_REGISTER;
			}
		}
	}

	// Consume the function body's opening brace
	expect(BEGIN, ER_S_CC);

	if (phase == 1) {
		/* Phase 1: Skip statement parsing, but capture locals.
		 * Locals have ref_count populated during parseExpr. */
#ifdef DEBUG
		fdprintf(2, "parsefunc phase1: %s entering statement()\n",
		         nameOf(f->id));
#endif
		statement();  /* Skips through function body */
#ifdef DEBUG
		fdprintf(2, "parsefunc phase1: %s done\n", nameOf(f->id));
#endif
		f->kind = kfdef;
		f->u.locals = capLocals();  /* Capture before popScope */
	} else {
		/* Phase 2: Streaming emit - parse one statement at a time,
		 * emit immediately, free immediately.
		 * f->u.locals contains locals from phase 1 for register alloc. */
		emitFuncPre(f);     /* Emit header, params, locals, block prefix */
		statement();        /* Streams: parses, emits each stmt */
	}

	// Consume the function body's closing brace
	expect(END, ER_S_CC);

	// Pop the function scope
	popScope();

	/* Free this function's scope names now instead of at exit: the
	 * graveyard is LIFO, so entries above gravemark are all ours.
	 * Phase 1 already captured copies in f->u.locals; initializer
	 * exprs were freed when consumed; type->elem param entries are
	 * never on the lookup chain, so none of these are referenced. */
	while (deadNames != gravemark) {
		param = deadNames;
		deadNames = param->chain;
		free(param);
#ifdef DEBUG
		nameCurCnt--;
#endif
	}
	/* After phase 2 the function is fully emitted; the phase 1 local
	 * copies have served register allocation and can go too. */
	if (phase == 2 && f->kind == kfdef && f->u.locals) {
		freeLocals(f->u.locals);
		f->u.locals = NULL;
	}

	/*
	 * Debug assertion: verify we're back at global scope and all
	 * locals are cleaned up
	 */
#ifdef DEBUG
	if (lexlevel != 1) {
		fdprintf(2, "ASSERTION FAILED: lexlevel=%d after parsing "
		         "function %s (expected 1)\n", lexlevel, nameOf(f->id));
		fatal(ER_WTF);
	}
	/* Verify no local names remain in symbol table (phase 2 only) */
	/* In phase 1, names are preserved for phase 2 lookup */
	if (phase == 2) {
		struct name *n;
		for (n = names; n; n = n->chain) {
			if (n->level > 1) {
				fdprintf(2, "ASSERTION FAILED: found local name "
				         "'%s' at level %d after parsing "
				         "function %s\n",
				         nameOf(n->id), n->level, nameOf(f->id));
				fatal(ER_WTF);
			}
		}
	}
#endif

	// Clear current function context
	curFunc = NULL;
}

/*
 * Parse storage class specifiers in a declaration
 *
 * Storage class specifiers control visibility, lifetime, and storage
 * location of variables and functions. This function parses any combination
 * of storage class keywords and returns them as a bitmask.
 *
 * Recognized keywords:
 *   - extern:   External linkage (defined elsewhere)
 *   - static:   Internal linkage or persistent local storage
 *   - auto:     Automatic (stack) storage (default for locals)
 *   - register: Request register allocation (hint only)
 *   - typedef:  Type alias declaration
 *
 * Invalid combinations detected:
 *   - extern with static/auto/register (conflicting linkage)
 *   - register with static (conflicting storage)
 *   - static with auto (conflicting storage)
 *   - typedef with any storage class (typedef is not storage)
 *
 * Note: This function only handles parsing. The code generator determines
 * actual storage location based on context (global vs local) and storage
 * class.
 *
 * Returns:
 *   Bitmask of storage class flags (SC_EXTERN, SC_STATIC, etc.)
 */
/* token -> storage class bit mapping */
unsigned char
sclassBit(token_t t)
{
    static unsigned char sc_bit[] = {
        SC_TYPEDEF, SC_AUTO, SC_EXTERN, SC_STATIC, SC_REGISTER
    };
    if (t < SCLASS_MIN || t > SCLASS_MAX)
        return 0;

    t -= SCLASS_MIN;
    return sc_bit[t];
}

unsigned char
parseSclass()
{
	unsigned char ret = 0;
	unsigned char bit;

	while ((bit = sclassBit(cur.type)) != 0) {
		if (ret & bit)
			gripe(ER_P_SC);
		ret |= bit;
		gettoken();
	}
	/* bogosity checks: conflicting storage classes */
	if ((ret & SC_EXTERN) && (ret & (SC_STATIC|SC_AUTO|SC_REGISTER)))
		gripe(ER_P_SC);
	if ((ret & SC_REGISTER) && (ret & SC_STATIC))
		gripe(ER_P_SC);
	if ((ret & SC_STATIC) && (ret & SC_AUTO))
		gripe(ER_P_SC);
	if ((ret & SC_TYPEDEF) && (ret & (SC_EXTERN|SC_STATIC|SC_AUTO|SC_REGISTER)))
		gripe(ER_P_SC);
	return ret;
}

/*
 * Parse a complete declaration statement
 *
 * Handles variable and function declarations at global or local scope.
 * A declaration consists of a storage class (optional), base type, and
 * one or more declarators separated by commas.
 *
 * Declaration forms:
 *   - Variables:    int x, *p, arr[10];
 *   - Functions:    int foo(int x);
 *   - Typedefs:     typedef int* intptr;
 *   - Initializers: int x = 10, arr[] = {1, 2, 3};
 *
 * Processing:
 *   1. Parse storage class keywords (extern, static, typedef, etc.)
 *   2. Parse base type once (shared across all declarators)
 *   3. For each declarator:
 *      - Parse declarator (name, pointers, arrays, functions)
 *      - Handle initializers (= expr or = { ... })
 *      - Apply storage class
 *      - Emit to AST if global or static
 *      - If function body present ({ ... }), parse function
 *
 * Typedef handling:
 *   - Changes name kind from var to tdef
 *   - Typedefs cannot have initializers or function bodies
 *   - Creates type alias in current scope
 *
 * Function definitions:
 *   - Detected by BEGIN token after function declarator
 *   - Calls parsefunc() to parse body
 *   - Statement tree freed after emission
 *
 * Local variable initializers:
 *   - Added to declInits list for conversion to assignments
 *   - Static locals are initialized in data section, not converted
 *
 * Array size inference:
 *   - char[] = "string" infers size from string length
 *   - int[] = {1, 2, 3} infers size from initializer count
 */
void
declaration()
{
	unsigned char sclass;
	struct type *basetype;
	struct name *v;

#ifdef DEBUG
	if (VERBOSE(V_PHASE1) && phase == 1)
		fdprintf(2, "P1 decl start: cur.type=%d\n", cur.type);
#endif
	/* Parse storage class and base type once at the beginning */
	sclass = parseSclass();
	/* Initialize once, then shared across comma-separated declarators */
	basetype = 0;

	while (1) {
        v = declare(&basetype, 0);
#ifdef DEBUG
	if (VERBOSE(V_PHASE1) && phase == 1 && v)
		fdprintf(2, "P1 decl after declare(%s): cur.type=%d\n",
		         nameOf(v->id), cur.type);
#endif

        /* error recovery: if declare failed, skip to next ; or , */
        if (!v) {
            while (cur.type != SEMI && cur.type != COMMA && cur.type != E_O_F) {
                gettoken();
            }
            if (cur.type == SEMI) {
                gettoken();
                break;
            }
            if (cur.type == COMMA) {
                gettoken();
                continue;
            }
            /* E_O_F */
            break;
        }

        /* handle typedef declarations */
        if (sclass & SC_TYPEDEF) {
            /* change the name kind from var to tdef */
#ifdef DEBUG
            if (VERBOSE(V_SYM)) {
                fdprintf(2,"CONVERTING %s from var to tdef "
                         "(sclass=0x%02x)\n", nameOf(v->id), sclass);
            }
#endif
            v->kind = ktdef;
            slimFnArgs(v->type);
            /* typedefs cannot have initializers or function bodies */
            if (cur.type == ASSIGN) {
                gripe(ER_T_TD);
                /* skip the initializer */
                while (cur.type != SEMI && cur.type != COMMA &&
                       cur.type != E_O_F) {
                    gettoken();
                }
            }
            /* continue to next declarator or end of statement */
            if (cur.type == COMMA) {
                gettoken();
                continue;
            }
            if (cur.type == SEMI) {
                gettoken();
                break;
            }
            /* unexpected token */
            break;
        }

        if (v->type->flags & TF_FUNC) {
#ifdef DEBUG
            if ((VERBOSE(V_PHASE1) && phase == 1) || (VERBOSE(V_PHASE2) && phase == 2))
                fdprintf(2, "P%d func %s: cur.type=%d (BEGIN=%d)\n",
                         phase, nameOf(v->id), cur.type, BEGIN);
#endif
            if (cur.type == BEGIN) {
                /* Assign storage class BEFORE parsing function body
                 * so it's available when emitting the AST */
                if (sclass & SC_STATIC) {
                    v->sclass = SC_STATIC;
                    /* Static functions get static_id for S<id> naming */
                    if (!v->static_id)
                        v->static_id = ++staticCtr;
                } else if (sclass & SC_EXTERN) {
                    v->sclass = SC_EXTERN;
                }

                parsefunc(v);

                v->next = global;
                global = v;
                break;
            }
        }

        /*
         * Not a function definition: any function type in this
         * declarator keeps only parameter types, not names.
         * Never slim a type owned by a function definition, though:
         * in phase 2 a prototype resolves to the phase 1 fdef entry,
         * and its type still carries the parameter names the body
         * needs (parsefunc/emitPrmDecls/regalloc bind by name).
         */
        if (v->kind != kfdef)
            slimFnArgs(v->type);

        /*
         * Assign storage class for variables (non-functions or
         * function prototypes)
         */
        if (sclass & SC_STATIC) {
            v->sclass = SC_STATIC;
            /* All statics get static_id for S<id> naming */
            if (!v->static_id)
                v->static_id = ++staticCtr;
        } else if (sclass & SC_EXTERN) {
            v->sclass = SC_EXTERN;
        } else if (sclass & SC_REGISTER) {
            v->sclass = SC_REGISTER;
        } else {
            /* Clear extern flag if this is a definition (not extern decl) */
            v->sclass &= ~SC_EXTERN;
        }

        if (cur.type == ASSIGN) {
            /* Auto initializers only for scalars - cpp splits them into decl + assign.
             * Aggregate auto init (arrays, structs) is not supported. */
            if (lexlevel > 1 && !(sclass & SC_STATIC) &&
                (v->type->flags & (TF_AGGREGATE | TF_ARRAY))) {
                gripe(ER_D_AI);
                /* skip the initializer */
                while (cur.type != SEMI && cur.type != COMMA && cur.type != E_O_F)
                    gettoken();
                goto next_decl;
            }
            doInitlzr(v);
            v->emitted = 1;
            goto next_decl;
        }

		/* Emit global variables and static locals immediately */
		/*
		 * Emit if: (global scope OR static storage) AND not
		 * typedef AND not function def AND not extern declaration
		 * Extern declarations don't define storage - they just
		 * reference symbols defined elsewhere.
		 * Skip if already emitted via streaming (v->emitted).
		 */
		if ((lexlevel == 1 || (sclass & SC_STATIC)) &&
		    v->kind != ktdef && v->kind != kfdef &&
		    !(sclass & SC_EXTERN) && !v->emitted) {
			/* Skip function declarations - only emit actual variables */
			if (!(v->type->flags & TF_FUNC)) {
				emitGv(v);
				/* Free initializer after emission to avoid memory buildup */
				if (v->u.init) {
					FreeExpr(v->u.init);
					v->u.init = NULL;
				}
			}
		}

next_decl:
		if (cur.type == COMMA) {
			gettoken();
			continue;
		}
		if (cur.type == SEMI) {
			gettoken();
			break;
		}
		/* Error recovery: unexpected token, break out of loop */
		break;
	}
}

/*
 * Parse a C source file at global scope
 *
 * This is the top-level entry point for parsing a translation unit.
 * It processes all global declarations (variables, functions, typedefs)
 * until EOF is reached.
 *
 * Initialization:
 *   1. Push global scope (level 1)
 *   2. Initialize basic types (char, int, long, void, etc.)
 *   3. Process declarations until EOF
 *
 * Declaration recognition:
 *   - Type keywords: int, char, struct, etc.
 *   - Storage class: extern, static, typedef, etc.
 *   - Typedef names: Previously declared type aliases
 *
 * Incremental emission:
 *   - Global variables are emitted to AST as they're parsed
 *   - Functions are emitted after their body is parsed
 *   - This avoids buffering entire program in memory
 *
 * Cleanup:
 *   - Pops global scope on completion
 *   - Debug builds verify all names properly cleaned up
 *   - Only basic types (level 0) should remain after parsing
 *
 * Error recovery:
 *   - Unrecognized tokens are skipped to prevent infinite loops
 *   - TOK_NONE tokens (from lexer errors) are consumed and ignored
 */
void
parse()
{
	struct name *poss_typedef;

	pushScope("global");
	while (cur.type != E_O_F) {
		while (cur.type == TOK_NONE) {
			gettoken();
		}
#ifdef DEBUG
		if (VERBOSE(V_PHASE1) && phase == 1 && cur.type == BEGIN)
			fdprintf(2, "P1 parse() found BEGIN at global\n");
#endif
		/* Check if current token looks like start of a declaration */
		/* Also check if it's a typedef name (SYM that's a typedef) */
		poss_typedef = NULL;
		if (cur.type == SYM) {
			poss_typedef = findName(cur.v.id, 0);
		}

		if (isTypeToken(cur.type) ||
			cur.type == STATIC || cur.type == REGISTER ||
			cur.type == AUTO || cur.type == EXTERN ||
			cur.type == TYPEDEF ||
			(poss_typedef && poss_typedef->kind == ktdef)) {
			declaration();
		} else if (cur.type == ASM) {
			/* Global asm block - get text and emit directly */
			char *text = getAsmText();
			emitGlobalAsm(text);
			free(text);
		} else {
			/* Not a declaration - skip this token to avoid getting stuck */
#ifdef DEBUG
			if (VERBOSE(V_PHASE1) && phase == 1)
				fdprintf(2, "P1 parse() skip: cur.type=%d\n", cur.type);
#endif
			gettoken();
		}
	}

	popScope();

	/*
	 * Debug assertion: verify all allocations have been freed
	 * after parsing file
	 */
#ifdef DEBUG
	if (lexlevel != 0) {
		fdprintf(2, "ASSERTION FAILED: lexlevel=%d after parsing "
		         "file (expected 0)\n", lexlevel);
		fatal(ER_WTF);
	}
	/* Verify only basic types remain in symbol table (level 0) */
	/* Skip in phase 1: names are preserved for phase 2 lookup */
	if (phase == 2) {
		struct name *n;
		int nonBasicCnt = 0;
		for (n = names; n; n = n->chain) {
			if (n->level > 0) {
				fdprintf(2, "WARNING: name '%s' at level %d "
				         "still in symbol table after "
				         "file parse\n",
				         nameOf(n->id), n->level);
				nonBasicCnt++;
			}
		}
		if (nonBasicCnt > 0) {
			fdprintf(2, "ASSERTION FAILED: found %d non-basic "
			         "names after parsing file\n", nonBasicCnt);
			fatal(ER_WTF);
		}
	}
#endif
}

/*
 * Free a function's locals list
 */
void
freeLocals(struct name *local)
{
	struct name *next;
	while (local) {
		next = local->next;
		free(local);
		local = next;
	}
}

/*
 * Clean up allocated memory after parsing, preserving basic types
 * Traverse chain and free non-basic (level > 0) names
 */
void
cleanupParse()
{
	struct name *n;

	/* Free non-basic names by traversing until we hit level 0 */
	while (names && names->level > 0) {
		n = names;
		names = n->chain;
		/* u.locals (for fdef) and u.init (for var) share a union */
		if (n->kind == kfdef) {
			if (n->u.locals)
				freeLocals(n->u.locals);
		} else {
			if (n->u.init)
				FreeExpr(n->u.init);
		}
		if (n->kind != kfunarg) {
			free(n);
#ifdef DEBUG
			nameCurCnt--;
#endif
		}
	}
	/* names now points to first basic type (level 0) */

	/* Free the graveyard: names unlinked by popScope over the whole
	 * run.  Nothing references them at exit.  fdef entries own their
	 * u.locals copies; initializer exprs were freed when emitted. */
	while (deadNames) {
		n = deadNames;
		deadNames = n->chain;
		if (n->kind == kfdef && n->u.locals)
			freeLocals(n->u.locals);
		free(n);
#ifdef DEBUG
		nameCurCnt--;
#endif
	}

	/* Types live in the permalloc arena and are never freed. */
}

/*
 * vim: tabstop=4 shiftwidth=4 expandtab:
 */
