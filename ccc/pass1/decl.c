/*
 * declaration parsing and top-level parse driver
 */

#include "cc1.h"

/* List of global declarations built during parsing */
static struct name *global = 0;

/*
 * Parse an initializer list for arrays and structs
 *
 * Handles the { expr, expr, ... } syntax for aggregate initializers.
 * Supports nested initializer lists for multi-dimensional arrays and
 * nested structs. Commas separate elements, and the list is terminated
 * by a closing brace.
 *
 * The parser stops at comma operator priority (15) to treat commas as
 * element separators rather than the comma operator.
 *
 * Examples:
 *   int arr[3] = { 1, 2, 3 };
 *   int matrix[2][2] = { {1, 2}, {3, 4} };
 *   struct { int x, y; } p = { 10, 20 };
 *
 * Returns:
 *   Linked list of initializer expressions (via next pointers), or
 *   NULL on error
 */
static struct expr *
parseInitList(void)
{
    struct expr *head = NULL;
    struct expr *tail = NULL;
    struct expr *item;

    gettoken(); /* consume { */

    /* Phase 1: just skip tokens, don't build tree */
    if (phase == 1) {
        int depth = 1;
        while (depth > 0 && cur.type != E_O_F) {
            if (cur.type == BEGIN)
                depth++;
            else if (cur.type == END)
                depth--;
            if (depth > 0)
                gettoken();
        }
        gettoken();  /* consume final } */
        return NULL;
    }

    while (cur.type != END) {
        if (cur.type == BEGIN) {
            /* Nested initializer for struct/array member - wrap in INITLIST
             * node so the nested list's ->next chain isn't overwritten */
            struct expr *nested = parseInitList();
            item = mkexpr(INITLIST, nested);
        } else {
            /* Parse expression but stop before comma operator (priority 15)
             * so that comma is treated as element separator, not an operator */
            item = parseExpr(OP_PRI_COMMA, NULL);
        }

        if (item == NULL) {
            gripe(ER_E_IT); /* Invalid initializer term */
            return NULL;
        }

        /* Link initializer expressions together */
        if (head == NULL) {
            head = item;
            tail = item;
        } else {
            tail->next = item;
            tail = item;
        }

        if (cur.type == END) {
            break;
        } else if (cur.type != COMMA) {
            gripe(ER_S_SN); /* need semicolon/separator */
            return NULL;
        }
        gettoken(); /* consume , */
    }

    gettoken(); /* consume } */
    return head;
}

/*
 * Parse a variable initializer
 *
 * Handles both simple expression initializers and aggregate initializer
 * lists for variable declarations. Called when an = token is encountered
 * after a declarator.
 *
 * Initializer forms:
 *   - Simple expression: int x = 10;
 *   - Initializer list: int arr[] = { 1, 2, 3 };
 *   - Nested lists: int matrix[][2] = { {1, 2}, {3, 4} };
 *   - String literals: char str[] = "hello";
 *
 * The parser uses precedence 15 for simple expressions to allow
 * assignment operators (priority 14) but exclude the comma operator
 * (priority 15), ensuring commas in function-like macros work correctly.
 *
 * Returns:
 *   Expression tree representing the initializer, or NULL on error
 */
struct expr *
doInitlzr(void)
{
    struct expr *init;

    gettoken(); /* consume = token */

    if (cur.type == BEGIN) {
        /* Handle {...} style initializer list - wrap in INITLIST */
        init = parseInitList();
        if (phase == 1)
            return NULL;  /* phase 1: no tree building */
        init = mkexpr(INITLIST, init);
    } else {
        /*
         * Handle simple expression initializer
         * Use precedence 15 to allow assignment (14) but exclude
         * comma operator (15)
         */
        init = parseExpr(15, NULL);
    }

    /* Phase 1: expressions return NULL, that's OK */
    if (phase == 1)
        return NULL;

    if (init == NULL) {
        gripe(ER_E_IT); /* Invalid initializer term */
        return NULL;
    }

    return init;
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
void
parsefunc(struct name *f)
{
	struct name *param;
	struct name *phase1_func = NULL;

	/* In phase 2, lookup the function definition from phase 1 to get
	 * its u.body which contains locals for register allocation.
	 * The 'f' from declare() might be a new entry without u.body. */
	if (phase == 2) {
		struct name *n;
		for (n = names; n; n = n->chain) {
			if (n->kind == fdef && strcmp(n->name, f->name) == 0 &&
			    n->u.body) {
				phase1_func = n;
				break;
			}
		}
		if (phase1_func) {
			f = phase1_func;  /* Use the phase 1 version with u.body */
		}
	}

#ifdef DEBUG
	if (phase == 2)
		fdprintf(2, "func %s: exprs=%d\n", f->name, exprCurCnt);
#endif

	// Set current function context for static variable name mangling
	curFunc = f;
	// staticCtr is file-global, not reset per function
	shadowCtr = 0;

	// Push a new scope for the function body
	pushScope(f->name);

	// Install function parameters into the scope at level 2
	// Read parameter info from f->type->elem but create NEW entries at level 2
	if (f->type && (f->type->flags & TF_FUNC)) {
		for (param = f->type->elem; param; param = param->next) {
			// Only add parameters with actual names (skip anonymous ones)
			if (param->name[0] != '\0') {
				// Create a NEW name entry at level 2 (don't reuse type->elem)
				newName(param->name, funarg, param->type, 0);
			}
		}
	}

	// Consume the function body's opening brace
	expect(BEGIN, ER_S_CC);

	if (phase == 1) {
		/* Phase 1: Skip statement parsing (returns NULL), but capture locals.
		 * Locals have ref_count populated during parseExpr. */
		statement(0);  /* Skips through function body */
		f->u.body = makestmt(BEGIN, 0);
		f->u.body->flags |= S_FUNC;
		f->kind = fdef;
		f->u.body->locals = capLocals();  /* Capture before popScope */
	} else {
		/* Phase 2: Streaming emit - parse one statement at a time,
		 * emit immediately, free immediately. No full tree built.
		 * f->u.body contains locals from phase 1 for register alloc. */
		emitFuncPre(f);     /* Emit header, params, locals, block prefix */
		statement(0);       /* Streams: parses, emits, frees each stmt */
		emitFuncPost();     /* Emit trailing newline */
	}

	// Consume the function body's closing brace
	expect(END, ER_S_CC);

	// Pop the function scope
	popScope();

	/*
	 * Debug assertion: verify we're back at global scope and all
	 * locals are cleaned up
	 */
#ifdef DEBUG
	if (lexlevel != 1) {
		fdprintf(2, "ASSERTION FAILED: lexlevel=%d after parsing "
		         "function %s (expected 1)\n", lexlevel, f->name);
		fatal(0);
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
				         n->name, n->level, f->name);
				fatal(0);
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
 *   - const:    Type qualifier (ignored by this compiler)
 *   - volatile: Type qualifier (ignored by this compiler)
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
static unsigned char
sclassBit(token_t t)
{
	switch (t) {
	case EXTERN:   return SC_EXTERN;
	case REGISTER: return SC_REGISTER;
	case STATIC:   return SC_STATIC;
	case CONST:    return SC_CONST;
	case VOLATILE: return SC_VOLATILE;
	case AUTO:     return SC_AUTO;
	case TYPEDEF:  return SC_TYPEDEF;
	default:       return 0;
	}
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

	/* Parse storage class and base type once at the beginning */
	sclass = parseSclass();
	/* Initialize once, then shared across comma-separated declarators */
	basetype = 0;

	while (1) {
        v = declare(&basetype, 0);

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
                         "(sclass=0x%02x)\n", v->name, sclass);
            }
#endif
            v->kind = tdef;
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

        if (v->type) {
        }
        if (v->type && (v->type->flags & TF_FUNC)) {
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

                /* Free the statement tree stub after phase 2 (phase 1 stub
                 * with locals is needed for phase 2 register allocation) */
                if (phase == 2 && v->u.body) {
                    frStmt(v->u.body);
                    v->u.body = 0;  /* Mark as freed */
                }
#ifdef DEBUG
                fdprintf(2, "  after free: exprs=%d\n", exprCurCnt);
#endif

                v->next = global;
                global = v;
                break;
            }
        }

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
            cstring str;
            int len;

            v->u.init = doInitlzr();

            /*
             * Check initializer compatibility
             * Arrays of non-char must use brace-enclosed list
             * Exception: char[] = "string" is valid
             */
            if (v->type && (v->type->flags & TF_ARRAY) && v->u.init) {
                struct type *elem = v->type->sub;
                int isCharArr = elem && !(elem->flags & TF_POINTER) &&
                    (elem == chartype || elem == uchartype);
                /* String init only valid for char arrays */
                if (v->u.init->op == STRING && !isCharArr) {
                    gripe(ER_D_IN);  /* bad init */
                    frExp(v->u.init);
                    v->u.init = NULL;
                }
            }

            /*
             * Fix array size for char[] = "string" syntax
             * (single string, not a list)
             */
            if (v->type && (v->type->flags & TF_ARRAY) &&
                v->type->count == -1 &&
                v->u.init && v->u.init->op == STRING && !v->u.init->next &&
                v->u.init->v) {
                /* Get string length from counted string */
                str = (cstring)v->u.init->v;
                if (str) {
                    /* First byte is length */
                    len = (unsigned char)str[0];
                    /*
                     * Create new array type with correct size
                     * (length + 1 for null terminator)
                     */
                    v->type = getType(TF_ARRAY|TF_POINTER, v->type->sub,
                                       len + 1);
                    /*
                     * Rename string literal to use array name directly
                     * instead of synthetic str0, str1, etc.
                     */
                    if (v->u.init->var) {
                        struct name *strname = (struct name *)v->u.init->var;
                        if (v->static_id) {
                            snprintf(strname->name, 16, "S%d", v->static_id - 1);
                        } else {
                            strname->name[0] = '_';
                            strncpy(strname->name + 1, v->name, 14);
                        }
                        strname->name[15] = 0;
                    }
                }
            }

            /* Fix array size for array[] = { ... } syntax */
            if (v->type && (v->type->flags & TF_ARRAY) &&
                v->type->count == -1 &&
                v->u.init && v->u.init->next) {
                /* Count elements in initializer list */
                struct expr *item;
                int count = 0;
                for (item = v->u.init; item; item = item->next) {
                    count++;
                }
                /* Create new array type with correct size */
                v->type = getType(TF_ARRAY|TF_POINTER, v->type->sub,
                                   count);
            }

            /*
             * Track local variable initializers for conversion
             * to assignments
             */
            if (lexlevel > 1 && v->u.init && !(sclass & SC_STATIC)) {
                addDeclInit(v);
            }
        }

		/* Emit global variables and static locals immediately */
		/*
		 * Emit if: (global scope OR static storage) AND not
		 * typedef AND not function def AND not extern declaration
		 * Extern declarations don't define storage - they just
		 * reference symbols defined elsewhere.
		 */
		if ((lexlevel == 1 || (sclass & SC_STATIC)) &&
		    v->kind != tdef && v->kind != fdef &&
		    !(sclass & SC_EXTERN)) {
			/* Skip function declarations - only emit actual variables */
			if (!(v->type && (v->type->flags & TF_FUNC))) {
				emitGv(v);
				/* Free initializer after emission to avoid memory buildup */
				if (v->u.init) {
					frExp(v->u.init);
					v->u.init = NULL;
				}
			}
		}

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
 *   - NONE tokens (from lexer errors) are consumed and ignored
 */
void
parse()
{
	struct name *poss_typedef;

	pushScope("global");
	while (cur.type != E_O_F) {
		while (cur.type == NONE) {
			gettoken();
		}
		/* Check if current token looks like start of a declaration */
		/* Also check if it's a typedef name (SYM that's a typedef) */
		poss_typedef = NULL;
		if (cur.type == SYM) {
			poss_typedef = findName(cur.v.name, 0);
		}

		if (isTypeToken(cur.type) ||
			cur.type == STATIC || cur.type == REGISTER ||
			cur.type == AUTO || cur.type == EXTERN ||
			(poss_typedef && poss_typedef->kind == tdef) ||
			(cur.type == SYM && next.type == LPAR)) {  /* K&R function */
			declaration();
		} else if (cur.type == ASM) {
			/* Global asm block */
			struct stmt *st;
			st = asmblock();  /* asmblock() handles token advancement */
			if (st) {
				emitGlobalAsm(st);
				frStmt(st);
			}
		} else {
			/* Not a declaration - skip this token to avoid getting stuck */
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
		fatal(0);
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
				         n->name, n->level);
				nonBasicCnt++;
			}
		}
		if (nonBasicCnt > 0) {
			fdprintf(2, "ASSERTION FAILED: found %d non-basic "
			         "names after parsing file\n", nonBasicCnt);
			fatal(0);
		}
	}
#endif
}

/*
 * Free a statement tree recursively
 *
 * Deallocates a statement tree and all its child nodes, including:
 *   - chain: Child/body statement (e.g., body of if/while/for/block)
 *   - otherwise: Else branch (for if statements)
 *   - next: Sibling statement in sequence
 *   - label: Synthetic labels or captured asm text
 *   - locals: Captured local variable list (for blocks)
 *
 * Note: Expression trees (left, right, middle) are NOT freed here as they
 * may be shared or owned elsewhere. A full implementation would need
 * reference counting for expressions.
 *
 * Parameters:
 *   st - Statement tree root to free (NULL-safe)
 */
void
frStmt(struct stmt *st)
{
	if (!st)
		return;

	/* Free child statements */
	if (st->chain)
		frStmt(st->chain);
	if (st->otherwise)
		frStmt(st->otherwise);
	if (st->next)
		frStmt(st->next);

	/* Free label string if present */
	if (st->label)
		free(st->label);

	/* Free locals list */
	if (st->locals) {
		struct name *local = st->locals;
		struct name *next;
		while (local) {
			next = local->next;
			free(local);
			local = next;
		}
	}

	/* Free expression trees */
	if (st->left)
		frExp(st->left);
	if (st->right)
		frExp(st->right);
	if (st->middle)
		frExp(st->middle);

	free(st);
}

/*
 * Clean up allocated memory after parsing, preserving basic types
 * Traverse chain and free non-basic (level > 0) names
 */
void
cleanupParse(void)
{
	struct name *n;
	struct type *t, *tnext;

	/* Free switch headers and case entries */
	resetSwitches();

	/* Free non-basic names by traversing until we hit level 0 */
	while (names && names->level > 0) {
		n = names;
		names = n->chain;
		/* u.body (for fdef) and u.init (for var) share a union */
		if (n->kind == fdef) {
			if (n->u.body)
				frStmt(n->u.body);
		} else {
			if (n->u.init)
				frExp(n->u.init);
		}
		if (n->kind != funarg) {
			free(n);
#ifdef DEBUG
			nameCurCnt--;
#endif
		}
	}
	/* names now points to first basic type (level 0) */

	/* Free non-basic types only */
	t = types;
	while (t && !isBasicType(t)) {
		tnext = t->next;
		free(t);
		t = tnext;
	}
	types = t;
}

/*
 * vim: tabstop=4 shiftwidth=4 expandtab:
 */
