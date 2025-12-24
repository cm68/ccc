/*
 * File - tree2.c Created 09.03.2019 Last Modified 17.06.2020
 */
#include "cgen.h"
/*********************************************************
 * parseExpr - Parse an expression from input stream
 *********************************************************/
node_t *parseExpr() {
    node_t *leftNode;
    char *token;
    int opCount;
    int tokCode;
    register node_t *node;

    for (;;) {
        token = getToken();
        if (isdigit(*token) || (*token == '-' && isdigit(token[1]))) {
            if (*token == '-') {
                tokCode = 1;
                token++;
            } else
                tokCode = 0;

            node = mkConstNode(atol(token));
            if (tokCode != 0)
                node->info.l = -node->info.l;
            return node;
        }

        if (*token == '_' || isalpha(*token)) {
            (node = allocNode())->op = IDOP;
            node->info.mp[0]         = lookupSymbol(token);
            node->pm                 = node->info.mp[0]->type;
            node->tFlags             = node->info.mp[0]->refl;
            if (node->info.mp[0]->tflag == 3 && (node->info.mp[0]->sflags & B_SLOC_GLOBAL) == 0) {
                prGlobalDef(node->info.mp[0]); /* Emit "global name" */
            }
            return node;
        }
        if (*token == '`') {
            node     = allocNode();
            node->pm = parseTypeSpec(token, &node->tFlags);
            node->op = TYPE;
            return node;
        }
        if (*token == '.' && token[1] && token[1] != '.') {
            node            = allocNode();
            node->pm        = typeDouble;                      /* "d" - double */
            node->info.sv.s = (char *)allocMem(strlen(token)); /* create string */
            strcpy(node->info.sv.s, token + 1);
            node->info.sv.v = newLocal();
            node->op        = FCONST;
            return node;
        }
        tokCode = lookupToken(token);
        opCount = dopetab[tokCode] & DOPE_OPCOUNT;
        switch (opCount) {
        case 0xc: /* statement class */
            switch (tokCode) {
            case '9':
                parseVariable();
                continue;
            case '7':
            case '8':
                parseMembers(tokCode);
                continue;
            case '4':
                parseEnum();
                continue;
            default:
                fatalErr("Expression error");
                break;
            }
            /* fall through */
        case 0: /* leaf */
            return mkNode(tokCode, 0, 0);
        case DOPE_UNARY:
            return mkNode(tokCode, parseExpr(), 0);
        case DOPE_BINARY:
            leftNode = parseExpr();
            return mkNode(tokCode, leftNode, parseExpr());
        }
    }
}

/* end of file tree2.c */
