#include <stdio.h>
#include <stdlib.h>

#include "cst.h"

#define NEW(type) (type *)safe_malloc(sizeof(type))

static void *safe_malloc(size_t size) {
    void *ptr = malloc(size);
    if (ptr == NULL) {
        exit(1);
    }
    return ptr;
}

CstDeclarationList *newCstDeclarationList(
    CstDeclarationType type,
    CstDeclarationValue declaration,
    CstDeclarationList *next
) {
    CstDeclarationList *x = NEW(CstDeclarationList);
    x->type = type;
    x->declaration = declaration;
    x->next = next;
    return x;
}

CstClassDeclaration *newCstClassDeclaration(
    Token name,
    bool hasSuperclass,
    Token superName,
    CstMethodList *methods
) {
    CstClassDeclaration *x = NEW(CstClassDeclaration);
    x->name = name;
    x->hasSuperclass = hasSuperclass;
    x->superName = superName;
    x->methods = methods;
    return x;
}

CstMethodList *newCstMethodList(
    Token name,
    CstFunction *function,
    CstMethodList *next
) {
    CstMethodList *x = NEW(CstMethodList);
    x->name = name;
    x->function = function;
    x->next = next;
    return x;
}

CstFunction *newCstFunction(
    CstArgumentList *arguments,
    CstDeclarationList *declarations
) {
    CstFunction *x = NEW(CstFunction);
    x->arguments = arguments;
    x->declarations = declarations;
    return x;
}

CstArgumentList *newCstArgumentList(Token name, CstArgumentList *next) {
    CstArgumentList *x = NEW(CstArgumentList);
    x->name = name;
    x->next = next;
    return x;
}

CstFunDeclaration *newCstFunDeclaration(Token name, CstFunction *function) {
    CstFunDeclaration *x = NEW(CstFunDeclaration);
    x->name = name;
    x->function = function;
    return x;
}

CstVarDeclaration *newCstVarDeclaration(
    Token name,
    CstExpression *initializer
) {
    CstVarDeclaration *x = NEW(CstVarDeclaration);
    x->name = name;
    x->initializer = initializer;
    return x;
}

CstStatement *newCstStatement(
    CstStatementType type,
    CstStatementValue statement
) {
    CstStatement *x = NEW(CstStatement);
    x->type = type;
    x->statement = statement;
    return x;
}

CstSwitchStatement *newCstSwitchStatement(
    CstExpression *expression,
    CstCaseList *cases
) {
    CstSwitchStatement *x = NEW(CstSwitchStatement);
    x->expression = expression;
    x->cases = cases;
    return x;
}

CstCaseList *newCstCaseList(
    bool isDefault,
    CstExpression *expression,
    CstDeclarationList *declarations,
    CstCaseList *next
) {
    CstCaseList *x = NEW(CstCaseList);
    x->isDefault = isDefault;
    x->expression = expression;
    x->declarations = declarations;
    x->next = next;
    return x;
}

CstForStatement *newCstForStatement(
    bool isDeclaration,
    CstForInitializer init,
    CstExpression *test,
    CstExpression *update,
    CstStatement *body
) {
    CstForStatement *x = NEW(CstForStatement);
    x->isDeclaration = isDeclaration;
    x->init = init;
    x->test = test;
    x->update = update;
    x->body = body;
    return x;
}

CstIfStatement *newCstIfStatement(
    CstExpression *expression,
    CstStatement *ifTrue,
    CstStatement *ifFalse
) {
    CstIfStatement *x = NEW(CstIfStatement);
    x->expression = expression;
    x->ifTrue = ifTrue;
    x->ifFalse = ifFalse;
    return x;
}

CstConditionalStatement *newCstConditionalStatement(
    CstExpression *expression,
    CstStatement *statement
) {
    CstConditionalStatement *x = NEW(CstConditionalStatement);
    x->expression = expression;
    x->statement = statement;
    return x;
}

CstExpression *newCstExpression(
    CstExpressionType type,
    CstExpressionValue expression
) {
    CstExpression *x = NEW(CstExpression);
    x->type = type;
    x->expression = expression;
    return x;
}

CstBinaryExpression *newCstBinaryExpression(
    CstExpression *left,
    CstExpression *right
) {
    CstBinaryExpression *x = NEW(CstBinaryExpression);
    x->left = left;
    x->right = right;
    return x;
}

CstCallExpression *newCstCallExpression(
    CstExpression *function,
    CstExpressionList *arguments
) {
    CstCallExpression *x = NEW(CstCallExpression);
    x->function = function;
    x->arguments = arguments;
    return x;
}

CstExpressionList *newCstExpressionList(
    CstExpression *expression,
    CstExpressionList *next
) {
    CstExpressionList *x = NEW(CstExpressionList);
    x->expression = expression;
    x->next = next;
    return x;
}

CstStringExpression *newCstStringExpression(Token value) {
    CstStringExpression *x = NEW(CstStringExpression);
    x->value = value;
    return x;
}

#ifdef DEBUG_PRINT_TREE

static void indent(int depth, const char *string) {
    printf("\n");
    while (depth-- > 0) {
        printf("  ");
    }
    printf("%s", string);
}

static void printToken(Token t) {
    printf("%.*s", t.length, t.start);
}


void printCstDeclarationList(CstDeclarationList *cst, int depth) {
    if (cst == NULL) return;

    switch (cst->type) {
        case CST_CLASS_DECLARATION:
            printCstClassDeclaration(cst->declaration.classDeclaration, depth + 1);
            break;
        case CST_FUN_DECLARATION:
            printCstFunDeclaration(cst->declaration.funDeclaration, depth + 1);
            break;
        case CST_VAR_DECLARATION:
            indent(depth + 1, "");
            printCstVarDeclaration(cst->declaration.varDeclaration, depth + 1);
            printf(";");
            break;
        case CST_STATEMENT_DECLARATION:
            printCstStatement(cst->declaration.statement, depth + 1);
            break;
    }
    printCstDeclarationList(cst->next, depth);
}

void printCstClassDeclaration(CstClassDeclaration *cst, int depth) {
    if (cst == NULL) return;

    indent(depth, "class ");
    printToken(cst->name);
    if (cst->hasSuperclass) {
        printf(" < ");
        printToken(cst->superName);
    }
    printf(" {");
    printCstMethodList(cst->methods, depth + 1);
    indent(depth, "}");
}

void printCstMethodList(CstMethodList *cst, int depth) {
    if (cst == NULL) return;

    indent(depth, "");
    printToken(cst->name);
    printCstFunction(cst->function, depth);
    printCstMethodList(cst->next, depth);
}

void printCstFunction(CstFunction *cst, int depth) {
    if (cst == NULL) return;

    printf("(");
    printCstArgumentList(cst->arguments, depth);
    printf(") {");
    printCstDeclarationList(cst->declarations, depth + 1);
    indent(depth, "}");
}

void printCstArgumentList(CstArgumentList *cst, int depth) {
    if (cst == NULL) return;

    printToken(cst->name);
    if (cst->next != NULL) {
        printf(", ");
        printCstArgumentList(cst->next, depth);
    }
}

void printCstFunDeclaration(CstFunDeclaration *cst, int depth) {
    if (cst == NULL) return;
    indent(depth, "fun ");
    printToken(cst->name);
    printCstFunction(cst->function, depth);
}

void printCstVarDeclaration(CstVarDeclaration *cst, int depth) {
    if (cst == NULL) return;
    printf("var ");
    printToken(cst->name);
    printf(" = ");
    if (cst->initializer) {
        printCstExpression(cst->initializer, depth);
    } else {
        printf("nil");
    }
}

void printCstWhileStatement(CstConditionalStatement *cst, int depth) {
    if (cst == NULL) return;

    indent(depth, "while ");
    printCstExpression(cst->expression, depth);
    printf(")");
    printCstStatement(cst->statement, depth + 1);
}

void printCstDoStatement(CstConditionalStatement *cst, int depth) {
    if (cst == NULL) return;

    indent(depth, "do");
    printCstStatement(cst->statement, depth + 1);
    indent(depth, "while (");
    printCstExpression(cst->expression, depth);
    printf(");");
}

void printCstStatement(CstStatement *cst, int depth) {
    if (cst == NULL) return;

    switch (cst->type) {
        case CST_PRINT_STATEMENT:
            indent(depth, "print ");
            printCstExpression(cst->statement.expression, depth);
            printf(";");
            break;
        case CST_SWITCH_STATEMENT:
            printCstSwitchStatement(cst->statement.switchStatement, depth);
            break;
        case CST_FOR_STATEMENT:
            printCstForStatement(cst->statement.forStatement, depth);
            break;
        case CST_IF_STATEMENT:
            printCstIfStatement(cst->statement.ifStatement, depth);
            break;
        case CST_RETURN_STATEMENT:
            indent(depth, "return");
            if (cst->statement.expression != NULL) {
                printf(" ");
                printCstExpression(cst->statement.expression, depth);
            }
            printf(";");
            break;
        case CST_WHILE_STATEMENT:
            printCstWhileStatement(cst->statement.conditionalStatement, depth);
            break;
        case CST_DO_STATEMENT:
            printCstDoStatement(cst->statement.conditionalStatement, depth);
            break;
        case CST_BLOCK_STATEMENT:
            indent(depth, "{");
            printCstDeclarationList(cst->statement.blockStatement, depth + 1);
            indent(depth, "}");
            break;
        case CST_EXPRESSION_STATEMENT:
            indent(depth, "");
            printCstExpression(cst->statement.expression, depth);
            printf(";");
            break;
    }
}

void printCstSwitchStatement(CstSwitchStatement *cst, int depth) {
    if (cst == NULL) return;

    indent(depth, "switch (");
    printCstExpression(cst->expression, depth);
    printf(") {");
    printCstCaseList(cst->cases, depth + 1);
    indent(depth, "}");
}

void printCstCaseList(CstCaseList *cst, int depth) {
    if (cst == NULL) return;

    if (cst->isDefault) {
        indent(depth, "default:");
    } else {
        indent(depth, "case ");
        printCstExpression(cst->expression, depth);
        printf(":");
    }
    if (cst->declarations != NULL) {
        printCstDeclarationList(cst->declarations, depth + 1);
    }
    printCstCaseList(cst->next, depth);
}

void printCstForStatement(CstForStatement *cst, int depth) {
    if (cst == NULL) return;

    indent(depth, "for (");
    if (cst->init.expression != NULL) {
        if (cst->isDeclaration) {
            printCstVarDeclaration(cst->init.declaration, depth);
        } else {
            printCstExpression(cst->init.expression, depth);
        }
    }
    printf("; ");
    printCstExpression(cst->test, depth);
    printf("; ");
    printCstExpression(cst->test, depth);
    printf(")");
    printCstStatement(cst->body, depth + 1);
}

void printCstIfStatement(CstIfStatement *cst, int depth) {
    if (cst == NULL) return;

    indent(depth, "if (");
    printCstExpression(cst->expression, depth);
    printf(")");
    printCstStatement(cst->ifTrue, depth + 1);
    if (cst->ifFalse != NULL) {
        indent(depth, "else");
        printCstStatement(cst->ifFalse, depth + 1);
    }
}

void printCstConditionalStatement(CstConditionalStatement *cst, int depth) {
    indent(depth, "// Unexpected call to CstConditionalStatement");
}

static void binary(CstBinaryExpression *cst, const char *op, int depth) {
    printf("(");
    printCstExpression(cst->left, depth);
    printf(" %s ", op);
    printCstExpression(cst->right, depth);
    printf(")");
}

static void unary(CstExpression *cst, const char *op, int depth) {
    printf("%s", op);
    printCstExpression(cst, depth);
}

void printCstExpression(CstExpression *cst, int depth) {
    if (cst == NULL) return;
    switch (cst->type) {
        case CST_CALL_EXPR:
            printCstCallExpression(cst->expression.call, depth);
            break;
        case CST_INVOKE_EXPR:
            printCstCallExpression(cst->expression.call, depth);
            break;
        case CST_DOT_EXPR:
            binary(cst->expression.binary, ".", depth);
            break;
        case CST_NEGATION_EXPR:
            unary(cst->expression.unary, "-", depth);
            break;
        case CST_SUBTRACTION_EXPR:
            binary(cst->expression.binary, "-", depth);
            break;
        case CST_ADDITION_EXPR:
            binary(cst->expression.binary, "+", depth);
            break;
        case CST_DIVISION_EXPR:
            binary(cst->expression.binary, "/", depth);
            break;
        case CST_MULTIPLICATION_EXPR:
            binary(cst->expression.binary, "*", depth);
            break;
        case CST_LIST_EXPR:
            binary(cst->expression.binary, "@", depth);
            break;
        case CST_CONS_EXPR:
            binary(cst->expression.binary, "@", depth);
            break;
        case CST_APPEND_EXPR:
            binary(cst->expression.binary, "@@", depth);
            break;
        case CST_NOT_EXPR:
            unary(cst->expression.unary, "!", depth);
            break;
        case CST_NE_EXPR:
            binary(cst->expression.binary, "!=", depth);
            break;
        case CST_EQ_EXPR:
            binary(cst->expression.binary, "==", depth);
            break;
        case CST_CDR_EXPR:
            unary(cst->expression.unary, ">", depth);
            break;
        case CST_GT_EXPR:
            binary(cst->expression.binary, ">", depth);
            break;
        case CST_GE_EXPR:
            binary(cst->expression.binary, ">=", depth);
            break;
        case CST_CAR_EXPR:
            unary(cst->expression.unary, "<", depth);
            break;
        case CST_LT_EXPR:
            binary(cst->expression.binary, "<", depth);
            break;
        case CST_LE_EXPR:
            binary(cst->expression.binary, "<=", depth);
            break;
        case CST_VAR_EXPR:
            printCstStringExpression(cst->expression.string, depth);
            break;
        case CST_ASSIGN_EXPR:
            binary(cst->expression.binary, "=", depth);
            break;
        case CST_SETPROP_EXPR:
            binary(cst->expression.binary, "<--", depth);
            break;
        case CST_STRING_EXPR:
            printCstStringExpression(cst->expression.string, depth);
            break;
        case CST_NUMBER_EXPR:
            printf("%g", cst->expression.value);
            break;
        case CST_AND_EXPR:
            binary(cst->expression.binary, "and", depth);
            break;
        case CST_FALSE_EXPR:
            printf("false");
            break;
        case CST_FUN_EXPR:
            printf("fun ");
            printCstFunction(cst->expression.function, depth);
            break;
        case CST_NIL_EXPR:
            printf("nil");
            break;
        case CST_OR_EXPR:
            binary(cst->expression.binary, "or", depth);
            break;
        case CST_SUPER_GET_EXPR:
            printf("super.");
            printCstStringExpression(cst->expression.string, depth);
            break;
        case CST_SUPER_INVOKE_EXPR:
            printf("super.");
            printCstCallExpression(cst->expression.call, depth);
            break;
        case CST_THIS_EXPR:
            printf("this");
            break;
        case CST_TRUE_EXPR:
            printf("true");
            break;
    }
}

void printCstBinaryExpression(CstBinaryExpression *cst, int depth) {
    indent(depth, "// Unexpected call to printCstBinaryExpression");
}

void printCstCallExpression(CstCallExpression *cst, int depth) {
    if (cst == NULL) return;

    printCstExpression(cst->function, depth);
    printf("(");
    printCstExpressionList(cst->arguments, depth);
    printf(")");
}

void printCstExpressionList(CstExpressionList *cst, int depth) {
    if (cst == NULL) return;
    printCstExpression(cst->expression, depth);
    if (cst->next != NULL) {
        printf(", ");
        printCstExpressionList(cst->next, depth);
    }
}

void printCstStringExpression(CstStringExpression *cst, int depth) {
    if (cst == NULL) return;
    printToken(cst->value);
}

#endif

