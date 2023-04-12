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
    bool isInit,
    CstFunction *function,
    CstMethodList *next
) {
    CstMethodList *x = NEW(CstMethodList);
    x->name = name;
    x->isInit = isInit;
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
    CstStatement *statement,
    CstCaseList *next
) {
    CstCaseList *x = NEW(CstCaseList);
    x->isDefault = isDefault;
    x->expression = expression;
    x->statement = statement;
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
