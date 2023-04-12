#ifndef clox_cst_h
#define clox_cst_h

/* concrete syntax tree (mirrors the language directly) */

#include <stdbool.h>

#include "scanner.h"

typedef enum {
    CST_CLASS_DECLARATION,
    CST_FUN_DECLARATION,
    CST_VAR_DECLARATION,
    CST_STATEMENT_DECLARATION
} CstDeclarationType;

typedef union {
    struct CstClassDeclaration *ClassDeclaration;
    struct CstFunDeclaration *funDeclaration;
    struct CstVarDeclaration *varDeclaration;
    struct CstStatement *statement;
} CstDeclarationValue;

typedef struct CstDeclarationList {
    CstDeclarationType type;
    CstDeclarationValue declaration;
    struct CstDeclarationList *next;
} CstDeclarationList;

typedef struct CstClassDeclaration {
    Token name;
    bool hasSuperclass;
    Token superName;
    struct CstMethodList *methods;
} CstClassDeclaration;

typedef struct CstMethodList {
    Token name;
    bool isInit;
    struct CstFunction *function;
    struct CstMethodList *next;
} CstMethodList;

typedef struct CstFunction {
    struct CstArgumentList *arguments;
    struct CstDeclarationList *declarations;
} CstFunction;

typedef struct CstArgumentList {
    Token name;
    struct CstArgumentList *next;
} CstArgumentList;

typedef struct CstFunDeclaration {
    Token name;
    struct CstFunction *function;
} CstFunDeclaration;

typedef struct CstVarDeclaration {
    Token name;
    struct CstExpression *initializer;
} CstVarDeclaration;

typedef enum {
    CST_PRINT_STATEMENT,
    CST_SWITCH_STATEMENT,
    CST_FOR_STATEMENT,
    CST_IF_STATEMENT,
    CST_RETURN_STATEMENT,
    CST_WHILE_STATEMENT,
    CST_DO_STATEMENT,
    CST_BLOCK_STATEMENT,
    CST_EXPRESSION_STATEMENT
} CstStatementType;

typedef union {
    struct CstSwitchStatement *switchStatement;
    struct CstForStatement *forStatement;
    // if, while and do are all conditional statements
    struct CstConditionalStatement *conditionalStatement;
    struct CstDeclarationList *blockStatement;
    // expression, print and return are just expressions
    struct CstExpression *expression;
} CstStatementValue;

typedef struct CstStatement {
    CstStatementType type;
    CstStatementValue statement;
} CstStatement;

typedef struct CstSwitchStatement {
    struct CstExpression *expression;
    struct CstCaseList *cases;
} CstSwitchStatement;

typedef struct CstCaseList {
    bool isDefault;
    struct CstExpression *expression;
    struct CstStatement *statement;
    struct CstCaseList *next;
} CstCaseList;

typedef union {
    struct CstVarDeclaration *declaration;
    struct CstExpression *expression;
} CstForInitializer;

typedef struct CstForStatement {
    bool isDeclaration;
    CstForInitializer init;
    struct CstExpression *test;
    struct CstExpression *update;
    struct CstStatement *body;
} CstForStatement;

typedef struct CstConditionalStatement {
    struct CstExpression *expression;
    struct CstStatement *statement;
} CstConditionalStatement;

typedef enum {
    CST_CALL_EXPR,
    CST_DOT_EXPR,
    CST_NEGATION_EXPR,
    CST_SUBTRACTION_EXPR,
    CST_ADDITION_EXPR,
    CST_DIVISION_EXPR,
    CST_MULTIPLICATION_EXPR,
    CST_LIST_EXPR,
    CST_CONS_EXPR,
    CST_APPEND_EXPR,
    CST_NOT_EXPR,
    CST_NE_EXPR,
    CST_EQ_EXPR,
    CST_CDR_EXPR,
    CST_GT_EXPR,
    CST_GE_EXPR,
    CST_CAR_EXPR,
    CST_LT_EXPR,
    CST_LE_EXPR,
    CST_VARIABLE_EXPR,
    CST_STRING_EXPR,
    CST_NUMBER_EXPR,
    CST_AND_EXPR,
    CST_FALSE_EXPR,
    CST_FUN_EXPR,
    CST_NIL_EXPR,
    CST_OR_EXPR,
    CST_SUPER_EXPR,
    CST_THIS_EXPR,
    CST_TRUE_EXPR
} CstExpressionType;

typedef union {
    struct CstCallExpression *call;
    struct CstBinaryExpression *binary;
    struct CstExpression *unary;
    struct CstStringExpression *string; // also var
    double value;
} CstExpressionValue;

typedef struct CstExpression {
    CstExpressionType type;
    CstExpressionValue expression;
} CstExpression;

typedef struct CstBinaryExpression {
    struct CstExpression *left;
    struct CstExpression *right;
} CstBinaryExpression;

typedef struct CstCallExpression {
    struct CstExpression *function;
    struct CstExpressionList *arguments;
} CstCallExpression;

typedef struct CstExpressionList {
    struct CstExpression *expression;
    struct CstExpressionList *next;
} CstExpressionList;

typedef struct CstStringExpression {
    Token value;
} CstStringExpression;

CstDeclarationList *newCstDeclarationList(
    CstDeclarationType type,
    CstDeclarationValue declaration,
    CstDeclarationList *next
);

CstClassDeclaration *newCstClassDeclaration(
    Token name,
    bool hasSuperclass,
    Token superName,
    CstMethodList *methods
);

CstMethodList *newCstMethodList(
    Token name,
    bool isInit,
    CstFunction *function,
    CstMethodList *next
);

CstFunction *newCstFunction(
    CstArgumentList *arguments,
    CstDeclarationList *declarations
);

CstArgumentList *newCstArgumentList(Token name, CstArgumentList *next);

CstFunDeclaration *newCstFunDeclaration(Token name, CstFunction *function);

CstVarDeclaration *newCstVarDeclaration(
    Token name,
    CstExpression *initializer
);

CstStatement *newCstStatement(
    CstStatementType type,
    CstStatementValue statement
);

CstSwitchStatement *newCstSwitchStatement(
    CstExpression *expression,
    CstCaseList *cases
);

CstCaseList *newCstCaseList(
    bool isDefault,
    CstExpression *expression,
    CstStatement *statement,
    CstCaseList *next
);

CstForStatement *newCstForStatement(
    bool isDeclaration,
    CstForInitializer init,
    CstExpression *test,
    CstExpression *update,
    CstStatement *body
);

CstConditionalStatement *newCstConditionalStatement(
    CstExpression *expression,
    CstStatement *statement
);

CstExpression *newCstExpression(
    CstExpressionType type,
    CstExpressionValue expression
);

CstBinaryExpression *newCstBinaryExpression(
    CstExpression *left,
    CstExpression *right
);

CstCallExpression *newCstCallExpression(
    CstExpression *function,
    CstExpressionList *arguments
);

CstExpressionList *newCstExpressionList(
    CstExpression *expression,
    CstExpressionList *next
);

CstStringExpression *newCstStringExpression(Token value);

#endif
