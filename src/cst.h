#ifndef clox_cst_h
#define clox_cst_h

/* concrete syntax tree (mirrors the language directly) */

#include <stdbool.h>
#include <stdlib.h>

#include "common.h"
#include "scanner.h"

typedef enum {
    CST_CLASS_DECLARATION,
    CST_FUN_DECLARATION,
    CST_VAR_DECLARATION,
    CST_STATEMENT_DECLARATION
} CstDeclarationType;

typedef union {
    struct CstClassDeclaration *classDeclaration;
    struct CstFunDeclaration *funDeclaration;
    struct CstVarDeclaration *varDeclaration;
    struct CstStatement *statement;
} CstDeclarationValue;

typedef struct CstDeclarationList {
    int line;
    CstDeclarationType type;
    CstDeclarationValue declaration;
    struct CstDeclarationList *next;
} CstDeclarationList;

typedef struct CstClassDeclaration {
    int line;
    Token name;
    bool hasSuperclass;
    Token superName;
    struct CstMethodList *methods;
} CstClassDeclaration;

typedef struct CstMethodList {
    int line;
    Token name;
    struct CstFunction *function;
    struct CstMethodList *next;
} CstMethodList;

typedef struct CstFunction {
    int line;
    struct CstArgumentList *arguments;
    struct CstDeclarationList *declarations;
} CstFunction;

typedef struct CstArgumentList {
    int line;
    Token name;
    struct CstArgumentList *next;
} CstArgumentList;

typedef struct CstFunDeclaration {
    int line;
    Token name;
    struct CstFunction *function;
} CstFunDeclaration;

typedef struct CstVarDeclaration {
    int line;
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
    struct CstIfStatement *ifStatement;
    // while and do are all conditional statements
    struct CstConditionalStatement *conditionalStatement;
    struct CstDeclarationList *blockStatement;
    // expression, print and return are just expressions
    struct CstExpression *expression;
} CstStatementValue;

typedef struct CstStatement {
    int line;
    CstStatementType type;
    CstStatementValue statement;
} CstStatement;

typedef struct CstSwitchStatement {
    int line;
    struct CstExpression *expression;
    struct CstCaseList *cases;
} CstSwitchStatement;

typedef struct CstCaseList {
    int line;
    bool isDefault;
    struct CstExpression *expression;
    struct CstDeclarationList *declarations;
    struct CstCaseList *next;
} CstCaseList;

typedef union {
    struct CstVarDeclaration *declaration;
    struct CstExpression *expression;
} CstForInitializer;

typedef struct CstForStatement {
    int line;
    bool isDeclaration;
    CstForInitializer init;
    struct CstExpression *test;
    struct CstExpression *update;
    struct CstStatement *body;
} CstForStatement;

typedef struct CstIfStatement {
    int line;
    struct CstExpression *expression;
    struct CstStatement *ifTrue;
    struct CstStatement *ifFalse;
} CstIfStatement;

typedef struct CstConditionalStatement {
    int line;
    struct CstExpression *expression;
    struct CstStatement *statement;
} CstConditionalStatement;

typedef enum {
    CST_CALL_EXPR, // call
    CST_DOT_EXPR, // binary
    CST_NEGATION_EXPR, // unary
    CST_SUBTRACTION_EXPR, // binary
    CST_ADDITION_EXPR, // binary
    CST_DIVISION_EXPR, // binary
    CST_MULTIPLICATION_EXPR, // binary
    CST_CONS_EXPR, // binary
    CST_APPEND_EXPR, // binary
    CST_NOT_EXPR,
    CST_NE_EXPR, // binary
    CST_EQ_EXPR, // binary
    CST_CDR_EXPR,
    CST_GT_EXPR, // binary
    CST_GE_EXPR, // binary
    CST_CAR_EXPR,
    CST_LT_EXPR, // binary
    CST_LE_EXPR, // binary
    CST_VAR_EXPR,
    CST_ASSIGN_EXPR,
    CST_STRING_EXPR,
    CST_NUMBER_EXPR,
    CST_AND_EXPR,
    CST_FALSE_EXPR,
    CST_FUN_EXPR,
    CST_NIL_EXPR,
    CST_OR_EXPR,
    CST_SUPER_GET_EXPR,
    CST_SUPER_INVOKE_EXPR,
    CST_THIS_EXPR,
    CST_TRUE_EXPR
} CstExpressionType;

typedef union {
    struct CstCallExpression *call;
    struct CstCallSuperExpression *callSuper;
    struct CstAssignExpression *assign;
    struct CstBinaryExpression *binary;
    struct CstDotExpression *dot;
    struct CstExpression *unary;
    struct CstStringExpression *string; // also var
    struct CstFunction *function;
    double number;
} CstExpressionValue;

#define CST_CALL_FIELD(expression) ((CstExpressionValue){.call = expression})
#define CST_CALL_SUPER_FIELD(expression) ((CstExpressionValue){.callSuper = expression})
#define CST_ASSIGN_FIELD(expression) ((CstExpressionValue){.assign = expression})
#define CST_BINARY_FIELD(expression) ((CstExpressionValue){.binary = expression})
#define CST_DOT_FIELD(expression) ((CstExpressionValue){.dot = expression})
#define CST_UNARY_FIELD(expression) ((CstExpressionValue){.unary = expression})
#define CST_STRING_FIELD(expression) ((CstExpressionValue){.string = expression})
#define CST_FUNCTION_FIELD(expression) ((CstExpressionValue){.function = expression})
#define CST_NUMBER_FIELD(expression) ((CstExpressionValue){.number = expression})

#define CST_NO_FIELD ((CstExpressionValue){.unary = NULL})

typedef struct CstExpression {
    int line;
    CstExpressionType type;
    CstExpressionValue expression;
} CstExpression;

typedef struct CstBinaryExpression {
    int line;
    struct CstExpression *left;
    struct CstExpression *right;
} CstBinaryExpression;

typedef enum {
    CST_DOT_INVOKE,
    CST_DOT_GET,
    CST_DOT_ASSIGN
} CstDotType;

typedef union {
    struct CstExpression *value;
    struct CstExpressionList *arguments;
} CstDotAction;

typedef struct CstDotExpression {
    int line;
    struct CstExpression *left;
    Token property;
    CstDotType type;
    CstDotAction action;
} CstDotExpression;

typedef struct CstCallExpression {
    int line;
    struct CstExpression *function;
    struct CstExpressionList *arguments;
} CstCallExpression;

typedef struct CstExpressionList {
    int line;
    struct CstExpression *expression;
    struct CstExpressionList *next;
} CstExpressionList;

typedef struct CstStringExpression {
    int line;
    Token value;
} CstStringExpression;

typedef struct CstCallSuperExpression {
    int line;
    Token methodName;
    struct CstExpressionList *arguments;
} CstCallSuperExpression;

typedef struct CstAssignExpression {
    int line;
    Token variable;
    struct CstExpression *value;
} CstAssignExpression;


CstDeclarationList *newCstDeclarationList(
    int line,
    CstDeclarationType type,
    CstDeclarationValue declaration,
    CstDeclarationList *next
);

CstClassDeclaration *newCstClassDeclaration(
    int line,
    Token name,
    bool hasSuperclass,
    Token superName,
    CstMethodList *methods
);

CstMethodList *newCstMethodList(
    int line,
    Token name,
    CstFunction *function,
    CstMethodList *next
);

CstFunction *newCstFunction(
    int line,
    CstArgumentList *arguments,
    CstDeclarationList *declarations
);

CstArgumentList *newCstArgumentList(
    int line,
    Token name,
    CstArgumentList *next
);

CstFunDeclaration *newCstFunDeclaration(
    int line,
    Token name,
    CstFunction *function
);

CstVarDeclaration *newCstVarDeclaration(
    int line,
    Token name,
    CstExpression *initializer
);

CstStatement *newCstStatement(
    int line,
    CstStatementType type,
    CstStatementValue statement
);

CstSwitchStatement *newCstSwitchStatement(
    int line,
    CstExpression *expression,
    CstCaseList *cases
);

CstCaseList *newCstCaseList(
    int line,
    bool isDefault,
    CstExpression *expression,
    CstDeclarationList *declarations,
    CstCaseList *next
);

CstForStatement *newCstForStatement(
    int line,
    bool isDeclaration,
    CstForInitializer init,
    CstExpression *test,
    CstExpression *update,
    CstStatement *body
);

CstIfStatement *newCstIfStatement(
    int line,
    CstExpression *expression,
    CstStatement *ifTrue,
    CstStatement *ifFalse
);

CstConditionalStatement *newCstConditionalStatement(
    int line,
    CstExpression *expression,
    CstStatement *statement
);

CstExpression *newCstExpression(
    int line,
    CstExpressionType type,
    CstExpressionValue expression
);

CstBinaryExpression *newCstBinaryExpression(
    int line,
    CstExpression *left,
    CstExpression *right
);

CstCallExpression *newCstCallExpression(
    int line,
    CstExpression *function,
    CstExpressionList *arguments
);

CstExpressionList *newCstExpressionList(
    int line,
    CstExpression *expression,
    CstExpressionList *next
);

CstStringExpression *newCstStringExpression(
    int line,
    Token value
);

CstDotExpression *newCstDotExpression(
    int line,
    struct CstExpression *left,
    Token property,
    CstDotType type,
    CstDotAction action
);

CstAssignExpression *newCstAssignExpression(
    int line,
    Token variable,
    struct CstExpression *value
);

CstCallSuperExpression *newCstCallSuperExpression(
    int line,
    Token methodName,
    struct CstExpressionList *arguments
);


#ifdef DEBUG_PRINT_TREE


void printCstDeclarationList(CstDeclarationList *cst, int depth);

void printCstClassDeclaration(CstClassDeclaration *cst, int depth);

void printCstMethodList(CstMethodList *cst, int depth);

void printCstFunction(CstFunction *cst, int depth);

void printCstArgumentList(CstArgumentList *cst, int depth);

void printCstFunDeclaration(CstFunDeclaration *cst, int depth);

void printCstVarDeclaration(CstVarDeclaration *cst, int depth);

void printCstStatement(CstStatement *cst, int depth);

void printCstSwitchStatement(CstSwitchStatement *cst, int depth);

void printCstCaseList(CstCaseList *cst, int depth);

void printCstForStatement(CstForStatement *cst, int depth);

void printCstIfStatement(CstIfStatement *cst, int depth);

void printCstConditionalStatement(CstConditionalStatement *cst, int depth);

void printCstExpression(CstExpression *cst, int depth);

void printCstBinaryExpression(CstBinaryExpression *cst, int depth);

void printCstCallExpression(CstCallExpression *cst, int depth);

void printCstCallSuperExpression(CstCallSuperExpression *cst, int depth);

void printCstExpressionList(CstExpressionList *cst, int depth);

void printCstStringExpression(CstStringExpression *cst, int depth);

void printCstDotExpression(CstDotExpression *cst, int depth);

void printCstAssignExpression(CstAssignExpression *cst, int depth);

#endif

#endif
