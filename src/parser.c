#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "common.h"
#include "parser.h"
#include "memory.h"
#include "scanner.h"

#ifdef DEBUG_PRINT_TREE
#include "debug.h"
#endif

#define MAX_CASES 256

typedef struct {
    Token current;
    Token previous;
    bool hadError;
    bool panicMode;
} Parser;

typedef enum {
    PREC_NONE,
    PREC_ASSIGNMENT,
    PREC_OR,
    PREC_AND,
    PREC_EQUALITY,
    PREC_COMPARISON,
    PREC_CONS,
    PREC_TERM,
    PREC_FACTOR,
    PREC_UNARY,
    PREC_CALL,
    PREC_PRIMARY
} Precedence;

typedef CstExpression *(*PrefixFn)(bool canAssign);
typedef CstExpression *(*InfixFn)(CstExpression *current, bool canAssign);

typedef bool (*checkFn)();

typedef struct {
    PrefixFn prefix;
    InfixFn infix;
    Precedence precedence;
} ParseRule;

typedef enum {
    TYPE_FUNCTION,
    TYPE_INITIALIZER,
    TYPE_METHOD,
    TYPE_SCRIPT
} FunctionType;

static Parser parser;

static void errorAt(Token *token, const char *message) {
    if (parser.panicMode) return;
    parser.panicMode = true;
    fprintf(stderr, "[line %d] Error", token->line);

    if (token->type == TOKEN_EOF) {
        fprintf(stderr, " at end");
    } else if (token->type == TOKEN_ERROR) {
    } else {
        fprintf(stderr, " at '%.*s'", token->length, token->start);
    }

    fprintf(stderr, ": %s\n", message);
    parser.hadError = true;
}

static void error(const char *message) {
    errorAt(&parser.previous, message);
}

static void errorAtCurrent(const char *message) {
    errorAt(&parser.current, message);
}

static void cant_happen(const char *msg) {
    fprintf(stderr, "internal error: %s\n", msg);
    exit(1);
}

static void advance() {
    parser.previous = parser.current;

    for (;;) {
        parser.current = scanToken();
        if (parser.current.type != TOKEN_ERROR) break;
        errorAtCurrent(parser.current.start);
    }
}

static void consume(TokenType type, const char *message) {
    if (parser.current.type == type) {
        advance();
        return;
    }

    errorAtCurrent(message);
}

static bool check(TokenType type) {
    return parser.current.type == type;
}

static bool match(TokenType type) {
    if (!check(type)) return false;
    advance();
    return true;
}

static bool checkEOF() {
    return check(TOKEN_EOF);
}

static bool matchRightBrace() {
    return match(TOKEN_RIGHT_BRACE) || checkEOF();
}

static bool matchRightParen() {
    return match(TOKEN_RIGHT_PAREN) || checkEOF();
}

static CstExpression *expression();
static CstStatement *statement();
static CstDeclarationList *declaration();
static ParseRule *getRule(TokenType type);
static CstExpression *parsePrecedence(Precedence precedence);
static CstFunction *function();

static bool identifiersEqual(Token *a, Token *b) {
    if (a->length != b->length) return false;
    return memcmp(a->start, b->start, a->length) == 0;
}

static Token parseVariable(const char *errorMessage) {
    consume(TOKEN_IDENTIFIER, errorMessage);

    return parser.previous;
}

static CstExpressionList *argumentList() {
    if (matchRightParen()) {
        return NULL;
    }
    CstExpression *arg = expression();
    match(TOKEN_COMMA);
    return newCstExpressionList(arg, argumentList());
}

static CstExpression *binary(CstExpression *lhs, bool canAssign) {
    TokenType operatorType = parser.previous.type;
    ParseRule *rule = getRule(operatorType);
    CstExpression *rhs = parsePrecedence((Precedence)(rule->precedence + 1));
    CstExpressionType type;
    switch (operatorType) {
        case TOKEN_AND:           type = CST_AND_EXPR;            break;
        case TOKEN_OR:            type = CST_OR_EXPR;             break;
        case TOKEN_BANG_EQUAL:    type = CST_NE_EXPR;             break;
        case TOKEN_EQUAL_EQUAL:   type = CST_EQ_EXPR;             break;
        case TOKEN_GREATER:       type = CST_GT_EXPR;             break;
        case TOKEN_GREATER_EQUAL: type = CST_GE_EXPR;             break;
        case TOKEN_LESS:          type = CST_LT_EXPR;             break;
        case TOKEN_LESS_EQUAL:    type = CST_LE_EXPR;             break;
        case TOKEN_PLUS:          type = CST_ADDITION_EXPR;       break;
        case TOKEN_MINUS:         type = CST_SUBTRACTION_EXPR;    break;
        case TOKEN_STAR:          type = CST_MULTIPLICATION_EXPR; break;
        case TOKEN_SLASH:         type = CST_DIVISION_EXPR;       break;
        default:
            cant_happen("unrecognised token type in binary()");
    }
    return newCstExpression(type, CST_BINARY_FIELD(newCstBinaryExpression(lhs, rhs)));
}

static CstExpression *right(CstExpression *lhs, bool canAssign) {
    TokenType operatorType = parser.previous.type;
    ParseRule *rule = getRule(operatorType);
    CstExpression *rhs = parsePrecedence(rule->precedence);
    CstExpressionType type;
    switch (operatorType) {
        case TOKEN_AT:    type = CST_CONS_EXPR;   break;
        case TOKEN_AT_AT: type = CST_APPEND_EXPR; break;
        default:
            cant_happen("unrecognised token in right()");
    }
    return newCstExpression(type, CST_BINARY_FIELD(newCstBinaryExpression(lhs, rhs)));
}

static CstExpression *call(CstExpression *lhs, bool canAssign) {
    CstExpressionList *args = argumentList();
    return newCstExpression(CST_CALL_EXPR, CST_CALL_FIELD(newCstCallExpression(lhs, args)));
}

static CstExpression *dot(CstExpression *lhs, bool canAssign) {
    Token name = parseVariable("Expect property name after '.'");
    CstStringExpression *property = newCstStringExpression(name);
    CstExpression *rhs = newCstExpression(CST_VAR_EXPR, CST_STRING_FIELD(property));
    CstExpression *pair = newCstExpression(
        CST_DOT_EXPR,
        CST_BINARY_FIELD(newCstBinaryExpression(lhs, rhs))
    );

    if (canAssign && match(TOKEN_EQUAL)) {
        CstExpression *value = expression();
        return newCstExpression(CST_SETPROP_EXPR, CST_BINARY_FIELD(newCstBinaryExpression(pair, value)));
    } else if (match(TOKEN_LEFT_PAREN)) {
        CstExpressionList *args = argumentList();
        return newCstExpression(
            CST_INVOKE_EXPR,
            CST_CALL_FIELD(newCstCallExpression(pair, args))
        );
    } else {
        return pair;
    }
}

static CstExpression *literal(bool canAssign) {
    switch (parser.previous.type) {
        case TOKEN_FALSE: return newCstExpression(CST_FALSE_EXPR, CST_NO_FIELD); break;
        case TOKEN_NIL:   return newCstExpression(CST_NIL_EXPR, CST_NO_FIELD);   break;
        case TOKEN_TRUE:  return newCstExpression(CST_TRUE_EXPR, CST_NO_FIELD);  break;
        default:
            cant_happen("unrecognised type in literal()");
    }
}

static CstExpression *grouping(bool canAssign) {
    CstExpression *expr = expression();
    consume(TOKEN_RIGHT_PAREN, "Expect ')' after '(' <expression>");
    return expr;
}

static CstExpression *number(bool canAssign) {
    double value = strtod(parser.previous.start, NULL);
    return newCstExpression(CST_NUMBER_EXPR, CST_NUMBER_FIELD(value));
}

static CstExpression *string(bool canAssign) {
    return newCstExpression(
        CST_STRING_EXPR,
        CST_STRING_FIELD(newCstStringExpression(parser.previous))
    );
}

static CstExpression *namedVariable(Token name, bool canAssign) {
    CstExpression *var = newCstExpression(CST_VAR_EXPR, CST_STRING_FIELD(newCstStringExpression(name)));
    if (canAssign && match(TOKEN_EQUAL)) {
        CstExpression *value = expression();
        return newCstExpression(
            CST_ASSIGN_EXPR,
            CST_BINARY_FIELD(newCstBinaryExpression(var, value))
        );
    } else {
        return var;
    }
}

static CstExpression *variable(bool canAssign) {
    return namedVariable(parser.previous, canAssign);
}

static Token syntheticToken(const char *text) {
    Token token;
    token.start = text;
    token.length = (int)strlen(text);
    return token;
}

static CstExpression *super_(bool canAssign) {
    consume(TOKEN_DOT, "Expect '.' after 'super'");
    Token methodName = parseVariable("Expect superclass method name");

    if (match(TOKEN_LEFT_PAREN)) {
        return newCstExpression(
            CST_SUPER_INVOKE_EXPR,
            CST_CALL_FIELD(
                newCstCallExpression(
                    newCstExpression(
                        CST_VAR_EXPR,
                        CST_STRING_FIELD(newCstStringExpression(methodName))
                    ),
                    argumentList()
                )
            )
        );
    } else {
        return newCstExpression(
            CST_SUPER_GET_EXPR,
            CST_STRING_FIELD(newCstStringExpression(methodName))
        );
    }
}

static CstExpression *this_(bool canAssign) {
    return newCstExpression(CST_THIS_EXPR, CST_NO_FIELD);
}

static CstExpression *unary(bool canAssign) {
    TokenType operatorType = parser.previous.type;
    CstExpression *operand = parsePrecedence(PREC_UNARY);
    CstExpressionType type;
    switch (operatorType) {
        case TOKEN_BANG:
            type = CST_NOT_EXPR;
            break;
        case TOKEN_MINUS:
            type = CST_NEGATION_EXPR;
            break;
        case TOKEN_LESS:
            type = CST_CAR_EXPR;
            break;
        case TOKEN_GREATER:
            type = CST_CDR_EXPR;
            break;
        default:
            cant_happen("happen unrecognised type in unary()");
    }

    return newCstExpression(type, CST_UNARY_FIELD(operand));
}

static CstExpression *list(bool canAssign) {
    if (match(TOKEN_RIGHT_SQUARE)) {
        return newCstExpression(CST_NIL_EXPR, CST_NO_FIELD);
    }
    CstExpression *car = expression();
    match(TOKEN_COMMA);
    return newCstExpression(
        CST_LIST_EXPR,
        CST_BINARY_FIELD(newCstBinaryExpression(car, list(canAssign)))
    );
}

static CstExpression *funExpr(bool canAssign) {
    return newCstExpression(CST_FUN_EXPR, CST_FUNCTION_FIELD(function()));
}

static ParseRule parseRules[] = {
    [TOKEN_LEFT_PAREN]      = {grouping,    call,   PREC_CALL},
    [TOKEN_RIGHT_PAREN]     = {NULL,        NULL,   PREC_NONE},
    [TOKEN_LEFT_BRACE]      = {NULL,        NULL,   PREC_NONE},
    [TOKEN_RIGHT_BRACE]     = {NULL,        NULL,   PREC_NONE},
    [TOKEN_COMMA]           = {NULL,        NULL,   PREC_NONE},
    [TOKEN_DOT]	            = {NULL,        dot,    PREC_CALL},
    [TOKEN_MINUS]           = {unary,       binary, PREC_TERM},
    [TOKEN_PLUS]            = {NULL,        binary, PREC_TERM},
    [TOKEN_SEMICOLON]       = {NULL,        NULL,   PREC_NONE},
    [TOKEN_SLASH]           = {NULL,        binary, PREC_FACTOR},
    [TOKEN_STAR]            = {NULL,        binary, PREC_FACTOR},
    [TOKEN_LEFT_SQUARE]     = {list,        NULL,   PREC_NONE},
    [TOKEN_RIGHT_SQUARE]    = {NULL,        NULL,   PREC_NONE},
    [TOKEN_AT]              = {NULL,        right,  PREC_CONS},
    [TOKEN_AT_AT]           = {NULL,        right,  PREC_CONS},
    [TOKEN_SWITCH]          = {NULL,        NULL,   PREC_NONE},
    [TOKEN_CASE]            = {NULL,        NULL,   PREC_NONE},
    [TOKEN_COLON]           = {NULL,        NULL,   PREC_NONE},
    [TOKEN_DEFAULT]         = {NULL,        NULL,   PREC_NONE},
    [TOKEN_BANG]            = {unary,       NULL,   PREC_NONE},
    [TOKEN_BANG_EQUAL]      = {NULL,        binary, PREC_EQUALITY},
    [TOKEN_EQUAL]           = {NULL,        NULL,   PREC_NONE},
    [TOKEN_EQUAL_EQUAL]     = {NULL,        binary, PREC_EQUALITY},
    [TOKEN_GREATER]         = {unary,       binary, PREC_COMPARISON},
    [TOKEN_GREATER_EQUAL]   = {NULL,        binary, PREC_COMPARISON},
    [TOKEN_LESS]            = {unary,       binary, PREC_COMPARISON},
    [TOKEN_LESS_EQUAL]      = {NULL,        binary, PREC_COMPARISON},
    [TOKEN_IDENTIFIER]      = {variable,    NULL,   PREC_NONE},
    [TOKEN_STRING]		    = {string,      NULL,   PREC_NONE},
    [TOKEN_NUMBER]		    = {number,      NULL,   PREC_NONE},
    [TOKEN_AND]		        = {NULL,        binary, PREC_AND},
    [TOKEN_CLASS]		    = {NULL,        NULL,   PREC_NONE},
    [TOKEN_ELSE]		    = {NULL,        NULL,   PREC_NONE},
    [TOKEN_FALSE]		    = {literal,     NULL,   PREC_NONE},
    [TOKEN_FOR]		        = {NULL,        NULL,   PREC_NONE},
    [TOKEN_FUN]		        = {funExpr,     NULL,   PREC_NONE},
    [TOKEN_IF]		        = {NULL,        NULL,   PREC_NONE},
    [TOKEN_NIL]		        = {literal,     NULL,   PREC_NONE},
    [TOKEN_OR]		        = {NULL,        binary, PREC_OR},
    [TOKEN_PRINT]		    = {NULL,        NULL,   PREC_NONE},
    [TOKEN_RETURN]		    = {NULL,        NULL,   PREC_NONE},
    [TOKEN_SUPER]		    = {super_,      NULL,   PREC_NONE},
    [TOKEN_THIS]		    = {this_,       NULL,   PREC_NONE},
    [TOKEN_TRUE]		    = {literal,     NULL,   PREC_NONE},
    [TOKEN_VAR]		        = {NULL,        NULL,   PREC_NONE},
    [TOKEN_WHILE]		    = {NULL,        NULL,   PREC_NONE},
    [TOKEN_ERROR]		    = {NULL,        NULL,   PREC_NONE},
    [TOKEN_EOF]		        = {NULL,        NULL,   PREC_NONE}
};

static CstExpression *parsePrecedence(Precedence precedence) {
    advance();
    PrefixFn prefixRule = getRule(parser.previous.type)->prefix;

    if (prefixRule == NULL) {
        error("Expect <expression>");
        return NULL;
    }

    bool canAssign = precedence <= PREC_ASSIGNMENT;
    CstExpression *current = prefixRule(canAssign);

    while (precedence <= getRule(parser.current.type)->precedence) {
        advance();
        InfixFn infixRule = getRule(parser.previous.type)->infix;
        current = infixRule(current, canAssign);
    }

    if (canAssign && match(TOKEN_EQUAL)) {
        error("Invalid assignment target");
    }

    return current;
}

static ParseRule *getRule(TokenType type) {
    return &parseRules[type];
}

static CstExpression *expression() {
    return parsePrecedence(PREC_ASSIGNMENT);
}

static CstDeclarationList *block() {
    return declaration(matchRightBrace);
}

static CstArgumentList *arguments() {
    if (match(TOKEN_RIGHT_PAREN)) {
        return NULL;
    }
    Token name = parseVariable("Expect variable name");
    match(TOKEN_COMMA);
    return newCstArgumentList(name, arguments());
}

static CstFunction *function() {
    consume(TOKEN_LEFT_PAREN, "Expect '(' after function name");
    CstArgumentList *args = arguments();

    consume(TOKEN_LEFT_BRACE, "Expect '{' before function body");
    CstDeclarationList *body = block();

    return newCstFunction(args, body);
}

static CstMethodList *method(checkFn check) {
    if (check()) {
        return NULL;
    }
    Token name = parseVariable("Expect method name");
    CstFunction *fun = function();
    return newCstMethodList(name, fun, method(check));
}

static CstClassDeclaration *classDeclaration() {
    consume(TOKEN_IDENTIFIER, "Expect class name");
    Token className = parser.previous;
    Token superName = emptyToken;
    bool hasSuperclass = false;

    if (match(TOKEN_LESS)) {
        consume(TOKEN_IDENTIFIER, "Expect superclass name");

        if (identifiersEqual(&className, &parser.previous)) {
            error("A class can't inherit from itself");
        }

        hasSuperclass = true;
        superName = parser.previous;
    }

    consume(TOKEN_LEFT_BRACE, "Expect '{' after class name");

    return newCstClassDeclaration(
        className,
        hasSuperclass,
        superName,
        method(matchRightBrace)
    );
}

static CstFunDeclaration *funDeclaration() {
    Token name = parseVariable("Expect function name");
    return newCstFunDeclaration(name, function());
}

static CstVarDeclaration *varDeclaration() {
    Token name = parseVariable("Expect variable name");

    CstVarDeclaration *result;

    if (match(TOKEN_EQUAL)) {
        result = newCstVarDeclaration(name, expression());
    } else {
        result = newCstVarDeclaration(name, NULL);
    }

    consume(TOKEN_SEMICOLON, "Expect ';' after variable declaration");
    return result;
}

static CstExpression *expressionStatement() {
    CstExpression *expr = expression();
    consume(TOKEN_SEMICOLON, "Expect ';' after <expression>");
    return expr;
}

static bool checkCaseEnd() {
    return(   check(TOKEN_RIGHT_BRACE)
           || check(TOKEN_EOF)
           || check(TOKEN_CASE)
           || check(TOKEN_DEFAULT));
}

static CstCaseList *caseList(checkFn check) {
    if (check()) {
        return NULL;
    }

    CstExpression *condition = NULL;
    CstDeclarationList *declarations = NULL;
    bool isDefault = false;

    if (match(TOKEN_CASE)) {
        condition = expression();
        consume(TOKEN_COLON, "Expect ':' afer 'case' <expression>");
        declarations = declaration(checkCaseEnd);
    } else if (match(TOKEN_DEFAULT)) {
        isDefault = true;
        consume(TOKEN_COLON, "Expect ':' afer 'default'");
        declarations = declaration(checkCaseEnd);
    } else {
        errorAtCurrent("Expect 'case' or 'default' in switch statement body");
        return NULL;
    }

    return newCstCaseList(
        isDefault,
        condition,
        declarations,
        caseList(check)
    );
}

static CstSwitchStatement *switchStatement() {
    consume(TOKEN_LEFT_PAREN, "Expect '(' after 'switch'");
    CstExpression *value = expression();
    consume(TOKEN_RIGHT_PAREN, "Expect ')' after <expression>");
    consume(TOKEN_LEFT_BRACE, "Expect '{' before switch statement body");

    return newCstSwitchStatement(
        value,
        caseList(matchRightBrace)
    );
}

static CstForStatement *forStatement() {
    consume(TOKEN_LEFT_PAREN, "Expect '(' after 'for'");
    bool isDeclaration = false;
    CstForInitializer init;
    CstExpression *test;
    CstExpression *update;

    //initializer
    if (match(TOKEN_SEMICOLON)) {
        init.expression = NULL;
    } else if (match(TOKEN_VAR)) {
        isDeclaration = true;
        init.declaration = varDeclaration();
    } else {
        init.expression = expressionStatement();
    }

    // test
    if(match(TOKEN_SEMICOLON)) {
        test = NULL;
    } else {
        test = expression();
        consume(TOKEN_SEMICOLON, "Expect ';' after loop condition");
    }

    // update
    if (match(TOKEN_RIGHT_PAREN)) {
        update = false;
    } else {
        update = expression();
        consume(TOKEN_RIGHT_PAREN, "Expect ')' after 'for' clauses");
    }

    // body
    CstStatement *body = statement();
    return newCstForStatement(isDeclaration, init, test, update, body);
}

static CstIfStatement *ifStatement() {
    consume(TOKEN_LEFT_PAREN, "Expect '(' after 'if'");
    CstExpression *condition = expression();
    consume(TOKEN_RIGHT_PAREN, "Expect ')' after '(' <condition>");
    CstStatement *ifTrue = statement();
    CstStatement *ifFalse = NULL;

    if (match(TOKEN_ELSE)) ifFalse = statement();

    return newCstIfStatement(condition, ifTrue, ifFalse);
}

static CstExpression *returnStatement() {
    if (match(TOKEN_SEMICOLON)) {
        return NULL;
    } else {
        CstExpression *value = expression();
        consume(TOKEN_SEMICOLON, "Expect ';' after 'return' <value>");
        return value;
    }
}

static CstConditionalStatement *whileStatement() {
    consume(TOKEN_LEFT_PAREN, "Expect '(' after 'while'");
    CstExpression *condition = expression();
    consume(TOKEN_RIGHT_PAREN, "Expect ')' after 'while' <condition>");
    CstStatement *body = statement();
    return newCstConditionalStatement(condition, body);
}

static CstConditionalStatement *doStatement() {
    CstStatement *body = statement();
    consume(TOKEN_WHILE, "Expect 'while' after 'do' <statement>");
    consume(TOKEN_LEFT_PAREN, "Expect '(' after 'while'");
    CstExpression *condition = expression();
    consume(TOKEN_RIGHT_PAREN, "Expect ')' after 'while' '(' <expression>");
    consume(TOKEN_SEMICOLON, "Expect ';' after '(' <expression> ')'");
    return newCstConditionalStatement(condition, body);
}

static void synchronize() {
    parser.panicMode = false;

    while(parser.current.type != TOKEN_EOF) {
        if (parser.previous.type == TOKEN_SEMICOLON) return;

        switch (parser.current.type) {
            case TOKEN_SWITCH:
            case TOKEN_CLASS:
            case TOKEN_FUN:
            case TOKEN_VAR:
            case TOKEN_FOR:
            case TOKEN_IF:
            case TOKEN_WHILE:
            case TOKEN_DO:
            case TOKEN_PRINT:
            case TOKEN_RETURN:
                return;
            default:
                ;
        }

        advance();
    }
}

static CstDeclarationList *declaration(checkFn check) {
    if (check()) {
        return NULL;
    }

    CstDeclarationType type;
    CstDeclarationValue value;

    if (match(TOKEN_CLASS)) {
        type = CST_CLASS_DECLARATION;
        value.classDeclaration = classDeclaration();
    } else if (match(TOKEN_FUN)) {
        type = CST_FUN_DECLARATION;
        value.funDeclaration = funDeclaration();
    } else if (match(TOKEN_VAR)) {
        type = CST_VAR_DECLARATION;
        value.varDeclaration = varDeclaration();
    } else {
        type = CST_STATEMENT_DECLARATION;
        value.statement = statement();
    }

    if (parser.panicMode) synchronize();

    return newCstDeclarationList(type, value, declaration(check));
}

static CstStatement *statement() {
    CstStatementType type;
    CstStatementValue value;
    if (match(TOKEN_PRINT)) {
        type = CST_PRINT_STATEMENT;
        value.expression = expression();
        consume(TOKEN_SEMICOLON, "Expect ';' after 'print' <statement>");
    } else if (match(TOKEN_SWITCH)) {
        type = CST_SWITCH_STATEMENT;
        value.switchStatement = switchStatement();
    } else if (match(TOKEN_FOR)) {
        type = CST_FOR_STATEMENT;
        value.forStatement = forStatement();
    } else if (match(TOKEN_IF)) {
        type = CST_IF_STATEMENT;
        value.ifStatement = ifStatement();
    } else if (match(TOKEN_RETURN)) {
        type = CST_RETURN_STATEMENT;
        value.expression = returnStatement();
    } else if (match(TOKEN_WHILE)) {
        type = CST_WHILE_STATEMENT;
        value.conditionalStatement = whileStatement();
    } else if (match(TOKEN_DO)) {
        type = CST_DO_STATEMENT;
        value.conditionalStatement = doStatement();
    } else if (match(TOKEN_LEFT_BRACE)) {
        type = CST_BLOCK_STATEMENT;
        value.blockStatement = block();
    } else {
        type = CST_EXPRESSION_STATEMENT;
        value.expression = expressionStatement();
    }

    return newCstStatement(type, value);
}

CstDeclarationList *parse(const char *source) {
    initScanner(source);
    parser.hadError = false;
    parser.panicMode = false;
    advance();

    CstDeclarationList *result = declaration(checkEOF);

    return parser.hadError ? NULL : result;
}
