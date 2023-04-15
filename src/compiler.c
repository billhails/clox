#ifdef DEBUG_DUMP_CORE
#include <signal.h>
#endif
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "common.h"
#include "compiler.h"
#include "parser.h"
#include "memory.h"
#include "scanner.h"

#ifdef DEBUG_PRINT_CODE
#include "debug.h"
#endif

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

typedef void (*ParseFn)(bool canAssign);

typedef struct {
    ParseFn prefix;
    ParseFn infix;
    Precedence precedence;
} ParseRule;

typedef struct {
    Token name;
    int depth;
    bool isCaptured;
} Local;

typedef struct {
    uint8_t index;
    bool isLocal;
} Upvalue;

typedef enum {
    TYPE_FUNCTION,
    TYPE_INITIALIZER,
    TYPE_METHOD,
    TYPE_SCRIPT
} FunctionType;

typedef struct Compiler {
    struct Compiler *enclosing;
    ObjFunction *function;
    FunctionType type;
    Local locals[UINT8_COUNT];
    int localCount;
    Upvalue upvalues[UINT8_COUNT];
    int scopeDepth;
} Compiler;

typedef struct ClassCompiler {
    struct ClassCompiler *enclosing;
    bool hasSuperclass;
} ClassCompiler;

Compiler *current = NULL;
ClassCompiler *currentClass = NULL;

static bool hadError;
static int line;

#define LINE(CST) (line = CST->line)

static Chunk *currentChunk() {
    return &current->function->chunk;
}

static void errorAt(Token *token, const char *message) {
    if (token != NULL) {
        fprintf(stderr, "[line %d] Error", token->line);

        if (token->type == TOKEN_EOF) {
            fprintf(stderr, " at end");
        } else if (token->type == TOKEN_ERROR) {
        } else {
            fprintf(stderr, " at '%.*s'", token->length, token->start);
        }
    } else {
        fprintf(stderr, "[line %d] Error", line);
    }
    fprintf(stderr, ": %s\n", message);
    hadError = true;
}

static void error(const char *message, Token *name) {
    errorAt(name, message);
}

void cant_happen(const char *message) {
    char buffer[256];
    snprintf(buffer, (size_t)256, "INTERNAL ERROR: %s\n", message);
    errorAt(NULL, buffer);
#ifdef DUMP_CORE
    raise(SIGABRT);
#else
    exit(1);
#endif
}

static void emitByte(uint8_t byte) {
    writeChunk(currentChunk(), byte, line);
}

static void emitBytes(uint8_t byte1, uint8_t byte2) {
    emitByte(byte1);
    emitByte(byte2);
}

static void emitLoop(int loopStart) {
    emitByte(OP_LOOP);
    int offset = currentChunk()->count - loopStart + 2;
    if (offset > UINT16_MAX) error("Loop body too large", NULL);
    emitByte((offset >> 8) & 0xff);
    emitByte(offset & 0xff);
}

static int emitJump(uint8_t instruction) {
    emitByte(instruction);
    emitByte(0xff);
    emitByte(0xff);
    return currentChunk()->count - 2;
}

static void emitReturn() {
    if (current->type == TYPE_INITIALIZER) {
        emitBytes(OP_GET_LOCAL, 0);
    } else {
        emitByte(OP_NIL);
    }
    emitByte(OP_RETURN);
}

static uint8_t makeConstant(Value value) {
    int constant = addConstant(currentChunk(), value);
    if (constant > UINT8_MAX) {
        error("Too many constants in one chunk", NULL);
        return 0;
    }

    return (uint8_t)constant;
}

static void emitConstant(Value value) {
    emitBytes(OP_CONSTANT, makeConstant(value));
}

static void patchJump(int offset) {
    int jump = currentChunk()->count - offset - 2;

    if (jump > UINT16_MAX) {
        error("Too much code to jump over", NULL);
    }

    currentChunk()->code[offset] = (jump >> 8) & 0xff;
    currentChunk()->code[offset + 1] = jump & 0xff;
}

static void patchJumpTo(int offset, int count) {
    int jump = count - offset - 2;

    if (jump > UINT16_MAX) {
        error("Too much code to jump over", NULL);
    }

    currentChunk()->code[offset] = (jump >> 8) & 0xff;
    currentChunk()->code[offset + 1] = jump & 0xff;
}

static void initCompiler(Compiler *compiler, FunctionType type, Token *name) {
    compiler->enclosing = current;
    compiler->function = NULL;
    compiler->type = type;
    compiler->localCount = 0;
    compiler->scopeDepth = 0;
    compiler->function = newFunction();
    current = compiler;

    if (type != TYPE_SCRIPT) {
        current->function->name = copyString(name->start, name->length);
    }

    Local *local = &current->locals[current->localCount++];
    local->depth = 0;
    local->isCaptured = false;
    if (type != TYPE_FUNCTION) {
        local->name.start = "this";
        local->name.length = 4;
    } else {
        local->name.start = "";
        local->name.length = 0;
    }
}

static ObjFunction *endCompiler() {
    emitReturn();
    ObjFunction *function = current->function;
#ifdef DEBUG_PRINT_CODE
    if (!hadError) {
        disassembleChunk(currentChunk(),
                         function->name != NULL ?
                         function->name->chars :
                         "<script>");
    }
#endif
    current = current->enclosing;
    return function;
}

static void beginScope() {
    current->scopeDepth++;
}

static void endScope() {
    current->scopeDepth--;

    while (current->localCount > 0 &&
           current->locals[current->localCount - 1].depth >
               current->scopeDepth) {
        if (current->locals[current->localCount - 1].isCaptured) {
            emitByte(OP_CLOSE_UPVALUE);
        } else {
            emitByte(OP_POP);
        }
        current->localCount--;
    }
}

static void expression(CstExpression *cst);
static void statement(CstStatement *cst);
static void declaration(CstDeclarationList *cst);
static ParseRule *getRule(TokenType type);
static void parsePrecedence(Precedence precedence);
static void function(CstFunction *cst, FunctionType type, Token name);

static uint8_t identifierConstant(Token *name) {
    return makeConstant(OBJ_VAL(copyString(name->start, name->length)));
}

static bool identifiersEqual(Token *a, Token *b) {
    if (a->length != b->length) return false;
    return memcmp(a->start, b->start, a->length) == 0;
}

static int resolveLocal(Compiler *compiler, Token *name) {
    for (int i = compiler->localCount - 1; i >= 0; i--) {
        Local *local = &compiler->locals[i];
        if (identifiersEqual(name, &local->name)) {
            if (local->depth == -1) {
                error("Can't read local variable in its own initializer", name);
            }
            return i;
        }
    }

    return -1;
}

static int addUpvalue(Compiler *compiler, uint8_t index, bool isLocal) {
    int upvalueCount = compiler->function->upvalueCount;

    for (int i = 0; i < upvalueCount; i++) {
        Upvalue *upvalue = &compiler->upvalues[i];
        if (upvalue->index == index && upvalue->isLocal == isLocal) {
            return i;
        }
    }

    if (upvalueCount == UINT8_COUNT) {
        error("Too many closure variables in function", NULL);
        return 0;
    }

    compiler->upvalues[upvalueCount].isLocal = isLocal;
    compiler->upvalues[upvalueCount].index = index;
    return compiler->function->upvalueCount++;
}

static int resolveUpvalue(Compiler *compiler, Token *name) {
    if (compiler->enclosing == NULL) return -1;

    int local = resolveLocal(compiler->enclosing, name);

    if (local != -1) {
        compiler->enclosing->locals[local].isCaptured = true;
        return addUpvalue(compiler, (uint8_t)local, true);
    }

    int upvalue = resolveUpvalue(compiler->enclosing, name);

    if (upvalue != -1) {
        return addUpvalue(compiler, (uint8_t)upvalue, false);
    }

    return -1;
}

static void addLocal(Token name) {
    if (current->localCount == UINT8_COUNT) {
        error("Too many local variables in function", &name);
        return;
    }

    Local *local = &current->locals[current->localCount++];
    local->name = name;
    local->depth = -1;
    local->isCaptured = false;
}

static void declareVariable(Token *name) {
    if (current->scopeDepth == 0) return;

    for (int i = current->localCount - 1; i >= 0; i--) {
        Local *local = &current->locals[i];
        if (local->depth != -1 && local->depth < current->scopeDepth) {
            break;
        }

        if (identifiersEqual(name, &local->name)) {
            error("Already a variable with this name in this scope", name);
        }
    }

    addLocal(*name);
}

static uint8_t parseVariable(Token name) {
    declareVariable(&name);

    if (current->scopeDepth > 0) return 0;

    return identifierConstant(&name);
}

static void markInitialized() {
    if (current->scopeDepth == 0) return;
    current->locals[current->localCount - 1].depth = current->scopeDepth;
}

static void defineVariable(uint8_t global) {
    if (current->scopeDepth > 0) {
        markInitialized();
        return;
    }

    emitBytes(OP_DEFINE_GLOBAL, global);
}

static uint8_t argumentList(CstExpressionList *cst) {
    if (cst == NULL) return (uint8_t)0;
    LINE(cst);
    expression(cst->expression);
    return (uint8_t)(1 + argumentList(cst->next));
}

static void and_(CstBinaryExpression *cst) {
    if (cst == NULL) cant_happen("null CstBinaryExpression (and_)");
    LINE(cst);

    expression(cst->left);
    int endJump = emitJump(OP_JUMP_IF_FALSE);
    emitByte(OP_POP);
    expression(cst->right);
    patchJump(endJump);
}

static void binary(CstBinaryExpression *cst, OpCode op) {
    if (cst == NULL) cant_happen("null CstBinaryExpression (binary)");
    LINE(cst);

    expression(cst->left);
    expression(cst->right);
    emitByte(op);
}


static void call(CstCallExpression *cst) {
    if (cst == NULL) cant_happen("null CstCallExpression (call)");
    LINE(cst);

    expression(cst->function);
    uint8_t argCount = argumentList(cst->arguments);
    emitBytes(OP_CALL, argCount);
}

static void dot(CstDotExpression *cst) {
    if (cst == NULL) cant_happen("null CstDotExpression (dot)");
    LINE(cst);

    expression(cst->left);
    uint8_t name = identifierConstant(&cst->property);

    if (cst->type == CST_DOT_ASSIGN) {
        expression(cst->action.value);
        emitBytes(OP_SET_PROPERTY, name);
    } else if (cst->type == CST_DOT_INVOKE) {
        uint8_t argCount = argumentList(cst->action.arguments);
        emitBytes(OP_INVOKE, name);
        emitByte(argCount);
    } else {
        emitBytes(OP_GET_PROPERTY, name);
    }
}

static void number(double value) {
    emitConstant(NUMBER_VAL(value));
}

static void or_(CstBinaryExpression *cst) {
    if (cst == NULL) cant_happen("null CstStringExpression (string)");
    LINE(cst);

    expression(cst->left);
    int elseJump = emitJump(OP_JUMP_IF_FALSE);
    int endJump = emitJump(OP_JUMP);
    patchJump(elseJump);
    emitByte(OP_POP);
    expression(cst->right);
    patchJump(endJump);
}

static void string(CstStringExpression *cst) {
    if (cst == NULL) cant_happen("null CstStringExpression (string)");
    LINE(cst);

    emitConstant(OBJ_VAL(copyString(cst->value.start + 1, cst->value.length - 2)));
}

static void namedVariable(Token name, CstExpression *cst) {
    uint8_t getOp, setOp;
    int arg = resolveLocal(current, &name);
    if (arg != -1) {
        getOp = OP_GET_LOCAL;
        setOp = OP_SET_LOCAL;
    } else if ((arg = resolveUpvalue(current, &name)) != -1) {
        getOp = OP_GET_UPVALUE;
        setOp = OP_GET_UPVALUE;
    } else {
        arg = identifierConstant(&name);
        getOp = OP_GET_GLOBAL;
        setOp = OP_SET_GLOBAL;
    }
    if (cst != NULL) {
        expression(cst);
        emitBytes(setOp, (uint8_t)arg);
    } else {
        emitBytes(getOp, (uint8_t)arg);
    }
}

static void variable(Token name) {
    namedVariable(name, NULL);
}

static void assign(CstAssignExpression *cst) {
    if (cst == NULL) cant_happen("null CstAssignExpression (assign)");
    LINE(cst);
    namedVariable(cst->variable, cst->value);
}

static Token syntheticToken(const char *text) {
    Token token;
    token.start = text;
    token.length = (int)strlen(text);
    return token;
}

static void checkClass(CstStringExpression *cst) { // arg for token for line etc.
    if (currentClass == NULL) {
        error("Can't use 'super' outside of a class", &cst->value);
    } else if (!currentClass->hasSuperclass) {
        error("Can't use 'super' in a class with no superclass", &cst->value);
    }
}

static void super_invoke(CstCallSuperExpression *cst) {
    if (cst == NULL) cant_happen("null CstCallExpression (super_invoke)");
    LINE(cst);
    uint8_t name = identifierConstant(&cst->methodName);
    namedVariable(syntheticToken("this"), false);
    uint8_t argCount = argumentList(cst->arguments);
    namedVariable(syntheticToken("super"), false);
    emitBytes(OP_SUPER_INVOKE, name);
    emitByte(argCount);
}

static void super_get(CstStringExpression *cst) {
    if (cst == NULL) cant_happen("null CstStringExpression (super_get)");
    LINE(cst);
    checkClass(cst);
    uint8_t name = identifierConstant(&cst->value);
    namedVariable(syntheticToken("this"), false);
    namedVariable(syntheticToken("super"), false);
    emitBytes(OP_GET_SUPER, name);
}

static void this_(CstStringExpression *cst) {
    if (cst == NULL) cant_happen("null CstStringExpression (this_)");
    LINE(cst);
    if (currentClass == NULL) {
        error("Can't use 'this' outside of a class", &cst->value);
        return;
    }
    variable(cst->value);
}

static void unary(CstExpression *cst, OpCode op) {
    if (cst == NULL) cant_happen("null CstExpression (unary)");
    LINE(cst);
    expression(cst);
    emitByte(op);
}

static void funExpr(CstFunction *cst) {
    if (cst == NULL) cant_happen("null CstFunction");
    LINE(cst);

    const char *name = "anon";
    Token token;
    token.type = TOKEN_IDENTIFIER;
    token.start = name;
    token.length = 4;
    token.line = line;
    markInitialized();
    function(cst, TYPE_FUNCTION, token);
}

static void expression(CstExpression *cst) {
    if (cst == NULL) cant_happen("null CstExpression (expression)");
    LINE(cst);

    switch(cst->type) {
        case CST_CALL_EXPR:
            call(cst->expression.call);
            break;
        case CST_DOT_EXPR:
            dot(cst->expression.dot);
            break;
        case CST_NEGATION_EXPR:
            unary(cst->expression.unary, OP_NEGATE);
            break;
        case CST_SUBTRACTION_EXPR:
            binary(cst->expression.binary, OP_SUBTRACT);
            break;
        case CST_ADDITION_EXPR:
            binary(cst->expression.binary, OP_ADD);
            break;
        case CST_DIVISION_EXPR:
            binary(cst->expression.binary, OP_DIVIDE);
            break;
        case CST_MULTIPLICATION_EXPR:
            binary(cst->expression.binary, OP_DIVIDE);
            break;
        case CST_CONS_EXPR:
            binary(cst->expression.binary, OP_CONS);
            break;
        case CST_APPEND_EXPR:
            binary(cst->expression.binary, OP_APPEND);
            break;
        case CST_NOT_EXPR:
            unary(cst->expression.unary, OP_NOT);
            break;
        case CST_NE_EXPR:
            binary(cst->expression.binary, OP_EQUAL);
            emitByte(OP_NOT);
            break;
        case CST_EQ_EXPR:
            binary(cst->expression.binary, OP_EQUAL);
            break;
        case CST_CDR_EXPR:
            unary(cst->expression.unary, OP_CDR);
            break;
        case CST_GT_EXPR:
            binary(cst->expression.binary, OP_GREATER);
            break;
        case CST_GE_EXPR:
            binary(cst->expression.binary, OP_LESS);
            emitByte(OP_NOT);
            break;
        case CST_CAR_EXPR:
            unary(cst->expression.unary, OP_CAR);
            break;
        case CST_LT_EXPR:
            binary(cst->expression.binary, OP_LESS);
            break;
        case CST_LE_EXPR:
            binary(cst->expression.binary, OP_GREATER);
            emitByte(OP_NOT);
            break;
        case CST_VAR_EXPR:
            variable(cst->expression.string->value);
            break;
        case CST_ASSIGN_EXPR:
            assign(cst->expression.assign);
            break;
        case CST_STRING_EXPR:
            string(cst->expression.string);
            break;
        case CST_NUMBER_EXPR:
            number(cst->expression.number);
            break;
        case CST_AND_EXPR:
            and_(cst->expression.binary);
            break;
        case CST_FALSE_EXPR:
            emitByte(OP_FALSE);
            break;
        case CST_FUN_EXPR:
            funExpr(cst->expression.function);
            break;
        case CST_NIL_EXPR:
            emitByte(OP_NIL);
            break;
        case CST_OR_EXPR:
            or_(cst->expression.binary);
            break;
        case CST_SUPER_GET_EXPR:
            super_get(cst->expression.string);
            break;
        case CST_SUPER_INVOKE_EXPR:
            super_invoke(cst->expression.callSuper);
            break;
        case CST_THIS_EXPR:
            this_(cst->expression.string);
            break;
        case CST_TRUE_EXPR:
            emitByte(OP_TRUE);
            break;
        default:
            cant_happen("unrecognised CstExpression type");
    }
}

static int arguments(CstArgumentList *cst) {
    if (cst == NULL) return 0;
    LINE(cst);

    uint8_t constant = parseVariable(cst->name);
    defineVariable(constant);

    return 1 + arguments(cst->next);
}

static void function(CstFunction *cst, FunctionType type, Token name) {
    if (cst == NULL) cant_happen("null CstFunction");
    LINE(cst);

    Compiler compiler;
    initCompiler(&compiler, type, &name);
    beginScope();

    current->function->arity = arguments(cst->arguments);
    declaration(cst->declarations);

    ObjFunction *function = endCompiler();
    emitBytes(OP_CLOSURE, makeConstant(OBJ_VAL(function)));

    for (int i = 0; i < function->upvalueCount; i++) {
        emitByte(compiler.upvalues[i].isLocal ? 1 : 0);
        emitByte(compiler.upvalues[i].index);
    }

}

static void method(CstMethodList *cst) {
    if (cst == NULL) return;
    LINE(cst);

    uint8_t constant = identifierConstant(&cst->name);
    FunctionType type = TYPE_METHOD;
    if (cst->name.length == 4 && memcmp(cst->name.start, "init", 4) == 0) {
        type = TYPE_INITIALIZER;
    }

    function(cst->function, type, cst->name);
    emitBytes(OP_METHOD, constant);
    method(cst->next);
}

static void classDeclaration(CstClassDeclaration *cst) {
    if (cst == NULL) cant_happen("null CstClassDeclaration");
    LINE(cst);

    Token className = cst->name;
    uint8_t nameConstant = identifierConstant(&className);
    declareVariable(&className);

    emitBytes(OP_CLASS, nameConstant);
    defineVariable(nameConstant);

    ClassCompiler classCompiler;
    classCompiler.enclosing = currentClass;
    classCompiler.hasSuperclass = false;
    currentClass = &classCompiler;

    if (cst->hasSuperclass) {
        Token superName = cst->superName;
        variable(superName);

        if (identifiersEqual(&className, &superName)) {
            error("A class can't inherit from itself", &superName);
        }

        beginScope();
        addLocal(syntheticToken("super"));
        defineVariable(0);

        namedVariable(className, false);
        emitByte(OP_INHERIT);
        classCompiler.hasSuperclass = true;
    }

    namedVariable(className, false);

    method(cst->methods);

    emitByte(OP_POP);

    if (classCompiler.hasSuperclass) {
        endScope();
    }

    currentClass = currentClass->enclosing;
}

static void funDeclaration(CstFunDeclaration *cst) {
    if (cst == NULL) cant_happen("null CstFunDeclaration");
    LINE(cst);

    uint8_t global = parseVariable(cst->name);
    markInitialized();
    function(cst->function, TYPE_FUNCTION, cst->name);
    defineVariable(global);
}

static void varDeclaration(CstVarDeclaration *cst) {
    if (cst == NULL) cant_happen("null CstVarDeclaration");
    LINE(cst);

    uint8_t global = parseVariable(cst->name);

    if (cst->initializer) {
        expression(cst->initializer);
    } else {
        emitByte(OP_NIL);
    }

    defineVariable(global);
}

static void expressionStatement(CstExpression *cst) {
    if (cst == NULL) cant_happen("null CstExpression statement");
    LINE(cst);

    expression(cst);
    emitByte(OP_POP);
}

static int caseStatements(CstDeclarationList *cst) {
    if (cst == NULL) return 0;
    LINE(cst);

    int start = currentChunk()->count;
    beginScope();
    declaration(cst);
    endScope();
    return start;
}


/*
 * semantics:
 *
 * switch (1) {
 *     case 1:          // implicit fall-through
 *     case 2:
 *         statements   // implicit break
 *     case 3:          // implicit fall-through
 *     default:
 *         statements
 * }
 */
static void switchStatement(CstSwitchStatement *cst) {
#define PATCH_CONTS_TO_STATEMENTS(s) \
    do { \
        while (contCount > 0) { \
            patchJumpTo(conts[--contCount], s); \
        } \
    } while(0)

    if (cst == NULL) cant_happen("null CstSwitchStatement");
    LINE(cst);

    int breaks[MAX_CASES]; // max 256 cases with statements (jumps to end)
    int breakCount = 0;
    int conts[MAX_CASES]; // max 256 consecutive cases with no statements (jumps to next statement)
    int contCount = 0;

    expression(cst->expression);                                          // [vala]

    CstCaseList *cases = cst->cases;

    while(cases != NULL) {
        if (!cases->isDefault) {                                  // [vala]
            emitByte(OP_DUP);                                     // [vala vala]
            expression(cases->expression);                        // [vala vala valb]
            emitByte(OP_EQUAL);                                   // [vala bool]
            int nextCase = emitJump(OP_JUMP_IF_FALSE);            // [vala bool] -> nextCase
            emitByte(OP_POP);                                     // [vala]
            int statements = caseStatements(cases->declarations); // [vala]      <- continue
            if (statements) {
                PATCH_CONTS_TO_STATEMENTS(statements);            // [vala]
                breaks[breakCount++] = emitJump(OP_JUMP);         // [vala]      -> break
            } else {
                conts[contCount++] = emitJump(OP_JUMP);           // [vala]      -> continue
            }
            patchJump(nextCase);                                  // [vala bool] <- nextCase
            emitByte(OP_POP);                                     // [vala]
        } else {                                                  // [vala]
            int statements = caseStatements(cases->declarations); // [vala]      <- continue
            if (statements) {
                PATCH_CONTS_TO_STATEMENTS(statements);            // [vala]
            }
        }
        cases = cases->next;
    }

    while (breakCount > 0) {
        patchJump(breaks[--breakCount]);                   // [vala]     <- break
    }

    while (contCount > 0) {
        patchJump(conts[--contCount]);                     // [vala]     <- continue
    }

    emitByte(OP_POP);                                      // []
#undef PATCH_CONTS_TO_STATEMENTS
}

static void forStatement(CstForStatement *cst) {
    if (cst == NULL) cant_happen("null CstForStatement");
    LINE(cst);

    beginScope();

    if (cst->init.expression != NULL) {
        if (cst->isDeclaration) {
            varDeclaration(cst->init.declaration);
        } else {
            expressionStatement(cst->init.expression);
        }
    }

    int loopStart = currentChunk()->count;
    int exitJump = -1;

    if(cst->test != NULL) {
        expression(cst->test);
        exitJump = emitJump(OP_JUMP_IF_FALSE);
        emitByte(OP_POP);
    }

    if (cst->update != NULL) {
        int bodyJump = emitJump(OP_JUMP);
        int incrementStart = currentChunk()->count;
        expression(cst->update);
        emitByte(OP_POP);
        emitLoop(loopStart);
        loopStart = incrementStart;
        patchJump(bodyJump);
    }

    statement(cst->body);
    emitLoop(loopStart);
    
    if (exitJump != -1) {
        patchJump(exitJump);
        emitByte(OP_POP);
    }

    endScope();
}

static void ifStatement(CstIfStatement *cst) {
    if (cst == NULL) cant_happen("null CstIfStatement");
    LINE(cst);

    expression(cst->expression);
    int thenJump = emitJump(OP_JUMP_IF_FALSE);
    emitByte(OP_POP);
    statement(cst->ifTrue);
    int elseJump = emitJump(OP_JUMP);
    patchJump(thenJump);
    emitByte(OP_POP);

    if (cst->ifFalse != NULL) statement(cst->ifFalse);
    patchJump(elseJump);
}

static void printStatement(CstExpression *cst) {
    if (cst == NULL) cant_happen("null CstExpression (printStatement)");
    LINE(cst);

    expression(cst);
    emitByte(OP_PRINT);
}

static void returnStatement(CstExpression *cst) {
    if (cst == NULL) cant_happen("null CstExpression (returnStatement)");
    LINE(cst);

    if (current->type == TYPE_SCRIPT) {
        error("Can't return from top-level code", NULL);
    }

    if (cst->type == CST_NIL_EXPR) {
        emitReturn();
    } else {
        if (current->type == TYPE_INITIALIZER) {
            error("Can't return a value from an initializer", NULL);
        }
        expression(cst);
        emitByte(OP_RETURN);
    }
}

static void whileStatement(CstConditionalStatement *cst) {
    if (cst == NULL) cant_happen("null CstConditionalStatement (whileStatement)");
    LINE(cst);

    int loopStart = currentChunk()->count;
    expression(cst->expression);
    int exitJump = emitJump(OP_JUMP_IF_FALSE);
    emitByte(OP_POP);
    statement(cst->statement);
    emitLoop(loopStart);
    patchJump(exitJump);
    emitByte(OP_POP);
}

static void doStatement(CstConditionalStatement *cst) {
    if (cst == NULL) cant_happen("null CstConditionalStatement (doStatement)");
    LINE(cst);

    int loopStart = currentChunk()->count;       // []     <- loopStart
    statement(cst->statement);                   // []
    expression(cst->expression);                 // [bool]
    int loopEnd = emitJump(OP_JUMP_IF_FALSE);    // [bool] -> loopEnd
    emitByte(OP_POP);                            // []
    emitLoop(loopStart);                         // []     -> loopStart
    patchJump(loopEnd);                          // [bool] <- loopEnd
    emitByte(OP_POP);                            // []
}

static void declaration(CstDeclarationList *cst) {
    if (cst == NULL) return;
    LINE(cst);

    switch (cst->type) {
        case CST_CLASS_DECLARATION:
            classDeclaration(cst->declaration.classDeclaration);
            break;
        case CST_FUN_DECLARATION:
            funDeclaration(cst->declaration.funDeclaration);
            break;
        case CST_VAR_DECLARATION:
            varDeclaration(cst->declaration.varDeclaration);
            break;
        case CST_STATEMENT_DECLARATION:
            statement(cst->declaration.statement);
            break;
        default:
            cant_happen("unrecognised CstDeclarationList type");
    }

    declaration(cst->next);
}

static void statement(CstStatement *cst) {
    if (cst == NULL) cant_happen("null CstStatement");
    LINE(cst);

    switch (cst->type) {
        case CST_PRINT_STATEMENT:
            printStatement(cst->statement.expression);
            break;
        case CST_SWITCH_STATEMENT:
            switchStatement(cst->statement.switchStatement);
            break;
        case CST_FOR_STATEMENT:
            forStatement(cst->statement.forStatement);
            break;
        case CST_IF_STATEMENT:
            ifStatement(cst->statement.ifStatement);
            break;
        case CST_RETURN_STATEMENT:
            returnStatement(cst->statement.expression);
            break;
        case CST_WHILE_STATEMENT:
            whileStatement(cst->statement.conditionalStatement);
            break;
        case CST_DO_STATEMENT:
            doStatement(cst->statement.conditionalStatement);
            break;
        case CST_BLOCK_STATEMENT:
            beginScope();
            declaration(cst->statement.blockStatement);
            endScope();
            break;
        case CST_EXPRESSION_STATEMENT:
            expressionStatement(cst->statement.expression);
            break;
        default:
            cant_happen("unrecognised CstStatement type");
    }
}

ObjFunction *compile(const char *source) {
    CstDeclarationList *cst = parse(source);

    if (cst == NULL) {
        return NULL;
    }
#ifdef DEBUG_PRINT_TREE
    printCstDeclarationList(cst, 0);
    printf("\n");
#endif

    Compiler compiler;
    initCompiler(&compiler, TYPE_SCRIPT, NULL);
    hadError = false;
    line = 0;

    declaration(cst);

    ObjFunction *result = endCompiler();
    if (hadError) return NULL;
    return result;
}

void markCompilerRoots() {
    Compiler *compiler = current;
    while (compiler != NULL) {
        markObject((Obj *)compiler->function);
        compiler = compiler->enclosing;
    }
}
