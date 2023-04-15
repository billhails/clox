#ifndef clox_parser_h
#define clox_parser_h

#include "cst.h"

CstDeclarationList *parse(const char *source);

#define MAX_CASES 256

#endif
