#ifndef clox_common_h
#define clox_common_h

#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>

#define NAN_BOXING
// #define DEBUG_DUMP_CORE
// #define DEBUG_LOG_GC
// #define DEBUG_PRINT_CODE
// #define DEBUG_PRINT_TREE
// #define DEBUG_SCANNER
// #define DEBUG_STRESS_GC
// #define DEBUG_TRACE_EXECUTION

#define UINT8_COUNT (UINT8_MAX + 1)

void cant_happen(const char *msg);

#endif
