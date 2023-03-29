#ifndef clox_value_h
#define clox_value_h

#include "common.h"

typedef struct Obj Obj;
typedef struct ObjString ObjString;

#ifdef NAN_BOXING

typedef union {
    uint64_t bits;
    double number;
} Value;

#define SIGN_BIT ((uint64_t)0x8000000000000000)
#define QNAN     ((uint64_t)0x7ffc000000000000)
#define TAG_NIL 1
#define TAG_FALSE 2
#define TAG_TRUE 3


#define IS_BOOL(val)    (((val).bits | 1) == TRUE_VAL.bits)
#define IS_NIL(val)     ((val).bits == NIL_VAL.bits)
#define IS_NUMBER(val)  (((val).bits & QNAN) != QNAN)
#define IS_OBJ(val)     (((val).bits & (QNAN | SIGN_BIT)) == (QNAN | SIGN_BIT))

#define AS_BOOL(val)    ((val).bits == TRUE_VAL.bits)
#define AS_NUMBER(val)  ((val).number)
#define AS_OBJ(val)     ((Obj *)(uintptr_t)((val).bits & ~(SIGN_BIT | QNAN)))

#define BOOL_VAL(b)     ((b) ? TRUE_VAL : FALSE_VAL)
#define TRUE_VAL        ((Value){.bits = (uint64_t)(QNAN | TAG_TRUE)})
#define FALSE_VAL       ((Value){.bits = (uint64_t)(QNAN | TAG_FALSE)})
#define NIL_VAL         ((Value){.bits = (uint64_t)(QNAN | TAG_NIL)})
#define NUMBER_VAL(num) ((Value){.number = (num)})
#define OBJ_VAL(obj)    ((Value){.bits = (SIGN_BIT | QNAN | (uint64_t)(uintptr_t)(obj))})

#else

typedef enum {
    VAL_BOOL,
    VAL_NIL,
    VAL_NUMBER,
    VAL_OBJ
} ValueType;

typedef struct {
    ValueType type;
    union {
        bool boolean;
        double number;
        Obj *obj;
    } as;
} Value;

#define IS_BOOL(value)   ((value).type == VAL_BOOL)
#define IS_NIL(value)    ((value).type == VAL_NIL)
#define IS_NUMBER(value) ((value).type == VAL_NUMBER)
#define IS_OBJ(value)    ((value).type == VAL_OBJ)
 
#define AS_BOOL(value)   ((value).as.boolean)
#define AS_NUMBER(value) ((value).as.number)
#define AS_OBJ(value)    ((value).as.obj)

#define BOOL_VAL(value)   ((Value){VAL_BOOL,   {.boolean = value}})
#define NIL_VAL           ((Value){VAL_NIL,    {.number = 0}})
#define NUMBER_VAL(value) ((Value){VAL_NUMBER, {.number = value}})
#define OBJ_VAL(object)   ((Value){VAL_OBJ,    {.obj = (Obj *)object}})

#endif

typedef struct {
    int capacity;
    int count;
    Value *values;
} ValueArray;

bool valuesEqual(Value a, Value b);
void initValueArray(ValueArray *array);
void writeValueArray(ValueArray *array, Value value);
void freeValueArray(ValueArray *array);
void printValue(Value value);

#endif
