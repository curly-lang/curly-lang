#ifndef CURLY64_H
#define CURLY64_H
#include <inttypes.h>
#include <stdio.h>
#include <stdlib.h>

typedef double float_t;
typedef int64_t int_t;
typedef uint64_t word_t;

typedef struct {
    unsigned int refc;
    void* func;
    void* wrapper;
    unsigned int arity;
    unsigned int argc;
    char (**cleaners)(void*);
    void** args;
} func_t;

typedef union {
    float_t d;
    void* v;
} double_wrapper_t;

void free(void*);

char force_free_func(void* _func);

char free_func(func_t* func);

char refc_func(func_t* func);

void copy_func(func_t* dest, func_t* source);

func_t* copy_func_arg(func_t* source);

#endif
