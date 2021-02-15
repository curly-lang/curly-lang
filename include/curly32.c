#include "curly32.h"

char force_free_func(void* _func) {
    // func_t* func = (func_t*) _func;
    // for (int i = 0; i < func->argc; i++) {
        // if (func->cleaners[i] != (void*) 0 && func->cleaners[i](func->args[i]))
            // free(func->args[i]);
    // }

    // free(func->args);
    // free(func->cleaners);
    return (char) 1;
}

char free_func(func_t* func) {
    // if (func->refc == 0) {
    //    return force_free_func(func);
    //}

    return (char) 0;
}

char refc_func(func_t* func) {
    // if (func->refc > 0)
        // func->refc--;
    return free_func(func);
}

void copy_func(func_t* dest, func_t* source) {
    dest->refc = 0;
    dest->func = source->func;
    dest->wrapper = source->wrapper;
    dest->arity = source->arity;
    dest->argc = source->argc;

    if (dest->argc != 0)
    {
        dest->cleaners = calloc(dest->arity, sizeof(void*));
        dest->args = calloc(dest->arity, sizeof(void*));
    } else
    {
        dest->cleaners = (void*) 0;
        dest->args = (void*) 0;
    }

    for (int i = 0; i < dest->argc; i++) {
        dest->cleaners[i] = source->cleaners[i];
        dest->args[i] = source->args[i];
    }
}

func_t* copy_func_arg(func_t* source) {
    func_t* dest = malloc(sizeof(func_t));
    copy_func(dest, source);
    return dest;
}

