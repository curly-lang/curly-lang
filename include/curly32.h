typedef float float_t;
typedef int int_t;
typedef unsigned int word_t;

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

int printf(const char*, ...);

void* calloc(word_t, word_t);

void* malloc(word_t);

void free(void*);

char force_free_func(void* _func);

char free_func(func_t* func);

char refc_func(func_t* func);

void copy_func(func_t* dest, func_t* source);

func_t* copy_func_arg(func_t* source);

