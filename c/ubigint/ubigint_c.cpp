#define UBIGINT_CPP

#include "ubigint.hpp"

typedef struct {
    ubigint cpp;
} ubigint_t;

#include "ubigint.h"

void new_ubigint(
    ubigint_handle_t* handle_ptr
) {
    (*handle_ptr) = new ubigint_t;
}

void new_ubigint_from_num(
    ubigint_handle_t* handle_ptr,
    unsigned long num
) {
    (*handle_ptr) = new ubigint_t;
    (*handle_ptr)->cpp = ubigint(num);
}

void new_ubigint_from_string(
    ubigint_handle_t* handle_ptr,
    const char* str
) {
    (*handle_ptr) = new ubigint_t;
    (*handle_ptr)->cpp = ubigint(str);
}

void free_ubigint(
    ubigint_handle_t* handle_ptr
) {
    delete (*handle_ptr);
    (*handle_ptr) = NULL;
}
