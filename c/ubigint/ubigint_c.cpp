#define UBIGINT_CPP

#include "libfns.hpp"
#include "ubigint.hpp"

typedef struct {
    ubigint cpp;
} ubigint_t;

#include "ubigint.h"

#include <iostream>

#define CPP_TO_C(...) \
    try { __VA_ARGS__ } \
    catch(...) { \
        return 1; \
    } \
    return 0;

int new_ubigint(
    ubigint_handle_t* handle_ptr
) {
    CPP_TO_C(
        (*handle_ptr) = new ubigint_t;
    )
}

int new_ubigint_from_num(
    ubigint_handle_t* handle_ptr,
    unsigned long num
) {
    CPP_TO_C(
        (*handle_ptr) = new ubigint_t;
        (*handle_ptr)->cpp = ubigint(num);
    )
}

int new_ubigint_from_string(
    ubigint_handle_t* handle_ptr,
    const char* str
) {
    CPP_TO_C(
        (*handle_ptr) = new ubigint_t;
        (*handle_ptr)->cpp = ubigint(str);
    )
}

static const ubigint _256(256);

int new_ubigint_from_binary(
    ubigint_handle_t* handle_ptr,
    uint8_t* binary,
    size_t num_bytes
) {
    CPP_TO_C(
        (*handle_ptr) = new ubigint_t;

        for(size_t i = 0; i < num_bytes; i++) {
            ubigint mult = upow(_256, ubigint(i));
            (*handle_ptr)->cpp = (*handle_ptr)->cpp + (mult * ubigint(binary[(num_bytes-1)-i]));
        }
    )
}

int free_ubigint(
    ubigint_handle_t* handle_ptr
) {
    CPP_TO_C(
        delete (*handle_ptr);
        (*handle_ptr) = NULL;
    )
}

int print_ubigint(
    ubigint_handle_t num
) {
    CPP_TO_C(
        std::cout << num->cpp << std::endl;
    )
}

int ubigint_modulus(
    ubigint_handle_t num1,
    ubigint_handle_t num2,
    ubigint_handle_t result
) {
    CPP_TO_C(
        result->cpp = num1->cpp % num2->cpp;
    )
}

int ubigint_pow(
    ubigint_handle_t num1,
    ubigint_handle_t num2,
    ubigint_handle_t result
) {
    CPP_TO_C(
        result->cpp = upow(num1->cpp, num2->cpp);
    )
}
