#include "BigIntegerLibrary.hh"

typedef struct {
    BigUnsigned cpp;
} bigint_t;

#include "BigInteger_c.h"

#define CPP_TO_C(...) \
    try { __VA_ARGS__ } \
    catch(...) { return 1; } \
    return 0;

int new_bigint_from_ulong(
    bigint_handle_t* handle_ptr,
    unsigned long x
) {
    CPP_TO_C(
        (*handle_ptr) = new bigint_t;
        (*handle_ptr)->cpp = BigUnsigned(x);
    )
}

int new_bigint_from_string(
    bigint_handle_t* handle_ptr,
    const char* s
) {
    CPP_TO_C(
        (*handle_ptr) = new bigint_t;
        (*handle_ptr)->cpp = stringToBigUnsigned(s);
    )
}

int new_bigint_from_binary(
    bigint_handle_t* handle_ptr,
    const uint8_t* data,
    size_t length
) {
    CPP_TO_C(
        (*handle_ptr) = new bigint_t;
        (*handle_ptr)->cpp = dataToBigInteger<uint8_t>(data, length, BigInteger::positive).getMagnitude();
    )
}

int free_bigint(
    bigint_handle_t* handle_ptr
) {
    CPP_TO_C(
        free(*handle_ptr);
        (*handle_ptr) = NULL;
    )
}

int bigint_modexp(
    bigint_handle_t base,
    bigint_handle_t exponent,
    bigint_handle_t modulus,
    bigint_handle_t result
) {
    CPP_TO_C(
        BigInteger signedBase(base->cpp, BigInteger::negative);
        result->cpp = modexp(signedBase, exponent->cpp, modulus->cpp);
    )
}
