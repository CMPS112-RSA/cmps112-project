#include "BigIntegerLibrary.hh"

typedef struct {
    BigUnsigned cpp;
} bigint_t;

#include "BigInteger_c.h"

void new_bigint_from_ulong(
    bigint_handle_t* handle_ptr,
    unsigned long x
) {
    (*handle_ptr) = new bigint_t;
    (*handle_ptr)->cpp = BigUnsigned(x);
}

void new_bigint_from_string(
    bigint_handle_t* handle_ptr,
    const char* s
) {
    (*handle_ptr) = new bigint_t;
    (*handle_ptr)->cpp = stringToBigUnsigned(s);
}

void new_bigint_from_binary(
    bigint_handle_t* handle_ptr,
    const uint8_t* data,
    size_t length
) {
    (*handle_ptr) = new bigint_t;
    (*handle_ptr)->cpp = dataToBigInteger<uint8_t>(data, length, BigInteger::positive).getMagnitude();
}

void free_bigint(
    bigint_handle_t* handle_ptr
) {
    free(*handle_ptr);
    (*handle_ptr) = NULL;
}

void bigint_modexp(
    bigint_handle_t base,
    bigint_handle_t exponent,
    bigint_handle_t modulus,
    bigint_handle_t result
) {
    BigInteger signedBase(base->cpp, BigInteger::negative);
    result->cpp = modexp(signedBase, exponent->cpp, modulus->cpp);
}
