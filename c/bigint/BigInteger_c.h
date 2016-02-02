#ifndef BIGINTEGER_C_H
#define BIGINTEGER_C_H

#include <stdint.h>
#include <stdlib.h>

#ifdef __cplusplus
extern "C" {
#else
struct bigint_t;
typedef struct bigint_t bigint_t;
#endif

typedef bigint_t* bigint_handle_t;

int new_bigint_from_ulong(
    bigint_handle_t* handle_ptr,
    unsigned long x
);

int new_bigint_from_string(
    bigint_handle_t* handle_ptr,
    const char* s
);

int new_bigint_from_binary(
    bigint_handle_t* handle_ptr,
    const uint8_t* data,
    size_t length
);

int free_bigint(
    bigint_handle_t* handle_ptr
);

int bigint_modexp(
    bigint_handle_t base,
    bigint_handle_t exponent,
    bigint_handle_t modulus,
    bigint_handle_t result
);

#ifdef __cplusplus
}
#endif

#endif /* BIGINTEGER_C_H */
