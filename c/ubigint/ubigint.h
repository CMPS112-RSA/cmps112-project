#ifndef UBIGINT_H
#define UBIGINT_H

#include <stdint.h>
#include <stdlib.h>

#ifdef __cplusplus
extern "C" {
#else
struct ubigint_t;
typedef struct ubigint_t ubigint_t;
#endif

typedef ubigint_t* ubigint_handle_t;

int new_ubigint(
    ubigint_handle_t* handle_ptr
);

int new_ubigint_from_num(
    ubigint_handle_t* handle_ptr,
    unsigned long num
);

int new_ubigint_from_string(
    ubigint_handle_t* handle_ptr,
    const char* str
);

int new_ubigint_from_binary(
    ubigint_handle_t* handle_ptr,
    uint8_t* binary,
    size_t num_bytes
);

int free_ubigint(
    ubigint_handle_t* handle_ptr
);

int print_ubigint(
    ubigint_handle_t num
);

int ubigint_modulus(
    ubigint_handle_t num1,
    ubigint_handle_t num2,
    ubigint_handle_t result
);

int ubigint_pow(
    ubigint_handle_t num1,
    ubigint_handle_t num2,
    ubigint_handle_t result
);

#ifdef __cplusplus
}
#endif

#endif /* UBIGINT_H */
