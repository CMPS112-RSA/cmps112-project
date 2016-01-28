#ifndef CMPS112_RSA_H
#define CMPS112_RSA_H

#include <stdint.h>
#include <stdlib.h>

#include "ubigint/ubigint.h"

typedef uint8_t* buffer_t;

void encrypt_message(
    ubigint_handle_t key_n,
    ubigint_handle_t key_e,
    buffer_t message_in,
    buffer_t* message_out,
    size_t message_len
);

void decrypt_message(
    ubigint_handle_t key_n,
    ubigint_handle_t key_d,
    buffer_t message_in,
    buffer_t* message_out,
    size_t message_len
);

#endif /* CMPS112_RSA_H */
