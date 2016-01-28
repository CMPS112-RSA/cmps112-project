#ifndef CMPS112_RSA_H
#define CMPS112_RSA_H

#include <stdint.h>
#include <stdlib.h>

#include "ubigint/ubigint.h"

#define RSA_KEY_SIZE_BITS 2048
#define RSA_KEY_SIZE_BYTES (RSA_KEY_SIZE_BITS / 8)

void encrypt_message(
    ubigint_handle_t key_n,
    ubigint_handle_t key_e,
    uint8_t* message_in,
    uint8_t* message_out,
    size_t message_len
);

void decrypt_message(
    ubigint_handle_t key_n,
    ubigint_handle_t key_d,
    uint8_t* message_in,
    uint8_t* message_out,
    size_t message_len
);

#endif /* CMPS112_RSA_H */
