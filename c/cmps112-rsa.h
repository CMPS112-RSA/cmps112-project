#ifndef CMPS112_RSA_H
#define CMPS112_RSA_H

#include <stdint.h>

#define RSA_KEY_SIZE_BITS 2048
#define RSA_KEY_SIZE_BYTES (RSA_KEY_SIZE_BITS / 8)

typedef struct {
    uint8_t key[RSA_KEY_SIZE_BYTES];
} rsa_key_t;

void opengpg_file_to_key(
    const char* filename,
    rsa_key_t* key_out
);

#endif /* CMPS112_RSA_H */
