#ifndef __CRYPT_H__
#define __CRYPT_H__

#include "rsa-lib/rsa.h"

int encrypt_message(
    const char* input_filename,
    const char* output_filename,
    rsa_private_key_t* key
);

int decrypt_message(
    const char* input_filename,
    const char* output_filename,
    rsa_public_key_t* key
);

#endif /* __CRYPT_H__ */
