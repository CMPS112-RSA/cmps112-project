#ifndef __CMPS112_RSA_H__
#define __CMPS112_RSA_H__

#include <gmp.h>

typedef struct {
    mpz_t e;
    mpz_t n;
} rsa_private_key_t;

typedef struct {
    mpz_t d;
} rsa_public_key_t;

void rsa_init_private_key(
    const char* filename,
    rsa_private_key_t* key
);

void rsa_init_public_key(
    const char* filename,
    rsa_public_key_t* key
);

void rsa_free_private_key(
    rsa_private_key_t* key
);

void rsa_free_public_key(
    rsa_public_key_t* key
);

#endif /* __CMPS112_RSA_H__ */
