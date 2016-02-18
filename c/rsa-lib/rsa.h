#ifndef __CMPS112_RSA_H__
#define __CMPS112_RSA_H__

#include <gmp.h>

typedef struct {
    mpz_t n;
    mpz_t e;
} rsa_private_key_t;

typedef struct {
    mpz_t n;
    mpz_t d;
} rsa_public_key_t;

/*
 * Read a private key from a file into our C struct.
 *
 * This function allocates memory, so the key must be passed
 * into rsa_free_private_key to free the memory.
 */
int rsa_init_private_key(
    const char* filename,
    rsa_private_key_t* key
);

/*
 * Read a public key from a file into our C struct.
 *
 * This function allocates memory, so the key must be passed
 * into rsa_free_public_key to free the memory.
 */
int rsa_init_public_key(
    const char* filename,
    rsa_public_key_t* key
);

/*
 * Free the memory allocated by rsa_init_private_key.
 */
void rsa_free_private_key(
    rsa_private_key_t* key
);

/*
 * Free the memory allocated by rsa_init_public_key.
 */
void rsa_free_public_key(
    rsa_public_key_t* key
);

/*
 * Write the given private key to a file. This file will
 * be able to passed into other programs that use the same
 * standard.
 */
int rsa_write_private_key(
    const char* filename,
    rsa_private_key_t* key
);

/*
 * Write the given public key to a file. This file will
 * be able to passed into other programs that use the same
 * standard.
 */
int rsa_write_public_key(
    const char* filename,
    rsa_public_key_t* key
);

#endif /* __CMPS112_RSA_H__ */
