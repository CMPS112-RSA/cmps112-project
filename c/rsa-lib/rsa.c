#include "rsa.h"

void rsa_init_private_key(
    const char* filename,
    rsa_private_key_t* key
) {
    mpz_set_str(key->e, "5905153727", 10);
    mpz_set_str(key->n, "9192668113", 10);
}

void rsa_init_public_key(
    const char* filename,
    rsa_public_key_t* key
) {
    mpz_set_str(key->d, "7422006203", 10);
}

void rsa_free_private_key(
    rsa_private_key_t* key
) {
    mpz_clear(key->e);
    mpz_clear(key->n);
}

void rsa_free_public_key(
    rsa_public_key_t* key
) {
    mpz_clear(key->d);
}
