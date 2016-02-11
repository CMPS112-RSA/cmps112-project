#include "calc.h"

#include <rsa-lib/rsa.h>

#include <gmp.h>

#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

int main(int argc, char** argv) {
    (void)argc;
    (void)argv;

    // So we can output without a newline
    setbuf(stdout, NULL);

    rsa_private_key_t privkey;
    mpz_init(privkey.e);
    mpz_init(privkey.n);

    rsa_public_key_t pubkey;
    mpz_init(pubkey.d);

    mpz_t p, q, totient;
    mpz_init(p);
    mpz_init(q);
    mpz_init(totient);

    printf("Generating p...");
    get_prime(p);
    gmp_printf("%Zd.\n", p);

    printf("Generating q...");
    get_prime(q);
    gmp_printf("%Zd.\n", q);

    printf("Generating n...");
    get_n(privkey.n, p, q);
    gmp_printf("%Zd.\n", privkey.n);

    printf("Calculating totient...");
    get_totient(totient, p, q);
    gmp_printf("%Zd.\n", totient);

    printf("Calculating e...");
    get_e(privkey.e, totient);
    gmp_printf("%Zd.\n", privkey.e);

    printf("Calculating d...");
    get_d(pubkey.d, privkey.e, totient);
    gmp_printf("%Zd.\n", pubkey.d);

    mpz_clear(totient);
    mpz_clear(q);
    mpz_clear(p);

    mpz_clear(pubkey.d);
    mpz_clear(privkey.n);
    mpz_clear(privkey.e);

    return 0;
}
