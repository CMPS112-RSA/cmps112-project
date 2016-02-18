#include "calc.h"

#include <rsa-lib/rsa.h>

#include <gmp.h>

#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

int main(int argc, char** argv) {
    (void)argc;
    (void)argv;

    int status_code = 0;

    if(argc != 3) {
        fprintf(stderr, "Usage: exec [privkey filename] [pubkey filename]\n");
        return 1;
    }

    const char* privkey_filename = argv[1];
    const char* pubkey_filename = argv[2];;

    // So we can output without a newline
    setbuf(stdout, NULL);

    rsa_private_key_t privkey;
    mpz_init(privkey.n);
    mpz_init(privkey.e);

    rsa_public_key_t pubkey;
    mpz_init(pubkey.n);
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
    get_n(pubkey.n, p, q);
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

    if(rsa_write_private_key(privkey_filename, &privkey)) {
        fprintf(stderr, "Failed to write private key file.\n");
        status_code = 1;
        goto gmp_num_cleanup;
    }
    if(rsa_write_public_key(pubkey_filename, &pubkey)) {
        fprintf(stderr, "Failed to write public key file.\n");
        status_code = 1;
    }

    gmp_num_cleanup:
        mpz_clear(totient);
        mpz_clear(q);
        mpz_clear(p);
        mpz_clear(pubkey.d);
        mpz_clear(pubkey.n);
        mpz_clear(privkey.e);
        mpz_clear(privkey.n);

    return 0;
}
