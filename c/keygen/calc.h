#ifndef __KEYGEN_CALC_H__
#define __KEYGEN_CALC_H__

#include <gmp.h>

#include <stdint.h>

/*
 * Returns a prime number in the range (1000000,2^32-1).
 */
void get_prime(mpz_t out);

// n = p * q
static inline void get_n(mpz_t out, mpz_t p, mpz_t q) {
    mpz_mul(out, p, q);
}

// totient = (p - 1)(q - 1)
void get_totient(mpz_t out, mpz_t p, mpz_t q);

/*
 * 1 < e < totient
 * gcd(e, totient) = 1
 */
void get_e(mpz_t out, mpz_t totient);

// (d)(e) % totient = 1
static inline void get_d(mpz_t out, mpz_t e, mpz_t totient) {
    mpz_invert(out, e, totient);
}

#endif /* __KEYGEN_CALC_H__ */
