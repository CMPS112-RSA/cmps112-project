#include <rsa-lib/rsa.h>

#include <gmp.h>

#include <fcntl.h>
#include <inttypes.h>
#include <math.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <unistd.h>

// https://primes.utm.edu/lists/small/1000.txt
static const uint32_t smallest_primes[] = {
    2,3,5,7,11,13,17,19,23,29,
    31,37,41,43,47,53,59,61,67,71,
    73,79,83,89,97,101,103,107,109,113,
    127,131,137,139,149,151,157,163,167,173,
    179,181,191,193,197,199,211,223,227,229,
    233,239,241,251,257,263,269,271,277,281,
    283,293,307,311,313,317,331,337,347,349,
    353,359,367,373,379,383,389,397,401,409,
    419,421,431,433,439,443,449,457,461,463,
    467,479,487,491,499,503,509,521,523,541
};

static bool is_prime(const uint32_t num) {
    // See if this is factorable by the smallest 100 primes
    for(size_t i = 0; i < 100; i++) {
        if(num % smallest_primes[i] == 0) {
            return false;
        }
    }

    // Only check odd numbers for primality
    uint32_t start_num = num / 2;
    if(start_num % 2 == 0) {
        start_num--;
    }
    for(uint32_t i = start_num; i > 1; i -= 2) {
        if(num % i == 0) {
            return false;
        }
    }

    return true;
}

/*
 * Based on: http://stackoverflow.com/a/7921017
 *
 * /dev/urandom isn't as secure as /dev/random, but it's faster and
 * works for our purposes.
 */
static uint32_t get_random_number() {
    int fd;
    uint32_t ret = 0;

    if((fd = open("/dev/urandom", O_RDONLY)) == -1) {
        fprintf(stderr, "failed to open /dev/urandom. Using rand() instead...");
        srand(time(NULL));
        return rand();
    }
    while(ret < 10000000) {
        read(fd, &ret, 4);
    }
    close(fd);

    return ret;
}

static inline uint32_t get_random_prime_number() {
    uint32_t ret = 0;
    do {
        ret = get_random_number();
    } while(!is_prime(ret));

    return ret;
}

// 1 < e < totient
// gcd(e, totient) = 1
void calculate_e(mpz_t e, mpz_t totient) {
    mpz_t gcd;
    mpz_init(gcd);

    mpz_set_ui(e, 0);
    while(true) {
        mpz_set_ui(e, get_random_number());
        if(mpz_cmp(e, totient) >= 0) {
            continue;
        }

        mpz_gcd(gcd, e, totient);
        if(mpz_cmp_ui(gcd, 1) == 0) {
            break;
        }
    }

    // Cleanup
    mpz_clear(gcd);
}

// (d)(e) % totient = 1
void calculate_d(mpz_t d, mpz_t e, mpz_t totient) {
    mpz_t i1, i2; // Intermediates
    mpz_init(i1);
    mpz_init(i2);

    while(true) {
        mpz_set_ui(d, get_random_number());
        mpz_mul(i1, d, e); // i1 = d * e
        mpz_mod(i2, i1, totient); // i2 = i1 % totient
        if(mpz_cmp_ui(d, 1) == 0) {
            break;
        }
    }

    // Cleanup
    mpz_clear(i2);
    mpz_clear(i1);
}

int main(int argc, char** argv) {
    (void)argc;
    (void)argv;

    // So we can output without a newline
    setbuf(stdout, NULL);

    mpz_t p, q, n, totient, e, d;

    // p is a large prime
    printf("Generating 32-bit p...");
    mpz_init_set_ui(p, get_random_prime_number());
    printf("done.\n");

    // q is a large prime
    printf("Generating 32-bit q...");
    mpz_init_set_ui(q, get_random_prime_number());
    printf("done.\n");

    // n = p * q
    printf("Generating n...");
    mpz_init(n);
    mpz_mul(n, p, q); // n = p * q
    printf("done.\n");

    // totient = (p - 1) * (q - 1)
    printf("Calculating totient...");
    mpz_init(totient);
    mpz_sub_ui(p, p, 1);    // p = p - 1
    mpz_sub_ui(q, q, 1);    // q = q - 1
    mpz_mul(totient, p, q); // totient = p * q
    printf("done.\n");

    printf("Calculating e...");
    mpz_init(e);
    calculate_e(e, totient);
    printf("done.\n");

    printf("Calculating d...");
    mpz_init(d);
    calculate_d(d, e, totient);
    printf("done.\n");

    mpz_clear(d);
    mpz_clear(e);
    mpz_clear(totient);
    mpz_clear(n);
    mpz_clear(q);
    mpz_clear(p);

    return 0;
}
