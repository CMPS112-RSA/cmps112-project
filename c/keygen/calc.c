#include "calc.h"

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
    for(uint32_t i = start_num; (i > 1 && i <= start_num); i -= 2) {
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

void get_prime(mpz_t out) {
    uint32_t num = 0;
    do {
        num = get_random_number();
    } while(!is_prime(num));

    mpz_set_ui(out, num);
}

// totient = (p-1)(q-1)
void get_totient(mpz_t out, mpz_t p, mpz_t q) {
    mpz_t p2, q2;
    mpz_init(p2);
    mpz_init(q2);

    mpz_sub_ui(p2, p, 1); // p2 = p - 1
    mpz_sub_ui(q2, q, 1); // q2 = q - 1
    mpz_mul(out, p2, q2); // totient = p * q

    // Cleanup
    mpz_clear(q2);
    mpz_clear(p2);
}

// 1 < e < totient
// gcd(e,totient) = 1
void get_e(mpz_t out, mpz_t totient) {
    mpz_t gcd;
    mpz_init(gcd);

    while(true) {
        mpz_set_ui(out, get_random_number());
        if(mpz_cmp(out, totient) >= 0) {
            continue;
        }

        mpz_gcd(gcd, out, totient);
        if(mpz_cmp_ui(gcd, 1) == 0) {
            break;
        }
    }

    // Cleanup
    mpz_clear(gcd);
}