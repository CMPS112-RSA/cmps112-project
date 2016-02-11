#include <rsa-lib/rsa.h>

#include <fcntl.h>
#include <inttypes.h>
#include <math.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <unistd.h>

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

// There has to be a way to make this faster
static bool is_prime(const uint32_t num) {
    // See if this is factorable by the smallest 100 primes
    printf("is_prime: testing %" PRIu32 "\n", num);
    for(size_t i = 0; i < 100; i++) {
        if(num % smallest_primes[i] == 0) {
            printf("Factorable by %" PRIu32 "\n", smallest_primes[i]);
            return false;
        }
    }

    uint32_t start_num = num / 2;
    if(start_num % 2 == 0) {
        start_num--;
    }
    // This check gets around the issue of underflow always being > 0
    for(uint32_t i = start_num; i < num; i -= 2) {
        if(num % i == 0) {
            printf("Factorable by %" PRIu32 "\n", i);
            return false;
        }
    }

    printf("Success!\n");
    return true;
}

// http://stackoverflow.com/a/7921017
static uint32_t get_random_number() {
    int fd;
    uint32_t ret = 0;

    if((fd = open("/dev/urandom", O_RDONLY)) == -1) {
        fprintf(stderr, "Failed to open /dev/urandom. Using rand() instead.\n");
        srand(time(NULL));
        return rand();
    }
    while(ret < 10001) {
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

int main(int argc, char** argv) {
    (void)argc;
    (void)argv;

    uint32_t p = get_random_prime_number();
    printf("p = %" PRIu32 "\n", p);
    uint32_t q = get_random_prime_number();
    printf("q = %" PRIu32 "\n", q);

    return 0;
}
