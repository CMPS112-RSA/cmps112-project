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

// Horribly inefficient
static bool is_prime(const uint64_t num) {
    printf("is_prime: testing %" PRIu64 "\n", num);
    if(num % 2 == 0) {
        return false;
    }

    // This check gets around the issue of underflow always being > 0
    for(uint64_t i = (sqrt(num)-1); i < num; i--) {
        if(num % i == 0) {
            return false;
        }
    }

    printf("Success!\n");
    return true;
}

// http://stackoverflow.com/a/7921017
static uint64_t get_random_number() {
    int fd;
    uint64_t ret;

    if((fd = open("/dev/random", O_RDONLY)) == -1) {
        fprintf(stderr, "Failed to open /dev/random. Using rand() instead.\n");
        srand(time(NULL));
        ret = rand();
    }
    read(fd, &ret, 8);
    close(fd);

    return ret;
}

static inline uint64_t get_random_prime_number() {
    uint64_t ret = 0;
    do {
        ret = get_random_number();
    } while(!is_prime(ret));

    return ret;
}

int main(int argc, char** argv) {
    (void)argc;
    (void)argv;

    uint64_t p = get_random_prime_number();
    printf("%" PRIu64 "\n", p);
    uint64_t q = get_random_prime_number();
    printf("%" PRIu64 "\n", q);

    return 0;
}
