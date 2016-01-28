#include "cmps112-rsa.h"
#include "ubigint/ubigint.h"

#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

static rsa_key_t rsa_key;

int main(int argc, char* argv[]) {
    (void)argc;
    (void)argv;

    memset(rsa_key.key, 0xFF, RSA_KEY_SIZE_BYTES);

    ubigint_handle_t num = NULL;
    //new_ubigint_from_binary(&num, rsa_key.key, RSA_KEY_SIZE_BYTES);
    new_ubigint_from_num(&num, 1234);
    uint64_t foo;
    ubigint_to_ulong(num, &foo);
    printf("%lu\n", foo);
    free_ubigint(&num);

    return 0;
}
