#include "cmps112-rsa.h"

#include <stdbool.h>
#include <stdio.h>

static rsa_key_t rsa_key;

int main(int argc, char* argv[]) {
    (void)argc;
    (void)argv;

    const char* key_filename = "";
    const char* input_filename = "";
    const char* output_filename = "";
    bool encrypt = false;

    opengpg_file_to_key(
        key_filename, &rsa_key
    );

    if(encrypt) {
    } else {
    }

    return 0;
}
