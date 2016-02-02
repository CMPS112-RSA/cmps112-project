#include "cmps112-rsa.h"

#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int main(int argc, char* argv[]) {
    (void)argc;
    (void)argv;

    // Read file
    size_t filesize = 0;
    buffer_t input_buffer, encrypted_buffer, output_buffer;
    FILE* file = fopen(argv[1], "r");
    fseek(file, 0L, SEEK_END);
    filesize = ftell(file);
    fseek(file, 0L, SEEK_SET);
    input_buffer = calloc(filesize, 1);
    fread(input_buffer, 1, filesize, file);
    fclose(file);

    bigint_handle_t key_d, key_e, key_n;
    new_bigint_from_short(&key_d, 103);
    new_bigint_from_short(&key_e, 7);
    new_bigint_from_short(&key_n, 143);

    size_t encrypted_len = encrypt_message(
        key_n, key_e, input_buffer,
        &encrypted_buffer, filesize
    );
    (void)encrypted_len;
    size_t decrypted_len = decrypt_message(
        key_n, key_d, encrypted_buffer,
        &output_buffer
    );

    printf("Output len: %d\n", (int)decrypted_len);

    free_bigint(&key_d);
    free_bigint(&key_e);
    free_bigint(&key_n);
    free(input_buffer);
    free(encrypted_buffer);
    free(output_buffer);

    return 0;
}
