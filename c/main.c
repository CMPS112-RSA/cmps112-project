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

    // Initialize key variables
    mpz_t key_d, key_e, key_n;
    mpz_init(key_d);
    mpz_init(key_e);
    mpz_init(key_n);

    mpz_set_str(key_d, "7422006203", 10);
    mpz_set_str(key_e, "5905153727", 10);
    mpz_set_str(key_n, "9192668113", 10);

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
    for(size_t i = 0; i < decrypted_len; i++) {
        printf("%c %d\n", output_buffer[i], (int)output_buffer[i]);
    }

    mpz_clear(key_n);
    mpz_clear(key_e);
    mpz_clear(key_d);
    free(input_buffer);
    free(encrypted_buffer);
    free(output_buffer);

    return 0;
}
