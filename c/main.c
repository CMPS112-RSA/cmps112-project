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

    printf("Input (size %d):\n%s\n", (int)filesize, input_buffer);

    /*size_t encrypted_len = encrypt_message(
        key_n, key_e, input_buffer,
        &encrypted_buffer, filesize
    );
    size_t decrypted_len = decrypt_message(
        key_n, key_d, encrypted_buffer,
        &output_buffer, encrypted_len
    );*/

    int decrypted_len = 0;
    printf("Output len: %d\n", (int)decrypted_len);
    printf("%s\n", output_buffer);

    free(input_buffer);
    free(encrypted_buffer);
    free(output_buffer);

    return 0;
}
