#include "crypt.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define BUFFER_LEN 1024
static char num_buffer[BUFFER_LEN];

static int write_num(FILE* output, mpz_t* num) {
    memset(num_buffer, '\0', BUFFER_LEN);
    mpz_get_str(num_buffer, 10, *num);
    size_t ascii_size = mpz_sizeinbase(*num, 10) + 2;
    size_t written = fwrite(num_buffer, 1, ascii_size, output);
    if(written != ascii_size) {
        return 1;
    }
    fputs("\n", output);

    return 0;
}

int encrypt_message(
    const char* input_filename,
    const char* output_filename,
    rsa_private_key_t* key
) {
    int status_code = 0;

    // Open input file
    FILE* input_file = fopen(input_filename, "r");
    if(!input_file) {
        fprintf(stderr, "Failed to open input file.\n");
        status_code = 1;
        goto done;
    }

    // Open file for encrypted output, write modulo value to it
    FILE* output_file = fopen(output_filename, "r");
    if(!output_file) {
        fprintf(stderr, "Failed to open output file.\n");
        status_code = 1;
        goto cleanup_input_file;
    }
    if(write_num(output_file, &(key->n))) {
        fprintf(stderr, "Failed to write modulo number to encrypted file.\n");
        status_code = 1;
        goto cleanup_output_file;
    }

    // Initialize GMP data
    mpz_t input_byte, output_num;
    mpz_init(input_byte);
    mpz_init(output_num);

    int input_num = 0;
    while(1) {
        // Read character
        input_num = fgetc(input_file);
        if(feof(input_file)) {
            break;
        }

        // Encrypt
        mpz_set_si(input_byte, input_num);
        mpz_powm(output_num, input_byte, key->e, key->n);

        // Write to output file
        if(write_num(output_file, &output_num)) {
            fprintf(stderr, "Failed to write encrypted number to encrypted file.\n");
            status_code = 1;
            goto cleanup_gmp_nums;
        }
    }

    // Clean up
    cleanup_gmp_nums:
        mpz_clear(output_num);
        mpz_clear(input_byte);
    cleanup_output_file:
        fclose(output_file);
    cleanup_input_file:
        fclose(input_file);

    done:
        return status_code;
}

int decrypt_message(
    const char* input_filename,
    const char* output_filename,
    rsa_public_key_t* key
) {
    int status_code = 0;

    // Open input file
    FILE* input_file = fopen(input_filename, "r");
    if(!input_file) {
        fprintf(stderr, "Failed to open input file.\n");
        status_code = 1;
        goto done;
    }

    // Open output file
    FILE* output_file = fopen(output_filename, "r");
    if(!output_file) {
        fprintf(stderr, "Failed to open output file.\n");
        status_code = 1;
        goto cleanup_input_file;
    }

    mpz_t input_num, output_byte;
    mpz_init(input_num);
    mpz_init(output_byte);

    // Clean up
    cleanup_gmp_nums:
        mpz_clear(output_byte);
        mpz_clear(input_num);
    cleanup_output_file:
        fclose(output_file);
    cleanup_input_file:
        fclose(input_file);

    done:
        return status_code;
}
