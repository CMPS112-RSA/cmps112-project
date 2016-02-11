#include "rsa.h"

#include <stdio.h>
#include <string.h>

#define BUFFER_LEN 1024
static char key_buffer[BUFFER_LEN];

int rsa_init_private_key(
    const char* filename,
    rsa_private_key_t* key
) {
    FILE* input_key = fopen(filename, "r");
    if(!input_key) {
        fprintf(stderr, "Failed to open private key file.\n");
        return 1;
    }

    // Read first number into buffer
    memset(key_buffer, '\0', BUFFER_LEN);
    char* line = fgets(key_buffer, BUFFER_LEN, input_key);
    if(!line) {
        fprintf(stderr, "Failed to read first number from private key file.\n");
        fclose(input_key);
        return 1;
    }

    // Make sure this line is actually a number
    if(mpz_init_set_str(key->e, key_buffer, 10) == -1) {
        fprintf(stderr, "First line of private key file is not a number.\n");
        mpz_clear(key->e);
        fclose(input_key);
        return 1;
    }

    // Read second number into buffer
    memset(key_buffer, '\0', BUFFER_LEN);
    line = fgets(key_buffer, BUFFER_LEN, input_key);
    if(!line) {
        fprintf(stderr, "Failed to read second number from private key file.\n");
        fclose(input_key);
        return 1;
    }

    // Make sure this line is actually a number
    if(mpz_init_set_str(key->n, key_buffer, 10) == -1) {
        fprintf(stderr, "Second line of private key file is not a number.\n");
        mpz_clear(key->n);
        mpz_clear(key->e);
        fclose(input_key);
        return 1;
    }

    fclose(input_key);
    return 0;
}

int rsa_init_public_key(
    const char* filename,
    rsa_public_key_t* key
) {
    FILE* input_key = fopen(filename, "r");
    if(!input_key) {
        fprintf(stderr, "Failed to open public key file.\n");
        return 1;
    }

    // Read number into buffer
    memset(key_buffer, '\0', BUFFER_LEN);
    char* line = fgets(key_buffer, BUFFER_LEN, input_key);
    if(!line) {
        fprintf(stderr, "Failed to read number from public key file.\n");
        fclose(input_key);
        return 1;
    }

    // Make sure this line is actually a number
    if(mpz_init_set_str(key->d, key_buffer, 10) == -1) {
        fprintf(stderr, "First line of public key file is not a number.\n");
        mpz_clear(key->d);
        fclose(input_key);
        return 1;
    }

    fclose(input_key);
    return 0;
}

void rsa_free_private_key(
    rsa_private_key_t* key
) {
    mpz_clear(key->e);
    mpz_clear(key->n);
}

void rsa_free_public_key(
    rsa_public_key_t* key
) {
    mpz_clear(key->d);
}

int rsa_write_private_key(
    const char* filename,
    rsa_private_key_t* key
) {
    FILE* output_key = fopen(filename, "w");
    if(!output_key) {
        fprintf(stderr, "Failed to open private key file.\n");
        return 1;
    }

    // Place first number into buffer
    memset(key_buffer, '\0', BUFFER_LEN);
    mpz_get_str(key_buffer, 10, key->e);
    size_t ascii_size = mpz_sizeinbase(key->e, 10) + 2;

    // Write first number to file
    size_t written = fwrite(key_buffer, 1, ascii_size, output_key);
    if(written != ascii_size) {
        fprintf(stderr, "Failed to write first line of private key file.\n");
        fclose(output_key);
        return 1;
    }
    fputs("\n", output_key);

    // Place second number into buffer
    memset(key_buffer, '\0', BUFFER_LEN);
    mpz_get_str(key_buffer, 10, key->n);
    ascii_size = mpz_sizeinbase(key->n, 10) + 2;

    // Write second number to file
    written = fwrite(key_buffer, 1, ascii_size, output_key);
    if(written != ascii_size) {
        fprintf(stderr, "Failed to write second line of private key file.\n");
        fclose(output_key);
        return 1;
    }
    fputs("\n", output_key);

    fclose(output_key);
    return 0;
}

int rsa_write_public_key(
    const char* filename,
    rsa_public_key_t* key
) {
    FILE* output_key = fopen(filename, "w");
    if(!output_key) {
        fprintf(stderr, "Failed to open private key file.\n");
        return 1;
    }

    // Place number into buffer
    memset(key_buffer, '\0', BUFFER_LEN);
    mpz_get_str(key_buffer, 10, key->d);
    size_t ascii_size = mpz_sizeinbase(key->d, 10) + 2;

    // Write number to file
    size_t written = fwrite(key_buffer, 1, ascii_size, output_key);
    if(written != ascii_size) {
        fprintf(stderr, "Failed to write first line of private key file.\n");
        fclose(output_key);
        return 1;
    }
    fputs("\n", output_key);

    fclose(output_key);
    return 0;
}
