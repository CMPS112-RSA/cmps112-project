// Needed for Clang to see strdup
#define _POSIX_C_SOURCE 200809L

#include "crypt.h"
#include <rsa-lib/getopt.h>
#include <rsa-lib/rsa.h>

#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

void print_help() {
}

int main(int argc, char* argv[]) {

    int return_code = EXIT_SUCCESS;
    int option = 0;
    char* key_filename = NULL;
    char* input_filename = NULL;
    char* output_filename = NULL;

    // Separating bools forces user to specify one
    bool encrypt = false;
    bool decrypt = false;

    rsa_private_key_t privkey;
    rsa_public_key_t pubkey;

    while((option = getopt(argc, argv, "dei:o:k:")) != -1) {
        switch(option) {
            case 'd':
                decrypt = true;
                break;

            case 'e':
                encrypt = true;
                break;

            case 'i':
                input_filename = strdup(optarg);
                break;

            case 'o':
                output_filename = strdup(optarg);
                break;

            case 'k':
                key_filename = strdup(optarg);
                break;

            default:
                print_help();
                return_code = EXIT_FAILURE;
                goto free_arg_strings;
        }
    }

    /*
     * Validate options
     */
    if(!input_filename) {
        fprintf(stderr, "You must give an input filename.\n");
        return_code = EXIT_FAILURE;
        goto free_arg_strings;
    }
    if(!output_filename) {
        fprintf(stderr, "You must give an output filename.\n");
        return_code = EXIT_FAILURE;
        goto free_arg_strings;
    }
    if(!key_filename) {
        fprintf(stderr, "You must give a key filename.\n");
        return_code = EXIT_FAILURE;
        goto free_arg_strings;
    }
    if(!decrypt && !encrypt) {
        fprintf(stderr, "You must specify encryption or decryption.\n");
        return_code = EXIT_FAILURE;
        goto free_arg_strings;
    }

    /*
     * Primary functionality
     */

    if(encrypt) {
        // Import private key
        if(rsa_init_private_key(key_filename, &privkey)) {
            fprintf(stderr, "Failed to import private key. Exiting.\n");
            return_code = EXIT_FAILURE;
            goto free_arg_strings;
        }
        printf("Public key imported.\n");

        // Encryption
        if(encrypt_message(input_filename, output_filename, &privkey)) {
            fprintf(stderr, "Failed to encrypt message. Exiting.\n");
            return_code = EXIT_FAILURE;
            goto free_rsa_key;
        }
        printf("Message encrypted.\n");
    } else {
        // Import public key
        if(rsa_init_public_key(key_filename, &pubkey)) {
            fprintf(stderr, "Failed to import public key. Exiting.\n");
            return_code = EXIT_FAILURE;
            goto free_arg_strings;
        }
        printf("Private key imported.\n");

        // Decryption
        if(decrypt_message(input_filename, output_filename, &pubkey)) {
            fprintf(stderr, "Failed to decrypt message.\n");
            return_code = EXIT_FAILURE;
            goto free_rsa_key;
        }
        printf("Message decrypted.\n");
    }

    /*
     * Cleanup
     */

    free_rsa_key:
        if(encrypt) {
            rsa_free_private_key(&privkey);
        } else {
            rsa_free_public_key(&pubkey);
        }

    free_arg_strings:
        if(key_filename) {
            free(key_filename);
        }
        if(output_filename) {
            free(output_filename);
        }
        if(input_filename) {
            free(input_filename);
        }

    return return_code;
}
