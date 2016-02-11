#include "crypt.h"
#include "rsa-lib/rsa.h"

#include <stdio.h>

int main(int argc, char* argv[]) {
    (void)argc;
    (void)argv;

    if(argc != 6) {
        fprintf(stderr, "exec priv_key pub_key msg_file\n");
        return 1;
    }

    rsa_private_key_t privkey;
    rsa_public_key_t pubkey;

    const char* privkey_file   = argv[1];
    const char* pubkey_file    = argv[2];
    const char* input_file     = argv[3];
    const char* encrypted_file = argv[3];
    const char* output_file    = argv[3];

    // Import keys
    if(rsa_init_private_key(privkey_file, &privkey)) {
        fprintf(stderr, "Failed to import private key.\n");
        return 1;
    }
    if(rsa_init_public_key(pubkey_file, &pubkey)) {
        fprintf(stderr, "Failed to import public key.\n");
        return 1;
    }

    // Encrypt and decrypt
    /*if(encrypt_message(input_file, encrypted_file, &privkey)) {
        fprintf(stderr, "Failed to encrypt message.\n");
        return 1;
    }
    if(decrypt_message(encrypted_file, output_file, &pubkey)) {
        fprintf(stderr, "Failed to decrypt message.\n");
        return 1;
    }*/

    return 0;
}
