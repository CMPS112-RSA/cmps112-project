#include "base64.h"
#include "cmps112-rsa.h"

#include <stdio.h>
#include <string.h>

void opengpg_file_to_key(
    const char* filename,
    rsa_key_t* key_out
) {
    (void)filename;
    (void)key_out;

    char* ascii_key; // Where ASCII key will be, TODO: malloc, fill
    Base64decode((char*)key_out->key, ascii_key);
}

void encrypt_message(
    rsa_key_t* key,
    uint8_t* message_in,
    uint8_t* message_out
) {
    (void)key;
    (void)message_in;
    (void)message_out;
}

void decrypt_message(
    rsa_key_t* key,
    uint8_t* message_in,
    uint8_t* message_out
) {
    (void)key;
    (void)message_in;
    (void)message_out;
}
