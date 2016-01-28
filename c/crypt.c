#include "base64.h"
#include "cmps112-rsa.h"

#include <stdio.h>
#include <string.h>

void encrypt_message(
    ubigint_handle_t key_n,
    ubigint_handle_t key_e,
    uint8_t* message_in,
    uint8_t* message_out,
    size_t message_len
) {
    ubigint_handle_t powop, input_byte, output_byte;
    new_ubigint(&powop);
    new_ubigint(&output_byte);

    for(size_t i = 0; i < message_len; i++) {
        new_ubigint_from_num(&input_byte, message_in[i]);
        ubigint_pow(input_byte, key_e, powop);
        ubigint_modulus(powop, key_n, output_byte);
        uint64_t output_ulong = 0;
        ubigint_to_ulong(output_byte, &output_ulong);
        message_out[i] = (uint8_t)output_ulong;

        free_ubigint(&input_byte);
    }

    free_ubigint(&powop);
    free_ubigint(&output_byte);
}

void decrypt_message(
    ubigint_handle_t key_n,
    ubigint_handle_t key_d,
    uint8_t* message_in,
    uint8_t* message_out,
    size_t message_len
) {
    ubigint_handle_t powop, input_byte, output_byte;
    new_ubigint(&powop);
    new_ubigint(&output_byte);

    for(size_t i = 0; i < message_len; i++) {
        new_ubigint_from_num(&input_byte, message_in[i]);
        ubigint_pow(input_byte, key_d, powop);
        ubigint_modulus(powop, key_n, output_byte);
        uint64_t output_ulong = 0;
        ubigint_to_ulong(output_byte, &output_ulong);
        message_out[i] = (uint8_t)output_ulong;

        free_ubigint(&input_byte);
    }

    free_ubigint(&powop);
    free_ubigint(&output_byte);
}
