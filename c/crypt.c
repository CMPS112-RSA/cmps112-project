#include "base64.h"
#include "cmps112-rsa.h"

#include <stdio.h>
#include <string.h>

size_t encrypt_message(
    ubigint_handle_t key_n,
    ubigint_handle_t key_e,
    buffer_t input_buffer,
    buffer_t* output_buffer_ptr,
    size_t message_len
) {
    buffer_t intermediate = calloc(message_len, 1);

    ubigint_handle_t powop, input_byte, output_byte;
    new_ubigint(&powop);
    new_ubigint(&output_byte);

    for(size_t i = 0; i < message_len; i++) {
        new_ubigint_from_num(&input_byte, input_buffer[i]);
        ubigint_pow(input_byte, key_e, powop);
        ubigint_modulus(powop, key_n, output_byte);
        uint64_t output_ulong = 0;
        ubigint_to_ulong(output_byte, &output_ulong);
        intermediate[i] = (char)output_ulong;
        //printf("%c -> %d\n", input_buffer[i], (int)intermediate[i]);

        free_ubigint(&input_byte);
    }

    size_t output_len = Base64encode_len((int)message_len);
    (*output_buffer_ptr) = calloc(output_len, 1);
    Base64encode((*output_buffer_ptr), intermediate, message_len);

    free(intermediate);
    free_ubigint(&powop);
    free_ubigint(&output_byte);

    return output_len;
}

size_t decrypt_message(
    ubigint_handle_t key_n,
    ubigint_handle_t key_d,
    buffer_t input_buffer,
    buffer_t* output_buffer_ptr,
    size_t message_len
) {
    buffer_t intermediate = calloc(message_len, 1);
    ubigint_handle_t powop, input_byte, output_byte;
    new_ubigint(&powop);
    new_ubigint(&output_byte);

    for(size_t i = 0; i < message_len; i++) {
        new_ubigint_from_num(&input_byte, input_buffer[i]);
        ubigint_pow(input_byte, key_d, powop);
        ubigint_modulus(powop, key_n, output_byte);
        uint64_t output_ulong = 0;
        ubigint_to_ulong(output_byte, &output_ulong);
        intermediate[i] = (uint8_t)output_ulong;

        free_ubigint(&input_byte);
    }

    size_t output_len = Base64decode_len(intermediate);
    (*output_buffer_ptr) = calloc(output_len, 1);
    Base64decode((*output_buffer_ptr), intermediate);

    free(intermediate);
    free_ubigint(&powop);
    free_ubigint(&output_byte);

    return output_len;
}
