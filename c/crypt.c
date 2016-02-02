#include "base64.h"
#include "cmps112-rsa.h"

#include <stdio.h>
#include <string.h>

size_t encrypt_message(
    bigint_handle_t key_n,
    bigint_handle_t key_e,
    buffer_t input_buffer,
    buffer_t* output_buffer_ptr,
    size_t message_len
) {
    buffer_t intermediate = calloc(message_len, 1);

    bigint_handle_t input_byte, output_byte;
    new_bigint(&output_byte);

    for(size_t i = 0; i < message_len; i++) {
        new_bigint_from_short(&input_byte, input_buffer[i]);
        bigint_modexp(input_byte, key_e, key_n, output_byte);

        short output = 0;
        bigint_to_short(output_byte, &output);
        intermediate[i] = (char)output;
        free_bigint(&input_byte);
    }

    size_t output_len = Base64encode_len((int)message_len);
    (*output_buffer_ptr) = calloc(output_len, 1);
    Base64encode((*output_buffer_ptr), intermediate, message_len);

    free(intermediate);
    free_bigint(&output_byte);

    return output_len;
}

size_t decrypt_message(
    bigint_handle_t key_n,
    bigint_handle_t key_d,
    buffer_t input_buffer,
    buffer_t* output_buffer_ptr,
    size_t message_len
) {
    buffer_t intermediate = calloc(message_len, 1);
    bigint_handle_t input_byte, output_byte;
    new_bigint(&output_byte);

    for(size_t i = 0; i < message_len; i++) {
        new_bigint_from_short(&input_byte, input_buffer[i]);
        bigint_modexp(input_byte, key_d, key_n, output_byte);

        short output = 0;
        bigint_to_short(output_byte, &output);
        intermediate[i] = (char)output;
        free_bigint(&input_byte);
    }

    size_t output_len = Base64decode_len(intermediate);
    (*output_buffer_ptr) = calloc(output_len, 1);
    Base64decode((*output_buffer_ptr), intermediate);

    free(intermediate);
    free_bigint(&output_byte);

    return output_len;
}
