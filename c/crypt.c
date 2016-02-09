#include "base64.h"
#include "cmps112-rsa.h"

#include <stdio.h>
#include <string.h>

size_t encrypt_message(
    mpz_t key_n,
    mpz_t key_e,
    buffer_t input_buffer,
    buffer_t* output_buffer_ptr,
    size_t message_len
) {
    buffer_t intermediate = calloc(message_len, 1);

    mpz_t input_byte, output_byte;

    for(size_t i = 0; i < message_len; i++) {
        mpz_init_set_si(input_byte, input_buffer[i]);
        mpz_powm(output_byte, input_byte, key_e, key_n);
        intermediate[i] = (char)mpz_get_si(output_byte);
    }

    size_t output_len = Base64encode_len((int)message_len);
    (*output_buffer_ptr) = calloc(output_len, 1);
    Base64encode((*output_buffer_ptr), intermediate, message_len);

    free(intermediate);
    mpz_clear(input_byte);
    mpz_clear(output_byte);

    return output_len;
}

size_t decrypt_message(
    mpz_t key_n,
    mpz_t key_d,
    buffer_t input_buffer,
    buffer_t* output_buffer_ptr
) {
    size_t decode_len = Base64decode_len(input_buffer);

    buffer_t intermediate = calloc(decode_len, 1);
    Base64decode(intermediate, input_buffer);

    (*output_buffer_ptr) = calloc(decode_len, 1);

    mpz_t input_byte, output_byte;

    for(size_t i = 0; i < decode_len; i++) {
        mpz_init_set_si(input_byte, intermediate[i]);
        mpz_powm(output_byte, input_byte, key_d, key_n);
        (*output_buffer_ptr)[i] = (char)mpz_get_si(output_byte);
    }

    free(intermediate);
    mpz_clear(output_byte);
    mpz_clear(input_byte);

    return decode_len;
}
