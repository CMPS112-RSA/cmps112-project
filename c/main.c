#include "cmps112-rsa.h"
#include "ubigint/ubigint.h"

#include <stdbool.h>
#include <stdio.h>

static rsa_key_t rsa_key;

int main(int argc, char* argv[]) {
    (void)argc;
    (void)argv;

    ubigint_handle_t num1 = NULL;
    ubigint_handle_t num2 = NULL;
    ubigint_handle_t num3 = NULL;

    new_ubigint_from_num(&num1, 358);
    new_ubigint_from_string(&num2, "37");
    new_ubigint(&num3);

    print_ubigint(num1);
    print_ubigint(num2);
    ubigint_modulus(num1, num2, num3);
    print_ubigint(num3);

    free_ubigint(&num1);
    free_ubigint(&num2);
    free_ubigint(&num3);

    return 0;
}
