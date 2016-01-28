#ifndef UBIGINT_H
#define UBIGINT_H

#ifdef __cplusplus
extern "C" {
#else
struct ubigint_t;
typedef struct ubigint_t ubigint_t;
#endif

typedef ubigint_t* ubigint_handle_t;

void new_ubigint(
    ubigint_handle_t* handle_ptr
);

void new_ubigint_from_num(
    ubigint_handle_t* handle_ptr,
    unsigned long num
);

void new_ubigint_from_string(
    ubigint_handle_t* handle_ptr,
    const char* str
);

void free_ubigint(
    ubigint_handle_t* handle_ptr
);

#ifdef __cplusplus
}
#endif

#endif /* UBIGINT_H */
