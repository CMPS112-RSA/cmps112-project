/*
 * This file comes from my solution to CMPS 109 W16's asg1.
 */

#ifndef INCLUDED_UBIGINT_LIBFNS_HPP
#define INCLUDED_UBIGINT_LIBFNS_HPP

#ifndef __cplusplus
#error Do not include this from a C file!
#endif

#include <cstdint>
#include <vector>

#include "ubigint.hpp"

ubigint upow (const ubigint& base, const ubigint& exponent);

void clear_zeros(std::vector<uint8_t> &vec);

#endif /* INCLUDED_UBIGINT_LIBFNS_HPP */
