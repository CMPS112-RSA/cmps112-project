// $Id: libfns.h,v 1.2 2015-07-02 16:03:36-07 - - $

// Library functions not members of any class.

#include <cstdint>
#include <vector>

#include "ubigint.hpp"

ubigint upow (const ubigint& base, const ubigint& exponent);

void clear_zeros(std::vector<uint8_t> &vec);
