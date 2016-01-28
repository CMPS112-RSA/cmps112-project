#define UBIGINT_CPP

#include "libfns.hpp"

ubigint upow(const ubigint& base_arg, const ubigint& exponent_arg) {
    static const ubigint U_ZERO(0);
    static const ubigint U_ONE(1);
    if(base_arg == U_ZERO) {
        return U_ZERO;
    } else if(exponent_arg == U_ZERO) {
        return base_arg;
    }

    ubigint ret(U_ONE);
    for(ubigint i = U_ONE; i <= exponent_arg; (i = i + 1)) {
        ret = ret * base_arg;
    }
    std::cout << "Done" << std::endl;
    return ret;
}

void clear_zeros(std::vector<uint8_t> &vec) {
    while(vec.size() > 0 and vec[vec.size()-1] == 0) {
        vec.pop_back();
    }
}
