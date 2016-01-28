/*
 * This file comes from my solution to CMPS 109 W16's asg1.
 */

#include <cmath>
#include <cstdlib>
#include <exception>
#include <sstream>
#include <stack>
#include <stdexcept>
using namespace std;

#include "libfns.hpp"
#include "ubigint.hpp"

ubigint::ubigint (unsigned long that) {
    while(that > 0) {
        ubig_value.push_back(udigit_t(that % 10));
        that /= 10;
    }
}

ubigint::ubigint (const string& that) {
    for(string::const_reverse_iterator rit = that.rbegin(); rit != that.rend(); ++rit) {
        ubig_value.push_back(udigit_t(*rit - '0'));
    }
}

uint64_t ubigint::to_ulong() const {
    uint64_t ret = 0;
    for(size_t i = 0; i < ubig_value.size(); i++) {
        ret += ubig_value[i] * pow(10,i);
    }
    return ret;
}

ubigint ubigint::operator+ (const ubigint& that) const {
    ubigint ret;
    bool carry_one = false;

    size_t max_len = std::max<size_t>(this->ubig_value.size(), that.ubig_value.size());
    for(size_t i = 0; i < max_len; i++) {
        udigit_t digit_sum = carry_one ? 1 : 0;

        if(this->ubig_value.size() > i) {
            digit_sum += this->ubig_value[i];
        }
        if(that.ubig_value.size() > i) {
            digit_sum += that.ubig_value[i];
        }
        if(digit_sum >= 10) {
            digit_sum -= 10;
            if(i == (max_len-1)) {
                ret.ubig_value.push_back(digit_sum);
                ret.ubig_value.push_back(1);
                break;
            } else {
                carry_one = true;
            }
        } else {
            carry_one = false;
        }

        ret.ubig_value.push_back(digit_sum);
    }

    clear_zeros(ret.ubig_value);
    return ret;
}

ubigint ubigint::operator- (const ubigint& that) const {
   if (*this < that) throw domain_error ("ubigint::operator-(a<b)");

    ubigint ret;
    bool borrow = false;

    for(size_t i = 0; i < this->ubig_value.size(); i++) {
        udigit_t first_num = this->ubig_value[i];
        udigit_t second_num = that.ubig_value[i];

        if(borrow) {
            if(first_num == 0) {
                first_num = 9;
            } else {
                first_num--;
                borrow = false;
            }
        }

        if(i >= that.ubig_value.size()) {
            ret.ubig_value.push_back(first_num);
            continue;
        } else if(first_num < second_num) {
            first_num += 10;
            borrow = true;
        }

        ret.ubig_value.push_back(first_num - second_num);
    }

    clear_zeros(ret.ubig_value);
    return ret;
}

ubigint ubigint::operator* (const ubigint& that) const {
    ubigint ret;
    ret.ubig_value = ubigvalue_t(this->ubig_value.size() + that.ubig_value.size());

    // To match the algorithm in the prompt
    ubigvalue_t& p = ret.ubig_value;
    const ubigvalue_t& u = this->ubig_value;
    const ubigvalue_t& v = that.ubig_value;

    for(size_t i = 0; i < this->ubig_value.size(); i++) {
        udigit_t c = 0;
        for(size_t j = 0; j < that.ubig_value.size(); j++) {
            udigit_t d = p[i+j] + u.at(i)*v.at(j) + c;
            p[i+j] = d % 10;
            c = d / 10;
        }
        p[i + that.ubig_value.size()] = c;
    }

    clear_zeros(ret.ubig_value);
    return ret;
}

void ubigint::multiply_by_2() {
    (*this) = (*this) * 2;
}

void ubigint::divide_by_2() {
    (*this) = (*this) / 2;
}

// http://www.seattlecentral.edu/sccconline/mat107/chapter02.pdf
// Trying to use division.cpp resulted in infinite recursion
ubigint::quot_rem ubigint::divide (const ubigint& that) const {

    ubigint dividend(*this);
    ubigint divisor(that);
    ubigint power_of_2(1);
    vector<ubigint> divisors;
    quot_rem ret = make_pair(0,0);

    ubigint next(divisor), next_p2(power_of_2);
    while(next <= dividend) {
        divisor = next;
        power_of_2 = next_p2;
        divisors.push_back(divisor);
        next = divisor * 2;
        next_p2 = power_of_2 * 2;
    }

    if(divisor == dividend) {
        ret.first = power_of_2;
        ret.second = 0;
        return ret;
    }

    ubigint new_power_of_2(power_of_2);
    while(new_power_of_2 <= power_of_2) {
        ubigint sum = divisor + (that * new_power_of_2);

        if(sum <= dividend) {
            ret.first = power_of_2 + new_power_of_2;
            ret.second = dividend - sum;
            return ret;
        }

        if(new_power_of_2 == 0) {
            // To exit the loop
            new_power_of_2 = power_of_2+1;
        } else if(new_power_of_2 == 1) {
            new_power_of_2 = 0;
        } else {
            new_power_of_2 = new_power_of_2 / 2;
        }
    }

    return ret;
}

ubigint ubigint::operator/ (const ubigint& that) const {
   return divide (that).first;
}

ubigint ubigint::operator% (const ubigint& that) const {
   return divide (that).second;
}

// Vectors have an equality operator, so let's just use that
bool ubigint::operator== (const ubigint& that) const {
    return ubig_value == that.ubig_value;
}

bool ubigint::operator< (const ubigint& that) const {
    if(that.ubig_value.size() > this->ubig_value.size()) {
        return true;
    } else if(that.ubig_value.size() < this->ubig_value.size()) {
        return false;
    }

    /*
     * At this point, we know the two numbers have the same number of columns,
     * so compare each column. By the time we get to a column, we know all
     * higher-order numbers are the same.
     *
     * We use ssize_t here because a size_t is always >= 0.
     */
    for(ssize_t i = ssize_t(this->ubig_value.size()-1); i >= 0; i--) {
        if(this->ubig_value[i] > that.ubig_value[i]) {
            return false;
        } else if(this->ubig_value[i] < that.ubig_value[i]) {
            return true;
        }
    }

    // This means all columns are equal
    return false;
}

ostream& operator<< (ostream& out, const ubigint& that) {
   for(ssize_t i = ssize_t(that.ubig_value.size()-1); i >= 0; i--) {
       out << int(that.ubig_value.at(i));
   }
   return out;
}
