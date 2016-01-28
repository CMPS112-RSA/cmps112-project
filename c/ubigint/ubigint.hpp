/*
 * This file comes from my solution to CMPS 109 W16's asg1.
 */

#ifndef __UBIGINT_H__
#define __UBIGINT_H__

#ifndef __cplusplus
#error Do not include this from a C file!
#endif

#include <exception>
#include <iostream>
#include <limits>
#include <utility>
#include <vector>
using namespace std;

#include "relops.hpp"

class ubigint {
   friend ostream& operator<< (ostream&, const ubigint&);
   private:
      using quot_rem = pair<ubigint,ubigint>;

      using udigit_t = unsigned char;
      using ubigvalue_t = vector<udigit_t>;
      ubigvalue_t ubig_value;

      quot_rem divide (const ubigint&) const;
      void multiply_by_2();
      void divide_by_2();
   public:

      ubigint() = default; // Need default ctor as well.
      ubigint (unsigned long);
      ubigint (const string&);

      ubigint operator+ (const ubigint&) const;
      ubigint operator- (const ubigint&) const;
      ubigint operator* (const ubigint&) const;
      ubigint operator/ (const ubigint&) const;
      ubigint operator% (const ubigint&) const;

      bool operator== (const ubigint&) const;
      bool operator<  (const ubigint&) const;
};

#endif

