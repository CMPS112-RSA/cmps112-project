/*
 * This file comes from my solution to CMPS 109 W16's asg1.
 */

//
// Assuming that for any given type T, there are operators
// bool operator< (const T&, const T&);
// bool operator== (const T&, const T&);
// as fundamental comparisons for type T, define the other
// six operators in terms of the basic ones.
//

#ifndef __REL_OPS_H__
#define __REL_OPS_H__

#ifndef __cplusplus
#error Do not include this from a C file!
#endif

template <typename value>
inline bool operator!= (const value &left, const value &right) {
   return not (left == right); 
}

template <typename value>
inline bool operator>  (const value &left, const value &right) {
   return right < left; 
}

template <typename value>
inline bool operator<= (const value &left, const value &right) {
   return not (right < left);
}

template <typename value>
inline bool operator>= (const value &left, const value &right) {
   return not (left < right); 
}

#endif

