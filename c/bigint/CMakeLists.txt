INCLUDE_DIRECTORIES(${CMAKE_CURRENT_SOURCE_DIR})

SET(bigint_srcs
    BigIntegerAlgorithms.cc
    BigInteger.cc
    BigIntegerUtils.cc
    BigUnsigned.cc
    BigUnsignedInABase.cc
    BigInteger_c.cc
)

ADD_LIBRARY(bigint STATIC ${bigint_srcs})
