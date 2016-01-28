INCLUDE_DIRECTORIES(
    ${CMAKE_CURRENT_SOURCE_DIR}
)

SET(ubigint_srcs
    ubigint.cpp
    libfns.cpp
)

ADD_LIBRARY(ubigint STATIC ${ubigint_srcs})
