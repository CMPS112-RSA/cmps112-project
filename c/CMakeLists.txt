####################################################
# CMake configuration
####################################################
CMAKE_MINIMUM_REQUIRED(VERSION 2.8.6)
PROJECT(cmps112-rsa C CXX)

SET(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -std=c99 -Wall -Wextra")
SET(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=gnu++11 -Wall -Wextra")

ADD_SUBDIRECTORY(ubigint)

SET(srcs
    base64.c
    crypt.c
    getopt.c
    main.c
)
ADD_EXECUTABLE(cmps112-rsa-c ${srcs})
TARGET_LINK_LIBRARIES(cmps112-rsa-c ubigint)
