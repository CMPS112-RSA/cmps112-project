####################################################
# CMake configuration
####################################################
CMAKE_MINIMUM_REQUIRED(VERSION 2.8.6)
PROJECT(cmps112-rsa C)

SET(CMAKE_MODULE_PATH "${CMAKE_CURRENT_SOURCE_DIR}/cmake;${CMAKE_MODULE_PATH}")
FIND_PACKAGE(GMP REQUIRED)

SET(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -std=c99 -Wall -Wextra")
SET(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=gnu++11 -Wall -Wextra")

ADD_SUBDIRECTORY(rsa-lib)

INCLUDE_DIRECTORIES(${GMP_INCLUDE_DIR})

SET(srcs
    base64.c
    crypt.c
    getopt.c
    main.c
)
ADD_EXECUTABLE(cmps112-rsa-c ${srcs})
TARGET_LINK_LIBRARIES(cmps112-rsa-c cmps112-rsa ${GMP_LIBRARIES})
