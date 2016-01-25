####################################################
# CMake configuration
####################################################
CMAKE_MINIMUM_REQUIRED(VERSION 2.8.6)
PROJECT(cmps112-rsa C)

SET(srcs
    getopt.c
    rsa.c
    main.c
)

SET(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -std=c99 -Wall -Wextra")
ADD_EXECUTABLE(cmps112-rsa-c ${srcs})
