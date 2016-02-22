####################################################
# CMake configuration
####################################################
CMAKE_MINIMUM_REQUIRED(VERSION 2.8.6)
PROJECT(cmps112-rsa C)

IF(WIN32)
    MESSAGE(FATAL_ERROR "Incompatible with Windows.")
ENDIF(WIN32)

SET(CMAKE_MODULE_PATH "${CMAKE_CURRENT_SOURCE_DIR}/cmake;${CMAKE_MODULE_PATH}")
FIND_PACKAGE(GMP REQUIRED)

SET(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -std=c99 -Wall -Wextra")

ADD_SUBDIRECTORY(rsa-lib)
ADD_SUBDIRECTORY(keygen)
ADD_SUBDIRECTORY(msg-crypt)
