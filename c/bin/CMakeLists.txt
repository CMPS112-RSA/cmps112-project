INCLUDE_DIRECTORIES(
    ${CMAKE_SOURCE_DIR}/include
)

SET(srcs
    main.c
)

ADD_EXECUTABLE(cmps112-rsa-c ${srcs})
