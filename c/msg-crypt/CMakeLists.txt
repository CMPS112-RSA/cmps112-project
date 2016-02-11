INCLUDE_DIRECTORIES(
    ${CMAKE_SOURCE_DIR}
    ${CMAKE_CURRENT_SOURCE_DIR}
    ${GMP_INCLUDE_DIRS}
)

SET(srcs
    crypt.c
    main.c
)
ADD_EXECUTABLE(rsa-msg-crypt ${srcs})
TARGET_LINK_LIBRARIES(rsa-msg-crypt cmps112-rsa ${GMP_LIBRARIES})
