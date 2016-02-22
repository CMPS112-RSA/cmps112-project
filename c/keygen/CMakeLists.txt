INCLUDE_DIRECTORIES(
    ${CMAKE_SOURCE_DIR}
    ${CMAKE_CURRENT_SOURCE_DIR}
    ${GMP_INCLUDE_DIRS}
)

SET(srcs
    calc.c
    main.c
)
ADD_EXECUTABLE(rsa-keygen-c ${srcs})
TARGET_LINK_LIBRARIES(rsa-keygen-c cmps112-rsa ${GMP_LIBRARIES} m)
