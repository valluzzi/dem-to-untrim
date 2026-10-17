cmake_minimum_required(VERSION 3.10)
project(DEM_to_UnTRIM Fortran)

# Set Fortran standard and compiler flags
set(CMAKE_Fortran_MODULE_DIRECTORY ${CMAKE_BINARY_DIR}/modules)

# Compiler-specific flags
if(CMAKE_Fortran_COMPILER_ID MATCHES "Intel")
    # For Intel Fortran Compiler
    set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} /O2 /W1")
elseif(CMAKE_Fortran_COMPILER_ID MATCHES "MSVC")
    # For MSVC Fortran
    set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} /O2 /W3")
else()
    # For GCC/Gfortran
    set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -O2")
endif()

# Ensure modules directory exists
file(MAKE_DIRECTORY ${CMAKE_Fortran_MODULE_DIRECTORY})

# Add the main executable
add_executable(DEM_to_UnTRIM src/DEM_to_UnTRIM.f90)

# Set output directory for the executable
set_target_properties(DEM_to_UnTRIM PROPERTIES
    RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin
)

# Link with necessary libraries if needed
# target_link_libraries(DEM_to_UnTRIM PRIVATE <libraries>)

# Installation target (optional)
install(TARGETS DEM_to_UnTRIM DESTINATION bin)

# Tests
enable_testing()

add_executable(test_asc tests/test_asc.f90 src/module_asc.f90)
set_target_properties(test_asc PROPERTIES
    RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin
)

add_test(NAME test_asc
    COMMAND test_asc
    WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}
)

add_executable(test_numpy tests/test_numpy.f90 src/module_numpy.f90)
set_target_properties(test_numpy PROPERTIES
    RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin
)

add_test(NAME test_numpy
    COMMAND test_numpy
    WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}
)
