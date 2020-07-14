cmake_minimum_required(VERSION 3.0.0)
set(CMAKE_TOOLCHAIN_FILE "${CMAKE_SOURCE_DIR}/cmake/toolchain.cmake")
project (struct_members CXX)

find_package(Boost COMPONENTS filesystem iostreams serialization REQUIRED)
if(Boost_FOUND)
    set(Boost_USE_STATIC_LIBS on)
    include_directories(${Boost_INCLUDE_DIRS})
    link_libraries(${Boost_LIBRARIES})
endif()

if(MSVC)
    message(STATUS "f**k MSVC")
    message(STATUS "not adapted yet.")
endif()


list(APPEND CMAKE_MODULE_PATH "${CMAKE_SOURCE_DIR}/cmake")
find_package(LibClang REQUIRED)
include_directories(${CLANG_INCLUDEDIR})

set(CMAKE_EXPORT_COMPILE_COMMANDS "ON")
set_property(GLOBAL PROPERTY USE_FOLDERS ON)

add_subdirectory(cppast)
include_directories(${CMAKE_SOURCE_DIR}) 
include_directories(cppast/include)
include_directories(cppast/external/cxxopts/include)
include_directories(cppast/external/type_safe/include)
include_directories(cppast/external/type_safe/external/debug_assert/)

link_libraries(cppast)

find_package(fmt REQUIRED)
include_directories(Fmt_INCLUDE_DIRS)
link_libraries(fmt)


add_compile_options(-g)
add_compile_options(-funroll-loops)
add_compile_options(-ffast-math)

add_executable(
    struct_members
    "${CMAKE_SOURCE_DIR}/struct_members.cpp"
)