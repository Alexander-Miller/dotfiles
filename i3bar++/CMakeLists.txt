# -*- mode: cmake -*-

cmake_minimum_required( VERSION 3.5.1 )
project( i3bar++ )

include_directories( include )
file( GLOB SOURCES "src/*.cpp" )

set( CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++14 -O3 -Wall -Wextra -Werror -Wpedantic" )
set( CMAKE_EXPORT_COMPILE_COMMANDS 1 )

add_executable( i3bar++ ${SOURCES} )
