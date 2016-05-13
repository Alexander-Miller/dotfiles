# -*- mode: cmake -*-

set( CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++14 -O3 -Wall -Wextra -Werror -Wpedantic" )
set( CMAKE_EXPORT_COMPILE_COMMANDS 1 )

include_directories( "inc" )

cmake_minimum_required( VERSION 3.5.1 )
project( i3bar++ )

add_executable( i3bar++
  main.cpp
  Format.cpp
  Utils.cpp
  ParseResult.cpp
  Block.cpp
  VolumeBlock.cpp
  TimeBlock.cpp
  DateBlock.cpp
  MemBlock.cpp
  MusicBlock.cpp
  CpuBlock.cpp
  ChargeBlock.cpp
  )
