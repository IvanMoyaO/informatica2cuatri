cmake_minimum_required(VERSION 3.21)
project(SistemasLineales Fortran)


enable_language(Fortran)

add_library(lib1 resolver.f90)
add_executable(exe C:/Users/Ivan/Desktop/Informatica/SistemasLineales/main.f90)
target_link_libraries (exe lib1)


