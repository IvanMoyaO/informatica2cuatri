cmake_minimum_required(VERSION 3.21)
project(Autovalores Fortran)


enable_language(Fortran)
add_library(mod mod_iterativos.f90)
add_executable(exe C:/Users/Ivan/Desktop/Informatica/SistemasIterativos/main.f90)
target_link_libraries (exe mod)

