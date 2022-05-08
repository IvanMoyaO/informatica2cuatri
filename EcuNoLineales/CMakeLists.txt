cmake_minimum_required(VERSION 3.21)
project(Derivadas Fortran)


enable_language(Fortran)
add_library(fx funciones.f90)
add_library(mod mod_integracion.f90)
add_executable(exe C:/Users/Ivan/Desktop/Informatica/EcuNoLineales/main.f90)
target_link_libraries (exe mod fx)

