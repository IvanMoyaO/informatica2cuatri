# 1 "C:/Users/Ivan/Desktop/Informatica/Derivadas/funciones.f90"
# 1 "<built-in>"
# 1 "<command-line>"
# 1 "C:\\Users\\Ivan\\Desktop\\Informatica\\Derivadas\\cmake-build-debug//"
# 1 "C:/Users/Ivan/Desktop/Informatica/Derivadas/funciones.f90"
module funciones
contains
    function f(x)

        real(8) :: x
        real(8) :: f

        f = 1d0/2d0 * x**2

    end function

end module funciones
