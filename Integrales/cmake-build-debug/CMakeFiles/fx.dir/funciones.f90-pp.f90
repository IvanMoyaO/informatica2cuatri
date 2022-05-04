# 1 "C:/Users/Ivan/Desktop/Informatica/Integrales/funciones.f90"
# 1 "<built-in>"
# 1 "<command-line>"
# 1 "C:\\Users\\Ivan\\Desktop\\Informatica\\Integrales\\cmake-build-debug//"
# 1 "C:/Users/Ivan/Desktop/Informatica/Integrales/funciones.f90"
module funciones
contains
    function f(x)

        real(8) :: x
        real(8) :: f

       ! f = 10*(x**2)*exp(-x**2)
        f = cos(x)
    end function

end module funciones
