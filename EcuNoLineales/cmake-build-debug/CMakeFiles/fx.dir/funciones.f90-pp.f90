# 1 "C:/Users/Ivan/Desktop/Informatica/EcuNoLineales/funciones.f90"
# 1 "<built-in>"
# 1 "<command-line>"
# 1 "C:\\Users\\Ivan\\Desktop\\Informatica\\EcuNoLineales\\cmake-build-debug//"
# 1 "C:/Users/Ivan/Desktop/Informatica/EcuNoLineales/funciones.f90"
module funciones
contains
    function f(x)

        real(8) :: x
        real(8) :: f

       ! f = 10*(x**2)*exp(-x**2)
       ! f = cos(x)
        f = x**2 - 3
    end function

end module funciones
