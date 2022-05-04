module funciones
contains
    function f(x)

        real(8) :: x
        real(8) :: f

       ! f = 10*(x**2)*exp(-x**2)
        f = cos(x)
    end function

end module funciones