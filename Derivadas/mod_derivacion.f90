! Created by Ivan on 17/03/2022.

module mod_derivacion
    contains

    function d1_centrada(f,x,h)
        real(8) :: x, h

        interface
            function f(x)
                real(8) :: x
                real(8) :: f
            end function
        end interface

        real(8) :: d1_centrada

        d1_centrada = ( f(x+h) - f(x-h) )/( 2.d0*h )

    end function

        function d2_centrada(f,x,h)
            real(8) :: x, h

            interface
                function f(x)
                    real(8) :: x
                    real(8) :: f
                end function
            end interface

            real(8) :: d2_centrada

            d2_centrada = ( f(x+h) - 2*f(x) + f(x-h))/( h**2 )

        end function

        function d1_regresiva(f,x,h)
            real(8) :: x, h

            interface
                function f(x)
                    real(8) :: x
                    real(8) :: f
                end function
            end interface

            real(8) :: d1_regresiva

            d1_regresiva = ( f(x) - f(x-h) )/h

        end function

        function d1_progresiva(f,x,h)
            real(8) :: x, h

            interface
                function f(x)
                    real(8) :: x
                    real(8) :: f
                end function
            end interface

            real(8) :: d1_progresiva

            d1_progresiva = ( -f(x) + f(x+h) )/h

        end function

end module