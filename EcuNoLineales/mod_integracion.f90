! Created by Ivan on 17/03/2022.

module mod_ecunolineales
    contains


        subroutine metodo_biseccion(f, a, b, x, tol, n)
            real(8), intent(inout) ::  tol, a,b ! extremos intervalo (aunque no salgan, tiene que ser inout para poder modificarlas)
            interface
                function f(x)
                    real(8) :: x
                    real(8) :: f
                end function
            end interface

            real(8), intent(out) :: x
            real(8) :: x_ant
            integer :: i, n !n = num interaciones


            do i=1,n
                x_ant = x
                x = (a+b)/2

                ! comprobar si es negativo/positivo
                if( f(x) * f(a) < 0) then; b=(a+b)/2; else; a=(a+b)/2
                endif


                if((abs(x-x_ant)) < tol) exit
                if(i==n)then;  write(*,"(A,I8,A,F10.4)") "Con ", i, " iteraciones B no ha convergido. Ultimo valor: ", x;
                endif
            end do


        end subroutine


    subroutine metodo_newton(f, a, x, tol, n)
        real(8), intent(in) :: tol, a !val inicial
        interface
            function f(x)
                real(8) :: x
                real(8) :: f
            end function
        end interface

        real(8), intent(out) :: x
        real(8) :: x_ant, h
        integer :: i, n !n = num interaciones

        h = tol

        do i=1,n
           x_ant = x

           ! f(x-1)/f'(x) (deri centrada)
            x = x_ant - (f(x_ant) )/( ( f(a+h) - f(a-h) )/(2*h) )

            if((abs(x-x_ant)) < tol) exit
            if(i==n)then;  write(*,"(A,I8,A,F10.4)") "Con ", i, " iteraciones NR no ha convergido. Ultimo valor: ", x;
             endif
        end do


    end subroutine


end module