! Created by Ivan on 17/03/2022.

module mod_derivacion
    contains

        subroutine integracion_sumariemman(f, A, c,d, n)
            integer, intent(in) :: n
            real(8), intent(in) ::  c,d
            interface
                function f(x)
                    real(8) :: x
                    real(8) :: f
                end function
            end interface

            real(8), intent(out) :: A

            real(8) ::  h, p
            integer :: i, k

            p = c
            h = (d-c)/(n*1d0)

            do i=1,n
                p = p+h
                A = A + f( (p) )
            end do

            A = h*A

        endsubroutine

    subroutine integracion_trapecio(f, A, c,d, n)
        integer, intent(in) :: n
        real(8), intent(in) ::  c,d
        interface
            function f(x)
                real(8) :: x
                real(8) :: f
            end function
        end interface

        real(8), intent(out) :: A

        real(8) ::  h, p
        integer :: i, k

        p = c
        h = (d-c)/(n*1d0)

        do i=1,n-1
            p = p+h
            A = A + 2*f(p)
        end do

        A = A + f(c) + f(d)
        A = A * h/2d0
    endsubroutine

    subroutine integracion_simpson(f, A, c,d ,n)
        integer, intent(in) :: n
        real(8), intent(in) ::  c,d
        interface
            function f(x)
                real(8) :: x
                real(8) :: f
            end function
        end interface

        real(8), intent(out) :: a

        real(8) ::  h, p
        integer :: i, k

        p = c
        h = (d-c)/(n*1d0)

        do i=1,n
            p = p+h
            if(mod(i,2) == 0)then
               k = 2
            else
               k = 4
            end if

            A = A + k*f(p)

        end do
        A = A + f(c) + f(d)
        A = h/3d0 * A

    end subroutine integracion_simpson

end module