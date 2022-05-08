! Created by Ivan on 17/03/2022.

module mod_iterativos
    contains

    subroutine iterativos_jacobi(A, b, max_iter, tolerencia, sol)
        implicit none
        integer, intent(in) :: max_iter
        real(8), intent(in) :: tolerencia
        real(8), intent(in) :: A(:,:)
        real(8), intent(out) :: b(:),sol(:)


        integer :: i,j,k, n
        real(8), allocatable :: x(:), x_new(:)
        real(8) :: aux, valor

        n = size(A,1)
        allocate(x(n),x_new(n))

        x = 0.d0
        x_new = 0.d0

        do k=1, max_iter
            do i=1, n
                valor = 0.d0
                do j=1, i-1
                    valor = valor + A(i,j)*x(j)
                end do
                do j=i+1, n
                    valor = valor + A(i,j)*x(j)
                end do
                x_new(i) = ( b(i)-valor )/A(i,i)
           end do



            if( norm2(x_new-x)/norm2(x_new) <= tolerencia )then; sol = x_new; exit; endif
            if(k==max_iter)then; sol = x_new; write(*,*) "Max iteracion (J)";endif
           x = x_new
        end do

    end subroutine

        subroutine iterativos_gauss(A, b, max_iter, tolerencia, sol)
            implicit none
            integer, intent(in) :: max_iter
            real(8), intent(in) :: A(:,:)
            real(8), intent(in) :: tolerencia
            real(8), intent(inout) :: b(:)
            real(8), intent(out) :: sol(:)


            integer :: i,j,k, n
            real(8), allocatable :: x(:), x_ant(:)
            real(8) :: aux, valor

            n = size(b)
            allocate(x(n),x_ant(n))


            x = 0.d0
            x_ant = 0.d0

            do k=1, max_iter

                do i=1, n
                    valor = 0.d0
                    do j=1, i-1
                        valor = valor + A(i,j)*x(j)
                    end do
                    do j=i+1, n
                        valor = valor + A(i,j)*x(j)
                    end do
                    x(i) = ( b(i)-valor )/A(i,i)
                end do


                if( norm2(x-x_ant)/norm2(x) <= tolerencia )then; sol = x; exit; endif
                if(k==max_iter)then; sol = x; write(*,*) "Max iteracion (G)";endif
                x_ant = x
            end do

end subroutine

end module