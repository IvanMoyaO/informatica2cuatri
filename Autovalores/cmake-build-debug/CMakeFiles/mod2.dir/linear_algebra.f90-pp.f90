# 1 "C:/Users/Ivan/Desktop/Informatica/Autovalores/linear_algebra.f90"
# 1 "<built-in>"
# 1 "<command-line>"
# 1 "C:\\Users\\Ivan\\Desktop\\Informatica\\Autovalores\\cmake-build-debug//"
# 1 "C:/Users/Ivan/Desktop/Informatica/Autovalores/linear_algebra.f90"

module resolver

    contains


        subroutine gauss_factor(A, X)
            ! A = matriz; x = soluciones (suele ser llamado b)
            real(8), intent(inout) :: A(:,:)
            real(8), intent(inout) :: x(:)

            ! i,k varibales para los bucles
            ! n = tamaño matriz
            ! pivo_fila = fila donde se halla el pivote, sirve para hacver el cambio
            ! aux = variable auxiliar
            ! --------
            ! vaux = vector auxiliar para el cambio de filas
            !--------
            ! pivote = el pivote
            ! factor = resultado de la división del elemento elegido de la fila y el pivote
            integer :: i,j, k, n, pivo_fila
            real(8), allocatable :: vaux(:)
            real(8) :: pivote, factor, aux

            n = size(A,1)
            allocate(vaux(n))


            do k=1, n-1
                ! elegimos como predeterminado el que está en la diagonal axtual
               pivote = abs(A(k,k))
            !   print*, "pivote", A(k,k), abs(A(k,k))
               pivo_fila = k

               ! buscar pivote (un elemento mayor que el pivote actual)
               ! se opera en abs
               do i=k+1, n
                   if( pivote < abs(A(i,k)) ) then; pivo_fila = i; pivote = abs(A(i,k)); endif ! guardo pivote nuevo y almaceno fila
               enddo

               if(pivote <= epsilon(1d0)) return; ! S.I.
               ! cambiar filas
               ! vec aux guarda fila; fila antigua -> fila pivote; fila pivote -> vec (fila antigua)
               vaux = A(k,:)
               A(k,:) = A(pivo_fila,:)
               A(pivo_fila,:) = vaux

               ! lo mismo para el vecto x (b)
               aux = x(k)
               x(k) = x(pivo_fila)
               x(pivo_fila) = aux

                ! operación de eliminaci´pn
                do i=k+1, n
                      factor = A(i,k)/A(k,k)

                      ! fila i-esima = fila i-esima - factor * fila "fija"
                      A(i,:) = A(i,:) - factor*A(k,:)

                      ! elemento i-esimo = elemento i-esima - factor * elemento "fijo"
                      x(i) = x(i) - factor*x(k)
                enddo
            enddo

        end subroutine gauss_factor




        subroutine LU_factor(A,L,U,P)

            ! ****** Descripción  *********
            ! Esta subrutina devuelve la factorización LU de una matriz A de la forma  PA = LU
            ! Siendo P una matriz de permutación, L una matriz triangular inferior y U una matriz
            ! triangular superior todas de tamaño nxn.

            real(8), intent(in)     :: A(:,:)
            real(8), intent(inout)  :: L(:,:), U(:,:), P(:,:)

            integer             :: n,i,k,pos(1)
            real(8),allocatable :: V(:)
            real(8)             :: V_aux,m

            n = size(A,1)
            allocate(V(n))

            ! Inicializo las variables
            U = A
            L = 0.d0
            P = 0.d0
            do i =1,n
                L(i,i) = 1.d0
                P(i,i) = 1.d0
            enddo

            ! Bucle por elementos de la diagonal
            do k = 1, n
                ! Pivote parcial
                if (abs(U(k,k))<epsilon(1.0)) then
                    pos = maxloc(abs(U(k+1:n,k)))
                    if (abs(U(k+pos(1),k))<epsilon(1.d0)) then
                        write(*,*)'Sistema incompatible'
                        stop
                    else
                        V = U(k,:)
                        U(k,:) = U(k+pos(1),:)
                        U(k+pos(1),:) = V

                        V = P(k,:)
                        P(k,:) = P(k+pos(1),:)
                        P(k+pos(1),:) = V

                        do i = 1, k-1 ! Cambio las filas de L. Solo las columnas ya calculadas
                            v_aux = L(k,i)
                            L(k,i)= L(k+pos(1),i)
                            L(k+pos(1),i) = v_aux
                        enddo
                    endif
                endif

                do i = k+1, n !Bucle por las filas debajo de la diagonal
                    m = U(i,k)/U(k,k)
                    U(i,:) = U(i,:) - m*U(k,:)
                    L(i,k) = m
                enddo
            enddo

        end subroutine


        subroutine upper_solver(U,b,x)

            ! ****** Descripción  *********
            ! Esta subrutina resuelve un sistema de la forma Ux = b
            ! Siendo U una matriz triangular superior de nxn y b un vector de tamaño n

            real(8),intent(in)  :: U(:,:)
            real(8),intent(in)  :: b(:)
            real(8),intent(out) :: x(:)

            ! Variables locales
            ! Añade las variables locales que necesites
            integer :: i,j,z = 0
            integer :: n
            real(8),allocatable :: k(:)

            ! Implementa el código para resolver el sistema

            n = size(U,1)
            allocate(k(n))


            k = 0d0
            x = 0
            do i=n,1, -1

                if(abs(u(i,i)) <= (10e-14))then; print*, "Sin solucion (U)"; stop; endif !epsilon(1d0,1) da errores
                k(i) = b(i)
         !       print*, "val",U(i,i), k(i), b(i)
                do j=1, n
                        k(i) = k(i) - U(i,j) * x(j)
                end do
                x(i) = k(i)/U(i,i)
            end do

        end subroutine

        subroutine LU_solver(A,b,x)

            real(8),intent(in)  :: A(:,:)
            real(8),intent(in)  :: b(:)
            real(8),intent(out) :: x(:)


            !Locales
            integer :: n
            real(8),allocatable     :: L(:,:),U(:,:),P(:,:)
            real(8),allocatable     :: y(:),baux(:)

            n = size(A,1)
            allocate(L(n,n),U(n,n),P(n,n),y(n),baux(n))

            call LU_factor(A,L,U,P)
            baux = matmul(P,b)
            call lower_solver(L,baux,y)
            call upper_solver(U,y,x)

            deallocate(L,U,P,y,baux)

        end subroutine

        subroutine lower_solver(L,b,x)

            ! ****** Descripción  *********
            ! Esta subrutina resuelve un sistema de la forma Lx = b
            ! Siendo L una matriz triangular inferior de nxn y b un vector de tamaño n

            real(8),intent(in)  :: L(:,:)
            real(8),intent(in)  :: b(:)
            real(8),intent(out) :: x(:)

            ! Variables locales
            ! Añade las variables locales que necesites
            integer :: i,j
            integer :: n
            real(8),allocatable :: k(:)

            ! Implementa el código para resolver el sistema

            n = size(L,1)
            allocate(k(n))
            k = 0d0
            x = 0
            do i=1,n
                if(abs(L(i,i)) <= (10e-14))then; print*, "Sin solucion (l)"; stop; endif !epsilon(1d0,1) da errores
                k(i) = b(i)
                      ! print*, "val (L)",L(i,i), k(i), b(i)
                do j=1, n
                    k(i) = k(i) - L(i,j) * x(j)
                end do
                x(i) = k(i)/L(i,i)
            end do

        end subroutine



    ! ********************************************
    !
    ! PARTE SISTEMAS ITERATIVOS
    !
    ! ********************************************



end module resolver
