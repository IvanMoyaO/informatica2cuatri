program SistemasLineales

    ! siempre sistemas cuadrados, n incógnitas y n ecuaciones!
    use resolver
    real(8), allocatable :: A(:,:),Aux(:,:), b(:), baux(:), x_gauss(:), x_LU(:)
    integer :: n

    print*, "Introduce el tamaño"

    read*, n
    allocate(A(n,n),Aux(n,n), b(n), x_gauss(n), x_LU(n))


    ! -----------------------------------------------
    ! GAUSS
    ! -----------------------------------------------

    A(1,:)= (/ 1.d0,  2.d0, -3.d0, -1.d0/)
    A(2,:)= (/ 0.d0, -3.d0,  2.d0,  6.d0/)
    A(3,:)= (/-3.d0, -1.d0,  3.d0,  1.d0/)
    A(4,:)= (/ 2.d0,  3.d0,  2.d0, -1.d0/)

    B=(/0.d0, -8.d0, 0.d0, -8.d0/)

    ! Puesto que Gauss altera el vector b y la matriz A a fin de tener una matriz triangular inferior,
    ! necesitamos variables auxiliares para que no haga falta redefinir el sistema para la factorización LU
    Aux = A
    baux = b

    write(*,*) "Trinagulacion por gauss.."
    call gauss_factor(Aux, baux)

    do i=1, n
        write(*,fmt='(6(f7.2,1x))') Aux(i,:)
    end do

    write(*,*) "Solucion por gauss"
    call upper_solver(Aux,baux,x_gauss )

    do i=1,n
        write(*,fmt='(6(f7.2,1x))') x_gauss(i)
    end do



    ! -----------------------------------------------
    ! LU
    ! -----------------------------------------------




    write(*,*) "Resolucion por LU "
    call LU_solver(A,b,x_LU)

    write(*,*)
    do i=1,n
        write(*,fmt='(6(f7.2,1x))') x_LU(i)
    end do

    write(*,*) "Escribe algo para salir"
   read(*,*) n



end program
