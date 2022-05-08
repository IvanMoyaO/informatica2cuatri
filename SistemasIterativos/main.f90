
program Iterativos
    use mod_iterativos

    real(8) :: A(2,2)
    real(8) :: b(2), solj(2), solg(2)
    real(8) :: tol = 5*10**(-4)
    integer :: iters = 1000

    A(1,:) = (/1d0, 1d0/)
    A(2,:) = (/1d0, -3d0/)

    b(:) = (/48d0, 4d0/)

  !  A(1,:) = (/1d0,2d0,-1d0/)
   ! A(2,:) = (/1d0,0d0,1d0/)
    !A(3,:) = (/4d0,-4d0,5d0/)

    call iterativos_gauss(A, b, iters, tol, solg)
    call iterativos_jacobi(A, b, iters, tol, solj)

  !  write(*,"(A, F10.3, A, F11.3)") "Jacobi:", solj, " Gauss:",  solg
    print*, "Jacobi:", solj, "Gauss:", solg


end program
