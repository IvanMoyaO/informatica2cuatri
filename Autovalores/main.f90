
program Autovalores
    use mod_autovals

    real(8) :: A(3,3)
    real(8), parameter :: tol = 0.001
    integer, parameter :: iters = 500

    A(1,:) = (/2d0,1d0,0d0/)
    A(2,:) = (/1d0,2d0,1d0/)
    A(3,:) = (/-1d0,0d0,1d0/)

    !A(1,:) = (/1d0,2d0,-1d0/)
   ! A(2,:) = (/1d0,0d0,1d0/)
  !  A(3,:) = (/4d0,-4d0,5d0/)

    call autovals_invpotencia(A, iters,tol)
    call autovals_potencia(A, iters, tol)



end program
