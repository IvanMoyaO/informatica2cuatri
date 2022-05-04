# 1 "C:/Users/Ivan/Desktop/Informatica/SistemasLineales/main.f90"
# 1 "<built-in>"
# 1 "<command-line>"
# 1 "C:\\Users\\Ivan\\Desktop\\Informatica\\SistemasLineales\\cmake-build-debug//"
# 1 "C:/Users/Ivan/Desktop/Informatica/SistemasLineales/main.f90"
program SistemasLineales

    ! siempre sistemas cuadrados, n incógnitas y n ecuaciones!
    use resolver
    real, allocatable :: A(:,:), B(:,:)
    integer :: n

    n = 2
    allocate(A(n,n+1)); allocate(B(n,n+1))

    write(*,*) "matriz ampliada"
    read*, A

    call opera_gauss(A,B,n)


end program
