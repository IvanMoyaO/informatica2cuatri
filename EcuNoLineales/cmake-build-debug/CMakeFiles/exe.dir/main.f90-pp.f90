# 1 "C:/Users/Ivan/Desktop/Informatica/EcuNoLineales/main.f90"
# 1 "<built-in>"
# 1 "<command-line>"
# 1 "C:\\Users\\Ivan\\Desktop\\Informatica\\EcuNoLineales\\cmake-build-debug//"
# 1 "C:/Users/Ivan/Desktop/Informatica/EcuNoLineales/main.f90"

program Autovalores
    use mod_ecunolineales
    use funciones

    real(8) :: a, b,tol = 0.01
    real(8) :: lim_inf = 1, lim_sup = 2


    call metodo_newton(f, 2d0, a, tol, 1000)
    call metodo_biseccion(f, lim_inf, lim_sup, b, tol, 1000)

    write(*,"(A, F10.4, A, F10.4, A, F10.4)") "NR:", a
    write(*,"(A, F10.4, A, F10.4, A, F10.4)") "B:", b

end program


