# 1 "C:/Users/Ivan/Desktop/Informatica/Integrales/main.f90"
# 1 "<built-in>"
# 1 "<command-line>"
# 1 "C:\\Users\\Ivan\\Desktop\\Informatica\\Integrales\\cmake-build-debug//"
# 1 "C:/Users/Ivan/Desktop/Informatica/Integrales/main.f90"

program Autovalores
    use mod_derivacion
    use funciones

    real(8) :: a,b,c
    real(8), parameter :: inicio = 0d0, fin = 4d0
    integer, parameter :: divisiones = 1000

   ! funcion = f
    ! a = area (salida)
    ! c,d = inciio/fin
    ! fragmentos
    call integracion_simpson(f, a, inicio,fin, divisiones)
    call integracion_trapecio(f, b, inicio,fin, divisiones)
    call integracion_sumariemman(f, c, inicio,fin, divisiones)

    write(*,"(A, F10.4, A, F10.4, A, F10.4)") "Simpson:", a, " Trapecio: ",b, " Riemann: ", c


end program


