
program Autovalores
    use mod_derivacion
    use funciones

    real(8) :: x_0, x, h
    h = 0.01 !incremento

    do i=1,10
        write(*,*) f(i*1d0), d1_centrada(f,i*1d0, h),d1_regresiva(f,i*1d0, h), d1_progresiva(f,i*1d0, h)
        write(*,*) "Segunda", d2_centrada(f,i*1d0, h)
    end do


end program


