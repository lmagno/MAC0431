module Altura
    implicit none
contains
    function h(rho, t, v)
        real             :: h
        real, intent(in) :: rho, t, v

        real :: d
        d = rho - v*t
        h = d*exp(-d*d)*exp(-t/10)
    end function h
end module Altura

program Ondas
    use Entrada, only: Input, load
    use Saida,   only: save
    use Altura,  only: h
    implicit none

    type(Input) :: in
    real, allocatable :: mapa(:, :)
    real, allocatable :: gx(:), gy(:), gt(:)
    integer :: i, j, n, k, gotas
    real :: dx, dy, rho, t
    real :: rx, ry, timestep, ht

    ! Carrega os parâmetros de entrada
    in = load("entrada")

    ! Define a semente do gerador de números aleatórios
    call random_seed(in%s)

    ! Aloca os arrays necessários
    allocate(mapa(in%H, in%L))
    allocate(gx(in%Niter))
    allocate(gy(in%Niter))
    allocate(gt(in%Niter))

    gotas = 0
    rx = in%alt/in%H
    ry = in%larg/in%L
    timestep = in%T/in%Niter
    do n = 1, in%Niter
        do k = 1, gotas
            t = n*timestep - gt(k)
            if (t > sqrt(real(in%alt*in%alt + in%larg*in%larg))/in%v) then
                continue
            end if
            do j = 1, in%L
                do i = 1, in%H
                    ht = 0.0

                    dx  = i*rx - gx(k)
                    dy  = j*ry - gy(k)
                    rho = sqrt(dx*dx + dy*dy)

                    mapa(i, j) = mapa(i, j) + h(rho, t, in%v)
                end do
            end do
        end do

        if (rand() < in%P/100) then
            gotas = gotas + 1
            gx(gotas) = rand()*in%alt
            gy(gotas) = rand()*in%larg
            gt(gotas) = n*timestep
        end if
    end do
    call save(mapa)
    deallocate(mapa)
    deallocate(gx)
    deallocate(gy)
    deallocate(gt)
end program Ondas
