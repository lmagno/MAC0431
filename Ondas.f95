module Altura
    implicit none
contains
    function height(rho, t, v)
        real             :: height
        real, intent(in) :: rho, t, v

        real :: d
        d = rho - v*t
        height = d*exp(-d*d)*exp(-t/10)
    end function height
end module Altura

program Ondas
    use Entrada, only: Input, load
    use Saida,   only: save
    use Altura,  only: height
    implicit none

    type(Input) :: in
    real, allocatable :: mapa(:, :)
    real, allocatable :: gx(:), gy(:), gt(:)
    integer :: i, j, n, k, gotas
    real :: dx, dy, dr, dt
    real :: rx, ry, timestep, ht

    ! Parâmetros de entrada
    integer :: larg, alt ! Largura e altura do lago
    integer :: L, H      ! Largura e altura da matriz
    real    :: T         ! Tempo (virtual) de simulação
    real    :: v         ! Velocidade de propagação da onda
    real    :: eps       ! Limiar de altura
    integer :: Niter     ! Número de iterações
    real    :: P         ! Propabilidade de surgimento de uma gota
    integer :: s         ! Semente para o gerador aleatório

    ! Carrega os parâmetros de entrada
    in = load("entrada")

    larg  = in%larg
    alt   = in%alt
    L     = in%L
    H     = in%H
    T     = in%T
    v     = in%v
    eps   = in%eps
    Niter = in%Niter
    P     = in%P
    s     = in%s

    ! Define a semente do gerador de números aleatórios
    call random_seed(s)

    ! Aloca os arrays necessários
    allocate(mapa(H, L))
    allocate(gx(Niter))
    allocate(gy(Niter))
    allocate(gt(Niter))

    gotas = 0

    ! Razões entre as dimensões do lago e as da matriz
    rx = alt/H
    ry = larg/L

    ! Passo
    timestep = T/Niter
    do n = 1, Niter
        mapa(:, :) = 0.0
        do k = 1, gotas

            ! Só considera gotas cuja onda ainda está no lago
            dt = n*timestep - gt(k)
            if (dt > sqrt(real(alt*alt + larg*larg))/v) then
                cycle
            end if

            do j = 1, L
                do i = 1, H
                    dx  = i*rx - gx(k)
                    dy  = j*ry - gy(k)
                    dr = sqrt(dx*dx + dy*dy)

                    ! Só considera contribuições não desprezíveis
                    ht = height(dr, dt, v)
                    if (abs(ht) > eps) then
                        mapa(i, j) = mapa(i, j) + ht
                    end if
                end do
            end do
        end do

        ! Sorteia gotas
        if (rand() < P/100) then
            gotas = gotas + 1
            gx(gotas) = rand()*alt
            gy(gotas) = rand()*larg
            gt(gotas) = n*timestep
        end if
    end do


    call save(mapa)
    deallocate(mapa)
    deallocate(gx)
    deallocate(gy)
    deallocate(gt)
end program Ondas
