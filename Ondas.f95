program Ondas
    use Entrada, only: Input, load
    use Saida,   only: save
    implicit none

    type(Input) :: in
    real, allocatable :: mapa(:, :)
    real, allocatable :: gx(:), gy(:), gt(:)
    integer :: i, j, n, k, gotas
    real :: dx2, dy2, dr, dt, d
    real :: rx, ry, timestep, time, ht

    ! Parâmetros de entrada
    real    :: larg, alt ! Largura e altura do lago
    integer :: L, H      ! Largura e altura da matriz
    real    :: T         ! Tempo (virtual) de simulação
    real    :: v         ! Velocidade de propagação da onda
    real    :: eps       ! Limiar de altura
    integer :: Niter     ! Número de iterações
    real    :: P         ! Propabilidade de surgimento de uma gota
    integer :: s         ! Semente para o gerador aleatório

    ! Carrega os parâmetros de entrada
    in = load("entrada")

    larg  = real(in%larg)
    alt   = real(in%alt)
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
    mapa(:, :) = 0.0

    ! Razões entre as dimensões do lago e as da matriz
    rx = alt/H
    ry = larg/L

    ! Tempo por iteração
    timestep = T/Niter

    ! Gera as gotas
    do n = 1, Niter
        time = n*timestep
        if (rand() < P/100) then
            gotas = gotas + 1
            gx(gotas) = rand()*alt
            gy(gotas) = rand()*larg
            gt(gotas) = time
        end if
    end do
    
    ! Desenha as ondas na última iteração
    do k = 1, gotas
        ! Só considera gotas cuja onda ainda está no lago
        dt = time - gt(k)
        if (dt > sqrt(alt*alt + larg*larg)/v) then
            cycle
        end if

        do j = 1, L
            dy2 = (j*ry - gy(k))**2
            do i = 1, H
                dx2  = (i*rx - gx(k))**2
                dr = sqrt(dx2 + dy2)

                ! Só considera contribuições não desprezíveis
                d = dr - v*dt
                ht = d*exp(-d*d-(dt/10))
                if (abs(ht) > eps) then
                    mapa(i, j) = mapa(i, j) + ht
                end if
            end do
        end do
    end do

    call save(mapa)
    deallocate(mapa)
    deallocate(gx)
    deallocate(gy)
    deallocate(gt)
end program Ondas
