program Ondas
    use Entrada, only: Input, load
    use Saida,   only: save, save_stats
    implicit none

    type(Input) :: in
    character(len=80) :: filename
    real, allocatable :: mapa(:, :), S(:, :), Q(:, :)
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
    integer :: seed      ! Semente para o gerador aleatório

    ! Carrega os parâmetros de entrada
    call get_command_argument(1, filename)
    in = load(filename)

    larg  = real(in%larg)
    alt   = real(in%alt)
    L     = in%L
    H     = in%H
    T     = in%T
    v     = in%v
    eps   = in%eps
    Niter = in%Niter
    P     = in%P
    seed  = in%s

    ! Define a semente do gerador de números aleatórios
    call random_seed(seed)

    ! Aloca os arrays necessários
    allocate(mapa(H, L))
    allocate(S(H, L))
    allocate(Q(H, L))
    allocate(gx(Niter))
    allocate(gy(Niter))
    allocate(gt(Niter))

    gotas = 0

    ! Razões entre as dimensões do lago e as da matriz
    rx = alt/H
    ry = larg/L

    S(:, :) = 0.0
    Q(:, :) = 0.0

    ! Passo
    timestep = T/Niter
    do n = 1, Niter
        mapa(:, :) = 0.0
        time = n*timestep
        do k = 1, gotas

            ! Só considera gotas cuja onda ainda está no lago
            dt = time - gt(k)
            if (dt > sqrt(alt*alt + larg*larg)/v) then
                cycle
            end if

            !$omp parallel do default(shared) private(i,dx2,dy2,dr,d,ht)
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

                        ! Mantém a soma das alturas e dos seus quadrados
                        ! para o cálculo da média e do desvio padrão depois
                        S(i, j) = S(i, j) + ht
                        Q(i, j) = Q(i, j) + ht*ht
                    end if
                end do
            end do
            !$omp end parallel do
        end do

        ! Sorteia gotas
        if (rand() < P/100) then
            gotas = gotas + 1
            gx(gotas) = rand()*alt
            gy(gotas) = rand()*larg
            gt(gotas) = time
        end if
    end do


    call save(mapa)
    call save_stats(S, Q, in)
    deallocate(mapa)
    deallocate(S)
    deallocate(Q)
    deallocate(gx)
    deallocate(gy)
    deallocate(gt)
end program Ondas
