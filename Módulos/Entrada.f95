module Entrada
    implicit none
    private
    public :: Input, load

    type Input
        integer :: larg, alt ! Largura e altura do lago
        integer :: L, H      ! Largura e altura da matriz
        real    :: T         ! Tempo (virtual) de simulação
        real    :: v         ! Velocidade de propagação da onda
        real    :: eps       ! Limiar de altura
        integer :: Niter     ! Número de iterações
        real    :: P         ! Propabilidade de surgimento de uma gota
        integer :: s         ! Semente para o gerador aleatório
    end type Input
contains
    function load(filename) result(in)
        character(len=*), intent(in) :: filename
        type(Input)                  :: in

        integer            :: ioerr
        character(len=300) :: line
        integer            :: lp, c, rp !

        open(1, file = filename, status = 'old', iostat = ioerr)
        if (ioerr /= 0) stop "Não foi possível abrir o arquivo de entrada!"

        ! Lê a primeira linha para um buffer
        read(1, '(A)') line

        ! Acha a posição dos delimitadores
        lp = scan(line, "(")
        c  = scan(line, ",")
        rp = scan(line, ")")

        ! Escreve os valores para as variáveis
        read(line(lp+1:c-1), *) in%larg
        read(line(c+1:rp-1), *) in%alt

        ! Lê a segunda linha para um buffer
        read(1, '(A)') line

        ! Acha a posição dos delimitadores
        lp = scan(line, "(")
        c  = scan(line, ",")
        rp = scan(line, ")")

        ! Escreve os valores para as variáveis
        read(line(lp+1:c-1), *) in%L
        read(line(c+1:rp-1), *) in%H

        ! Lê o restante do arquivo
        read(1, *) in%T
        read(1, *) in%v
        read(1, *) in%eps
        read(1, *) in%Niter
        read(1, *) in%P
        read(1, *) in%s
        
        close(1)
    end function load
end module Entrada
