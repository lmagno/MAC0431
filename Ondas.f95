program Ondas
    use Entrada, only: Input, load
    use Saida
    implicit none

    type(Input) :: in
    real, allocatable :: mapa(:, :)
    integer :: i, j

    in = load("entrada")
    allocate(mapa(in%H, in%L))

    do j = 1, in%L
        do i = 1, in%H
            mapa(i, j) = (2*rand() - 1)*255
        end do
    end do

    call save(mapa)
    deallocate(mapa)
end program Ondas
