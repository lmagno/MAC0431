module Saida
    implicit none

contains
    subroutine save(mapa)
        real, intent(in) :: mapa(:, :)

        integer :: m, n
        integer :: i, j
        integer :: r, g, b
        real    :: h, hmax, hmin, delta

        open(2, file = "saida.ppm", status = 'replace')
        ! Formato da imagem
        write(2, '(A)') "P3"

        m = size(mapa, 1) ! Número de linhas
        n = size(mapa, 2) ! Número de colunas

        ! Dimensões da imagem
        write(2, '(i0, 1x, i0)') n, m

        ! Valor máximo de intensidade
        write(2, '(i0)') 255

        ! Altura e profundidade máximas
        hmax = maxval(mapa)
        hmin = minval(mapa)

        ! Altura máxima em módulo
        if (hmax < 0.0) then
            delta = max(hmax, -1*hmin)
        else
            delta = hmax
        end if

        ! Escreve a cor de cada pixel pro arquivo
        do i = 1, m
            do j = 1, n
                h = mapa(i, j)
                if (h > 0) then
                    r = 0
                    g = 0
                    b = int(255*h/delta)
                else
                    r = int(255*(-1*h)/delta)
                    g = 0
                    b = 0
                end if

                write(2, '(1x, i0, 1x, i0, 1x, i0, $)') r, g, b
            end do
            write(2, '(1x)')
        end do

        close(2)
    end subroutine save
end module Saida
