#!/bin/bash

# Formato da saída do time (só o real)
TIMEFORMAT="%E"

branches=("master" "write_on_last" "last_only")
inputs=(Entradas/*)

# Cabeçalho
printf "%-30s" "Entrada"
for branch in "${branches[@]}"
do
    printf "%-15s" $branch
done
printf "\n"

for input in "${inputs[@]}"
do
    printf "%-30s" $input
    for branch in "${branches[@]}"
    do
        # Muda de Branch
        git checkout $branch --quiet

        # Deleta o executável pra forçar compilação
        rm -rf Ondas.out

        # Compila e descarta a saída pra não poluir
        make >/dev/null

        # Ficou feio, mas precisei pra poder controlar a saída do time,
        # que geralmente é stderr, então pra usar no printf tem que
        # redirecionar pra stdout (2>&1)
        printf "%-15s" "$({ time ./Ondas.out $input; } 2>&1)"
    done
    printf "\n"
done
