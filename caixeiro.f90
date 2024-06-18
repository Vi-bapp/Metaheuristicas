!   Programa para resolução do problema do caixeiro viajante como proposto
!no exercício 9.1 da matéria de metaheurísticas

program caixeiro

Implicit none

Integer :: cidades = 10, block = 3, max_it = 15
Integer :: i, j, it, k, counter
Integer :: ierror
Character(len=20) :: io_msg
Integer :: a(3), b(3)
Integer, allocatable :: Cities(:), aux(:,:), bestway(:)
Real, allocatable :: tabu_list(:,:), dist(:,:)
Real :: sum, hold, keep

open(unit=1,file='caixeiro.txt',status='replace',action='write', iostat=ierror, iomsg=io_msg)

allocate(aux(cidades+1,3))
allocate(cities(cidades+1), bestway(cidades+1))
allocate(tabu_list(cidades,cidades), dist(cidades,cidades))

tabu_list = 0
Cities = [10,4,8,2,5,1,6,9,3,7,10]
Write(1,*) 'Caminho inicial:',cities
dist = reshape([0.0, 3.7, 6.0, 6.0, 7.02, 9.8, 9.68, 5.94, 5.91, 3.66, 3.7, &
 0.0, 3.76, 2.3, 3.66, 6.1, 7.0, 3.67, 5.88, 2.21, 6.0, 3.76, 0.0, 3.79, 6.01, &
 6.16, 9.71, 7.1, 9.65, 5.98, 6.0, 2.3, 3.79, 0.0, 2.21, 3.8, 5.91, 3.7, 6.94, &
 3.66, 7.02, 3.66, 6.01, 2.21, 0.0, 3.74, 3.7, 2.36, 5.94, 3.7, 9.8, 6.1, 6.16, &
 3.8, 3.74, 0.0, 5.91, 6.1, 9.68, 7.12, 9.68, 7.0, 9.71, 5.91, 3.7, 5.91, 0.0, &
 3.74, 6.0, 6.02, 5.94, 3.57, 7.1, 3.7, 2.36, 6.1, 3.74, 0.0, 3.58, 2.28, 5.91, &
 5.88, 9.65, 6.94, 5.94, 9.68, 6.0, 3.58, 0.0, 3.67, 3.66, 2.21, 5.98, 3.66, &
 3.7, 7.12, 6.02, 2.28, 3.67, 0.0],[cidades,cidades])
bestway = cities

it = 1
Do while (it <= max_it)
    Write(1,*) 'iteração:', it
    Do j = 1, 3
        Write(1,*) 'Movimento:', j
        !gerar movimentos aleatórios
        k = 1
        Do while (k == 1)
            a(j) = random(cities)
            b(j) = random(cities)
            if ((tabu_list(a(j),b(j)) == 0) .and. a(j) /= b(j) ) then
                k = 0
            End if
        End do
        Write(1,*) 'Substitui:',a(j),b(j)
        !vetor auxiliar como movimentação
        Do i = 1, size(cities)-1
            if (Cities(i) == a(j)) then
                aux(i,j) = b(j)
            Else if (cities(i) == b(j)) then
                aux(i,j) = a(j)
            Else
                Aux(i,j) = cities(i)
            End if
        End do
        Aux(cidades+1,j) = aux(1,j)
        !Distância entre cidades 
        sum = 0 
        Do i = 1, size(cities)-1
            sum = sum + dist(aux(i,j),aux(i+1,j))
        End do
        Write(1,*) 'Distância:',sum
        if (j == 1) then
            hold = sum
            counter = 1
        Else if (sum < hold) then
            hold = sum
            counter = j
        End if
        if (it == 1 .and. j == 1) keep = hold
    End do
    !Atualização do caminho
    Do i = 1, size(cities)
        cities(i) = aux(i,counter)
    End do
    Write(1,*) 'Caminho ótimo:',cities
    if (hold < keep) then
        keep = hold
        bestway = Cities
    End if
    !Atualizar bloqueios da lista
    Do i = 1, cidades
        Do j = 1, cidades
            if (tabu_list(i,j) > 0) tabu_list(i,j) = tabu_list(i,j) - 1
        End do
    End do
    !Adicionar movimento bloqueado
    tabu_list(a(counter),b(counter)) = block
    tabu_list(b(counter),a(counter)) = block
    !Atualizar iteração
    it = it + 1
End do
Write(1,*) 'Melhor caminho é:', bestway
Write(1,*) 'Distância a ser percorrida:', keep


contains

function random(m) result(j)

Real :: u
Integer :: m(:)
Integer :: j

call random_number(u)
j = 1 + floor((size(m)-1)*u)

End Function 


End program Caixeiro