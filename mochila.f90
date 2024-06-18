program mochila

Implicit none

Integer :: cidades = 15, block = 3, max_it = 10
Integer :: i, j, it, k, counter
Integer :: ierror
Character(len=20) :: io_msg
Integer :: a(3)
Integer, allocatable :: Cities(:), aux(:,:), bestway(:)
Real, allocatable :: tabu_list(:), peso(:), valor(:)
Real :: sum, hold, custo, Pcomb, keep
Real :: P = 257

open(unit=1,file='mochila.txt',status='replace',action='write', iostat=ierror, iomsg=io_msg)

allocate(aux(cidades,3))
allocate(cities(cidades),bestway(cidades))
allocate(tabu_list(cidades),peso(cidades),valor(cidades))

tabu_list = 0
Cities = [1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
bestway = cities
Write(1,*) 'conjunto inicial:',cities
Peso = [63, 21, 2, 32, 13, 80, 19, 37, 56, 41, 14, 8, 32, 42, 7]
valor = [13, 2, 20, 10, 7, 14, 7, 2, 2, 4, 16, 17, 17, 3, 21]

custo = 0
Do i = 1, cidades
    custo = custo + valor(i)
End do
Write(1,*) 'Peso máximo:',custo 

it = 1
keep = 0
Do while (it <= max_it)
    Write(1,*) 'iteração:',it
    Do j = 1, 3
        Write(1,*) 'Movimento:',j
        !gerar movimentos aleatórios
        k = 1
        Do while (k == 1)
            a(j) = random(cities)
            if (tabu_list(a(j)) == 0) k = 0
        End do
        !vetor auxiliar como movimentação
        Do i = 1, size(cities)
            if (i == a(j)) then
                if (Cities(i) == 1) then
                    aux(i,j) = 0
                Else
                    aux(i,j) = 1
                End if
            Else
                Aux(i,j) = cities(i)
            End if
        End do
        Write(1,*) 'Sequência:',Aux(:,j)
        !Função de avaliação da solução
        sum = 0 
        Pcomb = 0
        Do i = 1, size(cities)
            sum = sum + aux(i,j)*valor(i)
            Pcomb = pcomb + aux(i,j)*peso(i)
        End do
        if (pcomb - P < 0) pcomb = 0
        sum = sum - custo * pcomb
        Write(1,*) 'Função:',sum
        !Adoção do maior valor
        if (j == 1) then
            hold = sum
            counter = 1
        Else if (sum > hold) then
            hold = sum
            counter = j
        End if
    End do
    !Atualização do caminho
    Do i = 1, size(cities)
        cities(i) = aux(i,counter)
    End do
    if (hold > keep) then
        keep = hold
        bestway = cities
    End if
    Write(1,*) 'conjunto ótimo:',cities
    !Atualizar bloqueios da lista
    Do i = 1, cidades
        if (tabu_list(i) > 0) tabu_list(i) = tabu_list(i) - 1
    End do
    !Adicionar movimento bloqueado
    tabu_list(a(counter)) = block
    !Atualizar iteração
    it = it + 1
End do
Write(1,*) 'Melhor sequência:',bestway


contains

function random(m) result(j)

Real :: u
Integer :: m(:)
Integer :: j

call random_number(u)
j = 1 + floor((size(m)-1)*u)

End Function 


End program mochila